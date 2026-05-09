gpa: Allocator,
pool: *StringPool,
errors: Io.Writer.Allocating,
constants: builtin.Constants,

root_dir: Io.Dir,
output_dir: TmpDir,
polka_dir: Io.Dir,

/// Reset after evaluating each template file.
ephemeral_value_allocator: std.heap.ArenaAllocator,
/// Lasts the entire duration of syncing, used for .polka files.
constant_value_allocator: std.heap.ArenaAllocator,

/// Temporary directory with random name that may delete itself
///
/// Copied from testing.tmpDir
const TmpDir = struct {
    dir: Io.Dir,
    sub_path: [sub_path_len]u8,

    const random_bytes_count = 12;
    const sub_path_len = std.base64.url_safe.Encoder.calcSize(random_bytes_count);

    pub fn init(io: Io, parent_dir: Io.Dir, opts: Io.Dir.OpenOptions) !TmpDir {
        var random_bytes: [random_bytes_count]u8 = undefined;
        io.random(&random_bytes);
        var sub_path: [sub_path_len]u8 = undefined;
        _ = std.base64.url_safe.Encoder.encode(&sub_path, &random_bytes);

        const dir = try parent_dir.createDirPathOpen(io, &sub_path, .{ .open_options = opts });
        return .{
            .dir = dir,
            .sub_path = sub_path,
        };
    }

    pub fn cleanup(self: *TmpDir, io: Io, parent_dir: Io.Dir) void {
        self.dir.close(io);
        parent_dir.deleteTree(io, &self.sub_path) catch {};
        self.* = undefined;
    }
};

const Runner = @This();

pub const config_file_name = "config.polka";
pub const polka_dir_name = ".polka" ++ path.sep_str;

pub fn init(io: Io, gpa: Allocator, pool: *StringPool) !Runner {
    var path_buf: [2048]u8 = undefined;
    var len: usize = 0;

    const constants: builtin.Constants = try .init(io, gpa, pool);
    errdefer constants.deinit(gpa);

    const cwd = try Io.Dir.cwd().openDir(io, ".", .{ .iterate = true });
    defer cwd.close(io);

    const root_dir = try findRootDirectory(io, cwd, .{});
    errdefer root_dir.close(io);
    len = try root_dir.realPath(io, &path_buf);
    std.debug.print("{s}\n", .{path_buf[0..len]});

    const polka_dir = try root_dir.openDir(io, polka_dir_name, .{});
    errdefer polka_dir.close(io);

    const output_dir: TmpDir = try .init(io, polka_dir, .{});
    errdefer output_dir.cleanup(io, polka_dir);

    return .{
        .gpa = gpa,
        .pool = pool,
        .errors = .init(gpa),
        .root_dir = root_dir,
        .polka_dir = polka_dir,
        .constants = constants,
        .output_dir = output_dir,
        .constant_value_allocator = .init(gpa),
        .ephemeral_value_allocator = .init(gpa),
    };
}

pub fn deinit(runner: *Runner, io: Io) void {
    runner.errors.deinit();
    runner.constant_value_allocator.deinit();
    runner.ephemeral_value_allocator.deinit();

    // Ordering of directory cleanup matters
    runner.output_dir.cleanup(io, runner.polka_dir);
    runner.polka_dir.close(io);
    runner.root_dir.close(io);

    runner.* = undefined;
}

/// Search upwards in the filesystem from starting_dir to find where the `.polka/` directory is
pub fn findRootDirectory(io: Io, starting_dir: Io.Dir, opts: Io.Dir.OpenOptions) !Io.Dir {
    var current = try starting_dir.openDir(io, ".", opts);
    errdefer current.close(io);

    while (true) {
        if (current.access(io, polka_dir_name, .{})) {
            return current;
        } else |err| switch (err) {
            error.FileNotFound => {
                const new = try current.openDir(io, "..", opts);
                errdefer new.close(io);

                // We reached '/'
                if ((try new.stat(io)).inode == (try current.stat(io)).inode)
                    return error.PolkaDirNotFound;

                current.close(io);
                current = new;
            },

            error.InputOutput,
            error.SystemResources,
            error.AccessDenied,
            error.Unexpected,
            error.PermissionDenied,
            error.FileBusy,
            error.SymLinkLoop,
            error.ReadOnlyFileSystem,
            error.NameTooLong,
            error.BadPathName,
            => return err,

            error.Canceled => unreachable,
        }
    }
}

pub fn sync(runner: *Runner, io: Io) !void {
    var config: PolkaConfig = .init(runner.gpa, runner.pool);
    defer config.deinit();

    try runner.syncDir(io, &config, runner.root_dir, "");
}

fn syncDir(
    runner: *Runner,
    io: Io,
    starting_config: *PolkaConfig,
    dir: Io.Dir,
    dirname: []const u8,
) !void {
    var new_config: ?PolkaConfig = config: {
        const file = dir.openFile(io, config_file_name, .{}) catch {
            // TODO log the error
            break :config null;
        };

        var new_config = starting_config.copy();
        try runner.runFile(io, &new_config, .{
            .handle = file,
            .basename = config_file_name,
            .dirname = dirname,
        });
        break :config new_config;
    };
    defer if (new_config) |_| new_config.?.deinit();
    const config = new_config orelse starting_config.*;
    _ = config;

    var iter = dir.iterate();
    while (true) {
        const entry = iter.next(io) catch break orelse break;
        std.debug.print("{s}", .{entry.name});
    }
}

const FileOpts = struct {
    handle: Io.File,
    basename: []const u8,
    dirname: []const u8,
};
fn runFile(
    runner: *Runner,
    io: Io,
    config: *PolkaConfig,
    file: FileOpts,
) !void {
    const gpa = runner.gpa;
    _ = runner.ephemeral_value_allocator.reset(.retain_capacity);

    const mode, var value_allocator = if (std.mem.endsWith(u8, file.basename, ".spot"))
        .{ ParseMode.code_file, runner.constant_value_allocator }
    else
        .{ ParseMode.text, runner.ephemeral_value_allocator };

    var file_arena: std.heap.ArenaAllocator = .init(gpa);
    defer file_arena.deinit();

    var src_buffer: [2048]u8 = undefined;
    var src_reader = file.handle.reader(io, &src_buffer);
    const src = try src_reader.interface.readAlloc(
        file_arena.allocator(),
        try file.handle.length(io),
    );

    // TODO we can probably use the file_arena for parsing; we just need to preallocate parser.stack
    // with like ~4096 entries.
    const parsed = try parser.parse(src, mode, gpa);
    if (parsed.errors.len == 0) {
        try runner.printSyntaxErrors(parsed.errors, file.basename);
        return;
    }
    defer parsed.deinit(gpa);

    var out: Io.Writer.Allocating = .init(file_arena.allocator());

    var vm: Vm = try .init(gpa, .{
        .src = src,
        .config = config,
        .output = &out.writer,
        .nodes = parsed.nodes,
        .string_pool = runner.pool,
        .constants = runner.constants,
        .value_allocator = value_allocator.allocator(),
    });
    defer vm.deinit(gpa);

    const err = vm.run();
    if (err) |_| {
        try runner.printRuntimeError(src, parsed.nodes, file.basename, err.?);
        return;
    }
}

pub fn printRuntimeError(
    runner: *Runner,
    src: []const u8,
    nodes: []const SyntaxNode,
    file_name: []const u8,
    err: Vm.RuntimeErrorPayload,
) !void {
    // TODO make this better
    try runner.errors.writer.print("Runtime error: {s}:{}; {f}", .{
        file_name,
        err.index,
        err.formatWith(nodes, src, runner.pool),
    });
}

pub fn printSyntaxErrors(
    runner: *Runner,
    errors: []const parser.SyntaxError,
    file_name: []const u8,
) !void {
    // TODO make this better.
    for (errors) |err| {
        try runner.errors.writer.print("Syntax Error: {s}:{d}:{d}: {}\n{s}", .{
            file_name, err.position.line, err.position.col, err.kind, err.range,
        });
    }
}

const std = @import("std");
const Io = std.Io;
const Allocator = std.mem.Allocator;
const path = Io.Dir.path;

const lang = @import("lang.zig");
const builtin = lang.builtin;
const Value = lang.Value;
const polka = @import("polka.zig");
const PolkaConfig = polka.Config;
const StringPool = Value.String.Pool;
const parser = lang.parser;
const ParseMode = parser.ParseMode;
const Vm = lang.Vm;
const SyntaxNode = lang.SyntaxNode;
