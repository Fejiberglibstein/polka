gpa: Allocator,
pool: *StringPool,
errors: Io.Writer.Allocating,
constants: builtin.Constants,

polka_dir: Io.Dir,
/// Relative to .root_dir
root_path: VirtualFilesystem.PathBuf,
root_dir: Io.Dir,
home_path: []const u8,
home_dir: Io.Dir,
output_dir: VirtualFilesystem.TmpDir,
vfs: VirtualFilesystem,

/// Reset after evaluating each template file.
ephemeral_value_allocator: std.heap.ArenaAllocator,
/// Lasts the entire duration of syncing, used for .polka files.
constant_value_allocator: std.heap.ArenaAllocator,

const Runner = @This();

const path_sep = Io.Dir.path.sep;
pub const language_filetype = ".polka";
pub const config_file_name = "config" ++ language_filetype;
pub const polka_dir_name = ".polka";

pub fn init(io: Io, gpa: Allocator, environ_map: *std.process.Environ.Map, pool: *StringPool) !Runner {
    const cwd = try Io.Dir.cwd().openDir(io, ".", .{});
    defer cwd.close(io);

    var path_buf: VirtualFilesystem.PathBuf = .init(gpa);
    errdefer path_buf.deinit();

    const root_dir = try findRootDirectory(io, cwd, &path_buf, .{ .iterate = true });
    errdefer root_dir.close(io);

    const polka_dir = try root_dir.openDir(io, polka_dir_name, .{});
    errdefer polka_dir.close(io);
    {
        const m = try path_buf.visitDir(polka_dir_name);
        defer path_buf.exit(m);
        std.log.info("Found .polka directory at {s}", .{path_buf.path()});
    }

    var output_dir: VirtualFilesystem.TmpDir = try .init(io, polka_dir, .{});
    errdefer output_dir.cleanup(io, polka_dir);

    var constants: builtin.Constants = try .init(io, gpa, pool);
    errdefer constants.deinit(gpa);

    const home_path = environ_map.get("HOME") orelse return error.HomeDirectoryNotFound;
    const string_home_path = try pool.put(home_path);
    const home_dir = try Io.Dir.openDirAbsolute(io, home_path, .{});
    errdefer home_dir.close(io);

    const virtual_filesystem: VirtualFilesystem = try .init(gpa, pool, string_home_path);
    errdefer virtual_filesystem.deinit();

    return .{
        .gpa = gpa,
        .pool = pool,
        .errors = .init(gpa),
        .root_dir = root_dir,
        .home_dir = home_dir,
        .root_path = path_buf,
        .polka_dir = polka_dir,
        .constants = constants,
        .home_path = home_path,
        .output_dir = output_dir,
        .vfs = virtual_filesystem,
        .constant_value_allocator = .init(gpa),
        .ephemeral_value_allocator = .init(gpa),
    };
}

pub fn deinit(runner: *Runner, io: Io, gpa: std.mem.Allocator) void {
    runner.constants.deinit(gpa);
    runner.errors.deinit();
    runner.constant_value_allocator.deinit();
    runner.ephemeral_value_allocator.deinit();

    // runner.output_dir.cleanup(io, runner.polka_dir);
    runner.polka_dir.close(io);
    runner.root_dir.close(io);

    runner.root_path.deinit();

    runner.* = undefined;
}

/// Search upwards in the filesystem from starting_dir to find where the `.polka/` directory is
pub fn findRootDirectory(io: Io, starting_dir: Io.Dir, path_buf: *VirtualFilesystem.PathBuf, opts: Io.Dir.OpenOptions) !Io.Dir {
    var current = try starting_dir.openDir(io, ".", opts);
    errdefer current.close(io);

    while (true) {
        if (current.access(io, polka_dir_name, .{})) {
            return current;
        } else |err| switch (err) {
            error.FileNotFound => {
                const new = try current.openDir(io, "..", opts);
                errdefer new.close(io);

                const parent_path: []const u8 = ".." ++ .{path_sep};
                _ = try path_buf.visitDir(parent_path);

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

    try runner.syncDir(io, &config, runner.root_dir, runner.output_dir.dir);
}

fn syncDir(
    runner: *Runner,
    io: Io,
    starting_config: *PolkaConfig,
    start_dir: Io.Dir,
    dest_dir: Io.Dir,
) !void {
    var discarding = Io.Writer.Discarding.init(&.{});

    var new_config: ?PolkaConfig = config: {
        const config_file = start_dir.openFile(io, config_file_name, .{}) catch {
            // TODO log the error
            break :config null;
        };

        const m = try runner.root_path.visitFile(config_file_name);
        defer runner.root_path.exit(m);

        var new_config = starting_config.copy();

        runner.runFile(io, &new_config, config_file, &discarding.writer, .polka) catch |e| switch (e) {
            error.ParseError, error.RuntimeError => {
                std.log.err("Error while parsing {s}, skipping directory", .{runner.root_path.path()});
                return;
            },
            inline else => |err| return err,
        };
        break :config new_config;
    };
    defer if (new_config) |_| new_config.?.deinit();
    var config = new_config orelse starting_config.*;

    var out_buf: [2048]u8 = undefined;
    var entries = start_dir.iterate();

    while (entries.next(io) catch null) |entry| {
        if (entry.kind == .directory) {
            if (std.mem.eql(u8, polka_dir_name, entry.name)) continue;
        }

        switch (entry.kind) {
            .directory => {
                const m = try runner.root_path.visitDir(entry.name);
                defer runner.root_path.exit(m);

                const new_start_dir = try start_dir.openDir(io, entry.name, .{ .iterate = true });
                defer new_start_dir.close(io);
                const new_dest_dir = try dest_dir.createDirPathOpen(io, entry.name, .{});
                defer new_dest_dir.close(io);
                try runner.syncDir(io, &config, new_start_dir, new_dest_dir);
            },
            .file => {
                const m = try runner.root_path.visitFile(entry.name);
                defer runner.root_path.exit(m);

                const src_file = try start_dir.openFile(io, entry.name, .{});
                defer src_file.close(io);
                const out_file = try dest_dir.createFile(io, entry.name, .{});
                defer out_file.close(io);
                var out_writer = out_file.writer(io, &out_buf);
                defer out_writer.flush() catch {}; // TODO handle error
                const file_kind = getFileKind(entry.name);
                const out = switch (file_kind) {
                    .template => &out_writer.interface,
                    .polka => &discarding.writer,
                };
                runner.runFile(io, &config, src_file, out, file_kind) catch |e| switch (e) {
                    error.ParseError, error.RuntimeError => {},
                    inline else => |err| return err,
                };
            },
            else => {},
        }
    }
}

const FileKind = enum { template, polka };
fn getFileKind(file_name: []const u8) FileKind {
    return if (std.mem.endsWith(u8, file_name, language_filetype)) .polka else .template;
}

fn runFile(runner: *Runner, io: Io, config: *PolkaConfig, file: Io.File, out: *Io.Writer, kind: FileKind) !void {
    const gpa = runner.gpa;
    _ = runner.ephemeral_value_allocator.reset(.retain_capacity);

    const mode, var value_allocator = switch (kind) {
        .polka => .{ ParseMode.code_file, runner.constant_value_allocator },
        .template => .{ ParseMode.text, runner.ephemeral_value_allocator },
    };

    var file_arena: std.heap.ArenaAllocator = .init(gpa);
    defer file_arena.deinit();

    var src_buffer: [2048]u8 = undefined;
    var src_reader = file.reader(io, &src_buffer);
    const src = try src_reader.interface.readAlloc(
        file_arena.allocator(),
        try file.length(io),
    );

    std.log.debug("{s}", .{runner.root_path.bytes.items});

    // TODO we can probably use the file_arena for parsing; we just need to preallocate parser.stack
    // with like ~4096 entries.
    const parsed = try parser.parse(src, mode, gpa);
    defer parsed.deinit(gpa);
    if (parsed.errors.len != 0) {
        try runner.printSyntaxErrors(parsed.errors, runner.root_path.path());
        return error.ParseError;
    }

    var vm: Vm = try .init(gpa, .{
        .src = src,
        .output = out,
        .config = config,
        .nodes = parsed.nodes,
        .string_pool = runner.pool,
        .constants = runner.constants,
        .value_allocator = value_allocator.allocator(),
    });
    defer vm.deinit(gpa);

    const err = vm.run();
    if (err) |_| {
        try runner.printRuntimeError(src, parsed.nodes, runner.root_path.path(), err.?);
        return error.RuntimeError;
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

const lang = @import("lang.zig");
const builtin = lang.builtin;
const Value = lang.Value;
const StringPool = Value.String.Pool;
const parser = lang.parser;
const ParseMode = parser.ParseMode;
const Vm = lang.Vm;
const SyntaxNode = lang.SyntaxNode;
const VirtualFilesystem = @import("VirtualFilesystem.zig");
const polka = @import("polka.zig");
const PolkaConfig = polka.Config;
