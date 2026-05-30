gpa: Allocator,
pool: *StringPool,
/// Retrieved from Envron.Map when .init is called and is never modified after that.
home_path: String,
/// Directory opened up to home_path
home_dir: Io.Dir,

error_log: Io.Writer.Allocating,

constants: builtin.Constants,

/// Initialized as relative path from cwd to root_dir, used for keeping track of file names for
/// logging/printing.
src_path: PathBuf,
/// Directory containing the .polka directory. Typically the .dotfiles directory, or wherever you
/// store your version controlled dotfiles.
root_dir: Io.Dir,
/// The directory located at `root_dir/.polka`
polka_dir: Io.Dir,

vfs: VirtualFilesystem,
tmp: VirtualFilesystem.TmpDir,

/// Reset after evaluating each template file.
ephemeral_value_allocator: std.heap.ArenaAllocator,
/// Lasts the entire duration of syncing, used for .polka files.
constant_value_allocator: std.heap.ArenaAllocator,

const Runner = @This();

pub const language_filetype = ".polka";
pub const config_file_name = "config" ++ language_filetype;
pub const polka_dir_name = ".polka";

pub fn init(io: Io, gpa: Allocator, env: *std.process.Environ.Map, pool: *StringPool) !Runner {
    const cwd = try Io.Dir.cwd().openDir(io, ".", .{});
    defer cwd.close(io);

    var path_buf: PathBuf = .empty;
    errdefer path_buf.deinit(gpa);

    const root_dir = try findRootDirectory(io, gpa, cwd, &path_buf, .{ .iterate = true });
    errdefer root_dir.close(io);

    const polka_dir = try root_dir.openDir(io, polka_dir_name, .{});
    errdefer polka_dir.close(io);
    {
        const m = try path_buf.enterDir(gpa, polka_dir_name);
        defer path_buf.exit(m);
        std.log.info("Found .polka directory at {s}", .{path_buf.path()});
    }

    var constants: builtin.Constants = try .init(io, gpa, pool);
    errdefer constants.deinit(gpa);

    const home_path = env.get("HOME") orelse return error.HomeDirectoryNotFound;
    const home_path_string = try pool.put(home_path);
    const home_dir = try Io.Dir.openDirAbsolute(io, home_path, .{});
    errdefer home_dir.close(io);

    var tmp: VirtualFilesystem.TmpDir = try .init(io, polka_dir, .{});
    errdefer tmp.deinit(io, polka_dir);

    const virtual_filesystem: VirtualFilesystem = try .init(gpa, pool, home_path);
    errdefer virtual_filesystem.deinit();

    return .{
        .tmp = tmp,
        .gpa = gpa,
        .pool = pool,
        .root_dir = root_dir,
        .home_dir = home_dir,
        .src_path = path_buf,
        .polka_dir = polka_dir,
        .constants = constants,
        .error_log = .init(gpa),
        .vfs = virtual_filesystem,
        .home_path = home_path_string,
        .constant_value_allocator = .init(gpa),
        .ephemeral_value_allocator = .init(gpa),
    };
}

pub fn deinit(runner: *Runner, io: Io) void {
    runner.constants.deinit(runner.gpa);
    runner.error_log.deinit();
    runner.constant_value_allocator.deinit();
    runner.ephemeral_value_allocator.deinit();

    runner.tmp.deinit(io, runner.polka_dir);
    runner.polka_dir.close(io);
    runner.root_dir.close(io);

    runner.src_path.deinit(runner.gpa);
    runner.vfs.deinit(io);

    runner.* = undefined;
}

/// Search upwards in the filesystem from starting_dir to find where the `.polka/` directory is
pub fn findRootDirectory(
    io: Io,
    gpa: Allocator,
    starting_dir: Io.Dir,
    path_buf: *PathBuf,
    opts: Io.Dir.OpenOptions,
) !Io.Dir {
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
                _ = try path_buf.enterDir(gpa, parent_path);

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
    var config: PolkaConfig = .init(runner.gpa, runner.pool, runner.home_path);
    defer config.deinit();

    const starting_dir = try runner.root_dir.openDir(io, ".", .{ .iterate = true });
    defer starting_dir.close(io);

    var file_arena: std.heap.ArenaAllocator = .init(runner.gpa);
    try runner.syncDir(io, &file_arena, &config, starting_dir);
}

const SyncDirError = error{} || Allocator.Error;

fn syncDir(
    runner: *Runner,
    io: Io,
    file_arena: *std.heap.ArenaAllocator,
    starting_config: *const PolkaConfig,
    start_dir: Io.Dir,
) SyncDirError!void {
    const vfs = &runner.vfs;
    // Get the config
    const new_config: ?PolkaConfig = config: {
        const src_m = try runner.src_path.enterFile(runner.gpa, config_file_name);
        defer runner.src_path.exit(src_m);

        const vfs_m = try vfs.cwd.enterFile(runner.gpa, config_file_name);
        defer vfs.cwd.exit(vfs_m);

        const config_file = start_dir.openFile(io, config_file_name, .{}) catch {
            // TODO log the error
            break :config null;
        };
        defer config_file.close(io);

        var config = starting_config.copy();
        runner.syncFile(io, &config, config_file) catch |e| switch (e) {
            error.ParseError, error.RuntimeError => {
                std.log.err("Error in {s}, skipping directory", .{runner.src_path.path()});
                return;
            },

            inline else => |err| {
                std.log.err(
                    "Could not execute config file {s} due to {t}",
                    .{ runner.src_path.path(), err },
                );
            },
        };

        break :config config;
    };
    var config = new_config orelse starting_config.copy();
    defer config.deinit();
    const cwd_marker = if (config.isModified(.destination_path)) blk: {
        const m = try vfs.cwd.new(runner.gpa);
        _ = try vfs.cwd.enterDir(vfs.gpa, runner.pool.get(config.destination_path));
        break :blk m;
    } else null;
    defer if (cwd_marker) |marker| vfs.cwd.delete(marker);
    _ = try vfs.addCwdDir(try runner.pool.put(runner.src_path.dirname()));

    var iter = start_dir.iterate();
    while (iter.next(io) catch null) |entry| {
        runner.syncDirEntry(io, file_arena, &config, start_dir, entry) catch {
            // TODO handle and log error
            continue;
        };
    }
}

fn syncDirEntry(
    runner: *Runner,
    io: Io,
    file_arena: *std.heap.ArenaAllocator,
    config: *PolkaConfig,
    start_dir: Io.Dir,
    entry: Io.Dir.Entry,
) !void {
    const vfs = &runner.vfs;
    switch (entry.kind) {
        .directory => {
            if (std.mem.eql(u8, entry.name, polka_dir_name)) return;

            const src_path_m = try runner.src_path.enterDir(runner.gpa, entry.name);
            const vfs_m2 = try vfs.cwd.enterDir(vfs.gpa, entry.name);
            defer vfs.cwd.exit(vfs_m2);
            defer runner.src_path.exit(src_path_m);
            _ = try vfs.addCwdDir(try runner.pool.put(runner.src_path.dirname()));

            const dir = try start_dir.openDir(io, entry.name, .{ .iterate = true });
            defer dir.close(io);
            try runner.syncDir(io, file_arena, config, dir);
        },
        .file => {
            if (std.mem.eql(u8, entry.name, config_file_name)) return;
            std.log.err("{s}", .{entry.name});

            const src_path_m = try runner.src_path.enterFile(runner.gpa, entry.name);
            defer runner.src_path.exit(src_path_m);
            const cwd_m = try runner.vfs.cwd.enterFile(vfs.gpa, entry.name);
            defer vfs.cwd.exit(cwd_m);

            const file = try start_dir.openFile(io, entry.name, .{});
            defer file.close(io);
            runner.syncFile(io, config, file) catch |e| switch (e) {
                error.ParseError, error.RuntimeError => {
                    std.log.err("Error in {s}, skipping", .{runner.src_path.path()});
                    return;
                },
                inline else => |err| return err,
            };
        },
        else => {},
    }
}

fn syncFile(
    runner: *Runner,
    io: Io,
    config: *PolkaConfig,
    src_file: Io.File,
) !void {
    const gpa = runner.gpa;
    const vfs = &runner.vfs;
    _ = runner.ephemeral_value_allocator.reset(.retain_capacity);

    const ext = Io.Dir.path.extension(runner.src_path.basename() orelse unreachable);
    const Status = struct {
        parse_mode: ParseMode,
        value_allocator: Allocator,
        kind: enum { template, polka },
    };
    const status: Status = if (std.mem.eql(u8, ext, ".polka")) .{
        .kind = .polka,
        .parse_mode = .code_file,
        .value_allocator = runner.constant_value_allocator.allocator(),
    } else .{
        .kind = .template,
        .parse_mode = .text,
        .value_allocator = runner.ephemeral_value_allocator.allocator(),
    };

    var src_buffer: [2048]u8 = undefined;
    var src_reader = src_file.reader(io, &src_buffer);
    const src = try src_reader.interface.readAlloc(gpa, try src_file.length(io));
    defer gpa.free(src);

    std.log.info("syncing file {s}", .{runner.src_path.path()});

    const parsed = try parser.parse(src, status.parse_mode, gpa);
    defer parsed.deinit(gpa);
    if (parsed.errors.len != 0) {
        try runner.logSyntaxErrors(parsed.errors, runner.src_path.path());
        return error.ParseError;
    }

    // create output file
    const Out = struct {
        writer: *Io.Writer,
        buf: [2048]u8,
        kind: union(enum) {
            discarding: Io.Writer.Discarding,
            tmp: struct {
                file: Io.File,
                writer: Io.File.Writer,
            },
        },
    };
    var out: Out = undefined;
    if (status.kind == .polka) {
        out.kind = .{ .discarding = .init(&out.buf) };
        out.writer = &out.kind.discarding.writer;
    } else {
        out.kind = .{ .tmp = .{
            .file = try runner.tmp.createFile(io, .{}),
            .writer = out.kind.tmp.file.writer(io, &out.buf),
        } };
        out.writer = &out.kind.tmp.writer.interface;
    }
    errdefer if (out.kind == .tmp) out.kind.tmp.file.close(io);

    var vm: Vm = try .init(gpa, .{
        .src = src,
        .config = config,
        .output = out.writer,
        .nodes = parsed.nodes,
        .string_pool = runner.pool,
        .constants = runner.constants,
        .value_allocator = status.value_allocator,
    });
    defer vm.deinit(gpa);

    const result = vm.run();
    if (result == .err) {
        try runner.logRuntimeError(src, parsed.nodes, runner.src_path.path(), result.err);
        return error.RuntimeError;
    }

    const m = blk: {
        if (!config.isModified(.destination_path)) break :blk null;
        const m = try vfs.cwd.new(runner.gpa);
        _ = try vfs.cwd.enterDir(vfs.gpa, runner.pool.get(config.destination_path));
        _ = try vfs.cwd.enterFile(vfs.gpa, runner.src_path.basename() orelse unreachable);
        const source_path = try runner.pool.put(runner.src_path.dirname());
        _ = try vfs.addCwdDir(source_path);
        break :blk m;
    } orelse null;
    defer if (m) |marker| vfs.cwd.delete(marker);

    const file_status: VirtualFilesystem.File.Status = if (status.kind == .polka)
        .weak_link
    else switch (result.output.status) {
        .text => .symlinked,
        .templated => .{ .templated = .{ .content = out.kind.tmp.file } },
    };
    const source_path = try runner.pool.put(runner.src_path.path());
    _ = try vfs.addCwdFile(source_path, file_status);
}

pub fn logRuntimeError(
    runner: *Runner,
    src: []const u8,
    nodes: []const SyntaxNode,
    file_name: []const u8,
    err: Vm.RuntimeErrorPayload,
) !void {
    // TODO make this better
    try runner.error_log.writer.print("Runtime error: {s}:{}; {f}", .{
        file_name,
        err.index,
        err.formatWith(nodes, src, runner.pool),
    });
}

pub fn logSyntaxErrors(
    runner: *Runner,
    errors: []const parser.SyntaxError,
    file_name: []const u8,
) !void {
    // TODO make this better.
    for (errors) |err| {
        try runner.error_log.writer.print("Syntax Error: {s}:{d}:{d}: {}\n{s}", .{
            file_name, err.position.line, err.position.col, err.kind, err.range,
        });
    }
}

const std = @import("std");
const Io = std.Io;
const Allocator = std.mem.Allocator;
const path_sep = Io.Dir.path.sep;

const lang = @import("lang.zig");
const builtin = lang.builtin;
const Value = lang.Value;
const String = Value.String;
const StringPool = String.Pool;
const parser = lang.parser;
const ParseMode = parser.ParseMode;
const Vm = lang.Vm;
const SyntaxNode = lang.SyntaxNode;
const polka = @import("polka.zig");
const PolkaConfig = polka.Config;
const VirtualFilesystem = @import("VirtualFilesystem.zig");
const PathBuf = VirtualFilesystem.PathBuf;
