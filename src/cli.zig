/// Initialize `conf.polk` in the current directory
pub fn init() void {}

pub fn run(paths: []const []const u8, cwd: fs.Dir, cli_args: anytype, gpa: std.mem.Allocator) void {
    _ = cli_args;

    for (paths) |path| {
        runFile(path, cwd, gpa) catch |err| switch (err) {
            anyerror.IsDir => runDir(path, cwd, gpa) catch |dir_err|
                fatal("Error while accessing {s}: {s]}\n", .{ path, @errorName(dir_err) }),

            anyerror.RuntimeError => {},
            else => fatal("Error while accessing {s}: {s]}\n", .{ path, @errorName(err) }),
        };
    }
}

pub fn runFile(path: []const u8, cwd: fs.Dir, gpa: std.mem.Allocator) !void {
    const file = try cwd.openFile(path, .{});
    defer file.close();

    const stat = try file.stat();
    if (stat.kind == .directory)
        return error.IsDir;

    const file_text = try file.readToEndAlloc(gpa, std.math.maxInt(u64));
    defer gpa.free(file_text);

    const root_node, const nodes = try parser.parse(file_text, gpa);
    defer gpa.free(nodes);

    var vm = try Vm.init(gpa, nodes);
    defer vm.deinit();

    const result = vm.eval(&root_node) catch {
        std.debug.print(
            \\polka: Error while executing {}
            \\  {}
            \\
        , .{ file, vm.err.? });

        return error.RuntimeError;
    };
    defer gpa.free(result);

    std.debug.print("{s}", .{result});
}

pub fn runDir(path: []const u8, cwd: fs.Dir, gpa: std.mem.Allocator) !void {
    _ = path; // autofix
    _ = cwd; // autofix
    _ = gpa; // autofix

}

pub fn oom() noreturn {
    fatal("out of memory\n", .{});
}

pub fn fatal(comptime fmt: []const u8, args: anytype) noreturn {
    std.debug.print("polka: ", .{});
    std.debug.print(fmt, args);
    std.process.exit(1);
}

const std = @import("std");
const fs = std.fs;
const parser = @import("syntax/parser.zig");
const Vm = @import("eval/Vm.zig");
