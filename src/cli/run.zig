const RunError = fs.Dir.OpenError;

pub fn run(paths: []const []const u8, cwd: fs.Dir, cli_args: anytype, gpa: Allocator) void {
    _ = cli_args;

    for (paths) |path| {
        const stat = cwd.statFile(path) catch |err| {
            std.debug.print("could not stat {}: {}", .{ path, @errorName(err) });
            continue;
        };

        if (stat.kind == .directory) {
            var dir = cwd.openDir(path, .{ .iterate = true, .access_sub_paths = true }) catch |err| {
                std.debug.print("polka: could not open {}: {}\n", .{ path, @errorName(err) });
                continue;
            };
            defer dir.close();

            var walker = dir.walk(gpa) catch cli.oom();
            defer walker.deinit();

            while (walker.next() catch |err| {
                switch (err) {
                    Allocator.Error.OutOfMemory => cli.oom(),
                    else => std.debug.print("polka: error while walking directory {}: {}\n", .{
                        path,
                        @errorName(err),
                    }),
                }
                continue;
            }) |entry| {
                if (entry.kind == .file) {
                    const file = cwd.openFile(entry.path, .{}) catch |err| {
                        std.debug.print("polka: could not open {}: {}\n", .{
                            entry.path,
                            @errorName(err),
                        });
                        continue;
                    };
                    defer file.close();
                    handleFile(file, entry.path, gpa);
                }
            }

            continue;
        }

        if (stat.kind == .file) {
            const file = cwd.openFile(path, .{}) catch |err| {
                std.debug.print("polka: could not open {}: {}\n", .{ path, @errorName(err) });
                continue;
            };
            defer file.close();
            handleFile(file, path, gpa);
        }
    }
}

fn handleFile(file: fs.File, path: []const u8, gpa: Allocator) !void {
    const run_result = runFile(file, gpa) catch |err| return switch (err) {
        Allocator.Error.OutOfMemory => cli.oom(),
        else => std.debug.print("polka: error while reading file {}: {}\n", .{
            path,
            @errorName(err),
        }),
    };
}

const RunResult = union(enum) {
    success: []const u8,
    parser_error: struct { err: CstErrorIterator, src: []const u8 },
    runtime_error: struct { err: Vm.RuntimeErrorPayload, src: []const u8 },
};

fn runFile(file: fs.File, gpa: Allocator) !RunResult {
    const text = try file.readToEndAlloc(gpa, std.math.maxInt(usize));
    errdefer gpa.free(text);

    const res = try parser.parse(text, gpa);
    if (res.has_error) {
        errdefer gpa.free(res.all_nodes);
        return .{ .parser_error = .{
            .err = try CstErrorIterator.init(res.root_node, res.all_nodes, gpa),
            .src = text,
        } };
    }
    defer gpa.free(res.all_nodes);

    var vm = try Vm.init(gpa, res.all_nodes);
    defer vm.deinit();
    const vm_result = vm.eval(&res.root_node) catch {
        return .{ .runtime_error = .{
            .err = vm.err.?,
            .src = text,
        } };
    };

    gpa.free(text);
    return .{ .success = vm_result };
}

const std = @import("std");
const fs = std.fs;
const parser = @import("../syntax/parser.zig");
const Vm = @import("../eval/Vm.zig");
const CstErrorIterator = @import("../syntax/node.zig").ErrorIterator;
const cli = @import("../cli.zig");
const Colors = cli.Colors;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
