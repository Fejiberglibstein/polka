const RunError = fs.Dir.OpenError;

pub fn run(paths: []const []const u8, cwd: fs.Dir, cli_args: anytype, gpa: Allocator) void {
    _ = cli_args;

    for (paths) |path| {
        const stat = cwd.statFile(path) catch |err| {
            std.debug.print("could not stat {s}: {s}", .{ path, @errorName(err) });
            continue;
        };

        if (stat.kind == .directory) {
            var dir = cwd.openDir(path, .{ .iterate = true, .access_sub_paths = true }) catch |err| {
                std.debug.print("polka: could not open {s}: {s}\n", .{ path, @errorName(err) });
                continue;
            };
            defer dir.close();

            var walker = dir.walk(gpa) catch cli.oom();
            defer walker.deinit();

            while (walker.next() catch |err| {
                switch (err) {
                    Allocator.Error.OutOfMemory => cli.oom(),
                    else => std.debug.print("polka: error while walking directory {s}: {s}\n", .{
                        path,
                        @errorName(err),
                    }),
                }
                continue;
            }) |entry| {
                if (entry.kind == .file) {
                    if (std.mem.containsAtLeast(u8, entry.path, 1, ".git/")) {
                        continue;
                    }
                    const file = entry.dir.openFile(entry.basename, .{}) catch |err| {
                        std.debug.print("polka: could not open {s}: {s}\n", .{
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
                std.debug.print("polka: could not open {s}: {s}\n", .{ path, @errorName(err) });
                continue;
            };
            defer file.close();
            handleFile(file, path, gpa);
        }
    }
}

fn handleFile(file: fs.File, path: []const u8, gpa: Allocator) void {
    const run_result = runFile(file, gpa) catch |err| return switch (err) {
        Allocator.Error.OutOfMemory => cli.oom(),
        else => std.debug.print("polka: error while reading file {s}: {s}\n", .{
            path,
            @errorName(err),
        }),
    };

    const stdout = std.io.getStdOut();
    const padding = 2;
    const heading_width = path.len + padding;

    const color: struct { fg: []const u8, bg: []const u8 } = switch (run_result) {
        .success => .{ .fg = Colors.green, .bg = Colors.green_bg },
        else => .{ .fg = Colors.red, .bg = Colors.red_bg },
    };
    _ = stdout.writeAll(color.fg) catch {};

    // Print the filename header
    stdout.writer().writeBytesNTimes("▄", heading_width) catch {};
    stdout.writer().print(
        Colors.black ++ "\n{s} {s} " ++ Colors.reset ++ "{s}\n",
        .{ color.bg, path, color.fg },
    ) catch {};
    stdout.writer().writeBytesNTimes("▀", heading_width) catch {};
    stdout.writeAll("\n" ++ Colors.default ++ Colors.default_bg) catch {};

    switch (run_result) {
        .success => |res| {
            defer gpa.free(res);

            if (res.len == 0) {
                return;
            }

            const lines = std.mem.count(u8, res, "\n");
            const number_width = std.math.log10(lines);

            var len: u64 = 0;

            for (0..lines) |i| {
                const line = std.mem.sliceTo(res[len..], '\n');
                len += line.len + 1;
                std.fmt.formatInt(i + 1, 10, .lower, .{
                    .width = number_width + 1,
                    .fill = '0',
                }, stdout.writer()) catch {};
                stdout.writer().print(" │ {s}\n", .{line}) catch {};
            }
        },
        .parser_error => |err| {
            defer gpa.free(err.src);
            defer gpa.free(err.err.all_nodes);
            defer err.err.deinit();

            var iter = err.err;

            while (iter.next() catch cli.oom()) |cst_err| {
                stdout.writer().print(
                    \\Error while parsing (line: {d}, col: {d})
                    \\  {}
                    \\
                , .{ cst_err.line, cst_err.col, cst_err.err }) catch {};
            }
        },
        .runtime_error => |err| {
            defer gpa.free(err.src);

            stdout.writer().print(
                \\Error while parsing (line: TODO, col: TODO)
                \\  {}
                \\
            , .{err.err}) catch {};
        },
    }
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
