pub fn main(init: std.process.Init) !void {
    const gpa = init.gpa;
    const io = init.io;
    const environ_map = init.environ_map;

    var stdout_buf: [2048]u8 = undefined;
    var stdout_writer = std.Io.File.stdout().writer(io, &stdout_buf);
    const stdout = &stdout_writer.interface;
    defer stdout.flush() catch {};

    var stderr_buf: [2048]u8 = undefined;
    var stderr_writer = std.Io.File.stderr().writer(io, &stderr_buf);
    const stderr = &stderr_writer.interface;
    defer stderr.flush() catch {};

    var pool: Value.String.Pool = try .init(gpa);
    defer pool.deinit();

    var runner: Runner = try .init(io, gpa, environ_map, &pool);
    defer runner.deinit(io);

    try runner.sync(io);
    try stdout.writeAll(runner.error_log.written());

    try runner.vfs.print(stdout);
}

test {
    _ = @import("lang/test_eval.zig");
    _ = @import("lang/test_syntax.zig");

    std.testing.refAllDecls(@import("VirtualFilesystem.zig"));
}

const std = @import("std");

const parser = @import("lang.zig").parser;
const ast = @import("lang.zig").ast;
const Vm = @import("lang.zig").Vm;
const Value = @import("lang.zig").Value;
const builtin = @import("lang.zig").builtin;
const polka = @import("polka.zig");
const Runner = @import("Runner.zig");
