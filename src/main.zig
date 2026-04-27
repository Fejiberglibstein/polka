pub fn main() !void {
    const gpa = std.heap.smp_allocator;
    var threaded = std.Io.Threaded.init(gpa, .{});
    const io = threaded.io();

    var pool: Value.String.Pool = .init(gpa);
    defer pool.deinit();

    var config: polka.Config = .init(gpa, &pool);
    defer config.deinit();

    const constants = try builtin.Constants.init(io, gpa, &pool);
    defer constants.deinit(gpa);

    var value_allocator = std.heap.ArenaAllocator.init((std.heap.page_allocator));
    defer value_allocator.deinit();

    const parsed = try parser.parse("", .text, gpa);
    defer parsed.deinit(std.heap.page_allocator);

    var output: std.Io.Writer.Allocating = .init(gpa);
    defer output.deinit();
    const writer = &output.writer;

    var vm = try Vm.init(.{
        .src = "",
        .gpa = gpa,
        .output = writer,
        .config = &config,
        .string_pool = &pool,
        .nodes = parsed.nodes,
        .constants = constants,
        .value_arena = &value_allocator,
    });
    if (vm.run()) |err| {
        std.debug.print("Error: {f}", .{err.formatWith(parsed.nodes, "", &pool)});
    }

    _ = builtin.functions.get("hfk");
}

test {
    _ = @import("lang/test_eval.zig");
    _ = @import("lang/test_syntax.zig");
}

const std = @import("std");

const parser = @import("lang.zig").parser;
const ast = @import("lang.zig").ast;
const Vm = @import("lang.zig").Vm;
const Value = @import("lang.zig").Value;
const builtin = @import("lang.zig").builtin;
const polka = @import("polka.zig");
