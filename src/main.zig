pub fn main() !void {
    const gpa = std.heap.smp_allocator;

    const parsed = try parser.parse("", .text, gpa);
    defer parsed.deinit(std.heap.page_allocator);
    var writer = std.Io.Writer.Allocating.init(gpa).writer;

    var value_allocator = std.heap.ArenaAllocator.init((std.heap.page_allocator));
    var vm = try Vm.init(.{
        .nodes = parsed.nodes,
        .src = "",
        .gpa = gpa,
        .output = &writer,
        .value_arena = &value_allocator,
        .string_pool = undefined,
    });
    _ = &vm;

    _ = builtins.functions.get("hfk");
    _ = (ast.Color{ .node_index = undefined }).get(undefined, undefined);
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
const builtins = @import("lang.zig").builtins;
