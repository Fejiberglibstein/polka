pub fn main() !void {
    var lexer = Lexer.init("", .text, "#");
    _ = lexer.next();

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
    try eval.evalText(&vm, undefined);

    _ = builtin.functions.get("hfk");
    _ = (ast.Color{ .node_index = undefined }).get(undefined, undefined);
}

test {
    _ = @import("lang/test_eval.zig");
    _ = @import("lang/test_syntax.zig");
}

const std = @import("std");

const Lexer = @import("lang/Lexer.zig");
const SyntaxNode = @import("lang/node.zig").SyntaxNode;
const parser = @import("lang/parser.zig");
const ast = @import("lang/ast.zig");
const Vm = @import("lang/Vm.zig");
const eval = @import("lang/eval.zig");
const Value = @import("lang/value.zig").Value;
const builtin = @import("lang/builtin.zig");
