//! By convention, main.zig is where your main function lives in the case that
//! you are building an executable. If you are making a library, the convention
//! is to delete this file and start with root.zig instead.

const std = @import("std");

const ast = @import("syntax/ast.zig");
const SyntaxNode = @import("syntax/node.zig").SyntaxNode;
const SyntaxKind = @import("syntax/node.zig").SyntaxKind;
const parse = @import("syntax/parser.zig");
const Scanner = @import("syntax/Scanner.zig");
const syntax_tests = @import("syntax/test.zig");
const eval_test = @import("eval/test.zig");
const Vm = @import("eval/Vm.zig");

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    const node, const nodes = try parse.parse("", allocator);
    var vm = try Vm.init(allocator, nodes.items);
    defer vm.deinit();

    const stdout = std.io.getStdOut().writer();

    const result = try vm.eval(node);
    try stdout.print("{s}", .{result});
}

test "all" {
    std.testing.refAllDeclsRecursive(@This());
    _ = syntax_tests;
    _ = eval_test;
}
