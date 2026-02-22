pub fn main() !void {
    var lexer = Lexer.init("", .text, "#");
    _ = lexer.next();

    const parsed = try parser.parse("", .text, std.heap.page_allocator);

    const f = ast.toASTNode(ast.Text, parsed.rootNode().?.node) orelse unreachable;
    var iter = f.parts(&.{});
    _ = iter.next();
}

test {
    _ = @import("syntax/test.zig");
}

const std = @import("std");

const Lexer = @import("syntax/Lexer.zig");
const SyntaxNode = @import("syntax/node.zig").SyntaxNode;
const parser = @import("syntax/parser.zig");
const ast = @import("syntax/ast.zig");
