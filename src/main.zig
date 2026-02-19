pub fn main() !void {
    var lexer = Lexer.init("", .text, "#");
    _ = lexer.next();

    const nodes = try parser.parse("", .text, std.heap.page_allocator);

    const f = ast.toTyped(ast.Text, nodes[nodes.len - 1]) orelse unreachable;
    var iter = f.parts(&.{});
    _ = iter.next();
}

const std = @import("std");

const Lexer = @import("syntax/Lexer.zig");
const SyntaxNode = @import("syntax/node.zig").SyntaxNode;
const parser = @import("syntax/parser.zig");
const ast = @import("syntax/ast.zig");
