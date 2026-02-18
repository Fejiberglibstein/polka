pub fn main() !void {
    var lexer = Lexer.init("", .text, "#");
    _ = lexer.next();
}

const std = @import("std");

const Lexer = @import("syntax/Lexer.zig");
const SyntaxNode = @import("syntax/node.zig").SyntaxNode;
const parser = @import("syntax/parser.zig");
