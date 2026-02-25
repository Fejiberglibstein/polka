pub fn main() !void {
    var lexer = Lexer.init("", .text, "#");
    _ = lexer.next();

    const parsed = try parser.parse("", .text, std.heap.page_allocator);
    defer parsed.deinit(std.heap.page_allocator);
    var c: Compiler = .init(parsed.nodes, "", std.heap.page_allocator);
    _ = try c.compile();
}

test {
    _ = @import("lang/syntax/test.zig");
    std.testing.refAllDeclsRecursive(@This());
}

const std = @import("std");

const Lexer = @import("lang/syntax/Lexer.zig");
const SyntaxNode = @import("lang/syntax/node.zig").SyntaxNode;
const parser = @import("lang/syntax/parser.zig");
const ast = @import("lang/syntax/ast.zig");
const Compiler = @import("lang/Compiler.zig");
