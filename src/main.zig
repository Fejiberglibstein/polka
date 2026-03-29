pub fn main() !void {
    var lexer = Lexer.init("", .text, "#");
    _ = lexer.next();

    const gpa = std.heap.smp_allocator;

    const parsed = try parser.parse("", .text, gpa);
    defer parsed.deinit(std.heap.page_allocator);
    var writer = std.Io.Writer.Allocating.init(gpa).writer;

    var value_allocator = std.heap.ArenaAllocator.init((std.heap.page_allocator));
    var vm = try Vm.init(parsed.nodes, "", gpa, &value_allocator, undefined, &writer);
    try treewalk.evalText(&vm, undefined);
}

test {
    _ = @import("lang/test_eval.zig");
    _ = @import("lang/test_syntax.zig");
    std.testing.refAllDeclsRecursive(@This());
}

const std = @import("std");

const Lexer = @import("lang/Lexer.zig");
const SyntaxNode = @import("lang/node.zig").SyntaxNode;
const parser = @import("lang/parser.zig");
const ast = @import("lang/ast.zig");
const Vm = @import("lang/Vm.zig");
const treewalk = @import("lang/treewalk.zig");
