//! By convention, main.zig is where your main function lives in the case that
//! you are building an executable. If you are making a library, the convention
//! is to delete this file and start with root.zig instead.

const _node = @import("syntax/node.zig");
const SyntaxNode = _node.SyntaxNode;
const Scanner = @import("syntax/Scanner.zig");
const ast = @import("syntax/ast.zig");
const syntax_tests = @import("syntax/test.zig");

pub fn main() !void {
    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)
    std.debug.print("All your {s} are belong to us.\n", .{"codebase"});

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("Run `zig build test` to run the tests.\n", .{});

    try bw.flush(); // Don't forget to flush!
}

test "all" {
    std.testing.refAllDeclsRecursive(@This());

    const node = ast.Code{ .@"0" = SyntaxNode.tree(.Code, &[_]SyntaxNode{.leaf(.Ident, " ")}) };
    node.foo();
    _ = syntax_tests;
}

const std = @import("std");
