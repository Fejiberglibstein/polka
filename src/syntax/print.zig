const std = @import("std");
const SyntaxNode = @import("node.zig").SyntaxNode;
const SyntaxKind = @import("node.zig").SyntaxKind;

pub fn printNode(node: SyntaxNode, all_nodes: []const SyntaxNode, indent: usize) void {
    for (0..indent) |_| {
        std.debug.print("  ", .{});
    }

    const range = if (std.mem.eql(u8, node.range, "\n")) "" else node.range;

    switch (node.inner) {
        .@"error" => |e| std.debug.print("[ERROR]{s},\n", .{@tagName(e.err)}),
        .leaf => |l| std.debug.print("{s} `{s}`,\n", .{ @tagName(l.kind), range }),
        .tree => |t| {
            std.debug.print("{s} [\n", .{@tagName(t.kind)});
            const children = t.getChildren(all_nodes);
            for (children) |child| {
                printNode(child, all_nodes, indent + 1);
            }

            for (0..indent) |_| {
                std.debug.print("  ", .{});
            }
            std.debug.print("],\n", .{});
        },
    }
}
