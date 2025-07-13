//! Testing the parser
//!
//! Test files are located in tests/

const std = @import("std");
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const SyntaxNode = @import("node.zig").SyntaxNode;
const SyntaxKind = @import("node.zig").SyntaxKind;
const Scanner = @import("Scanner.zig");
const parser = @import("parser.zig");

inline fn isKind(c: u8) bool {
    return (c >= 'a' and c <= 'z') or c == '_';
}

fn parseFile(
    comptime text: []const u8,
) !struct { SyntaxNode, std.BoundedArray(SyntaxNode, 500) } {
    // list of all nodes
    var nodes = try std.BoundedArray(SyntaxNode, 500).init(0);
    // stack to append on
    var stack = try std.BoundedArray(SyntaxNode, 500).init(0);

    // Stack we push for the start of each tree node
    var ind_stack = try std.BoundedArray(struct { u32, SyntaxKind }, 500).init(0);

    var s = Scanner.init(text);

    while (!s.isDone()) {
        s.eatWhitespace();
        const m = s.cursor;

        // Eat the identifier and get the kind, if any
        s.eatWhile(isKind);
        if (m != s.cursor and !@hasField(SyntaxKind, s.from(m))) {
            @compileError("SyntaxKind " ++ s.from(m) ++ " does not exist");
        }

        if (m != s.cursor) {
            const kind: SyntaxKind = @field(SyntaxKind, s.from(m));
            s.eatWhitespace();

            if (s.eatIf('[')) {
                // Start a tree node by pushing where the children nodes slice will start and the
                // kind.
                try ind_stack.append(.{ stack.slice().len, kind });
            } else {
                try stack.append(SyntaxNode.leafNode(kind, ""));
            }
        } else if (s.eatIf(']')) {
            // Close the tree node

            const ind = ind_stack.pop() orelse @panic("malformed test data");

            const offset = nodes.len;
            try nodes.appendSlice(stack.slice()[ind.@"0"..]);
            stack.resize(ind.@"0") catch unreachable;
            const len = nodes.len - offset;

            try stack.append(SyntaxNode.treeNode(ind.@"1", nodes.slice(), .{
                .len = len,
                .offset = offset,
            }));
        }

        s.eatWhitespace();
        _ = s.eatIf(',');
        s.eatWhitespace();
    }

    try std.testing.expectEqual(stack.slice().len, 1);
    const node = stack.slice()[0];

    return .{ node, nodes };
}

fn nodeEql(n1: SyntaxNode, n2: SyntaxNode, all1: []const SyntaxNode, all2: []const SyntaxNode) !void {
    try expectEqual(n1.kind(), n2.kind());
    switch (n1.inner) {
        .leaf => switch (n2.inner) {
            .leaf => {},
            else => try expect(false),
        },
        .tree => switch (n2.inner) {
            .tree => {
                try expectEqual(n1.children(all1).len, n2.children(all2).len);
                for (n1.children(all1), n2.children(all2)) |c1, c2| {
                    try nodeEql(c1, c2, all1, all2);
                }
            },
            else => try expect(false),
        },
        .@"error" => switch (n2.inner) {
            .@"error" => {},
            else => try expect(false),
        },
    }
}

fn testParser(comptime path: []const u8, allocator: std.mem.Allocator) !void {
    const file = @embedFile("tests/" ++ path ++ ".polk");
    const SEP = "\n$$$\n";

    const expected_node, const expected_nodes = comptime blk: {
        const index = std.mem.indexOf(u8, file, SEP).?;
        const expected_source = file[(index + SEP.len)..];

        break :blk try parseFile(expected_source);
    };

    const index = std.mem.indexOf(u8, file, SEP).?;

    const source = file[0..index];

    const parsed_node, const parsed_nodes = try parser.parse(source, allocator);
    defer parsed_nodes.deinit();

    nodeEql(parsed_node, expected_node, parsed_nodes.items, expected_nodes.slice()) catch {
        std.debug.print("{s}\n", .{source});
        std.debug.print("Expected \n", .{});
        printNode(expected_node, expected_nodes.slice(), 0);

        std.debug.print("\nGot \n", .{});
        printNode(parsed_node, parsed_nodes.items, 0);
    };

    std.debug.print("test " ++ path ++ " passed\n", .{});
}

fn printNode(node: SyntaxNode, all_nodes: []const SyntaxNode, indent: usize) void {
    for (0..indent) |_| {
        std.debug.print("    ", .{});
    }

    switch (node.inner) {
        .@"error" => |e| std.debug.print("{s},\n", .{@tagName(e.err)}),
        .leaf => |l| std.debug.print("{s},\n", .{@tagName(l.kind)}),
        .tree => |t| {
            std.debug.print("{s} [\n", .{@tagName(t.kind)});
            const children = t.getChildren(all_nodes);
            for (children) |child| {
                printNode(child, all_nodes, indent + 1);
            }

            for (0..indent) |_| {
                std.debug.print("    ", .{});
            }
            std.debug.print("],\n", .{});
        },
    }
}

test "testParser" {
    @setEvalBranchQuota(100000);
    var allocator = std.heap.DebugAllocator(.{}).init;
    defer _ = allocator.deinit();
    try testParser("complex_text", allocator.allocator());
    try testParser("forloop", allocator.allocator());
    try testParser("function", allocator.allocator());
    try testParser("if", allocator.allocator());
    try testParser("inline_expressions", allocator.allocator());
    try testParser("multiline_code", allocator.allocator());
    try testParser("simple_code", allocator.allocator());
    try testParser("simple_text", allocator.allocator());
}
