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

fn parseFile(
    comptime text: []const u8,
) !struct { SyntaxNode, std.ArrayList(SyntaxNode) } {
    // list of all nodes
    //
    // Make both nodes and stack 500 capacity so that the list won't be resized ever and our
    // pointers won't be invalidated
    var nodes = try std.BoundedArray(SyntaxNode, 500).init(0);
    // stack to append on
    var stack = try std.BoundedArray(SyntaxNode, 500).init(0);

    // Stack we push for the start of each tree node
    var ind_stack = try std.BoundedArray(struct { u32, SyntaxKind }, 500).init(0);

    var s = Scanner.init(text);

    while (true) {
        const m = s.cursor;

        s.eatWhitespace();

        @compileLog(std.fmt.comptimePrint("{s}\n\n", .{s.after()}));

        // Eat the identifier and get the kind, if any
        _ = s.eatAlpha();
        const kind: ?SyntaxKind = if (m != s.cursor) @field(SyntaxKind, s.from(m)) else null;

        s.eatWhitespace();

        if (s.eatIf(.{ .Char = '[' })) {
            // Start a tree node
            if (kind) |k| {
                try ind_stack.append(.{ ind_stack.slice().len, k });
            } else {
                @panic("malformed test data");
            }
        } else if (s.eatIf(.{ .Char = ']' })) {
            // Close the tree node
            const ind = ind_stack.pop() orelse @panic("malformed test data");

            const len = nodes.slice().len;
            try nodes.appendSlice(stack.slice()[ind.@"0"..]);
            try stack.resize(stack.slice().len - ind.@"0");

            stack.append(.{ .Tree = .init(ind.@"1", nodes.slice()[len..]) });
        } else {
            // If neither [ nor ], then it's a leaf node
            if (kind) |k| {
                try stack.append(.{ .Leaf = .init(k, " ") });
            } else {
                @panic("malformed test data");
            }
        }
        s.eatWhitespace();
        _ = s.eatIf(.{ .Char = ',' });
        s.eatWhitespace();
    }

    try std.testing.expectEqual(stack.items.len, 1);
    const node = stack.items[0];

    return struct { node, nodes };
}

fn nodeEql(n1: SyntaxNode, n2: SyntaxNode) !void {
    try expectEqual(n1.kind(), n2.kind());
    switch (n1) {
        .Leaf => switch (n2) {
            .Leaf => {},
            else => try expect(false),
        },
        .Tree => switch (n2) {
            .Tree => {
                expectEqual(n1.children().len, n2.children().len);
                for (n1, n2) |c1, c2| {
                    try nodeEql(c1, c2);
                }
            },
            else => try expect(false),
        },
        .Error => switch (n2) {
            .Error => {},
            else => try expect(false),
        },
    }
}

fn testParser(comptime path: []const u8, allocator: std.mem.Allocator) !void {
    const file = @embedFile("tests/" ++ path ++ ".polk");
    const SEP = "\n$$$\n";

    var expected_node: SyntaxNode = undefined;
    var expected_nodes: []SyntaxNode = undefined;

    comptime {
        const index = std.mem.indexOf(u8, file, SEP).?;
        const expected_source = file[(index + SEP.len)..];

        expected_node, expected_nodes = try parseFile(expected_source);
    }

    const index = std.mem.indexOf(u8, file, SEP).?;

    const source = file[0..index];

    const parsed_node, const parsed_nodes = parser.parse(source, allocator);
    defer parsed_nodes.deinit();

    try nodeEql(parsed_node, expected_node) catch {
        std.debug.print("Expected \n", .{});
        std.json.stringify(expected_node, .{}, std.io.getStdErr().writer());

        std.debug.print("\n\nGot \n", .{});
        std.json.stringify(parsed_node, .{}, std.io.getStdErr().writer());
    };
}

test "testParser" {
    var allocator = std.heap.DebugAllocator(.{}).init;
    try testParser("complex_text", allocator.allocator());
    _ = allocator.deinit();
    try testParser("forloop", allocator.allocator());
    _ = allocator.deinit();
    try testParser("function", allocator.allocator());
    _ = allocator.deinit();
    try testParser("if", allocator.allocator());
    _ = allocator.deinit();
    try testParser("inline_expressions", allocator.allocator());
    _ = allocator.deinit();
    try testParser("multiline_code", allocator.allocator());
    _ = allocator.deinit();
    try testParser("simple_code", allocator.allocator());
    _ = allocator.deinit();
    try testParser("simple_text", allocator.allocator());
    _ = allocator.deinit();
}
