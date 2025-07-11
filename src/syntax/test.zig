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
) !struct { SyntaxNode, std.BoundedArray(SyntaxNode, 500) } {
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

    while (!s.isDone()) {
        const m = s.cursor;
        s.eatWhitespace();

        // Eat the identifier and get the kind, if any
        _ = s.eatAlpha();

        if (m != s.cursor and !@hasField(SyntaxKind, s.from(m))) {
            @compileError("SyntaxKind " ++ s.from(m) ++ " does not exist");
        }
        const kind: ?SyntaxKind = if (m != s.cursor) @field(SyntaxKind, s.from(m)) else null;

        s.eatWhitespace();

        if (s.eatIf(.{ .Char = '[' })) {
            // Start a tree node
            if (kind) |k| {
                @compileLog(std.fmt.comptimePrint("{d}", .{stack.slice().len}));
                try ind_stack.append(.{ stack.slice().len, k });
            } else {
                @panic("malformed test data");
            }
        } else if (s.eatIf(.{ .Char = ']' })) {
            // Close the tree node
            const ind = ind_stack.pop() orelse @panic("malformed test data");

            const offset = nodes.slice().len;
            try nodes.appendSlice(stack.slice()[ind.@"0"..]);
            try stack.resize(ind.@"0");
            const len = nodes.slice().len - offset;

            try stack.append(SyntaxNode.tree(ind.@"1", nodes.slice(), .{
                .len = len,
                .offset = offset,
            }));
        } else {
            // If neither [ nor ], then it's a leaf node
            if (kind) |k| {
                try stack.append(SyntaxNode.leaf(k, 0, 0));
            } else {
                @panic("malformed test data");
            }
        }
        s.eatWhitespace();
        _ = s.eatIf(.{ .Char = ',' });
        s.eatWhitespace();
    }

    try std.testing.expectEqual(stack.slice().len, 1);
    const node = stack.slice()[0];

    return .{ node, nodes };
}

fn nodeEql(n1: SyntaxNode, n2: SyntaxNode, all1: []SyntaxNode, all2: []SyntaxNode) !void {
    try expectEqual(n1.kind(), n2.kind());
    switch (n1) {
        .Leaf => switch (n2) {
            .Leaf => {},
            else => try expect(false),
        },
        .Tree => switch (n2) {
            .Tree => {
                expectEqual(n1.children(all1).len, n2.children(all2).len);
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

    const expected_node, const expected_nodes = comptime blk: {
        const index = std.mem.indexOf(u8, file, SEP).?;
        const expected_source = file[(index + SEP.len)..];

        break :blk try parseFile(expected_source);
    };

    const index = std.mem.indexOf(u8, file, SEP).?;

    const source = file[0..index];

    const parsed_node, const parsed_nodes = parser.parse(source, allocator);
    defer parsed_nodes.deinit();

    try nodeEql(parsed_node, expected_node, parsed_nodes, expected_nodes) catch {
        std.debug.print("Expected \n", .{});
        std.json.stringify(expected_node, .{}, std.io.getStdErr().writer());

        std.debug.print("\n\nGot \n", .{});
        std.json.stringify(parsed_node, .{}, std.io.getStdErr().writer());
    };
}

test "testParser" {
    @setEvalBranchQuota(100000);
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
