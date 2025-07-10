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

fn Parse(comptime text: []const u8, allocator: std.mem.Allocator) !SyntaxNode {
    // list of all nodes
    //
    // Make both nodes and stack 500 capacity so that the list won't be resized ever and our
    // pointers won't be invalidated
    var nodes = try std.ArrayList(SyntaxNode).initCapacity(allocator, 500);
    // stack to append on
    var stack = try std.ArrayList(SyntaxNode).initCapacity(allocator, 500);

    // Stack we push for the start of each tree node
    var ind_stack = std.ArrayList(.{ u32, SyntaxKind }).init(allocator);

    const s = Scanner.init(text);

    while (true) {
        const m = s.cursor;

        s.eatWhitespace();

        if (s.eatAlpha()) {
            const kind = @field(SyntaxKind, s.from(m));

            s.eatWhitespace();

            if (s.eatIf(.{ .Char = '[' })) {
                ind_stack.append(.{ ind_stack.items.len, kind });
            } else if (s.eatIf(.{ .Char = ']' })) {
                const ind = ind_stack.pop() orelse @panic("malformed test data");

                const len = nodes.items.len;
                try nodes.appendSlice(stack.items[ind.@"0"..]);
                try stack.resize(stack.items.len - ind.@"0");

                stack.append(.{ .Tree = .init(ind.@"1", nodes[len..]) });
            } else {
                _ = s.eatIf(.{ .Char = ',' });

                stack.append(.{ .Leaf = .init(kind, " ") });
            }
            s.eatWhitespace();
        }
    }

    try std.testing.expectEqual(nodes.items.len, 1);

    return nodes.items[0];
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
    const index = std.mem.indexOf(u8, file, "\n$$$\n");

    const source = file[0..index];
    const expected_source = file[index..];

    const expected = try Parse(expected_source, allocator);

    const parsed = parser.parse(source, allocator);

    try nodeEql(parsed, expected) catch {
        std.debug.print("Expected \n", .{});
        std.json.stringify(expected, .{}, std.io.getStdErr().writer());

        std.debug.print("\n\nGot \n", .{});
        std.json.stringify(parsed, .{}, std.io.getStdErr().writer());
    };
}

test "testParser" {
    const allocator = std.heap.testAllocator(std.heap.PageAllocator);
    testParser("ifs", allocator);
}
