test "text" {
    var gpa = std.heap.DebugAllocator(.{}).init;
    var x: TreeConstructor = .init(gpa.allocator());
    try testParser(gpa.allocator(),
        \\
        \\foo
        \\bar
        \\
        \\bar
    , x.root(&.{
        x.leaf(.newline),
        x.leaf(.text_line),
        x.leaf(.newline),
        x.leaf(.text_line),
        x.leaf(.newline),
        x.leaf(.newline),
        x.leaf(.text_line),
    }));
}

test "Empty code" {
    var gpa = std.heap.DebugAllocator(.{}).init;
    var x: TreeConstructor = .init(gpa.allocator());
    try testParser(gpa.allocator(),
        \\
        \\#*
        \\#*
        \\
        \\#*
        \\
    , x.root(&.{
        x.leaf(.newline),
        x.tree(.code, &.{
            x.leaf(.code_begin),
            x.leaf(.newline),
            x.leaf(.code_begin),
        }),
    }));
}

test "simple statements" {
    var gpa = std.heap.DebugAllocator(.{}).init;
    var x: TreeConstructor = .init(gpa.allocator());
    try testParser(gpa.allocator(),
        \\
        \\#* return
        \\#* return nil
        \\#* continue
        \\#* break
        \\#* let foo
        \\#* let foo = 10
        \\#* export let foo
        \\#* export let foo = 10
        \\#*
        \\
    , x.root(&.{
        x.leaf(.newline),
        x.tree(.code, &.{
            x.leaf(.code_begin),
            x.tree(.return_statement, &.{x.leaf(.keyword_return)}),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.return_statement, &.{ x.leaf(.keyword_return), x.leaf(.keyword_nil) }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.leaf(.keyword_continue),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.leaf(.keyword_break),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.let_statement, &.{ x.leaf(.keyword_let), x.leaf(.ident) }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.let_statement, &.{
                x.leaf(.keyword_let),
                x.leaf(.ident),
                x.leaf(.eq),
                x.leaf(.number),
            }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.export_statement, &.{
                x.leaf(.keyword_export),
                x.tree(.let_statement, &.{ x.leaf(.keyword_let), x.leaf(.ident) }),
            }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.export_statement, &.{
                x.leaf(.keyword_export),
                x.tree(.let_statement, &.{
                    x.leaf(.keyword_let),
                    x.leaf(.ident),
                    x.leaf(.eq),
                    x.leaf(.number),
                }),
            }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.leaf(.newline),
        }),
    }));
}

fn assertNodeEql(n1: SyntaxNode, n2: SyntaxNode, all1: []const SyntaxNode, all2: []const SyntaxNode) !void {
    try expectEqual(n1.kind, n2.kind);
    switch (n1.kind.getType()) {
        .leaf => {},
        .tree => {
            try expectEqual(n1.getTreeChildren(all1).len, n2.getTreeChildren(all2).len);
            for (n1.getTreeChildren(all1), n2.getTreeChildren(all2)) |c1, c2| {
                try assertNodeEql(c1, c2, all1, all2);
            }
        },
    }
}

const TreeConstructor = struct {
    gpa: std.mem.Allocator,
    nodes: std.ArrayList(SyntaxNode),
    offset: usize,

    pub fn init(gpa: std.mem.Allocator) TreeConstructor {
        return .{
            .gpa = gpa,
            .offset = 0,
            .nodes = .empty,
        };
    }

    pub fn tree(self: *TreeConstructor, kind: SyntaxKind, children: []const SyntaxNode) SyntaxNode {
        self.nodes.appendSlice(self.gpa, children) catch unreachable;
        const end: usize = self.nodes.items.len;
        const ret: SyntaxNode = .{
            .kind = kind,
            .data = .{
                .tree = .{ .offset = @intCast(self.offset), .len = @intCast(end - self.offset) },
            },
        };

        self.offset = self.nodes.items.len;
        return ret;
    }

    pub fn root(
        self: *TreeConstructor,
        children: []const SyntaxNode,
    ) []const SyntaxNode {
        const node = self.tree(.text, children);
        _ = self.tree(.eof, &.{node});
        return self.nodes.toOwnedSlice(self.gpa) catch unreachable;
    }

    pub fn leaf(self: *TreeConstructor, kind: SyntaxKind) SyntaxNode {
        _ = self;
        return .{
            .kind = kind,
            .data = .{
                .leaf = .{ .offset = 0, .len = 0 },
            },
        };
    }
};

fn testParser(gpa: std.mem.Allocator, source: []const u8, expected: []const SyntaxNode) !void {
    const parsed = try parser.parse(source, .text, gpa);

    std.debug.print(" \n", .{});
    for (parsed.errors) |err| {
        std.debug.print("ERROR: {}\n", .{err});
    }
    try expectEqual(0, parsed.errors.len);
    defer parsed.deinit(gpa) catch unreachable;

    const expected_root = expected[expected.len - 1];

    assertNodeEql(
        parsed.rootNode().?.node,
        expected_root,
        parsed.nodes,
        expected,
    ) catch |err| {
        std.debug.print("{s}\n", .{source});
        std.debug.print("Expected \n", .{});
        var buffer: [2048]u8 = undefined;
        var stdout = std.fs.File.stderr().writer(&buffer);
        try expected_root.print(expected, "", 0, &stdout.interface);
        try stdout.interface.flush();

        std.debug.print("\nGot \n", .{});
        try parsed.rootNode().?.node.print(parsed.nodes, source, 0, &stdout.interface);
        try stdout.interface.flush();
        return err;
    };
}

const std = @import("std");
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;

const parser = @import("parser.zig");
const Scanner = @import("Scanner.zig");
const SyntaxKind = @import("node.zig").SyntaxKind;
const SyntaxNode = @import("node.zig").SyntaxNode;
