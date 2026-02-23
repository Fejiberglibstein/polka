test "text" {
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();
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
    defer _ = gpa.deinit();
    var x: TreeConstructor = .init(gpa.allocator());
    try testParser(gpa.allocator(),
        \\
        \\#*
        \\#*
        \\
        \\#*
        \\
        \\
    , x.root(&.{
        x.leaf(.newline),
        x.tree(.code, &.{
            x.leaf(.code_begin),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.leaf(.newline),
        }),
        x.leaf(.newline),
        x.tree(.code, &.{
            x.leaf(.code_begin),
            x.leaf(.newline),
        }),
        x.leaf(.newline),
    }));
}

test "Statements" {
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();
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
        }),
    }));
}

test "Single expressions" {
    std.testing.log_level = .debug;
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();
    var x: TreeConstructor = .init(gpa.allocator());
    try testParser(gpa.allocator(),
        \\#* "n"
        \\#* foo
        \\#* 10
        \\#* nil
        \\#* true
        \\#* false
        \\#* (3)
        \\#*
    , x.root(&.{
        x.tree(.code, &.{
            x.leaf(.code_begin),
            x.leaf(.string),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.leaf(.ident),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.leaf(.number),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.leaf(.keyword_nil),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.leaf(.keyword_true),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.leaf(.keyword_false),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.grouping, &.{
                x.leaf(.l_paren),
                x.leaf(.number),
                x.leaf(.r_paren),
            }),
            x.leaf(.newline),
            x.leaf(.code_begin),
        }),
    }));
}

test "Binary expressions" {
    std.testing.log_level = .debug;
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();
    var x: TreeConstructor = .init(gpa.allocator());
    try testParser(gpa.allocator(),
        \\#*2 + f
        \\#*"true" - 4
        \\#*true and 10
        \\#*10 or false
        \\#*nil in list
        \\#*a = 3
        \\#*4 < 2
        \\#*403 * 2.23
        \\#*"hfi" % 2
        \\#*4 > 2
        \\#*nil ~= nil
        \\#*nil >= nil
        \\#*nil <= nil
    , x.root(&.{
        x.tree(.code, &.{
            x.leaf(.code_begin),
            x.tree(.binary, &.{ x.leaf(.number), x.leaf(.plus), x.leaf(.ident) }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.binary, &.{ x.leaf(.string), x.leaf(.minus), x.leaf(.number) }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.binary, &.{ x.leaf(.keyword_true), x.leaf(.keyword_and), x.leaf(.number) }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.binary, &.{ x.leaf(.number), x.leaf(.keyword_or), x.leaf(.keyword_false) }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.binary, &.{ x.leaf(.keyword_nil), x.leaf(.keyword_in), x.leaf(.ident) }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.binary, &.{ x.leaf(.ident), x.leaf(.eq), x.leaf(.number) }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.binary, &.{ x.leaf(.number), x.leaf(.lt), x.leaf(.number) }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.binary, &.{ x.leaf(.number), x.leaf(.star), x.leaf(.number) }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.binary, &.{ x.leaf(.string), x.leaf(.percent), x.leaf(.number) }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.binary, &.{ x.leaf(.number), x.leaf(.gt), x.leaf(.number) }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.binary, &.{ x.leaf(.keyword_nil), x.leaf(.not_eq), x.leaf(.keyword_nil) }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.binary, &.{ x.leaf(.keyword_nil), x.leaf(.gt_eq), x.leaf(.keyword_nil) }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.binary, &.{ x.leaf(.keyword_nil), x.leaf(.lt_eq), x.leaf(.keyword_nil) }),
        }),
    }));
}

test "Unary expressions" {
    std.testing.log_level = .debug;
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();
    var x: TreeConstructor = .init(gpa.allocator());
    try testParser(gpa.allocator(),
        \\#*-3
        \\#*not true
    , x.root(&.{
        x.tree(.code, &.{
            x.leaf(.code_begin),
            x.tree(.unary, &.{ x.leaf(.minus), x.leaf(.number) }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.unary, &.{ x.leaf(.keyword_not), x.leaf(.keyword_true) }),
        }),
    }));
}

test "Complex expression" {
    std.testing.log_level = .debug;
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();
    var x: TreeConstructor = .init(gpa.allocator());
    try testParser(gpa.allocator(),
        \\#*10 > 2 and nil or not 4 - 3 * (6 + 2) == 1
    , x.root(&.{
        x.tree(.code, &.{
            x.leaf(.code_begin),
            x.tree(.binary, &.{
                x.tree(.binary, &.{
                    x.tree(.binary, &.{
                        x.leaf(.number),
                        x.leaf(.gt),
                        x.leaf(.number),
                    }),
                    x.leaf(.keyword_and),
                    x.leaf(.keyword_nil),
                }),
                x.leaf(.keyword_or),
                x.tree(.unary, &.{
                    x.leaf(.keyword_not),
                    x.tree(.binary, &.{
                        x.tree(.binary, &.{
                            x.leaf(.number),
                            x.leaf(.minus),
                            x.tree(.binary, &.{
                                x.leaf(.number),
                                x.leaf(.star),
                                x.tree(.grouping, &.{
                                    x.leaf(.l_paren),
                                    x.tree(.binary, &.{
                                        x.leaf(.number),
                                        x.leaf(.plus),
                                        x.leaf(.number),
                                    }),
                                    x.leaf(.r_paren),
                                }),
                            }),
                        }),
                        x.leaf(.eq_eq),
                        x.leaf(.number),
                    }),
                }),
            }),
        }),
    }));
}

test "Function calling" {
    std.testing.log_level = .debug;
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();
    var x: TreeConstructor = .init(gpa.allocator());
    try testParser(gpa.allocator(),
        \\#*foo(du37, 3 - 2, "str")(f())
    , x.root(&.{
        x.tree(.code, &.{
            x.leaf(.code_begin),
            x.tree(.function_call, &.{
                x.tree(.function_call, &.{
                    x.leaf(.ident),
                    x.tree(.function_args, &.{
                        x.leaf(.l_paren),
                        x.leaf(.ident),
                        x.leaf(.comma),
                        x.tree(.binary, &.{
                            x.leaf(.number),
                            x.leaf(.minus),
                            x.leaf(.number),
                        }),
                        x.leaf(.comma),
                        x.leaf(.string),
                        x.leaf(.r_paren),
                    }),
                }),
                x.tree(.function_args, &.{
                    x.leaf(.l_paren),
                    x.tree(.function_call, &.{
                        x.leaf(.ident),
                        x.tree(.function_args, &.{
                            x.leaf(.l_paren),
                            x.leaf(.r_paren),
                        }),
                    }),
                    x.leaf(.r_paren),
                }),
            }),
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
    for (parsed.errors) |err| {
        std.log.err(" \nERROR: {}", .{err});
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
        var exp_writer: std.Io.Writer.Allocating = .init(gpa);
        try expected_root.print(expected, "", 0, &exp_writer.writer);

        var act_writer: std.Io.Writer.Allocating = .init(gpa);
        try parsed.rootNode().?.node.print(parsed.nodes, source, 0, &act_writer.writer);

        var exp = std.mem.splitSequence(u8, try exp_writer.toOwnedSlice(), "\n");
        var act = std.mem.splitSequence(u8, try act_writer.toOwnedSlice(), "\n");

        while (true) {
            const exp_line = exp.next() orelse "";
            const act_line = act.next() orelse "";
            if (exp_line.len == 0 and act_line.len == 0) break;

            const color = if (!std.mem.eql(u8, exp_line, act_line))
                "\x1b[31m"
            else
                "\x1b[0m";

            std.debug.print("{s}{s: <25}{s}\n", .{ color, exp_line, act_line });
        }
        std.debug.print("\x1b[0m", .{});

        return err;
    };
    gpa.free(expected);
}

const std = @import("std");
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;

const parser = @import("parser.zig");
const Scanner = @import("Scanner.zig");
const SyntaxKind = @import("node.zig").SyntaxKind;
const SyntaxNode = @import("node.zig").SyntaxNode;
