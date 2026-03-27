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
                x.leaf(.integer),
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
                    x.leaf(.integer),
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
            x.leaf(.static_string),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.leaf(.ident),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.leaf(.integer),
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
                x.leaf(.integer),
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
            x.tree(.binary, &.{ x.leaf(.integer), x.leaf(.plus), x.leaf(.ident) }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.binary, &.{ x.leaf(.static_string), x.leaf(.minus), x.leaf(.integer) }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.binary, &.{ x.leaf(.keyword_true), x.leaf(.keyword_and), x.leaf(.integer) }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.binary, &.{ x.leaf(.integer), x.leaf(.keyword_or), x.leaf(.keyword_false) }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.binary, &.{ x.leaf(.keyword_nil), x.leaf(.keyword_in), x.leaf(.ident) }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.binary, &.{ x.leaf(.ident), x.leaf(.eq), x.leaf(.integer) }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.binary, &.{ x.leaf(.integer), x.leaf(.lt), x.leaf(.integer) }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.binary, &.{ x.leaf(.integer), x.leaf(.star), x.leaf(.number) }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.binary, &.{ x.leaf(.static_string), x.leaf(.percent), x.leaf(.integer) }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.binary, &.{ x.leaf(.integer), x.leaf(.gt), x.leaf(.integer) }),
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
            x.tree(.unary, &.{ x.leaf(.minus), x.leaf(.integer) }),
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
                        x.leaf(.integer),
                        x.leaf(.gt),
                        x.leaf(.integer),
                    }),
                    x.leaf(.keyword_and),
                    x.leaf(.keyword_nil),
                }),
                x.leaf(.keyword_or),
                x.tree(.unary, &.{
                    x.leaf(.keyword_not),
                    x.tree(.binary, &.{
                        x.tree(.binary, &.{
                            x.leaf(.integer),
                            x.leaf(.minus),
                            x.tree(.binary, &.{
                                x.leaf(.integer),
                                x.leaf(.star),
                                x.tree(.grouping, &.{
                                    x.leaf(.l_paren),
                                    x.tree(.binary, &.{
                                        x.leaf(.integer),
                                        x.leaf(.plus),
                                        x.leaf(.integer),
                                    }),
                                    x.leaf(.r_paren),
                                }),
                            }),
                        }),
                        x.leaf(.eq_eq),
                        x.leaf(.integer),
                    }),
                }),
            }),
        }),
    }));
}

test "Simple conditional" {
    std.testing.log_level = .debug;
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();
    var x: TreeConstructor = .init(gpa.allocator());
    try testParser(gpa.allocator(),
        \\#* if true then
        \\jkfljlk
        \\#* end
        \\jhekjl
    , x.root(&.{
        x.tree(.code, &.{
            x.leaf(.code_begin),
            x.tree(.conditional, &.{
                x.leaf(.keyword_if),
                x.leaf(.keyword_true),
                x.leaf(.keyword_then),
                x.leaf(.newline),
                x.tree(.text, &.{
                    x.leaf(.text_line),
                    x.leaf(.newline),
                    x.tree(.code, &.{
                        x.leaf(.code_begin),
                    }),
                }),
                x.leaf(.keyword_end),
            }),
            x.leaf(.newline),
        }),
        x.leaf(.text_line),
    }));
}

test "Conditional with else" {
    std.testing.log_level = .debug;
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();
    var x: TreeConstructor = .init(gpa.allocator());
    try testParser(gpa.allocator(),
        \\#* if true then
        \\#*   break
        \\jckl
        \\#* else
        \\#*   continue
        \\#* end
    , x.root(&.{
        x.tree(.code, &.{
            x.leaf(.code_begin),
            x.tree(.conditional, &.{
                x.leaf(.keyword_if),
                x.leaf(.keyword_true),
                x.leaf(.keyword_then),
                x.leaf(.newline),
                x.tree(.text, &.{
                    x.tree(.code, &.{
                        x.leaf(.code_begin),
                        x.leaf(.keyword_break),
                        x.leaf(.newline),
                    }),
                    x.leaf(.text_line),
                    x.leaf(.newline),
                    x.tree(.code, &.{
                        x.leaf(.code_begin),
                    }),
                }),
                x.leaf(.keyword_else),
                x.leaf(.newline),
                x.tree(.text, &.{
                    x.tree(.code, &.{
                        x.leaf(.code_begin),
                        x.leaf(.keyword_continue),
                        x.leaf(.newline),
                        x.leaf(.code_begin),
                    }),
                }),
                x.leaf(.keyword_end),
            }),
        }),
    }));
}

test "Conditional with else if" {
    std.testing.log_level = .debug;
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();
    var x: TreeConstructor = .init(gpa.allocator());
    try testParser(gpa.allocator(),
        \\#* if true then
        \\#*   break
        \\#* elseif true then
        \\jkf
        \\#*
        \\jkffjk
        \\#* else
        \\#*   nil
        \\#* end
    , x.root(&.{
        x.tree(.code, &.{
            x.leaf(.code_begin),
            x.tree(.conditional, &.{
                x.leaf(.keyword_if),
                x.leaf(.keyword_true),
                x.leaf(.keyword_then),
                x.leaf(.newline),
                x.tree(.text, &.{
                    x.tree(.code, &.{
                        x.leaf(.code_begin),
                        x.leaf(.keyword_break),
                        x.leaf(.newline),
                        x.leaf(.code_begin),
                    }),
                }),
                x.leaf(.keyword_elseif),
                x.leaf(.keyword_true),
                x.leaf(.keyword_then),
                x.leaf(.newline),
                x.tree(.text, &.{
                    x.leaf(.text_line),
                    x.leaf(.newline),
                    x.tree(.code, &.{
                        x.leaf(.code_begin),
                        x.leaf(.newline),
                    }),
                    x.leaf(.text_line),
                    x.leaf(.newline),
                    x.tree(.code, &.{x.leaf(.code_begin)}),
                }),
                x.leaf(.keyword_else),
                x.leaf(.newline),
                x.tree(.text, &.{
                    x.tree(.code, &.{
                        x.leaf(.code_begin),
                        x.leaf(.keyword_nil),
                        x.leaf(.newline),
                        x.leaf(.code_begin),
                    }),
                }),
                x.leaf(.keyword_end),
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
                            x.leaf(.integer),
                            x.leaf(.minus),
                            x.leaf(.integer),
                        }),
                        x.leaf(.comma),
                        x.leaf(.static_string),
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

test "simple multiline string" {
    std.testing.log_level = .debug;
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();
    var x: TreeConstructor = .init(gpa.allocator());
    try testParser(gpa.allocator(),
        \\#* `Hello world
        \\#*
        \\#* h = `Hello world @\(
        \\#*     `this is a multiline string
    , x.root(&.{
        x.tree(.code, &.{
            x.leaf(.code_begin),
            x.tree(.multiline_string, &.{
                x.leaf(.backtick),
                x.leaf(.mls_text),
            }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.binary, &.{
                x.leaf(.ident),
                x.leaf(.eq),
                x.tree(.multiline_string, &.{
                    x.leaf(.backtick),
                    x.leaf(.mls_text),
                    x.leaf(.newline),
                    x.leaf(.code_begin),
                    x.leaf(.backtick),
                    x.leaf(.mls_text),
                }),
            }),
        }),
    }));
}

test "Dicts" {
    std.testing.log_level = .debug;
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();
    var x: TreeConstructor = .init(gpa.allocator());
    try testParser(gpa.allocator(),
        \\#* {foo = 10, bar = []}
    , x.root(&.{
        x.tree(.code, &.{
            x.leaf(.code_begin),
            x.tree(.dict, &.{
                x.leaf(.l_brace),
                x.tree(.dict_field, &.{
                    x.leaf(.ident),
                    x.leaf(.eq),
                    x.leaf(.integer),
                }),
                x.leaf(.comma),
                x.tree(.dict_field, &.{
                    x.leaf(.ident), x.leaf(.eq), x.tree(.list, &.{
                        x.leaf(.l_bracket),
                        x.leaf(.r_bracket),
                    }),
                }),
                x.leaf(.r_brace),
            }),
        }),
    }));
}

test "Lists" {
    std.testing.log_level = .debug;
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();
    var x: TreeConstructor = .init(gpa.allocator());
    try testParser(gpa.allocator(),
        \\#* [ 10, 2 ]
    , x.root(&.{
        x.tree(.code, &.{
            x.leaf(.code_begin),
            x.tree(.list, &.{
                x.leaf(.l_bracket),
                x.leaf(.integer),
                x.leaf(.comma),
                x.leaf(.integer),
                x.leaf(.r_bracket),
            }),
        }),
    }));
}

test "Multiline string with expressions" {
    std.testing.log_level = .debug;
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();
    var x: TreeConstructor = .init(gpa.allocator());
    try testParser(gpa.allocator(),
        \\#* `@(10 - 2)
        \\#* `foo jlk @("hi") jkf
    , x.root(&.{
        x.tree(.code, &.{
            x.leaf(.code_begin),
            x.tree(.multiline_string, &.{
                x.leaf(.backtick),
                x.tree(.mls_expression, &.{
                    x.leaf(.at),
                    x.leaf(.l_paren),
                    x.tree(.binary, &.{
                        x.leaf(.integer),
                        x.leaf(.minus),
                        x.leaf(.integer),
                    }),
                    x.leaf(.r_paren),
                }),
                x.leaf(.newline),
                x.leaf(.code_begin),
                x.leaf(.backtick),
                x.leaf(.mls_text),
                x.tree(.mls_expression, &.{
                    x.leaf(.at),
                    x.leaf(.l_paren),
                    x.leaf(.static_string),
                    x.leaf(.r_paren),
                }),
                x.leaf(.mls_text),
            }),
        }),
    }));
}

test "errors" {
    std.testing.log_level = .debug;
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();
    var x: TreeConstructor = .init(gpa.allocator());
    try testParser(gpa.allocator(),
        \\1jk
        \\#* let h = )
        \\#* 12
        \\foo
        \\#* 10 -
        \\j
        \\#* if (false + (true and (wfalse)) then
    , x.root(&.{}));
}

test "Multiline string recovery" {
    std.testing.log_level = .debug;
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();
    var x: TreeConstructor = .init(gpa.allocator());
    try testParser(gpa.allocator(),
        \\#* `@(10 - 2)
        \\#* let h = 10
        \\#* `hifooo
        \\jkljfkl
    , x.root(&.{
        x.tree(.code, &.{
            x.leaf(.code_begin),
            x.tree(.multiline_string, &.{
                x.leaf(.backtick),
                x.tree(.mls_expression, &.{
                    x.leaf(.at),
                    x.leaf(.l_paren),
                    x.tree(.binary, &.{
                        x.leaf(.integer),
                        x.leaf(.minus),
                        x.leaf(.integer),
                    }),
                    x.leaf(.r_paren),
                }),
            }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.let_statement, &.{
                x.leaf(.keyword_let),
                x.leaf(.ident),
                x.leaf(.eq),
                x.leaf(.integer),
            }),
            x.leaf(.newline),
            x.leaf(.code_begin),
            x.tree(.multiline_string, &.{
                x.leaf(.backtick),
                x.leaf(.mls_text),
            }),
            x.leaf(.newline),
        }),
        x.leaf(.text_line),
    }));
}

test "Function def" {
    std.testing.log_level = .debug;
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();
    var x: TreeConstructor = .init(gpa.allocator());
    try testParser(gpa.allocator(),
        \\#*func foo(a, b)
        \\#*    if true then 
        \\#*        return a + b
        \\#*    end
        \\#*    "hello'"
        \\#*end
    , x.root(&.{
        x.tree(.code, &.{
            x.leaf(.code_begin),
            x.tree(.function_def, &.{
                x.leaf(.keyword_func),
                x.leaf(.ident),
                x.tree(.function_parameters, &.{
                    x.leaf(.l_paren),
                    x.leaf(.ident),
                    x.leaf(.comma),
                    x.leaf(.ident),
                    x.leaf(.r_paren),
                }),
                x.leaf(.newline),
                x.tree(.text, &.{x.tree(.code, &.{
                    x.leaf(.code_begin),
                    x.tree(.conditional, &.{
                        x.leaf(.keyword_if),
                        x.leaf(.keyword_true),
                        x.leaf(.keyword_then),
                        x.leaf(.newline),
                        x.tree(.text, &.{x.tree(.code, &.{
                            x.leaf(.code_begin),
                            x.tree(.return_statement, &.{
                                x.leaf(.keyword_return),
                                x.tree(.binary, &.{
                                    x.leaf(.ident),
                                    x.leaf(.plus),
                                    x.leaf(.ident),
                                }),
                            }),
                            x.leaf(.newline),
                            x.leaf(.code_begin),
                        })}),
                        x.leaf(.keyword_end),
                    }),
                    x.leaf(.newline),
                    x.leaf(.code_begin),
                    x.leaf(.static_string),
                    x.leaf(.newline),
                    x.leaf(.code_begin),
                })}),
                x.leaf(.keyword_end),
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
    defer parsed.deinit(gpa);
    defer gpa.free(expected);

    for (parsed.errors) |err| {
        std.log.err(" \nERROR: {}", .{err});
    }

    const expected_root = expected[expected.len - 1];
    const actual_root = parsed.nodes[parsed.nodes.len - 1];

    if (parsed.errors.len != 0) {
        var buffer: [2048]u8 = undefined;
        var act_writer = std.fs.File.stderr().writer(&buffer);
        try actual_root.print(parsed.nodes, source, 0, &act_writer.interface);
        try act_writer.interface.flush();

        try expectEqual(0, parsed.errors.len);
    }

    assertNodeEql(
        expected_root,
        actual_root,
        expected,
        parsed.nodes,
    ) catch |err| {
        var exp_writer: std.Io.Writer.Allocating = .init(gpa);
        try expected_root.print(expected, "", 0, &exp_writer.writer);
        defer exp_writer.deinit();

        var act_writer: std.Io.Writer.Allocating = .init(gpa);
        try actual_root.print(parsed.nodes, source, 0, &act_writer.writer);
        defer act_writer.deinit();

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
}

const std = @import("std");
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;

const parser = @import("parser.zig");
const Scanner = @import("Scanner.zig");
const SyntaxKind = @import("node.zig").SyntaxKind;
const SyntaxNode = @import("node.zig").SyntaxNode;
