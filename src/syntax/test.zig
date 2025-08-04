test "binary_expr" {
    @setEvalBranchQuota(1000000);
    try testParser(
        \\#* 3 *4 - foo()
        \\#* 3 == 2 and 3 - 6 * 2 > (h and 1)
        \\#* -3 + 3 + 3
        \\#* 3 = 3 = 3
    ,
        \\text_node [
        \\  code [
        \\    code_begin,
        \\    binary [
        \\      binary [ number, star, number, ],
        \\      minus,
        \\      function_call [
        \\        ident,
        \\        argument_list [ left_paren, right_paren, ]
        \\      ]
        \\    ],
        \\    newline,
        \\    code_begin,
        \\    binary [
        \\      binary [ number, eq_eq, number ],
        \\      and,
        \\      binary [
        \\        binary [
        \\          number,
        \\          minus,
        \\          binary [ number, star, number, ]
        \\        ],
        \\        gt,
        \\        grouping [
        \\          left_paren,
        \\          binary [ ident, and, number ],
        \\          right_paren,
        \\        ]
        \\      ]
        \\    ],
        \\    newline,
        \\    code_begin,
        \\    binary [
        \\      binary [ 
        \\          unary [ minus, number ]
        \\          plus,
        \\          number 
        \\      ],
        \\      plus,
        \\      number
        \\    ],
        \\    newline,
        \\    code_begin,
        \\    binary [
        \\      number,
        \\      eq,
        \\      binary [ number, eq, number, ]
        \\    ]
        \\  ]
        \\]
    );
}

test "complex_text" {
    @setEvalBranchQuota(1000000);
    try testParser(
        \\
        \\# Colors
        \\[colors.primary]
        \\#* for color in colors do
        \\color: #*color.bar foo
        \\#* end
        \\foo
        \\
    ,
        \\text_node [
        \\  newline,
        \\  text,
        \\  newline,
        \\  text,
        \\  newline,
        \\  code [
        \\    code_begin,
        \\    for_loop [
        \\      for,
        \\      ident,
        \\      in,
        \\      ident,
        \\      do,
        \\      newline,
        \\      text_node [
        \\        text,
        \\        code [
        \\          code_begin,
        \\          dot_access [ ident, dot, ident ],
        \\        ],
        \\        text,
        \\        newline,
        \\        code [ code_begin ]
        \\      ],
        \\      end
        \\    ],
        \\    newline
        \\  ],
        \\  text,
        \\  newline
        \\]
    );
}

test "forloop" {
    @setEvalBranchQuota(1000000);
    try testParser(
        \\
        \\#* for i in range(4) do
        \\hi
        \\#* end
        \\
    ,
        \\text_node [
        \\  newline,
        \\  code [
        \\    code_begin,
        \\    for_loop [
        \\      for,
        \\      ident,
        \\      in,
        \\      function_call [
        \\        ident,
        \\        argument_list [ left_paren, number, right_paren ]
        \\      ],
        \\      do,
        \\      newline,
        \\      text_node [
        \\        text,
        \\        newline,
        \\        code [ code_begin ]
        \\      ],
        \\      end
        \\    ],
        \\    newline
        \\  ]
        \\]
    );
}

test "function" {
    @setEvalBranchQuota(1000000);
    try testParser(
        \\
        \\hello
        \\#* function foo(bar, baz) 
        \\#*     return 4
        \\#* end
        \\#* function foo() [a, b]
        \\bar
        \\#* end
        \\> world
        \\
    ,
        \\text_node [
        \\  newline,
        \\  text,
        \\  newline,
        \\  code [
        \\    code_begin,
        \\    function_def [
        \\      function,
        \\      ident,
        \\      function_parameters [ left_paren, ident, comma, ident, right_paren ],
        \\      newline,
        \\      text_node [
        \\        code [
        \\          code_begin,
        \\          return_expr [ return, number ]
        \\          newline,
        \\          code_begin
        \\        ]
        \\      ],
        \\      end,
        \\    ],
        \\    newline,
        \\    code_begin,
        \\    function_def [
        \\      function,
        \\      ident,
        \\      function_parameters [ left_paren, right_paren ],
        \\      closure_captures [ left_bracket, ident, comma, ident, right_bracket ]
        \\      newline,
        \\      text_node [
        \\        text,
        \\        newline,
        \\        code [ code_begin ],
        \\      ],
        \\      end,
        \\    ],
        \\    newline
        \\  ],
        \\  text,
        \\  newline
        \\]
    );
}

test "if" {
    @setEvalBranchQuota(1000000);
    try testParser(
        \\
        \\#* if true then 
        \\#*     if false then
        \\hello there
        \\#*     end
        \\#* end
        \\
    ,
        \\text_node [
        \\  newline,
        \\  code [
        \\    code_begin,
        \\    conditional [
        \\      if,
        \\      bool,
        \\      then,
        \\      newline,
        \\      text_node [
        \\        code [
        \\          code_begin,
        \\          conditional [
        \\            if,
        \\            bool,
        \\            then,
        \\            newline,
        \\            text_node [
        \\              text,
        \\              newline,
        \\              code [ code_begin ],
        \\            ],
        \\            end
        \\          ],
        \\          newline,
        \\          code_begin
        \\        ],
        \\      ],
        \\      end
        \\    ],
        \\    newline
        \\  ]
        \\]
    );
}

test "inline_expressions" {
    @setEvalBranchQuota(1000000);
    try testParser(
        \\foreground = \"#*foo.baz\"
    ,
        \\text_node [
        \\  text,
        \\  code [
        \\    code_begin,
        \\    dot_access [ ident, dot, ident ]
        \\  ],
        \\  text
        \\]
        \\
    );
}

test "multiline_code" {
    @setEvalBranchQuota(1000000);
    try testParser(
        \\#* let foo = 10 
        \\#* foo 
    ,
        \\text_node [
        \\  code [
        \\    code_begin,
        \\    let_expr [ let, ident, eq, number ],
        \\    newline,
        \\    code_begin,
        \\    ident
        \\  ]
        \\]
    );
}

test "simple_code" {
    @setEvalBranchQuota(1000000);
    try testParser(
        \\#* foo.bar.bar(4, 10)
    ,
        \\text_node [
        \\  code [
        \\    code_begin,
        \\    function_call [
        \\      dot_access [
        \\        dot_access [ ident, dot, ident ]
        \\        dot,
        \\        ident
        \\      ],
        \\      argument_list [
        \\        left_paren,
        \\        number,
        \\        comma,
        \\        number,
        \\        right_paren
        \\      ]
        \\    ]
        \\  ]
        \\]
    );
}

test "simple_text" {
    @setEvalBranchQuota(1000000);
    try testParser(
        \\
        \\foo
        \\bar
        \\
        \\bar
    ,
        \\text_node[
        \\  newline,
        \\  text,
        \\  newline,
        \\  text,
        \\  newline,
        \\  newline,
        \\  text
        \\]
    );
}

const std = @import("std");
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;

const parser = @import("parser.zig");
const Scanner = @import("Scanner.zig");
const SyntaxKind = @import("node.zig").SyntaxKind;
const SyntaxNode = @import("node.zig").SyntaxNode;
const printNode = @import("print.zig").printNode;

inline fn isKind(c: u8) bool {
    return (c >= 'a' and c <= 'z') or c == '_';
}

fn parseTree(
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

fn assertNodeEql(n1: SyntaxNode, n2: SyntaxNode, all1: []const SyntaxNode, all2: []const SyntaxNode) !void {
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
                    try assertNodeEql(c1, c2, all1, all2);
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

fn testParser(source: []const u8, comptime expected_source: []const u8) !void {
    var allocator = std.heap.DebugAllocator(.{}).init;
    defer {
        _ = allocator.deinit();
    } 

    const expected_node, const expected_nodes = comptime blk: {
        break :blk try parseTree(expected_source);
    };
    const parsed_node, const parsed_nodes = try parser.parse(source, allocator.allocator());
    defer parsed_nodes.deinit();

    assertNodeEql(
        parsed_node,
        expected_node,
        parsed_nodes.items,
        expected_nodes.slice(),
    ) catch |err| {
        std.debug.print("{s}\n", .{source});
        std.debug.print("Expected \n", .{});
        printNode(expected_node, expected_nodes.slice(), 0);

        std.debug.print("\nGot \n", .{});
        printNode(parsed_node, parsed_nodes.items, 0);
        return err;
    };
}
