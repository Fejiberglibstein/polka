pub const std = @import("std");

pub const SyntaxKind = enum(u8) {
    /// Error node
    err,
    /// End of file
    eof,
    /// Begins code `#*`
    code_begin,
    /// Begins code block `#**`
    codeblock_begin,
    /// Ends code block `**#`
    codeblock_end,

    /// Body of text, either a line not beginning with `#*`, or content inside `
    text,
    /// code body, either an entire codeblock or multiple sequential lines of #*
    code,

    // Tokens
    /// Addition `+`
    plus,
    /// Subtraction `-`
    minus,
    /// Multiplication `*`
    star,
    /// Division `/`
    slash,
    /// Modulo `%`
    perc,
    /// Begins array/object access `[`
    left_bracket,
    /// Terminates array/object access `]`
    right_bracket,
    /// Begins grouped expression or function/arguments `(`
    left_paren,
    /// Terminates grouped expression or function/arguments `)`
    right_paren,
    /// Begins object initialization `{`
    left_brace,
    /// Terminates object initialization `}`
    right_brace,
    /// Field access/function calling `.`
    dot,
    /// Expression separator in a sequence `,`
    comma,
    /// Assignment `=`
    eq,
    /// Dictionary keys `:`
    colon,
    /// Equality `==`.
    eq_eq,
    /// Inequality `!=`.
    not_eq,
    /// Less than `<`.
    lt,
    /// Less than or equal `<=`.
    lt_eq,
    /// Greater than `>`.
    gt,
    /// Greater than or equal `>=`.
    gt_eq,

    // Keywords
    /// `let` keyword
    let,
    /// `export` keyword
    @"export",
    /// `if` keyword
    @"if",
    /// 'then' keyword
    then,
    /// 'do' keyword
    do,
    /// `in` keyword
    in,
    /// `for` keyword
    @"for",
    /// `while` keyword
    @"while",
    /// `function` keyword
    function,
    /// `else` keyword
    @"else",
    /// `end` keyword
    end,
    /// `or` keyword
    @"or",
    /// `and` keyword
    @"and",
    /// `true` or `false` keyword
    bool,
    /// `nil` keyword
    nil,
    /// `return` keyword
    @"return",
    /// `continue` keyword
    @"continue",
    /// `break` keyword
    @"break",

    // Nodes
    /// Identifier `foo`
    ident,
    /// Either an integer or floating point number
    number,
    /// String with quotes `"..."`
    string,
    /// Expression inside parenthesis
    grouping,
    /// List declaration
    list,
    /// Dict declaration
    dict,

    /// Export
    ///
    /// Can be either `export function foo() ... end` or `export let foo = 10`
    ///
    /// [`Export`, (`FunctionDef` | `LetExpr`)]
    export_expr,
    /// Function declaration `function foo() ... end`
    ///
    /// [`Function`, `Ident`, `LeftParen`, `FunctionParameters`, `right_paren`, `Code`, `End`]
    function_def,
    /// Function Parameters
    ///
    /// [`LeftParen`, `Ident`..., `RightParen`]
    function_parameters,
    /// The optional list of variables to capture from the preceding scope
    ///
    /// [`LeftBracket`, `Ident`..., `RightBracket`]
    closure_captures,
    /// When a function returns
    ///
    /// [`Return`, `Expr`]
    return_expr,
    /// If conditional `if (foo == nil) foo = 10 else foo = foo - 10 end`
    ///
    /// [`If`, `Expr`, `Then`, `Code`, (`End` | `Else`, (`If` `Expr` `Then`)?, `Code`, `End`)]
    conditional,
    /// For loop `for (i in range(4)) ... end`
    ///
    /// [`For`, `Ident`, `In`, `Expr`, `Do`, `Code`, `End`]
    for_loop,
    /// While loop `while (true) ... end`
    ///
    /// [`While`, `Expr`, `Do`, `Code`, `End`]
    while_loop,
    /// Variable declaration `let foo = 10`
    ///
    /// [`Let`, `Ident`, `eq`, `Expr`]
    let_expr,
    /// Binary Operatoion
    binary,
    /// Unary Operatoion
    unary,
    /// Function calling `foo()`
    ///
    /// [`Access`, `LeftParen`, `ArgumentList`, `right_paren`]
    function_call,
    /// Argument list in a function call
    ///
    /// [`Ident`, `Comma`, ...]
    argument_list,
    /// Expression accessing by `.`
    ///
    /// [`Dot`, `Ident`, (`DotAccess` | `BracketAccess`)]
    dot_access,
    /// Expression accessing by `["foo"]`
    ///
    /// [`LeftBracket`, `Expr`, `RightBracket`, (`DotAccess` | `BracketAccess`)]
    bracket_access,
    /// Encompasses multiple text and code nodes into one. For example,
    ///
    /// `foo #*bar baz`
    ///
    /// would be one text node with three children, foo (text), bar (code), and baz (text)
    text_node,

    /// Begins text mode while in code mode ```
    backtick,

    /// Newline, a literal `\n`.
    newline,

    pub fn isBinaryOp(self: SyntaxKind) bool {
        // TODO add `in` binary op. ("foo" in {"bar", "foo"})
        return switch (self) {
            .plus,
            .minus,
            .slash,
            .star,
            .eq_eq,
            .lt_eq,
            .lt,
            .gt_eq,
            .gt,
            .eq,
            .perc,
            .not_eq,
            .@"and",
            .@"or",
            => true,

            else => false,
        };
    }

    pub fn isUnaryOp(self: SyntaxKind) bool {
        // TODO add `not` unary op
        return self == .minus or self == .plus;
    }

    pub fn name(self: SyntaxKind) []const u8 {
        return switch (self) {
            .err => "Syntax error",
            .eof => "eof",
            .code_begin => "#*",
            .codeblock_begin => "#**",
            .codeblock_end => "**#",
            .text => "text block",
            .code => "code block",
            .plus => "+",
            .minus => "-",
            .star => "*",
            .slash => "/",
            .perc => "%",
            .left_bracket => "[",
            .right_bracket => "]",
            .left_paren => "(",
            .right_paren => ")",
            .left_brace => "{",
            .right_brace => "}",
            .dot => ".",
            .comma => ",",
            .eq => "=",
            .eq_eq => "==",
            .not_eq => "!=",
            .lt => "<",
            .lt_eq => "<=",
            .gt => ">",
            .gt_eq => ">=",
            .let => "`let` keyword",
            .@"export" => "`export` keyword",
            .@"if" => "`if` keyword",
            .then => "`then` keyword",
            .do => "`do` keyword",
            .in => "`in` keyword",
            .@"for" => "`for` keyword",
            .@"while" => "`while` keyword",
            .function => "`function` keyword",
            .@"else" => "`else` keyword",
            .end => "`end` keyword",
            .@"or" => "`or` keyword",
            .@"and" => "`and` keyword",
            .bool => "boolean",
            .nil => "`nil` keyword",
            .@"return" => "`return` keyword",
            .@"continue" => "`continue` keyword",
            .@"break" => "`break` keyword",
            .ident => "identifier",
            .number => "number",
            .string => "string",
            .grouping => "grouping",
            .function_def => "function definition",
            .function_parameters => "function parameters",
            .return_expr => "return expression",
            .conditional => "`if then` expression",
            .for_loop => "`for in` expression",
            .while_loop => "`while` expression",
            .export_expr => "`export` expression",
            .let_expr => "variable declaration",
            .assignment => "variable assignment",
            .binary => "binary expression",
            .unary => "unary expression",
            .function_call => "function call",
            .argument_list => "argument list",
            .dot_access => "field access via .",
            .bracket_access => "field access via []",
            .text_node => "text",
            .backtick => "`",
            .newline => "newline",
        };
    }
};

pub const SyntaxNode = struct {
    range: []const u8,

    inner: union(enum(u8)) {
        leaf: LeafNode,
        tree: TreeNode,
        @"error": ErrorNode,
    },

    pub const ErrorNode = struct { err: SyntaxError };
    pub const LeafNode = struct { kind: SyntaxKind };
    pub const TreeNode = struct {
        kind: SyntaxKind,
        children: Children,

        pub fn getChildren(self: TreeNode, all_nodes: []const SyntaxNode) []const SyntaxNode {
            return all_nodes[self.children.offset..(self.children.offset + self.children.len)];
        }

        /// A range of nodes inside the AST. `offset` acts as an index into the list of all the
        /// nodes
        pub const Children = struct { offset: u32, len: u32 };
    };

    pub fn leafNode(k: SyntaxKind, range: []const u8) SyntaxNode {
        return SyntaxNode{
            .range = range,
            .inner = .{
                .leaf = LeafNode{ .kind = k },
            },
        };
    }

    pub fn treeNode(k: SyntaxKind, all_nodes: []const SyntaxNode, c: TreeNode.Children) SyntaxNode {
        const tree = TreeNode{
            .kind = k,
            .children = c,
        };

        // Get the range that this tree node encompasses
        const childSlice = tree.getChildren(all_nodes);
        const start = childSlice[0].range;
        const end = childSlice[childSlice.len - 1].range;
        const range = if (@inComptime())
            " " // Ensure everything works in our tests since pointer math doesnt work at comptime
        else
            start.ptr[0..(&end[end.len - 1] - start.ptr)];

        return SyntaxNode{
            .range = range,
            .inner = .{ .tree = tree },
        };
    }

    pub fn errorNode(e: SyntaxError, range: []const u8) SyntaxNode {
        return SyntaxNode{
            .range = range,
            .inner = .{
                .@"error" = ErrorNode{ .err = e },
            },
        };
    }

    pub fn isError(self: SyntaxNode) bool {
        return switch (self.inner) {
            .@"error" => true,
            else => false,
        };
    }

    pub fn kind(self: SyntaxNode) SyntaxKind {
        return switch (self.inner) {
            .leaf => |v| v.kind,
            .tree => |v| v.kind,
            .@"error" => |_| .err,
        };
    }

    pub fn children(self: SyntaxNode, all_nodes: []const SyntaxNode) []const SyntaxNode {
        return switch (self.inner) {
            .tree => |v| v.getChildren(all_nodes),
            else => ([_]SyntaxNode{})[0..],
        };
    }

    pub fn intoError(self: *SyntaxNode, e: SyntaxError) void {
        switch (self.inner) {
            .@"error" => {},
            else => self.* = SyntaxNode{
                .range = self.range,
                .inner = .{ .@"error" = ErrorNode{ .err = e } },
            },
        }
    }

    pub fn expected(self: *SyntaxNode, exp: SyntaxKind) void {
        self.intoError(.{ .expected_token = exp });
    }

    pub fn unexpected(self: *SyntaxNode) void {
        self.intoError(.{ .unexpected_token = self.kind() });
    }
};

pub const SyntaxError = union(enum(u8)) {
    unterminated_string: void,
    expected_token: SyntaxKind,
    unexpected_token: SyntaxKind,
    unexpected_character: u8,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;

        switch (self) {
            .unterminated_string => writer.print("Unterminated string", .{}),
            .expected_token => |v| writer.print("Expected token {s}", .{v.name()}),
            .unexpected_token => |v| writer.print("Unexpected token {s}", .{v.name()}),
            .unexpected_character => |v| writer.print("Unexpected character {c}", .{v}),
        }
    }
};

pub const ErrorIterator = struct {
    /// The current line in the file
    line: usize,
    /// The current column of the line in the file
    column: usize,
    /// All of the nodes in the CST
    all_nodes: []const SyntaxNode,
    /// Root node of the CST
    root_node: SyntaxNode,
    /// The path that has currently been traversed through the tree nodes of the CST.
    /// It is a stack of indices for each level of tree nodes
    path: Path,

    const Path = std.ArrayList(struct { node: SyntaxNode, index: u32 });

    pub fn init(
        root_node: SyntaxNode,
        all_nodes: []const SyntaxNode,
        gpa: std.mem.Allocator,
    ) !ErrorIterator {
        var path = try Path.initCapacity(gpa, 16);
        // Initialize path with the root node and index 0
        path.append(.{ .node = root_node, .index = 0 });

        return ErrorIterator{
            .line = 0,
            .column = 0,
            .all_nodes = all_nodes,
            .root_node = root_node,
            .path = path,
        };
    }

    pub fn deinit(self: ErrorIterator) void {
        self.path.deinit();
    }

    pub const Error = struct {
        err: SyntaxError,
        line: usize,
        col: usize,
    };

    pub fn next(self: *ErrorIterator) ?Error {
        while (true) {
            if (self.path.items.len == 0)
                return null;

            var curr = &self.path.items[self.path.items.len];
            var child_nodes = curr.node.children(self.all_nodes);

            while (curr.index < child_nodes.len) {
                const child_node = child_nodes[curr.index];

                if (child_node.isError()) {
                    return Error{
                        .err = child_node.inner.@"error".err,
                        .line = self.line,
                        .col = self.column,
                    };
                } else if (child_node.kind() == .newline) {
                    self.line += 1;
                    self.column = 0;
                } else {
                    self.column += child_node.range.len;
                }

                if (child_node.children(self.all_nodes).len > 0) {
                    const added = self.path.addOne();
                    added.* = .{ child_node, 0 };

                    curr = added;
                    child_nodes = curr.node.children(self.all_nodes);
                } else {
                    curr.index += 1;
                }
            }

            _ = self.path.pop();
        }
    }
};
