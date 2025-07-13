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
    /// [`Ident`...]
    function_parameters,
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
    /// Variable (re)assignment `foo = foo * 2`
    ///
    /// [`Ident`, `eq`, `Expr`]
    assignment,
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
        return self == .plus or self == .minus or self == .perc or self == .star or self == .slash;
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

    pub fn treeNode(k: SyntaxKind, all_nodes: []SyntaxNode, c: TreeNode.Children) SyntaxNode {
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

    pub fn precedingWhitespace(self: SyntaxNode) u16 {
        return switch (self) {
            .tree => |_| 0,
            .leaf => |v| v.preceding_whitespace,
            .@"error" => |v| v.preceding_whitespace,
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

    /// Used in the typed AST to get children matching a certain ASTNode type.
    ///
    /// If no nodes are found, will return a generic node
    pub fn lastChild(self: SyntaxNode, all_nodes: []const SyntaxNode, T: type) T {
        const childs = self.children(all_nodes);
        var i = childs.len - 1;

        while (i > 0) : (i -= 1) {
            if (T.toTyped(childs[i])) |c| {
                return c;
            }
        }

        return T{ .v = SyntaxNode.leafNode(T.kind, "") };
    }

    /// Used in the typed AST to get children matching a certain ASTNode type.
    ///
    /// If no nodes are found, will return a generic node
    pub fn firstChild(self: SyntaxNode, all_nodes: []const SyntaxNode, T: type) T {
        for (self.children(all_nodes)) |child| {
            if (T.toTyped(child)) |c| {
                return c;
            }
        }

        return T{ .v = SyntaxNode.leafNode(T.kind, "") };
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
        self.intoError(.{ .ExpectedToken = exp });
    }

    pub fn unexpected(self: *SyntaxNode) void {
        self.intoError(.{ .UnexpectedToken = self.kind() });
    }
};

pub const SyntaxError = union(enum(u8)) {
    UnterminatedString: void,
    ExpectedToken: SyntaxKind,
    UnexpectedToken: SyntaxKind,
    UnexpectedCharacter: u8,

    fn toString(self: SyntaxError, a: std.mem.Allocator) []const u8 {
        // TODO perhaps make this do comptime string concatenation instead of `allocPrint`ing ?
        switch (self) {
            .UnterminatedString => "Unterminated string",
            .ExpectedToken => |k| std.fmt.allocPrint(a, "Expected token {s}", .{k.name()}),
            .UnexpectedToken => |k| std.fmt.allocPrint(a, "Unexpected token {s}", .{k.name()}),
            .UnexpectedCharacter => |c| std.fmt.allocPrint(a, "Unexpected character {c}", .{c}),
        }
    }
};
