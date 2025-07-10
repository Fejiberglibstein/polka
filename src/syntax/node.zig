pub const std = @import("std");

pub const SyntaxKind = enum {
    /// Error node
    Error,
    /// End of file
    EOF,

    /// Begins code `#*`
    CodeBegin,
    /// Begins code block `#**`
    CodeblockBegin,
    /// Ends code block `**#`
    CodeblockEnd,

    /// Body of text, either a line not beginning with `#*`, or content inside `
    Text,
    /// Code body, either an entire codeblock or multiple sequential lines of #*
    Code,

    // Tokens
    /// Addition `+`
    Plus,
    /// Subtraction `-`
    Minus,
    /// Multiplication `*`
    Star,
    /// Division `/`
    Slash,
    /// Modulo `%`
    Perc,
    /// Begins array/object access `[`
    LeftBracket,
    /// Terminates array/object access `]`
    RightBracket,
    /// Begins grouped expression or function/arguments `(`
    LeftParen,
    /// Terminates grouped expression or function/arguments `)`
    RightParen,
    /// Begins object initialization `{`
    LeftBrace,
    /// Terminates object initialization `}`
    RightBrace,
    /// Field access/function calling `.`
    Dot,
    /// Expression separator in a sequence `,`
    Comma,
    /// Assignment `=`
    Eq,
    /// Equality `==`.
    EqEq,
    /// Inequality `!=`.
    NotEq,
    /// Less than `<`.
    Lt,
    /// Less than or equal `<=`.
    LtEq,
    /// Greater than `>`.
    Gt,
    /// Greater than or equal `>=`.
    GtEq,

    // Keywords
    /// `let` keyword
    Let,
    /// `export` keyword
    Export,
    /// `if` keyword
    If,
    /// 'then' keyword
    Then,
    /// 'do' keyword
    Do,
    /// `in` keyword
    In,
    /// `for` keyword
    For,
    /// `while` keyword
    While,
    /// `function` keyword
    Function,
    /// `else` keyword
    Else,
    /// `end` keyword
    End,
    /// `or` keyword
    Or,
    /// `and` keyword
    And,
    /// `true` or `false` keyword
    Bool,
    /// `nil` keyword
    Nil,
    /// `return` keyword
    Return,
    /// `continue` keyword
    Continue,
    /// `break` keyword
    Break,

    // Nodes
    /// Identifier `foo`
    Ident,
    /// Either an integer or floating point number
    Number,
    /// String with quotes `"..."`
    String,
    /// Expression inside parenthesis
    Grouping,

    /// Export
    ///
    /// Can be either `export function foo() ... end` or `export let foo = 10`
    ///
    /// [`Export`, (`FunctionDef` | `LetExpr`)]
    ExportExpr,
    /// Function declaration `function foo() ... end`
    ///
    /// [`Function`, `Ident`, `LeftParen`, `FunctionParameters`, `RightParen`, `Code`, `End`]
    FunctionDef,
    /// Function Parameters
    ///
    /// [`Ident`...]
    FunctionParameters,
    /// When a function returns
    ///
    /// [`Return`, `Expr`]
    ReturnExpr,
    /// If conditional `if (foo == nil) foo = 10 else foo = foo - 10 end`
    ///
    /// [`If`, `Expr`, `Then`, `Code`, (`End` | `Else`, (`If` `Expr` `Then`)?, `Code`, `End`)]
    Conditional,
    /// For loop `for (i in range(4)) ... end`
    ///
    /// [`For`, `Ident`, `In`, `Expr`, `Do`, `Code`, `End`]
    ForLoop,
    /// While loop `while (true) ... end`
    ///
    /// [`While`, `Expr`, `Do`, `Code`, `End`]
    WhileLoop,
    /// Variable declaration `let foo = 10`
    ///
    /// [`Let`, `Ident`, `Eq`, `Expr`]
    LetExpr,
    /// Variable (re)assignment `foo = foo * 2`
    ///
    /// [`Ident`, `Eq`, `Expr`]
    Assignment,
    /// Binary Operatoion
    Binary,
    /// Unary Operatoion
    Unary,
    /// Function calling `foo()`
    ///
    /// [`Access`, `LeftParen`, `ArgumentList`, `RightParen`]
    FunctionCall,
    /// Argument list in a function call
    ///
    /// [`Ident`, `Comma`, ...]
    ArgumentList,
    /// Expression accessing by `.`
    ///
    /// [`Dot`, `Ident`, (`DotAccess` | `BracketAccess`)]
    DotAccess,
    /// Expression accessing by `["foo"]`
    ///
    /// [`LeftBracket`, `Expr`, `RightBracket`, (`DotAccess` | `BracketAccess`)]
    BracketAccess,
    /// Encompasses multiple text and code nodes into one. For example,
    ///
    /// `foo #*bar baz`
    ///
    /// would be one text node with three children, foo (text), bar (code), and baz (text)
    TextNode,

    /// Begins text mode while in code mode ```
    Backtick,

    /// Newline, a literal `\n`.
    Newline,

    pub fn isBinaryOp(self: SyntaxKind) bool {
        return self == .Plus or self == .Minus or self == .Perc or self == .Star or self == .Slash;
    }

    pub fn name(self: SyntaxKind) []const u8 {
        return switch (self) {
            .Error => "Syntax error",
            .EOF => "EOF",
            .CodeBegin => "#*",
            .CodeblockBegin => "#**",
            .CodeblockEnd => "**#",
            .Text => "text block",
            .Code => "code block",
            .Plus => "+",
            .Minus => "-",
            .Star => "*",
            .Slash => "/",
            .Perc => "%",
            .LeftBracket => "[",
            .RightBracket => "]",
            .LeftParen => "(",
            .RightParen => ")",
            .LeftBrace => "{",
            .RightBrace => "}",
            .Dot => ".",
            .Comma => ",",
            .Eq => "=",
            .EqEq => "==",
            .NotEq => "!=",
            .Lt => "<",
            .LtEq => "<=",
            .Gt => ">",
            .GtEq => ">=",
            .Let => "`let` keyword",
            .Export => "`export` keyword",
            .If => "`if` keyword",
            .Then => "`then` keyword",
            .Do => "`do` keyword",
            .In => "`in` keyword",
            .For => "`for` keyword",
            .While => "`while` keyword",
            .Function => "`function` keyword",
            .Else => "`else` keyword",
            .End => "`end` keyword",
            .Or => "`or` keyword",
            .And => "`and` keyword",
            .Bool => "boolean",
            .Nil => "`nil` keyword",
            .Return => "`return` keyword",
            .Continue => "`continue` keyword",
            .Break => "`break` keyword",
            .Ident => "identifier",
            .Number => "number",
            .string => "string",
            .Grouping => "grouping",
            .FunctionDef => "function definition",
            .FunctionParameters => "function parameters",
            .ReturnExpr => "return expression",
            .Conditional => "`if then` expression",
            .ForLoop => "`for in` expression",
            .WhileLoop => "`while` expression",
            .ExportExpr => "`export` expression",
            .LetExpr => "variable declaration",
            .Assignment => "variable assignment",
            .Binary => "binary expression",
            .Unary => "unary expression",
            .FunctionCall => "function call",
            .ArgumentList => "argument list",
            .DotAccess => "field access via .",
            .BracketAccess => "field access via []",
            .TextNode => "text",
            .Backtick => "`",
            .Newline => "newline",
        };
    }
};

pub const SyntaxNode = union(enum) {
    Leaf: LeafNode,
    Tree: TreeNode,
    Error: ErrorNode,

    const PLACEHOLDER_SOURCE = " ";

    pub fn leaf(k: SyntaxKind, s: []const u8) SyntaxNode {
        return SyntaxNode{ .Leaf = .init(k, s) };
    }

    pub fn tree(k: SyntaxKind, c: []const SyntaxNode) SyntaxNode {
        return SyntaxNode{ .Tree = .init(k, c) };
    }

    pub fn err(e: SyntaxError, s: []const u8) SyntaxNode {
        return SyntaxNode{ .Error = .init(e, s) };
    }

    pub fn source(self: SyntaxNode) []const u8 {
        return switch (self) {
            .Tree => |v| v.source,
            .Error => |v| v.source,
            .Leaf => |v| v.source,
        };
    }

    pub fn kind(self: SyntaxNode) SyntaxKind {
        return switch (self) {
            .Leaf => |v| v.kind,
            .Tree => |v| v.kind,
            .Error => |_| .Error,
        };
    }

    pub fn children(self: SyntaxNode) []const SyntaxNode {
        return switch (self) {
            .Tree => |v| v.children,
            else => ([_]SyntaxNode{})[0..],
        };
    }

    pub fn intoError(self: *SyntaxNode, e: SyntaxError) void {
        switch (self) {
            .Error => {},
            else => {
                self.* = .{ .Error = .init(e, self.source()) };
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

pub const LeafNode = struct {
    kind: SyntaxKind,
    source: []const u8,

    fn init(kind: SyntaxKind, source: []const u8) LeafNode {
        return LeafNode{
            .kind = kind,
            .source = source,
        };
    }
};

pub const TreeNode = struct {
    kind: SyntaxKind,
    children: []const SyntaxNode,
    source: []const u8,

    pub fn init(kind: SyntaxKind, children: []const SyntaxNode) TreeNode {
        const start = children[0].source();
        const end = children[children.len - 1].source();

        // create the source to encompass the source of the children
        const length: usize = @intFromPtr(&end[end.len - 1]) - @intFromPtr(start.ptr);
        const source: []const u8 = start.ptr[0..length];

        return TreeNode{ .kind = kind, .children = children, .source = source };
    }
};

pub const ErrorNode = struct {
    source: []const u8,
    err: SyntaxError,

    pub fn init(err: SyntaxError, source: []const u8) ErrorNode {
        return ErrorNode{
            .source = source,
            .err = err,
        };
    }
};

pub const SyntaxError = union(enum) {
    UnterminatedString: void,
    ExpectedToken: SyntaxKind,
    UnexpectedToken: SyntaxKind,
    UnexpectedCharacter: u8,

    fn toString(self: SyntaxError, a: std.mem.Allocator) []const u8 {
        switch (self) {
            .UnterminatedString => "Unterminated string",
            .ExpectedToken => |k| std.fmt.allocPrint(a, "Expected token {s}", .{k.name()}),
            .UnexpectedToken => |k| std.fmt.allocPrint(a, "Unexpected token {s}", .{k.name()}),
            .UnexpectedCharacter => |c| std.fmt.allocPrint(a, "Unexpected character {c}", .{c}),
        }
    }
};
