pub const Mode = enum {
    /// Lines beginning with `#*`. If any text comes before the `#*`, then the Mode would be
    /// .code_expr instead
    ///
    /// ```text
    ///     #* if (sys.hostname == "foo")
    ///     #*   // ...
    ///     #* end
    /// ```
    code_line,
    /// While in text mode, expressions beginning with `#*`
    ///
    /// ```text
    ///     The hostname is #*sys.hostname
    /// ```
    code_expr,
    /// Code block beginning with `#**` and ending with `**#`
    ///
    /// Code expressions inside a block do not need to begin with the normal `#*`
    ///
    /// ```text
    ///     #**
    ///     local bar = function()
    ///         return "not"
    ///     end
    ///
    ///     local foo = {
    ///         bar = bar
    ///     }
    ///     #**
    /// ```
    code_block,
    /// Normal file contents
    text,

    fn isCode(self: Mode) bool {
        return self == .Codeline or self == .code_block or self == .code_expr;
    }
};
pub const Whitespace = enum { preceding_whitespace, none };

const comment_beginnings = [_]u8{ ';', '#' };

const Lexer = @This();

s: Scanner,
mode: Mode,
currentError: ?SyntaxError,

pub fn init(source: []const u8) Lexer {
    return Lexer{
        .mode = .text,
        .s = Scanner.init(source),
        .currentError = null,
    };
}

pub fn next(self: *Lexer) struct { SyntaxNode, SyntaxKind, Whitespace } {
    const before_spaces = self.s.cursor;
    self.s.eatSpaces();

    const start = self.s.cursor;

    const kind = if (self.s.eatNewline())
        .newline
    else if (self.s.peek() == null)
        .eof
    else switch (self.mode) {
        .code_line, .code_expr, .code_block => self.code(),
        .text,
        => self.text(),
    };

    const node: SyntaxNode = if (self.currentError) |err|
        .errorNode(err, self.s.from(start))
    else
        // Text nodes should retain leading spaces so that indentation works.
        .leafNode(kind, self.s.from(if (kind == .text) before_spaces else start));
    self.currentError = null;

    const whitespace: Whitespace = if (start - before_spaces != 0 and kind != .text)
        .preceding_whitespace
    else
        .none;

    return .{ node, kind, whitespace };
}

pub fn reparse(self: *Lexer, node: SyntaxNode) void {
    self.s.moveTo(self.s.cursor - node.range.len);
}

fn setErr(self: *Lexer, err: SyntaxError) SyntaxKind {
    if (self.currentError == null) {
        self.currentError = err;
    }
    return .err;
}

fn code(self: *Lexer) SyntaxKind {
    if (eatCodebegin(&self.s)) |tok| {
        return tok;
    }

    const c = self.s.eat() orelse unreachable;

    return sw: switch (c) {
        '.' => .dot,
        '+' => .plus,
        '%' => .perc,
        '*' => .star,
        '-' => .minus,
        '/' => .slash,
        ',' => .comma,
        ':' => .colon,
        '(' => .left_paren,
        '{' => .left_brace,
        ')' => .right_paren,
        '}' => .right_brace,
        '[' => .left_bracket,
        ']' => .right_bracket,
        '=' => if (self.s.eatIf('=')) .eq_eq else .eq,
        '!' => if (self.s.eatIf('=')) .not_eq else continue :sw 0,
        '<' => if (self.s.eatIf('=')) .lt_eq else .lt,
        '>' => if (self.s.eatIf('=')) .gt_eq else .gt,
        '"' => self.string(),
        '0'...'9' => self.number(),
        'a'...'z', 'A'...'Z', '_' => self.ident(),
        else => self.setErr(.{ .unexpected_character = c }),
    };
}

fn string(self: *Lexer) SyntaxKind {
    while (true) {
        self.s.eatUntil([_]u8{ '\\', '"', '\n', '\r' });

        if (self.s.eatNewline()) {
            return self.setErr(.unterminated_string);
        }

        switch (self.s.eat() orelse 0) {
            '\\' => if (self.s.eatIf('"')) {},
            '"' => break,

            else => {},
        }
    }

    return .string;
}

inline fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

inline fn isIdentChar(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or isDigit(c) or c == '_';
}

fn number(self: *Lexer) SyntaxKind {
    self.s.eatWhile(isDigit);
    if (self.s.eatIf('.')) {
        self.s.eatWhile(isDigit);
    }
    return .number;
}

fn ident(self: *Lexer) SyntaxKind {
    // we already parsed the first character of the ident
    const cursor = self.s.cursor - 1;

    self.s.eatWhile(isIdentChar);

    return if (keyword(self.s.from(cursor))) |k|
        k
    else
        .ident;
}

const keywords: std.StaticStringMap(SyntaxKind) = .initComptime(.{
    .{ "in", .in },
    .{ "do", .do },
    .{ "let", .let },
    .{ "end", .end },
    .{ "nil", .nil },
    .{ "or", .@"or" },
    .{ "if", .@"if" },
    .{ "then", .then },
    .{ "true", .bool },
    .{ "false", .bool },
    .{ "and", .@"and" },
    .{ "for", .@"for" },
    .{ "fun", .function },
    .{ "else", .@"else" },
    .{ "break", .@"break" },
    .{ "while", .@"while" },
    .{ "return", .@"return" },
    .{ "export", .@"export" },
    .{ "function", .function },
    .{ "continue", .@"continue" },
});

fn keyword(t: []const u8) ?SyntaxKind {
    return keywords.get(t);
}

/// Parse a token while in text mode
fn text(self: *Lexer) SyntaxKind {
    if (eatCodebegin(&self.s)) |tok| {
        return tok;
    }

    while (true) {
        self.s.eatUntil([_]u8{ '\r', '\n' } ++ comment_beginnings);

        var s = self.s;

        if (eatCodebegin(&s) != null or s.eatNewline() or s.eat() == null) {
            break;
        }

        _ = self.s.eat(); // Skip past the comment beginning that didn't end up creating code
    }

    return .text;
}

fn eatCodebegin(s: *Scanner) ?SyntaxKind {
    if (s.eatIf(comment_beginnings)) {
        if (s.eatIf('*')) {
            return if (s.eatIf('*')) .codeblock_delim else .code_begin;
        }
        s.moveTo(s.cursor - 1);
    }

    return null;
}
const std = @import("std");

const Scanner = @import("Scanner.zig");
const SyntaxError = @import("node.zig").SyntaxError;
const SyntaxKind = @import("node.zig").SyntaxKind;
const SyntaxNode = @import("node.zig").SyntaxNode;
