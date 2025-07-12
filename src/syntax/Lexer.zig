const std = @import("std");

const Scanner = @import("Scanner.zig");
const ErrorNode = @import("node.zig").ErrorNode;
const SyntaxError = @import("node.zig").SyntaxError;
const SyntaxNode = @import("node.zig").SyntaxNode;
const SyntaxKind = @import("node.zig").SyntaxKind;

const Lexer = @This();

s: Scanner,
mode: Mode,
currentError: ?SyntaxError,

pub fn init(source: []const u8) Lexer {
    return Lexer{
        .mode = .TopLevelText,
        .s = Scanner.init(source),
        .currentError = null,
    };
}

pub fn next(self: *Lexer) struct { SyntaxNode, SyntaxKind, Whitespace } {
    const before_spaces = self.s.cursor;
    self.s.eatSpaces();
    const start = self.s.cursor;
    const whitespace: u16 = @intCast(start - before_spaces);

    const kind = if (self.s.eatNewline())
        .newline
    else if (self.s.peek() == null)
        .eof
    else switch (self.mode) {
        .CodeLine, .CodeExpr, .CodeBlock => self.code(),
        .TopLevelText, .Text => self.text(),
    };

    const range = self.s.cursor - start;
    const node: SyntaxNode = if (self.currentError) |err|
        .err(err, range, whitespace)
    else
        .leaf(kind, range, whitespace);
    self.currentError = null;

    return .{ node, kind, if (whitespace != 0) .PrecedingWhitespace else .None };
}

pub fn reparse(self: *Lexer, node: SyntaxNode) void {
    self.s.moveTo(self.s.cursor - node.length());
}

fn setErr(self: *Lexer, err: SyntaxError) SyntaxKind {
    if (self.currentError == null) {
        self.currentError = err;
    }
    return .err;
}

fn code(self: *Lexer) SyntaxKind {
    const c = self.s.eat() orelse unreachable;

    return sw: switch (c) {
        '#' => if (self.s.eatIf('*')) .code_begin else continue :sw 0,
        '[' => .left_bracket,
        ']' => .right_bracket,
        '(' => .left_paren,
        ')' => .right_paren,
        '{' => .left_brace,
        '}' => .right_brace,
        '+' => .plus,
        '%' => .perc,
        '/' => .slash,
        // TODO: Fix this and make `self.eat_codeblockend`
        '*' => if (self.s.eatIf("*#"))
            .codeblock_end
        else
            .star,
        '-' => .minus,
        '.' => .dot,
        ',' => .comma,
        '`' => .backtick,
        '=' => if (self.s.eatIf('=')) .eq_eq else .eq,
        '!' => if (self.s.eatIf('=')) .not_eq else continue :sw 0,
        '<' => if (self.s.eatIf('=')) .lt_eq else .lt,
        '>' => if (self.s.eatIf('=')) .gt_eq else .gt,
        '"' => self.string(),
        '0'...'9' => self.number(),
        'a'...'z', 'A'...'Z', '_' => self.ident(),
        else => self.setErr(.{ .UnexpectedCharacter = c }),
    };
}

fn string(self: *Lexer) SyntaxKind {
    while (true) {
        self.s.eatUntil([_]u8{ '\\', '"', '\n', '\r' });

        if (self.s.eatNewline()) {
            return self.setErr(.UnterminatedString);
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

fn keyword(t: []const u8) ?SyntaxKind {
    return if (std.mem.eql(u8, t, "let"))
        .let
    else if (std.mem.eql(u8, t, "export"))
        .@"export"
    else if (std.mem.eql(u8, t, "if"))
        .@"if"
    else if (std.mem.eql(u8, t, "then"))
        .then
    else if (std.mem.eql(u8, t, "do"))
        .do
    else if (std.mem.eql(u8, t, "in"))
        .in
    else if (std.mem.eql(u8, t, "for"))
        .@"for"
    else if (std.mem.eql(u8, t, "while"))
        .@"while"
    else if (std.mem.eql(u8, t, "function"))
        .function
    else if (std.mem.eql(u8, t, "else"))
        .@"else"
    else if (std.mem.eql(u8, t, "end"))
        .end
    else if (std.mem.eql(u8, t, "or"))
        .@"or"
    else if (std.mem.eql(u8, t, "and"))
        .@"and"
    else if (std.mem.eql(u8, t, "true"))
        .bool
    else if (std.mem.eql(u8, t, "false"))
        .bool
    else if (std.mem.eql(u8, t, "nil"))
        .nil
    else if (std.mem.eql(u8, t, "return"))
        .@"return"
    else if (std.mem.eql(u8, t, "continue"))
        .@"continue"
    else if (std.mem.eql(u8, t, "break"))
        .@"break"
    else
        null;
}

/// Parse a token while in text mode
fn text(self: *Lexer) SyntaxKind {
    if (self.eatCodebegin()) {
        return .code_begin;
    }

    while (true) {
        self.s.eatUntil([_]u8{ '\r', '\n', '\\', '`', '#' });

        var s = self.s;

        switch (s.eat() orelse 0) {
            '\\' => _ = s.eatIf('`'),
            '#' => if (s.at('*')) {
                break;
            },

            '`' => if (self.mode != .TopLevelText) {
                // Consume the `
                _ = self.s.eat();
                break;
            },
            else => break,
        }

        self.s = s;
    }

    return .text;
}

fn eatCodebegin(self: *Lexer) bool {
    return if (self.mode != .CodeBlock)
        self.s.eatIf("#*") or self.s.eatIf(";*")
    else
        true;
}

pub const Mode = enum {
    /// Lines beginning with `#*`. If any text comes before the `#*`, then the Mode would be
    /// [Mode::CodeExpr] instead
    ///
    /// ```text
    ///     #* if (sys.hostname == "foo")
    ///     #*   // ...
    ///     #* end
    /// ```
    CodeLine,
    /// While in text mode, expressions beginning with `#*`
    ///
    /// ```text
    ///     The hostname is #*sys.hostname
    /// ```
    CodeExpr,
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
    ///     **#
    /// ```
    CodeBlock,
    /// Normal file contents
    TopLevelText,
    /// Text inside ``
    Text,

    fn isCode(self: Mode) bool {
        return self == .Codeline or self == .CodeBlock or self == .CodeExpr;
    }
};
pub const Whitespace = enum { PrecedingWhitespace, None };
