pub const Mode = enum {
    /// Parsing code line
    code_line,
    /// Parsing code block
    code_block,
    /// Parsing code file
    code_file,
    /// Parsing text
    text,
    /// Parsing a multiline string
    multiline_string,
};

src: []const u8,
position: Position,
s: Scanner,
mode: Mode,
comment_string: []const u8,

pub const Lexer = @This();

pub fn init(src: []const u8, mode: Mode, comment_string: []const u8) Lexer {
    return .{
        .position = .{
            .col = 1,
            .line = 1,
        },
        .src = src,
        .s = Scanner.init(src),
        .mode = mode,
        .comment_string = comment_string,
    };
}

pub const Position = struct {
    line: u32,
    col: u32,
};

pub const Token = struct {
    node: SyntaxNode,
    position: Position,
    start_index: u32,
    before_whitespace_index: u32,
};

pub fn reparse(self: *Lexer, token: Token) void {
    self.position = .{
        .line = token.position.line,
        .col = if (token.position.col != 1)
            token.position.col - (token.start_index - token.before_whitespace_index)
        else
            // It is 1 when we just started the newline and thus there wouldn't be any previous
            // whitespace
            1,
    };
    self.s.moveTo(token.before_whitespace_index);
}

pub fn next(self: *Lexer) Token {
    const before_whitespace = self.s.cursor;
    if (self.mode != .text) {
        self.s.eatWhitespace();
    }
    const start = self.s.cursor;
    self.position.col += @intCast(start - before_whitespace);

    const position = self.position;

    const kind: SyntaxKind = if (self.s.peek() == null)
        .eof
    else if (self.s.eatNewline()) blk: {
        self.position.line += 1;
        self.position.col = 1;
        break :blk .newline;
    } else blk: {
        self.position.col += @intCast(self.s.cursor - start);

        break :blk switch (self.mode) {
            .multiline_string => self.multilineString(),
            .code_file, .code_block, .code_line => self.code(),

            .text => {
                if (self.eatCodeBeginOrDelim()) |kind| break :blk kind;
                self.s.eatUntil(.{ .any = &.{ '\r', '\n' } });
                break :blk .text_line;
            },
        };
    };

    self.position.col += @intCast(self.s.cursor - start);
    const end = self.s.cursor;

    const node: SyntaxNode = .{
        .kind = kind,
        .data = .{
            .leaf = .{ .offset = @intCast(start), .len = @intCast(end - start) },
        },
    };

    return .{
        .node = node,
        .position = position,
        .start_index = @intCast(start),
        .before_whitespace_index = @intCast(before_whitespace),
    };
}

fn multilineString(self: *Lexer) SyntaxKind {
    if (self.s.at(.{ .str = "@(" })) {
        assert(self.s.eat() == '@');
        return .at;
    }

    while (true) {
        self.s.eatUntil(.{ .any = &.{ '\r', '\n', '@' } });

        if (!self.s.at(.{ .char = '@' })) break; // Break if we are at a newline
        if (self.s.at(.{ .str = "@(" })) break; //  Break _only_ if we have @(

        // If we don't have @(, we should consume the @
        const char = self.s.eat() orelse break;
        assert(char == '@');
    }
    return .mls_text;
}

fn code(self: *Lexer) SyntaxKind {
    if (eatCodeBeginOrDelim(self)) |tok| return tok;

    return sw: switch (self.s.eat() orelse 0) {
        '@' => .at,
        '.' => .dot,
        '+' => .plus,
        '*' => .star,
        '-' => .minus,
        '/' => .slash,
        ',' => .comma,
        '%' => .percent,
        '(' => .l_paren,
        ')' => .r_paren,
        '{' => .l_brace,
        '}' => .r_brace,
        '`' => .backtick,
        '[' => .l_bracket,
        ']' => .r_bracket,
        '=' => if (self.s.eatIf(.{ .char = '=' })) .eq_eq else .eq,
        '<' => if (self.s.eatIf(.{ .char = '=' })) .lt_eq else .lt,
        '>' => if (self.s.eatIf(.{ .char = '=' })) .gt_eq else .gt,
        '~' => if (self.s.eatIf(.{ .char = '=' })) .not_eq else continue :sw 0,

        '"' => string(self),
        '0'...'9' => self.number(),
        'a'...'z', 'A'...'Z', '_' => ident(self),

        else => .unexpected_character,
    };
}

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isIdentChar(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or isDigit(c) or c == '_';
}

fn number(self: *Lexer) SyntaxKind {
    var kind: SyntaxKind = .integer;
    self.s.eatWhile(.{ .func = isDigit });
    if (self.s.eatIf(.{ .char = '.' })) {
        kind = .number;
        self.s.eatWhile(.{ .func = isDigit });
    }
    return kind;
}

fn ident(self: *Lexer) SyntaxKind {
    // we already parsed the first character of the ident
    const cursor = self.s.cursor - 1;
    self.s.eatWhile(.{ .func = isIdentChar });

    return if (keyword(self.s.from(cursor))) |k|
        k
    else
        .ident;
}

const keywords: std.StaticStringMap(SyntaxKind) = .initComptime(.{
    .{ "nil", .keyword_nil },
    .{ "true", .keyword_true },
    .{ "false", .keyword_false },
    .{ "in", .keyword_in },
    .{ "or", .keyword_or },
    .{ "and", .keyword_and },
    .{ "not", .keyword_not },
    .{ "for", .keyword_for },
    .{ "while", .keyword_while },
    .{ "do", .keyword_do },
    .{ "if", .keyword_if },
    .{ "then", .keyword_then },
    .{ "else", .keyword_else },
    .{ "elseif", .keyword_elseif },
    .{ "end", .keyword_end },
    .{ "export", .keyword_export },
    .{ "return", .keyword_return },
    .{ "let", .keyword_let },
    .{ "func", .keyword_func },
    .{ "continue", .keyword_continue },
    .{ "break", .keyword_break },
});

pub fn keyword(str: []const u8) ?SyntaxKind {
    return keywords.get(str);
}

fn string(self: *Lexer) SyntaxKind {
    while (true) {
        self.s.eatUntil(.{ .any = &.{ '\\', '"', '\n', '\r' } });

        if (self.s.eatNewline()) {
            return .unexpected_character;
        }

        switch (self.s.eat() orelse 0) {
            '\\' => _ = self.s.eatIf(.{ .char = '"' }),
            '"' => break,

            else => {},
        }
    }
    return .static_string;
}

fn eatCodeBegin(self: *Lexer) ?SyntaxKind {
    const start = self.s.cursor;
    if (self.s.eatIf(.{ .str = self.comment_string })) {
        if (self.s.eatIf(.{ .char = '*' })) {
            return .code_begin;
        } else {
            self.s.moveTo(start);
        }
    }
    return null;
}

fn eatCodeDelim(self: *Lexer) ?SyntaxKind {
    const start = self.s.cursor;
    if (self.s.eatIf(.{ .str = self.comment_string })) {
        if (self.s.eatIf(.{ .str = "**" })) {
            return .code_delim;
        } else {
            self.s.moveTo(start);
        }
    }
    return null;
}

fn eatCodeBeginOrDelim(self: *Lexer) ?SyntaxKind {
    const start = self.s.cursor;
    if (self.s.eatIf(.{ .str = self.comment_string })) {
        if (self.s.eatIf(.{ .char = '*' })) {
            return if (self.s.eatIf(.{ .char = '*' }))
                .codeblock_delim
            else
                .code_begin;
        } else {
            self.s.moveTo(start);
        }
    }
    return null;
}

const std = @import("std");
const assert = std.debug.assert;

const Scanner = @import("Scanner.zig");
const SyntaxKind = @import("node.zig").SyntaxKind;
const SyntaxNode = @import("node.zig").SyntaxNode;
