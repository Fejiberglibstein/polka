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

    pub fn isCode(mode: Mode) bool {
        return mode != .text;
    }
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

pub fn reparse(lexer: *Lexer, token: Token) void {
    lexer.position = .{
        .line = token.position.line,
        .col = if (token.position.col != 1)
            token.position.col - (token.start_index - token.before_whitespace_index)
        else
            // It is 1 when we just started the newline and thus there wouldn't be any previous
            // whitespace
            1,
    };
    lexer.s.moveTo(token.before_whitespace_index);
}

pub fn next(lexer: *Lexer) Token {
    const before_whitespace = lexer.s.cursor;
    if (!(lexer.mode == .text or lexer.mode == .multiline_string)) {
        lexer.s.eatWhitespace();
    }
    const start = lexer.s.cursor;
    lexer.position.col += @intCast(start - before_whitespace);

    const position = lexer.position;

    const kind: SyntaxKind = if (lexer.s.peek() == null)
        .eof
    else if (lexer.s.eatNewline()) blk: {
        lexer.position.line += 1;
        lexer.position.col = 1;
        break :blk .newline;
    } else blk: {
        lexer.position.col += @intCast(lexer.s.cursor - start);

        break :blk switch (lexer.mode) {
            .multiline_string => lexer.multilineString(),
            .code_file, .code_block, .code_line => lexer.code(),

            .text => {
                if (lexer.eatCodeBeginOrDelim()) |kind| break :blk kind;
                lexer.s.eatUntil(.{ .any = &.{ '\r', '\n' } });
                break :blk .text_line;
            },
        };
    };

    lexer.position.col += @intCast(lexer.s.cursor - start);
    const end = lexer.s.cursor;

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

fn multilineString(lexer: *Lexer) SyntaxKind {
    if (lexer.s.at(.{ .str = "@(" })) {
        assert(lexer.s.eat() == '@');
        return .at;
    }

    while (true) {
        lexer.s.eatUntil(.{ .any = &.{ '\r', '\n', '@' } });

        if (!lexer.s.at(.{ .char = '@' })) break; // Break if we are at a newline
        if (lexer.s.at(.{ .str = "@(" })) break; //  Break _only_ if we have @(

        // If we don't have @(, we should consume the @
        const char = lexer.s.eat() orelse break;
        assert(char == '@');
    }
    return .mls_text;
}

fn code(lexer: *Lexer) SyntaxKind {
    if (eatCodeBeginOrDelim(lexer)) |tok| return tok;

    return sw: switch (lexer.s.eat() orelse 0) {
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
        '=' => if (lexer.s.eatIf(.{ .char = '=' })) .eq_eq else .eq,
        '<' => if (lexer.s.eatIf(.{ .char = '=' })) .lt_eq else .lt,
        '>' => if (lexer.s.eatIf(.{ .char = '=' })) .gt_eq else .gt,
        '~' => if (lexer.s.eatIf(.{ .char = '=' })) .not_eq else continue :sw 0,

        '#' => lexer.color(),
        '"' => lexer.string(),
        '0'...'9' => lexer.number(),
        'a'...'z', 'A'...'Z', '_' => lexer.ident(),

        else => .unexpected_character,
    };
}

pub fn isIdentChar(c: u8) bool {
    return switch (c) {
        '0'...'9', 'A'...'Z', 'a'...'z', '_' => true,
        else => false,
    };
}

fn isHex(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn number(lexer: *Lexer) SyntaxKind {
    var kind: SyntaxKind = .integer;
    lexer.s.eatWhile(.{ .func = std.ascii.isDigit });
    if (lexer.s.eatIf(.{ .char = '.' })) {
        kind = .number;
        lexer.s.eatWhile(.{ .func = std.ascii.isDigit });
    }
    return kind;
}

fn color(lexer: *Lexer) SyntaxKind {
    if (!lexer.s.at(.{ .func = std.ascii.isHex }))
        return .unexpected_character;

    const start = lexer.s.cursor;
    lexer.s.eatWhile(.{ .func = std.ascii.isHex });
    const len = lexer.s.cursor - start;

    return if (len == 6 or len == 8)
        .color
    else
        .invalid_color;
}

fn ident(lexer: *Lexer) SyntaxKind {
    // we already parsed the first character of the ident
    const cursor = lexer.s.cursor - 1;
    lexer.s.eatWhile(.{ .func = isIdentChar });

    return if (keyword(lexer.s.from(cursor))) |k|
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

fn string(lexer: *Lexer) SyntaxKind {
    while (true) {
        lexer.s.eatUntil(.{ .any = &.{ '\\', '"', '\n', '\r' } });

        switch (lexer.s.peek() orelse 0) {
            '\\' => {
                _ = lexer.s.eat();
                _ = lexer.s.eatIf(.{ .char = '"' });
            },
            '"' => {
                assert(lexer.s.eat() == '"');
                break;
            },

            '\n', '\r', 0 => return .unexpected_character,
            else => unreachable,
        }
    }
    return .static_string;
}

fn eatCodeBegin(lexer: *Lexer) ?SyntaxKind {
    const start = lexer.s.cursor;
    if (lexer.s.eatIf(.{ .str = lexer.comment_string })) {
        if (lexer.s.eatIf(.{ .char = '*' })) {
            return .code_begin;
        } else {
            lexer.s.moveTo(start);
        }
    }
    return null;
}

fn eatCodeDelim(lexer: *Lexer) ?SyntaxKind {
    const start = lexer.s.cursor;
    if (lexer.s.eatIf(.{ .str = lexer.comment_string })) {
        if (lexer.s.eatIf(.{ .str = "**" })) {
            return .code_delim;
        } else {
            lexer.s.moveTo(start);
        }
    }
    return null;
}

fn eatCodeBeginOrDelim(lexer: *Lexer) ?SyntaxKind {
    const start = lexer.s.cursor;
    if (lexer.s.eatIf(.{ .str = lexer.comment_string })) {
        if (lexer.s.eatIf(.{ .char = '*' })) {
            return if (lexer.s.eatIf(.{ .char = '*' }))
                .codeblock_delim
            else
                .code_begin;
        } else {
            lexer.s.moveTo(start);
        }
    }
    return null;
}

const std = @import("std");
const assert = std.debug.assert;

const Scanner = @import("Scanner.zig");
const SyntaxKind = @import("node.zig").SyntaxKind;
const SyntaxNode = @import("node.zig").SyntaxNode;
