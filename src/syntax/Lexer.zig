pub const Mode = enum {
    /// Parsing code line
    code_line,
    /// Parsing code block
    code_block,
    /// Parsing code file
    code_file,
    /// Parsing text
    text,
};

src: []const u8,
line: u32,
col: u32,
s: Scanner,
mode: Mode,

pub const Lexer = @This();

pub fn init(src: []const u8, mode: Mode) Lexer {
    return .{
        .col = 1,
        .line = 1,
        .src = src,
        .s = Scanner.init(src),
        .mode = mode,
    };
}

pub fn next(self: *Lexer) SyntaxNode {
    _ = self;
    return undefined;
}

const std = @import("std");

const node = @import("node.zig");
const SyntaxNode = node.SyntaxNode;
const SyntaxKind = node.SyntaxKind;
const Scanner = @import("Scanner.zig");
