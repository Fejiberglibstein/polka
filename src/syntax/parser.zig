pub fn parse(src: []const u8, mode: Lexer.Mode, gpa: Allocator) Allocator.Error![]const SyntaxNode {
    var parser: Parser = try .init(src, mode, gpa);
    try parseText(&parser);

    return &.{};
}

fn parseText(p: *Parser) Allocator.Error!void {
    const m = p.marker();
    while (true) {
        if (p.at(.eof)) {
            break;
        }

        switch (p.current.node.kind) {
            .text_line => try p.eat(),
            .newline => try p.eat(),
            .code_begin => {
                p.setMode(.code_line);
                const m2 = p.marker();
                try p.eatAssert(.code_begin);
                try parseCode(p);
                try p.wrap(m2, .code);
                p.setMode(.text);
            },
            .codeblock_delim => {
                p.setMode(.code_block);
                const m2 = p.marker();
                try p.eatAssert(.codeblock_delim);
                try parseCode(p);
                try p.wrap(m2, .code);
                p.setMode(.text);
            },
            // Lexer doesn't produce any other tokens
            else => unreachable,
        }
    }

    try p.wrap(m, .text);
}

fn parseCode(p: *Parser) Allocator.Error!void {
    while (true) {
        try parseStatement(p);
    }
}

fn parseStatement(p: *Parser) !void {
    switch (p.current.node.kind) {
        .keyword_while => try parseWhileLoop(p),
        .keyword_for => try parseForLoop(p),
        .keyword_let => try parseLetStatement(p),
        else => try parseExpression(p),
    }
}

fn parseExpression(p: *Parser) Allocator.Error!void {
    const m = p.marker();

    switch (p.current.node.kind) {
        .ident,
        .number,
        .string,
        .keyword_nil,
        .keyword_true,
        .keyword_false,
        => try p.eatAssert(.ident),

        .l_paren => {
            const m2 = p.marker();
            try p.eatAssert(.l_paren);
            try parseExpression(p);
            try p.eatExpect(.r_paren);
            try p.wrap(m2, .grouping);
        },

        .keyword_if => try parseConditional(p),

        .keyword_func => try parseFunctionDefinition(p),

        else => {},
    }

    while (true) {
        // Parse a function call
        if (p.current.node.kind == .l_paren) {
            try parseFunctionCallArguments(p);
            try p.wrap(m, .function_call);
            continue;
        }

        if (try p.eatIf(.l_bracket)) {
            try parseExpression(p);
            try p.eatExpect(.r_bracket);
            try p.wrap(m, .bracket_access);
            continue;
        }

        // Parse dot access
        if (try p.eatIf(.dot)) {
            try p.eatExpect(.ident);
            try p.wrap(m, .dot_access);
            continue;
        }

        if (m == p.marker()) try p.eatUnexpected();

        break;
    }
}

fn parseFunctionCallArguments(p: *Parser) !void {
    const m = p.marker();
    try p.eatAssert(.l_paren);

    while (!try p.eatIf(.r_paren)) {
        try parseExpression(p);
        if (!try p.eatIf(.comma)) {
            try p.eatExpect(.r_paren);
            break;
        }
        try p.wrap(m, .function_args);
    }
}

fn parseFunctionDefinition(p: *Parser) !void {
    try p.eatAssert(.keyword_func);

    const m = p.marker();
    try p.eatExpect(.l_paren);

    while (!try p.eatIf(.r_paren)) {
        try p.eatExpect(.ident);
        if (!try p.eatIf(.comma)) {
            try p.eatExpect(.r_paren);
            break;
        }
        try p.wrap(m, .function_parameters);
    }
}

fn parseConditional(p: *Parser) !void {
    try p.eatAssert(.keyword_if);
    try parseExpression(p);
    try p.eatExpect(.keyword_then);
}

fn parseWhileLoop(p: *Parser) !void {}
fn parseForLoop(p: *Parser) !void {}
fn parseLetStatement(p: *Parser) !void {
    const m = p.marker();
    try p.eatAssert(.keyword_let);
    try p.eatExpect(.ident);
    if (try p.eatIf(.eq)) try parseExpression(p);
    try p.wrap(m, .let_statement);
}

fn skipNewlines(p: *Parser) !void {
    while (eatNewline(p)) {}
}

fn eatNewline(p: *Parser) !bool {
    return switch (p.mode()) {
        .code_line => blk: {
            if (try p.eatIf(.newline)) {
                try p.eatExpect(.code_start);
                break :blk true;
            }
            break :blk false;
        },
        .code_file, .code_block => try p.eatIf(.newline),
        .text => try p.eatIf(.newline),
    };
}

const Parser = struct {
    text: []const u8,
    gpa: Allocator,
    l: Lexer,
    /// Stack of the current nodes we haven't yet parsed into a tree node.
    stack: std.ArrayList(SyntaxNode),
    /// A flattened list of the entire AST
    nodes: std.ArrayList(SyntaxNode),
    /// Current token we're looking at
    current: Token,
    /// List of all errors that have been encountered while parsing
    errors: std.ArrayList(SyntaxError),

    const Marker = usize;
    const Token = struct {
        node: SyntaxNode,
        position: Lexer.Position,
    };

    fn mode(self: *Parser) Lexer.Mode {
        return self.l.mode;
    }

    fn setMode(self: *Parser, m: Lexer.Mode) void {
        self.l.mode = m;
    }

    fn marker(self: Parser) Marker {
        return self.stack.items.len;
    }

    fn eat(self: *Parser) Allocator.Error!void {
        try self.stack.append(self.gpa, self.current.node);
        try self.nextToken();
    }

    fn eatAssert(self: *Parser, kind: SyntaxKind) Allocator.Error!void {
        std.debug.assert(self.current.node.kind == kind);
        try self.eat();
    }

    fn at(self: Parser, kind: SyntaxKind) bool {
        return self.current.node.kind == kind;
    }

    fn eatIf(self: *Parser, kind: SyntaxKind) Allocator.Error!bool {
        if (self.at(kind)) {
            try self.eat();
            return true;
        }
        return false;
    }

    fn wrap(self: *Parser, m: Marker, kind: SyntaxKind) Allocator.Error!void {
        const offset = self.nodes.items.len;

        try self.nodes.appendSlice(self.gpa, self.stack.items[m..]);

        // Sizing down, so can't get an allocation error
        std.debug.assert(m <= self.stack.items.len);
        self.stack.resize(self.gpa, m) catch unreachable;

        const len = self.nodes.items.len - offset;

        try self.stack.append(self.gpa, SyntaxNode{
            .kind = kind,
            .data = .{ .tree = .{ .len = @intCast(len), .offset = @intCast(offset) } },
        });
    }

    fn isEndingKind(self: Parser, kind: SyntaxKind) bool {
        return self.finish_on.hasKind(kind);
    }

    fn init(src: []const u8, m: Lexer.Mode, gpa: Allocator) !Parser {
        const lexer = Lexer.init(src, m, "#");

        var self: Parser = .{
            .text = src,
            .l = lexer,
            .nodes = .empty,
            .stack = .empty,
            .errors = .empty,
            .current = undefined,
            .gpa = gpa,
        };

        try self.nextToken();

        return self;
    }

    fn addError(self: *Parser, err: SyntaxError) !void {
        try self.errors.append(self.gpa, err);
    }

    fn eatUnexpected(self: *Parser) !void {
        try self.addError(.{
            .pos = self.current.position,
            .range = self.current.node.getLeafSource(self.text),
            .kind = .{ .unexpected_token = self.current.node.kind },
        });
        try self.eat();
    }

    fn eatExpect(self: *Parser, kind: SyntaxKind) !void {
        if (self.current.node.kind != kind) {
            try self.addError(.{
                .pos = self.current.position,
                .range = self.current.node.getLeafSource(self.text),
                .kind = .{
                    .expected_token = .{ .expected = kind, .actual = self.current.node.kind },
                },
            });
        }
        try self.eat();
    }

    fn nextToken(self: *Parser) !void {
        const node, const position, const err = self.l.next();

        if (err) |e| try self.addError(e);

        self.current = .{
            .node = node,
            .position = position,
        };
    }
};

const std = @import("std");
const Allocator = std.mem.Allocator;

const Lexer = @import("Lexer.zig");
const SyntaxKind = @import("node.zig").SyntaxKind;
const SyntaxNode = @import("node.zig").SyntaxNode;
const SyntaxError = @import("errors.zig").SyntaxError;
