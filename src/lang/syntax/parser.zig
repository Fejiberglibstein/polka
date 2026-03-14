pub const ParseResult = struct {
    /// Flattened list of all the nodes.
    nodes: []const SyntaxNode,
    /// List of all syntax errors.
    errors: []const SyntaxError,

    pub fn deinit(self: ParseResult, gpa: Allocator) void {
        gpa.free(self.nodes);
        gpa.free(self.errors);
    }
};

pub const SyntaxErrorKind = union(enum) {
    invalid_token,
    unexpected_token: SyntaxKind,
    expected_token: struct { expected: SyntaxKind, actual: SyntaxKind },
};

pub const SyntaxError = struct {
    range: []const u8,
    position: Lexer.Position,
    kind: SyntaxErrorKind,
};

pub fn parse(src: []const u8, mode: Lexer.Mode, gpa: Allocator) Allocator.Error!ParseResult {
    var p: Parser = try .init(src, mode, gpa);

    if (p.at(.eof)) {
        const m = p.marker();
        try p.eat();
        try p.wrap(m, .text);
    } else {
        try parseText(&p);
    }

    try p.eatExpect(.eof);
    _ = p.stack.pop(); // Discard the eof node

    const root_node = p.stack.pop() orelse unreachable;
    assert(root_node.kind == .text);
    assert(p.stack.items.len == 0);
    p.stack.deinit(gpa);

    try p.nodes.append(gpa, root_node);

    return .{
        .nodes = try p.nodes.toOwnedSlice(gpa),
        .errors = try p.errors.toOwnedSlice(gpa),
    };
}

fn parseText(p: *Parser) Allocator.Error!void {
    const m = p.marker();

    // Even when in a `.polka` file, all nodes are still wrapped in text
    if (p.mode() == .code_file) {
        try parseCode(p);
        try p.wrap(m, .text);
        return;
    }

    while (true) {
        if (p.isEndingKind(p.current.node.kind)) break;
        if (p.mode() != .text) {
            p.setMode(.text);
            p.reparse();
        }
        switch (p.current.node.kind) {
            .newline => try p.eat(),
            .text_line => try p.eat(),

            .code_begin => {
                p.setMode(.code_line);
                const m2 = p.marker();
                try p.eatAssert(.code_begin);
                try parseCode(p);
                try p.wrap(m2, .code);
            },

            .codeblock_delim => {
                p.setMode(.code_block);
                const m2 = p.marker();
                try p.eatAssert(.codeblock_delim);
                try parseCode(p);
                try p.eatExpect(.codeblock_delim);
                try p.wrap(m2, .code);
            },

            // Lexer doesn't produce any other tokens
            else => {
                try p.unexpected();
                try p.eat();
            },
        }
    }

    try p.wrap(m, .text);
}

fn parseCode(p: *Parser) Allocator.Error!void {
    while (true) {
        if (p.isEndingKind(p.current.node.kind)) break;
        try parseStatement(p);
        if (p.mode() == .code_line) {
            if (!try p.eatIf(.code_begin)) break;
        }
    }
}

fn parseStatement(p: *Parser) !void {
    if (try p.eatIf(.newline)) return;
    switch (p.current.node.kind) {
        .keyword_for => try parseForLoop(p),
        .keyword_if => try parseConditional(p),
        .keyword_while => try parseWhileLoop(p),
        .keyword_let => try parseLetStatement(p),
        .keyword_export => try parseExportStatement(p),
        .keyword_return => try parseReturnStatement(p),
        .keyword_break, .keyword_continue => try p.eat(),
        else => try parseExpression(p, 0),
    }

    if (!p.at(.eof)) {
        try p.eatExpect(.newline);
    }
}

fn parseExpression(p: *Parser, precedence: usize) Allocator.Error!void {
    const m = p.marker();

    if (ast.toASTNode(ast.UnaryOperator, 0, &.{p.current.node})) |op| {
        try p.eat();
        try parseExpression(p, op.precedence()); // Plus 1 since unary ops are right associative
        try p.wrap(m, .unary);
    }

    switch (p.current.node.kind) {
        .ident,
        .number,
        .integer,
        .keyword_nil,
        .keyword_true,
        .keyword_false,
        .static_string,
        => try p.eat(),

        .l_paren => {
            const m2 = p.marker();
            try p.eatAssert(.l_paren);
            try parseExpression(p, 0);
            try p.eatExpect(.r_paren);
            try p.wrap(m2, .grouping);
        },

        .keyword_func => try parseFunctionDefinition(p),

        .backtick => try parseMultilineString(p),

        else => {},
    }

    while (true) {
        // Parse a function call
        if (p.at(.l_paren)) {
            try parseFunctionCallArguments(p);
            try p.wrap(m, .function_call);
            continue;
        }

        if (try p.eatIf(.l_bracket)) {
            try parseExpression(p, 0);
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

        if (ast.toASTNode(ast.BinaryOperator, 0, &.{p.current.node})) |op| {
            if (op.precedence() >= precedence) {
                const new_precedence = switch (op.associativity()) {
                    .left => op.precedence() + 1,
                    .right => op.precedence(),
                };
                try p.eat();
                try parseExpression(p, new_precedence);
                try p.wrap(m, .binary);
                continue;
            }
        }

        if (m == p.marker()) try p.unexpected();

        break;
    }
}

fn parseFunctionCallArguments(p: *Parser) !void {
    const m = p.marker();
    try p.eatAssert(.l_paren);

    while (!try p.eatIf(.r_paren)) {
        try parseExpression(p, 0);
        if (!try p.eatIf(.comma)) {
            try p.eatExpect(.r_paren);
            break;
        }
    }
    try p.wrap(m, .function_args);
}

fn parseBlock(p: *Parser) !void {
    const old_end = p.ending_kind;
    const old_mode = p.switchToTextMode();
    p.ending_kind.addKind(.keyword_end);

    p.reparse();
    try parseText(p);

    // Switch mode before eating the end so that the next token is in the correct mode.
    p.setMode(old_mode);
    p.ending_kind = old_end;

    try p.eatExpect(.keyword_end);
}

fn parseFunctionParameters(p: *Parser) !void {
    const m = p.marker();
    try p.eatExpect(.l_paren);
    while (!try p.eatIf(.r_paren)) {
        try p.eatExpect(.ident);
        if (!try p.eatIf(.comma)) {
            try p.eatExpect(.r_paren);
            break;
        }
    }
    try p.wrap(m, .function_parameters);
}

fn parseFunctionDefinition(p: *Parser) !void {
    const m = p.marker();
    try p.eatAssert(.keyword_func);
    _ = try p.eatIf(.ident);
    try parseFunctionParameters(p);
    if (try p.eatIf(.newline))
        try parseBlock(p)
    else
        try parseExpression(p, 0);
    try p.wrap(m, .function_def);
}

fn parseMultilineString(p: *Parser) !void {
    const old_mode = p.mode();
    p.setMode(.multiline_string);
    defer p.setMode(old_mode);

    const m = p.marker();

    // Must eat backtick _after_ switching modes so that the next token is parsed in the correct
    // mode.
    try p.eatAssert(.backtick);

    while (true) {
        switch (p.current.node.kind) {
            .mls_text => try p.eat(),
            .eof => break,
            .newline => {
                p.setMode(old_mode);
                try p.eatAssert(.newline);

                if (old_mode == .code_line and !try p.eatIf(.code_begin)) break;
                if (!p.at(.backtick)) break;

                p.setMode(.multiline_string);
                try p.eatAssert(.backtick);
            },
            .at => {
                p.setMode(old_mode);
                const m2 = p.marker();

                // If the lexer yielded a `.at`, it _must be_ followed by an `.l_paren`. The lexer
                // specifically looks for the sequence "@(" to determine whether or not to return a
                // `.at`
                try p.eatAssert(.at);
                try p.eatAssert(.l_paren);

                try parseExpression(p, 0);

                // switch back to multiline string before eating the rparen so that the next token
                // is parsed in the correct mode.
                p.setMode(.multiline_string);
                try p.eatExpect(.r_paren);

                try p.wrap(m2, .mls_expression);
            },
            else => @panic("Lexer doesn't yield any other tokens while in multiline mode"),
        }
    }

    try p.wrap(m, .multiline_string);
}

fn parseConditional(p: *Parser) !void {
    const m = p.marker();
    try p.eatAssert(.keyword_if);
    try parseExpression(p, 0);
    try p.eatExpect(.keyword_then);
    // Switch nodes before consuming the newline so that the next token is tokenized in text mode
    const old_mode = p.switchToTextMode();
    try p.eatExpect(.newline);

    // Setup block to be parsed
    const old_end = p.ending_kind;
    p.ending_kind.addKind(.keyword_end);
    p.ending_kind.addKind(.keyword_else);
    p.ending_kind.addKind(.keyword_elseif);
    try parseText(p);

    while (true) {
        switch (p.current.node.kind) {
            .keyword_elseif => {
                try p.eatAssert(.keyword_elseif);
                try parseExpression(p, 0);
                try p.eatExpect(.keyword_then);
                _ = p.switchToTextMode();
                try p.eatExpect(.newline);

                try parseText(p);
            },

            .keyword_else => {
                try p.eatAssert(.keyword_else);
                _ = p.switchToTextMode();
                try p.eatExpect(.newline);

                p.ending_kind.removeKind(.keyword_else);
                p.ending_kind.removeKind(.keyword_elseif);
                try parseText(p);
            },

            .keyword_end => {
                try p.eatAssert(.keyword_end);
                break;
            },

            else => {
                try p.unexpected();
                break;
            },
        }
    }

    // Switch mode before eating the end so that the next token is in the correct mode.
    p.setMode(old_mode);
    p.reparse();
    p.ending_kind = old_end;

    try p.wrap(m, .conditional);
}

fn parseWhileLoop(p: *Parser) !void {
    const m = p.marker();
    try p.eatAssert(.keyword_while);
    try parseExpression(p, 0);
    try p.eatExpect(.keyword_do);
    try p.eatExpect(.newline);
    try parseBlock(p);
    try p.wrap(m, .while_loop);
}

fn parseForLoop(p: *Parser) !void {
    const m = p.marker();
    try p.eatAssert(.keyword_for);
    try p.eatExpect(.ident);
    try p.eatExpect(.keyword_in);
    try parseExpression(p, 0);
    try p.eatExpect(.keyword_do);
    try p.eatExpect(.newline);
    try parseBlock(p);
    try p.wrap(m, .for_loop);
}

fn parseReturnStatement(p: *Parser) !void {
    const m = p.marker();
    try p.eatAssert(.keyword_return);
    if (!p.at(.newline)) {
        try parseExpression(p, 0);
    }
    try p.wrap(m, .return_statement);
}

fn parseExportStatement(p: *Parser) !void {
    const m = p.marker();
    try p.eatAssert(.keyword_export);
    switch (p.current.node.kind) {
        .keyword_let => try parseLetStatement(p),
        .keyword_export => try parseExportStatement(p),
        else => try p.unexpected(),
    }
    try p.wrap(m, .export_statement);
}

fn parseLetStatement(p: *Parser) !void {
    const m = p.marker();
    try p.eatAssert(.keyword_let);
    try p.eatExpect(.ident);
    if (try p.eatIf(.eq)) try parseExpression(p, 0);
    try p.wrap(m, .let_statement);
}

fn skipNewlines(p: *Parser) !void {
    while (try eatNewline(p)) {}
}

fn expectNewline(p: *Parser) !void {
    if (p.at(.eof)) return;

    switch (p.mode()) {
        .code_line => {
            try p.eatExpect(.newline);
            try p.eatExpect(.code_begin);
        },
        .code_file, .code_block => _ = try p.eatExpect(.newline),
        .text => _ = try p.eatExpect(.newline),
        .multiline_string => _ = try p.eatExpect(.newline),
    }
}

fn eatNewline(p: *Parser) !bool {
    return switch (p.mode()) {
        .code_line => blk: {
            if (try p.eatIf(.newline)) {
                try p.eatExpect(.code_begin);
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
    current: Lexer.Token,
    /// List of all errors that have been encountered while parsing
    errors: std.ArrayList(SyntaxError),
    /// List of all SyntaxKinds that will cause the parser to break out of parsing code. This always
    /// include EOF, and may include `end`, `codeblock_delim`, and others.
    ending_kind: SyntaxSet,

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
            .ending_kind = .init(&.{.eof}),
        };

        self.current = self.l.next();

        return self;
    }

    fn mode(self: *Parser) Lexer.Mode {
        return self.l.mode;
    }

    fn setMode(self: *Parser, m: Lexer.Mode) void {
        self.l.mode = m;
    }

    fn switchToTextMode(self: *Parser) Lexer.Mode {
        const ret = self.mode();
        switch (self.mode()) {
            .code_file,
            .code_block,
            .text,
            => {},
            .code_line => self.setMode(.text),
            .multiline_string => self.setMode(.text),
        }
        return ret;
    }

    fn isEndingKind(self: Parser, kind: SyntaxKind) bool {
        return self.ending_kind.hasKind(kind);
    }

    fn at(self: Parser, kind: SyntaxKind) bool {
        return self.current.node.kind == kind;
    }

    fn eat(self: *Parser) !void {
        if (self.current.node.kind == .unexpected_character) {
            try self.addError(.{
                .position = self.current.position,
                .range = self.current.node.getLeafSource(self.text),
                .kind = .invalid_token,
            });
        }

        try self.stack.append(self.gpa, self.current.node);
        self.current = self.l.next();
    }

    fn eatAssert(self: *Parser, kind: SyntaxKind) !void {
        assert(self.current.node.kind == kind);
        try self.eat();
    }

    fn eatIf(self: *Parser, kind: SyntaxKind) !bool {
        if (self.at(kind)) {
            try self.eat();
            return true;
        }
        return false;
    }

    fn reparse(self: *Parser) void {
        self.l.reparse(self.current);
        self.current = self.l.next();
    }

    fn eatExpect(self: *Parser, kind: SyntaxKind) !void {
        if (self.current.node.kind != kind) {
            try self.addError(.{
                .position = self.current.position,
                .range = self.current.node.getLeafSource(self.text),
                .kind = .{
                    .expected_token = .{ .expected = kind, .actual = self.current.node.kind },
                },
            });
        }
        try self.eat();
    }

    fn unexpected(self: *Parser) !void {
        try self.addError(.{
            .position = self.current.position,
            .range = self.current.node.getLeafSource(self.text),
            .kind = .{ .unexpected_token = self.current.node.kind },
        });
    }

    fn addError(self: *Parser, err: SyntaxError) !void {
        try self.errors.append(self.gpa, err);
    }

    const Marker = usize;
    fn marker(self: Parser) Marker {
        return self.stack.items.len;
    }

    fn wrap(self: *Parser, m: Marker, kind: SyntaxKind) Allocator.Error!void {
        const offset = self.nodes.items.len;

        try self.nodes.appendSlice(self.gpa, self.stack.items[m..]);

        // Sizing down, so can't get an allocation error
        std.debug.assert(m <= self.stack.items.len);
        self.stack.resize(self.gpa, m) catch unreachable;

        const len = self.nodes.items.len - offset;

        assert(kind.getType() == .tree);
        try self.stack.append(self.gpa, SyntaxNode{
            .kind = kind,
            .data = .{ .tree = .{ .len = @intCast(len), .offset = @intCast(offset) } },
        });
    }
};

const std = @import("std");
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const Lexer = @import("Lexer.zig");
const SyntaxKind = @import("node.zig").SyntaxKind;
const SyntaxNode = @import("node.zig").SyntaxNode;
const SyntaxSet = @import("SyntaxSet.zig");
const assert = std.debug.assert;
