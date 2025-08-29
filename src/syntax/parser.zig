const std = @import("std");
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const Lexer = @import("Lexer.zig");
const Mode = @import("Lexer.zig").Mode;
const SyntaxKind = @import("node.zig").SyntaxKind;
const SyntaxNode = @import("node.zig").SyntaxNode;
const SyntaxSet = @import("SyntaxSet.zig");

const ParseResult = struct {
    root_node: SyntaxNode,
    all_nodes: []SyntaxNode,
    has_error: bool,
};

/// Parses top level text of a file
pub fn parse(text: []const u8, allocator: std.mem.Allocator) Allocator.Error!ParseResult {
    var parser = Parser.init(text, allocator);
    try parseText(&parser);

    std.debug.assert(parser.stack.items.len == 1);
    const node = parser.stack.items[0];
    parser.stack.deinit();
    return .{
        .root_node = node,
        .all_nodes = try parser.nodes.toOwnedSlice(),
        .has_error = parser.has_error,
    };
}

fn parseText(p: *Parser) Allocator.Error!void {
    const m = p.marker();
    if (p.mode() == .code_block) {
        try parseCode(p);
        try p.wrap(.code, m);
        try p.wrap(.text_node, m);
        return;
    }

    while (true) {
        if (p.isEndingKind(p.current.kind)) {
            break;
        }

        switch (p.current.kind) {
            .text => try p.eat(),
            .newline => try p.eat(),
            .codeblock_delim => {
                p.setMode(.code_block);

                const old_end = p.finish_on;
                p.finish_on.addKind(.codeblock_delim);

                const m2 = p.marker();
                try p.assert(.codeblock_delim);
                try parseCode(p);

                p.finish_on = old_end;
                // Set the mode before expecting delim since we need the next token to be parsed in
                // text mode.
                p.setMode(.text);
                try p.expect(.codeblock_delim);
                try p.wrap(.code, m2);
            },
            .code_begin => {
                p.setMode(if (p.lastKind() == .newline) .code_line else .code_expr);

                const m2 = p.marker();
                try p.assert(.code_begin);

                if (p.mode() == .code_line) {
                    try parseCode(p);
                } else {
                    try parseExpr(p, 0, true);
                }

                try p.wrap(.code, m2);
                p.setMode(.text);

                if (!p.isEndingKind(p.current.kind)) {
                    // Since the mode was just switched, the current token will be a code mode
                    // token, not a text mode token which is needed now that we're back in text
                    // mode.
                    p.reparse();
                }
            },
            // this shouldn't ever happen since we don't produce any other tokens in the lexer
            else => try p.unexpected(),
        }
    }

    try p.wrap(.text_node, m);
}

fn parseCode(p: *Parser) Allocator.Error!void {
    while (true) {
        if (p.isEndingKind(p.current.kind)) {
            break;
        }

        switch (p.current.kind) {
            .newline => {
                try p.eat();
                switch (p.mode()) {
                    .code_line => if (p.current.kind == .code_begin) {
                        try p.eat();
                    } else {
                        break;
                    },

                    .code_block => {},

                    // This should never happen
                    .text, .code_expr => try p.unexpected(),
                }
            },
            .@"export" => try parseExportExpr(p),
            .let => try parseLetExpr(p),
            .@"if" => try parseIfExpr(p),
            .@"for" => try parseForExpr(p),
            .@"while" => try parseWhileExpr(p),
            .function => try parseFunctionDef(p),
            .@"return" => {
                const m = p.marker();
                try p.assert(.@"return");
                if (!p.at(.newline)) {
                    try parseExpr(p, 0, false);
                }
                try p.wrap(.return_expr, m);
            },
            else => try parseExpr(p, 0, false),
        }
    }
}

/// Parse function call arguments
fn parseArgs(p: *Parser) Allocator.Error!void {
    const m = p.marker();

    try p.expect(.left_paren);
    if (!p.at(.right_paren)) {
        while (true) {
            try parseExpr(p, 0, false);
            if (!try p.eatIf(.comma)) {
                break;
            }
        }
    }

    try p.expect(.right_paren);
    try p.wrap(.argument_list, m);
}

/// Parse an expression
fn parseExpr(p: *Parser, prec: usize, expr: bool) Allocator.Error!void {
    if (expr and p.current.whitespace == .preceding_whitespace) {
        return;
    }

    const m = p.marker();
    try parsePrimary(p);

    while (true) {
        if (expr and p.current.whitespace == .preceding_whitespace) {
            break;
        }

        if (p.current.kind == .left_paren) {
            try parseArgs(p);
            try p.wrap(.function_call, m);
            continue;
        }

        if (try p.eatIf(.left_bracket)) {
            try parseExpr(p, 0, false);
            try p.expect(.right_bracket);
            try p.wrap(.bracket_access, m);
            continue;
        }

        if (try p.eatIf(.dot)) {
            try p.expect(.ident);
            try p.wrap(.dot_access, m);
            continue;
        }

        if (ast.BinaryOperator.toTyped(&p.current.node)) |op| {
            // If we have a higher precedence than the current precedence
            if (op.precedence() >= prec) {
                const new_prec = switch (op.associativity()) {
                    .left => op.precedence() + 1,
                    .right => op.precedence(),
                };
                try p.eat();
                try parseExpr(p, new_prec, expr);
                try p.wrap(.binary, m);
                continue;
            }
        }

        if (m == p.marker()) {
            try p.unexpected();
        }

        break;
    }
}
pub fn parseDictOrList(p: *Parser) Allocator.Error!void {
    const m = p.marker();

    try p.expect(.left_brace);

    if (try p.eatIf(.right_brace)) {
        try p.wrap(.list, m);
        return;
    } else if (try p.eatIf(.colon)) {
        try p.expect(.right_brace);
        try p.wrap(.dict, m);
        return;
    }

    // TODO fix this in the future, this is really messy

    _ = try p.eatNewlines();
    const finishing_kind: SyntaxKind = if (try p.eatIf(.ident)) blk: {
        if (try p.eatIf(.comma)) {
            // Parse a list
            _ = try p.eatNewlines();
            while (!try p.eatIf(.right_brace)) {
                try parseExpr(p, 0, false);
                if (!try p.eatIf(.comma)) {
                    _ = try p.eatNewlines();
                    try p.expect(.right_brace);
                    break;
                }
                _ = try p.eatNewlines();
            }
            break :blk .list;
        }

        if (try p.eatNewlines()) {
            try p.expect(.right_brace);
            break :blk .list;
        }

        if (try p.eatIf(.right_brace)) {
            break :blk .list;
        }

        // Parse a dictionary
        try p.expect(.colon);
        try parseExpr(p, 0, false);
        _ = try p.eatIf(.comma);

        _ = try p.eatNewlines();
        while (!try p.eatIf(.right_brace)) {
            try p.expect(.ident);
            try p.expect(.colon);
            try parseExpr(p, 0, false);

            if (!try p.eatIf(.comma)) {
                _ = try p.eatNewlines();
                try p.expect(.right_brace);
                break;
            }
            _ = try p.eatNewlines();
        }

        break :blk .dict;
    } else blk: {
        // Parse a list
        while (!try p.eatIf(.right_brace)) {
            try parseExpr(p, 0, false);
            if (!try p.eatIf(.comma)) {
                _ = try p.eatNewlines();
                try p.expect(.right_brace);
                break;
            }
            _ = try p.eatNewlines();
        }
        break :blk .list;
    };

    try p.wrap(finishing_kind, m);
}

/// Parse a primary node.
///
/// Primaries are strings, numbers, bools, identifiers (variables), and lua-like tables
fn parsePrimary(p: *Parser) Allocator.Error!void {
    switch (p.current.kind) {
        .ident, .string, .number, .bool, .nil => try p.eat(),
        .function => try parseFunctionDef(p),
        .left_brace => try parseDictOrList(p),
        .left_paren => {
            const m = p.marker();
            try p.assert(.left_paren);
            try parseExpr(p, 0, false);
            try p.expect(.right_paren);
            try p.wrap(.grouping, m);
        },
        .minus => {
            const m = p.marker();
            const tok = try p.eatGet();
            const op = ast.UnaryOperator.toTyped(tok).?;
            try parseExpr(p, op.precedence(), false);
            try p.wrap(.unary, m);
        },
        else => {},
    }
}

/// Parse an exported variable or function
fn parseExportExpr(p: *Parser) Allocator.Error!void {
    const m = p.marker();
    try p.assert(.@"export");
    switch (p.current.kind) {
        .let => try parseLetExpr(p),
        .function => try parseFunctionDef(p),
        else => try p.unexpected(),
    }

    try p.wrap(.export_expr, m);
}

/// Parse a `while x do ... end`
fn parseWhileExpr(p: *Parser) Allocator.Error!void {
    const m = p.marker();
    // Parse first line of while loop
    try p.assert(.@"while");
    try parseExpr(p, 0, false);
    try p.expect(.do);
    try p.expect(.newline);

    try parseBlock(p);

    try p.wrap(.while_loop, m);
}

/// Parse a `for x in y do ... end`
fn parseForExpr(p: *Parser) Allocator.Error!void {
    const m = p.marker();
    // Parse first line of for loop
    try p.assert(.@"for");
    try p.expect(.ident);
    try p.expect(.in);
    try parseExpr(p, 0, false);
    try p.expect(.do);
    try p.expect(.newline);

    try parseBlock(p);

    try p.wrap(.for_loop, m);
}

/// Parse a `if x then ... end`
fn parseIfExpr(p: *Parser) Allocator.Error!void {
    const m = p.marker();
    // Parse first line of if
    try p.assert(.@"if");
    try parseExpr(p, 0, false);
    try p.expect(.then);
    try p.expect(.newline);

    // Set things up for parsing body
    const old_end = p.finish_on;
    p.finish_on.addKind(.end);
    p.finish_on.addKind(.@"else");
    const mode = p.mode();
    if (p.mode() != .code_block) {
        p.setMode(.text);
    }

    // Parse the body
    p.reparse(); // Reparse the last token since we just switched modes
    try parseText(p);

    p.setMode(mode);
    p.finish_on = old_end;

    const expect_end = if (try p.eatIf(.@"else")) blk: {
        if (p.at(.@"if")) {
            const m1 = p.marker();
            try parseIfExpr(p);
            try p.wrap(.text_node, m1);
        } else {
            try p.expect(.newline);
            try parseBlock(p);
        }
        // The end will end up in the inner conditional
        break :blk false;
    } else true;

    if (expect_end) {
        try p.expect(.end);
    }
    try p.wrap(.conditional, m);
}

/// Parse parameters in a function declaration
fn parseParams(p: *Parser) Allocator.Error!void {
    const m = p.marker();

    try p.expect(.left_paren);
    if (!try p.eatIf(.right_paren)) {
        while (true) {
            try p.expect(.ident);
            if (!try p.eatIf(.comma)) {
                break;
            }
        }
        try p.expect(.right_paren);
    }

    try p.wrap(.function_parameters, m);
}

/// Parse the captures a function can close over to make it a closure
fn parseCaptures(p: *Parser) Allocator.Error!void {
    if (!p.at(.left_bracket)) {
        return;
    }

    const m = p.marker();
    try p.assert(.left_bracket);

    if (!try p.eatIf(.right_bracket)) {
        while (true) {
            try p.expect(.ident);
            if (!try p.eatIf(.comma)) {
                break;
            }
        }
        try p.expect(.right_bracket);
    }

    try p.wrap(.closure_captures, m);
}

/// Parse a function declaration
fn parseFunctionDef(p: *Parser) Allocator.Error!void {
    const m = p.marker();
    // Parse first line of declaration
    try p.assert(.function);
    _ = try p.eatIf(.ident);
    try parseParams(p);
    try parseCaptures(p);
    try p.expect(.newline);

    try parseBlock(p);

    try p.wrap(.function_def, m);
}

/// Parse a block like that inside of a function or loop
pub fn parseBlock(p: *Parser) Allocator.Error!void {
    // Set things up for parsing body
    const old_end = p.finish_on;
    p.finish_on.addKind(.end);
    const mode = p.mode();
    if (p.mode() != .code_block) {
        p.setMode(.text);
    }

    // Parse the body
    p.reparse(); // Reparse the last token since we just switched modes
    try parseText(p);

    p.setMode(mode);
    p.finish_on = old_end;

    try p.expect(.end);
}

/// Parses a variable declaration `let foo = 10`
fn parseLetExpr(p: *Parser) Allocator.Error!void {
    const m = p.marker();

    try p.assert(.let);
    try p.expect(.ident);
    try p.expect(.eq);
    try parseExpr(p, 0, false);

    try p.wrap(.let_expr, m);
}

const Parser = struct {
    text: []const u8,
    l: Lexer,
    /// Stack of the current nodes we haven't finished evaluating
    stack: std.ArrayList(SyntaxNode),
    /// A flattened list of the entire AST
    nodes: std.ArrayList(SyntaxNode),
    /// Current token we're looking at
    current: Token,
    /// If `SyntaxKind.end` should be considered an ending character
    finish_on: SyntaxSet,
    /// If the parser has had an error
    has_error: bool,

    const Marker = usize;
    const Token = struct {
        node: SyntaxNode,
        kind: SyntaxKind,
        /// if there was whitespace before the token
        whitespace: Lexer.Whitespace,
    };

    fn mode(self: *Parser) Mode {
        return self.l.mode;
    }

    fn setMode(self: *Parser, m: Mode) void {
        self.l.mode = m;
    }

    fn marker(self: Parser) Marker {
        return self.stack.items.len;
    }

    fn eat(self: *Parser) Allocator.Error!void {
        if (self.current.node.isError()) {
            self.has_error = true;
        }
        try self.stack.append(self.current.node);
        self.current = parseToken(&self.l);
    }

    fn reparse(self: *Parser) void {
        self.l.reparse(self.current.node);
        self.current = parseToken(&self.l);
    }

    fn eatGet(self: *Parser) Allocator.Error!*SyntaxNode {
        try self.eat();
        return &self.stack.items[self.stack.items.len - 1];
    }

    fn assert(self: *Parser, kind: SyntaxKind) Allocator.Error!void {
        std.debug.assert(self.current.kind == kind);
        try self.eat();
    }

    fn at(self: Parser, kind: SyntaxKind) bool {
        return self.current.kind == kind;
    }

    fn lastKind(self: *Parser) SyntaxKind {
        return if (self.stack.getLastOrNull()) |v| v.kind() else .newline;
    }

    fn eatIf(self: *Parser, kind: SyntaxKind) Allocator.Error!bool {
        if (self.at(kind)) {
            try self.eat();
            return true;
        }
        return false;
    }

    fn eatNewlines(self: *Parser) Allocator.Error!bool {
        var res = false;
        while (self.at(.newline)) {
            switch (self.mode()) {
                .text, .code_block => {
                    try self.assert(.newline);
                    res = true;
                },
                .code_line => {
                    try self.assert(.newline);
                    try self.expect(.code_begin);
                    res = true;
                },
                .code_expr => try self.unexpected(),
            }
        }
        return res;
    }

    fn expect(self: *Parser, kind: SyntaxKind) Allocator.Error!void {
        const tok = try self.eatGet();
        if (tok.kind() != kind) {
            self.has_error = true;
            tok.unexpected();
        }
    }

    fn unexpected(self: *Parser) Allocator.Error!void {
        self.has_error = true;
        (try self.eatGet()).unexpected();
    }

    fn wrap(self: *Parser, kind: SyntaxKind, m: Marker) Allocator.Error!void {
        const offset = self.nodes.items.len;

        try self.nodes.appendSlice(self.stack.items[m..]);
        // Sizing down, so can't get an allocation error
        self.stack.resize(m) catch unreachable;
        const len = self.nodes.items.len - offset;

        try self.stack.append(SyntaxNode.treeNode(
            kind,
            self.nodes.items,
            .{ .len = @intCast(len), .offset = @intCast(offset) },
        ));
    }

    fn isEndingKind(self: Parser, kind: SyntaxKind) bool {
        return self.finish_on.hasKind(kind);
    }

    fn init(t: []const u8, allocator: std.mem.Allocator) Parser {
        var lexer = Lexer.init(t);
        const token = Parser.parseToken(&lexer);

        return Parser{
            .text = t,
            .l = lexer,
            .nodes = .init(allocator),
            .stack = .init(allocator),
            .finish_on = SyntaxSet.init(&.{.eof}),
            .current = token,
            .has_error = false,
        };
    }

    fn parseToken(l: *Lexer) Token {
        const node, const kind, const whitespace = l.next();

        return Token{
            .node = node,
            .kind = kind,
            .whitespace = whitespace,
        };
    }
};
