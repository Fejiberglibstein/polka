const std = @import("std");
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const Lexer = @import("Lexer.zig");
const Mode = @import("Lexer.zig").Mode;
const SyntaxKind = @import("node.zig").SyntaxKind;
const SyntaxNode = @import("node.zig").SyntaxNode;
const TreeNode = @import("node.zig").TreeNode;

/// Parses top level text of a file
pub fn parse(
    t: []const u8,
    allocator: std.mem.Allocator,
) Allocator.Error!struct { SyntaxNode, std.ArrayList(SyntaxNode) } {
    var parser = Parser.init(t, allocator);
    try parseText(&parser);

    std.debug.assert(parser.stack.items.len == 1);
    const node = parser.stack.items[0];
    parser.stack.deinit();
    return .{ node, parser.nodes };
}

fn parseText(p: *Parser) Allocator.Error!void {
    const m = p.marker();

    while (true) {
        if (p.isEndingKind(p.current.kind)) {
            @branchHint(.unlikely);
            break;
        }

        switch (p.current.kind) {
            .text => try p.eat(),
            .newline => try p.eat(),
            .codeblock_begin => {
                p.setMode(.CodeBlock);
                // TODO
                break;
            },
            .code_begin => {
                // Save current mode, will be either top level text or normal text
                const old_mode = p.mode();

                p.setMode(if (p.lastKind() == .newline) .CodeLine else .CodeExpr);

                const m2 = p.marker();
                try p.assert(.code_begin);

                if (p.mode() == .CodeLine) {
                    try parseCode(p);
                } else {
                    try parseExpr(p, 0, true);
                }

                try p.wrap(.code, m2);
                p.setMode(old_mode);

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
            @branchHint(.unlikely);
            break;
        }

        switch (p.current.kind) {
            .newline => {
                try p.eat();
                switch (p.mode()) {
                    .CodeLine => if (p.current.kind == .code_begin) {
                        try p.eat();
                    } else {
                        break;
                    },

                    .CodeBlock => {},

                    // This should never happen
                    .TopLevelText, .Text, .CodeExpr => try p.unexpected(),
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
                try parseExpr(p, 0, false);
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
    if (expr and p.current.whitespace == .PrecedingWhitespace) {
        return;
    }

    const m = p.marker();
    try parsePrimary(p);

    while (true) {
        if (expr and p.current.whitespace == .PrecedingWhitespace) {
            break;
        }

        if (p.current.kind == .left_paren) {
            try parseArgs(p);
            try p.wrap(.function_call, m);
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

        break;
    }
}

/// Parse a primary node.
///
/// Primaries are strings, numbers, bools, identifiers (variables), and lua-like tables
fn parsePrimary(p: *Parser) Allocator.Error!void {
    switch (p.current.kind) {
        .ident, .string, .number, .bool, .nil => try p.eat(),
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

    // Set things up for parsing body
    const old_end = p.finish_on_end;
    p.finish_on_end = true;
    const mode = p.mode();
    p.setMode(.TopLevelText);

    // Parse the body
    p.reparse(); // Reparse the last token since we just switched modes
    try parseText(p);

    p.setMode(mode);
    p.finish_on_end = old_end;

    try p.expect(.end);
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

    // Set things up for parsing body
    const old_end = p.finish_on_end;
    p.finish_on_end = true;
    const mode = p.mode();
    p.setMode(.TopLevelText);

    // Parse the body
    p.reparse(); // Reparse the last token since we just switched modes
    try parseText(p);

    p.setMode(mode);
    p.finish_on_end = old_end;

    try p.expect(.end);
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
    const old_end = p.finish_on_end;
    p.finish_on_end = true;
    const mode = p.mode();
    p.setMode(.TopLevelText);

    // Parse the body
    p.reparse(); // Reparse the last token since we just switched modes
    try parseText(p);

    // TODO: add else if
    if (p.at(.@"else")) {
        if (try p.eatIf(.@"if")) {
            @panic("unimplemented");
        }
        try p.expect(.newline);
        try parseText(p);
    }

    p.setMode(mode);
    p.finish_on_end = old_end;

    try p.expect(.end);
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

/// Parse a function declaration
fn parseFunctionDef(p: *Parser) Allocator.Error!void {
    const m = p.marker();
    // Parse first line of declaration
    try p.assert(.function);
    try p.expect(.ident);
    try parseParams(p);
    try p.expect(.newline);

    // Set things up for parsing body
    const old_end = p.finish_on_end;
    p.finish_on_end = true;
    const mode = p.mode();
    p.setMode(.TopLevelText);

    // Parse the body
    p.reparse(); // Reparse the last token since we just switched modes
    try parseText(p);

    p.setMode(mode);
    p.finish_on_end = old_end;

    try p.expect(.end);
    try p.wrap(.function_def, m);
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
    finish_on_end: bool,

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

    fn expect(self: *Parser, kind: SyntaxKind) Allocator.Error!void {
        const tok = try self.eatGet();
        if (tok.kind() != kind) {
            tok.unexpected();
        }
    }

    fn unexpected(self: *Parser) Allocator.Error!void {
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
        return (self.finish_on_end and kind == .end) or kind == .eof;
    }

    fn init(t: []const u8, allocator: std.mem.Allocator) Parser {
        var lexer = Lexer.init(t);
        const token = Parser.parseToken(&lexer);

        return Parser{
            .text = t,
            .l = lexer,
            .nodes = .init(allocator),
            .stack = .init(allocator),
            .finish_on_end = false,
            .current = token,
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
