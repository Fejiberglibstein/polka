const std = @import("std");
const SyntaxNode = @import("node.zig").SyntaxNode;
const SyntaxKind = @import("node.zig").SyntaxKind;
const TreeNode = @import("node.zig").TreeNode;
const Lexer = @import("Lexer.zig");
const Mode = @import("Lexer.zig").Mode;

const Allocator = std.mem.Allocator;

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
            .Text => try p.eat(),
            .Newline => try p.eat(),
            .CodeblockBegin => {
                p.setMode(.CodeBlock);
                // TODO
                break;
            },
            .CodeBegin => {
                // Save current mode, will be either top level text or normal text
                const old_mode = p.mode();

                p.setMode(if (p.lastKind() == .Newline) .CodeLine else .CodeExpr);

                const m2 = p.marker();
                try p.assert(.CodeBegin);

                if (p.mode() == .CodeLine) {
                    try parseCode(p);
                } else {
                    try parseExpr(p, 0, true);
                }

                try p.wrap(.Code, m2);
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

    try p.wrap(.TextNode, m);
}

fn parseCode(p: *Parser) Allocator.Error!void {
    while (true) {
        if (p.isEndingKind(p.current.kind)) {
            @branchHint(.unlikely);
            break;
        }

        switch (p.current.kind) {
            .Newline => {
                try p.eat();
                switch (p.mode()) {
                    .CodeLine => if (p.current.kind == .CodeBegin) {
                        try p.eat();
                    } else {
                        break;
                    },

                    .CodeBlock => {},

                    // This should never happen
                    .TopLevelText, .Text, .CodeExpr => try p.unexpected(),
                }
            },
            .Export => try parseExportExpr(p),
            .Let => try parseLetExpr(p),
            .If => try parseIfExpr(p),
            .For => try parseForExpr(p),
            .While => try parseWhileExpr(p),
            .Function => try parseFunctionDef(p),
            .Return => {
                const m = p.marker();
                try p.assert(.Return);
                try parseExpr(p, 0, false);
                try p.wrap(.ReturnExpr, m);
            },
            else => try parseExpr(p, 0, false),
        }
    }
}

/// Parse function call arguments
fn parseArgs(p: *Parser) Allocator.Error!void {
    const m = p.marker();

    try p.expect(.LeftParen);
    if (!p.at(.RightParen)) {
        while (true) {
            try parseExpr(p, 0, false);
            if (!try p.eatIf(.Comma)) {
                break;
            }
        }
    }

    try p.expect(.RightParen);
    try p.wrap(.ArgumentList, m);
}

/// Parse an expression
fn parseExpr(p: *Parser, prec: usize, expr: bool) Allocator.Error!void {
    _ = prec;
    if (expr and p.current.whitespace == .PrecedingWhitespace) {
        return;
    }

    const m = p.marker();
    try parsePrimary(p);

    while (true) {
        if (expr and p.current.whitespace == .PrecedingWhitespace) {
            break;
        }

        if (p.current.kind == .LeftParen) {
            try parseArgs(p);
            try p.wrap(.FunctionCall, m);
            continue;
        }

        if (try p.eatIf(.Dot)) {
            try p.expect(.Ident);
            try p.wrap(.DotAccess, m);
            continue;
        }

        if (p.current.kind.isBinaryOp()) {
            @panic("unimplemented");
        }

        break;
    }
}

/// Parse a primary node.
///
/// Primaries are strings, numbers, bools, identifiers (variables), and lua-like tables
fn parsePrimary(p: *Parser) Allocator.Error!void {
    switch (p.current.kind) {
        .Ident, .String, .Number, .Bool => try p.eat(),
        .LeftParen => {
            try p.assert(.LeftParen);
            try parseExpr(p, 0, false);
            try p.expect(.RightParen);
        },
        else => {},
    }
}

/// Parse an exported variable or function
fn parseExportExpr(p: *Parser) Allocator.Error!void {
    const m = p.marker();
    try p.assert(.Export);
    switch (p.current.kind) {
        .Let => try parseLetExpr(p),
        .Function => try parseFunctionDef(p),
        else => try p.unexpected(),
    }

    try p.wrap(.ExportExpr, m);
}

/// Parse a `while x do ... end`
fn parseWhileExpr(p: *Parser) Allocator.Error!void {
    const m = p.marker();
    // Parse first line of while loop
    try p.assert(.While);
    try parseExpr(p, 0, false);
    try p.expect(.Do);
    try p.expect(.Newline);

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

    try p.expect(.End);
    try p.wrap(.WhileLoop, m);
}

/// Parse a `for x in y do ... end`
fn parseForExpr(p: *Parser) Allocator.Error!void {
    const m = p.marker();
    // Parse first line of for loop
    try p.assert(.For);
    try p.expect(.Ident);
    try p.expect(.In);
    try parseExpr(p, 0, false);
    try p.expect(.Do);
    try p.expect(.Newline);

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

    try p.expect(.End);
    try p.wrap(.ForLoop, m);
}

/// Parse a `if x then ... end`
fn parseIfExpr(p: *Parser) Allocator.Error!void {
    const m = p.marker();
    // Parse first line of if
    try p.assert(.If);
    try parseExpr(p, 0, false);
    try p.expect(.Then);
    try p.expect(.Newline);

    // Set things up for parsing body
    const old_end = p.finish_on_end;
    p.finish_on_end = true;
    const mode = p.mode();
    p.setMode(.TopLevelText);

    // Parse the body
    p.reparse(); // Reparse the last token since we just switched modes
    try parseText(p);

    // TODO: add else if
    if (p.at(.Else)) {
        if (try p.eatIf(.If)) {
            @panic("unimplemented");
        }
        try p.expect(.Newline);
        try parseText(p);
    }

    p.setMode(mode);
    p.finish_on_end = old_end;

    try p.expect(.End);
    try p.wrap(.Conditional, m);
}

/// Parse parameters in a function declaration
fn parseParams(p: *Parser) Allocator.Error!void {
    const m = p.marker();

    try p.expect(.LeftParen);
    if (!try p.eatIf(.RightParen)) {
        while (true) {
            try p.expect(.Ident);
            if (!try p.eatIf(.Comma)) {
                break;
            }
        }
        try p.expect(.RightParen);
    }

    try p.wrap(.FunctionParameters, m);
}

/// Parse a function declaration
fn parseFunctionDef(p: *Parser) Allocator.Error!void {
    const m = p.marker();
    // Parse first line of declaration
    try p.assert(.Function);
    try p.expect(.Ident);
    try parseParams(p);
    try p.expect(.Newline);

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

    try p.expect(.End);
    try p.wrap(.FunctionDef, m);
}

/// Parses a variable declaration `let foo = 10`
fn parseLetExpr(p: *Parser) Allocator.Error!void {
    const m = p.marker();

    try p.assert(.Let);
    try p.expect(.Ident);
    try p.expect(.Eq);
    try parseExpr(p, 0, false);

    try p.wrap(.LetExpr, m);
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
    /// If `SyntaxKind.End` should be considered an ending character
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
        return if (self.stack.getLastOrNull()) |v| v.kind() else .Newline;
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

        try self.stack.append(SyntaxNode.tree(
            kind,
            self.nodes.items,
            .{ .len = @intCast(len), .offset = @intCast(offset) },
        ));
    }

    fn isEndingKind(self: Parser, kind: SyntaxKind) bool {
        return (self.finish_on_end and kind == .End) or kind == .EOF;
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
