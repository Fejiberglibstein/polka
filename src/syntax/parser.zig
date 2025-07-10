const std = @import("std");
const SyntaxNode = @import("node.zig").SyntaxNode;
const SyntaxKind = @import("node.zig").SyntaxKind;
const TreeNode = @import("node.zig").TreeNode;
const Lexer = @import("Lexer.zig");
const Mode = @import("Lexer.zig").Mode;

/// Parses top level text of a file
pub fn parse(t: []const u8, allocator: std.mem.Allocator) struct { []SyntaxNode, Parser } {
    var parser = Parser.init(t, allocator);
    parseText(&parser);
    return struct { parser.nodes, parser };
}

fn parseText(p: *Parser) void {
    const m = p.marker();

    while (true) {
        if (p.isEndingKind(p.current.kind)) {
            @branchHint(.unlikely);
            break;
        }

        switch (p.current.kind) {
            .Text => p.eat(),
            .Newline => p.eat(),
            .CodeblockBegin => {
                p.setMode(.Codeblock);
                // TODO
                break;
            },
            .CodeBegin => {
                // Save current mode, will be either top level text or normal text
                const old_mode = p.mode();

                p.setMode(if (p.lastKind() == .Newline) .CodeLine else .CodeExpr);

                const m2 = p.marker();
                p.assert(.Codebegin);

                if (p.mode() == .CodeLine) {
                    parseCode(p);
                } else {
                    parseExpr(p, 0, true);
                }

                p.wrap(.Code, m2);
                p.setMode(old_mode);

                if (!p.isEndingKind(p.current.kind)) {
                    // Since the mode was just switched, the current token will be a code mode
                    // token, not a text mode token which is needed now that we're back in text
                    // mode.
                    p.reparse();
                }
            },
            // this shouldn't ever happen since we don't produce any other tokens in the lexer
            else => p.unexpected(),
        }
    }

    p.wrap(.TextNode, m);
}

fn parseCode(p: *Parser) void {
    while (true) {
        if (p.isEndingKind(p.current.kind)) {
            @branchHint(.unlikely);
            break;
        }

        switch (p.current.kind) {
            .Newline => {
                p.eat();
                switch (p.mode()) {
                    .CodeLine => if (p.current.kind == .CodeBegin) {
                        p.eat();
                    } else {
                        break;
                    },

                    .CodeBlock => {},

                    // This should never happen
                    .TopLevelText, .Text, .CodeExpr => p.unexpected(),
                }
            },
            .Export => parseExportExpr(p),
            .Let => parseLetExpr(p),
            .If => parseIfExpr(p),
            .For => parseForExpr(p),
            .While => parseWhileExpr(p),
            .Function => parseFunctionDef(p),
            .Return => {
                const m = p.marker();
                p.assert(.Return);
                parseExpr(p, 0, false);
                p.wrap(.ReturnExpr, m);
            },
            else => parseExpr(p, 0, false),
        }
    }
}

/// Parse function call arguments
fn parseArgs(p: *Parser) void {
    const m = p.marker();

    p.expect(.LeftParen);
    if (!p.at(.RightParen)) {
        while (true) {
            parseExpr(p, 0, false);
            if (!p.eatIf(.Comma)) {
                break;
            }
        }
    }

    p.expect(.RightParen);
    p.wrap(.ArgumentList, m);
}

/// Parse an expression
fn parseExpr(p: *Parser, prec: usize, expr: bool) void {
    _ = prec;
    if (expr and p.current.preceding_whitespace) {
        return;
    }

    const m = p.marker();
    parsePrimary(p);

    while (true) {
        if (expr and p.current.preceding_whitespace) {
            break;
        }

        if (p.current.kind == .LeftParen) {
            parseArgs(p);
            p.wrap(.FunctionCall, m);
            continue;
        }

        if (p.eatIf(.Dot)) {
            p.expect(.Ident);
            p.wrap(.DotAccess, m);
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
fn parsePrimary(p: *Parser) void {
    switch (p.current.kind) {
        .Ident | .String | .Number | .Bool => p.eat(),
        .LeftParen => {
            p.assert(.LeftParen);
            parseExpr(p, 0, false);
            p.expect(.RightParen);
        },
        else => {}
    }
}

/// Parse an exported variable or function
fn parseExportExpr(p: *Parser) void {
    const m = p.marker();
    p.assert(.Export);
    switch (p.current.kind) {
        .Let => parseLetExpr(p),
        .Function => parseFunctionDef(p),
        _ => p.unexpected(),
    }

    p.wrap(.ExportExpr, m);
}

/// Parse a `while x do ... end`
fn parseWhileExpr(p: *Parser) void {
    const m = p.marker();
    // Parse first line of while loop
    p.assert(.While);
    parseExpr(p, 0, false);
    p.expect(.Do);
    p.expect(.Newline);

    // Set things up for parsing body
    const old_end = p.finish_on_end;
    p.finish_on_end = true;
    const mode = p.mode();
    p.setMode(.TopLevelText);

    // Parse the body
    p.reparse(); // Reparse the last token since we just switched modes
    parseText(p);

    p.setMode(mode);
    p.finish_on_end = old_end;

    p.expect(.End);
    p.wrap(.WhileLoop, m);
}

/// Parse a `for x in y do ... end`
fn parseForExpr(p: *Parser) void {
    const m = p.marker();
    // Parse first line of for loop
    p.assert(.For);
    p.expect(.Ident);
    p.expect(.In);
    parseExpr(p, 0, false);
    p.expect(.Do);
    p.expect(.Newline);

    // Set things up for parsing body
    const old_end = p.finish_on_end;
    p.finish_on_end = true;
    const mode = p.mode();
    p.setMode(.TopLevelText);

    // Parse the body
    p.reparse(); // Reparse the last token since we just switched modes
    parseText(p);

    p.setMode(mode);
    p.finish_on_end = old_end;

    p.expect(.End);
    p.wrap(.ForLoop, m);
}

/// Parse a `if x then ... end`
fn parseIfExpr(p: *Parser) void {
    const m = p.marker();
    // Parse first line of if
    p.assert(.If);
    parseExpr(p, 0, false);
    p.expect(.Then);
    p.expect(.Newline);

    // Set things up for parsing body
    const old_end = p.finish_on_end;
    p.finish_on_end = true;
    const mode = p.mode();
    p.setMode(.TopLevelText);

    // Parse the body
    p.reparse(); // Reparse the last token since we just switched modes
    parseText(p);

    // TODO: add else if
    if (p.at(.Else)) {
        if (p.eatIf(.If)) {
            @panic("unimplemented");
        }
        p.expect(.Newline);
        parseText(p);
    }

    p.setMode(mode);
    p.finish_on_end = old_end;

    p.expect(.End);
    p.wrap(.Conditional, m);
}

/// Parse parameters in a function declaration
fn parseParams(p: *Parser) void {
    const m = p.marker();

    p.expect(.LeftParen);
    if (!p.eatIf(.RightParen)) {
        while (true) {
            p.expect(.Ident);
            if (!p.eatIf(.Comma)) {
                break;
            }
        }
        p.expect(.RightParen);
    }

    p.wrap(.FunctionParameters, m);
}

/// Parse a function declaration
fn parseFunctionDef(p: *Parser) void {
    const m = p.marker();
    // Parse first line of declaration
    p.assert(.Function);
    p.expect(.Ident);
    parseParams(p);
    p.expect(.Newline);

    // Set things up for parsing body
    const old_end = p.finish_on_end;
    p.finish_on_end = true;
    const mode = p.mode();
    p.set_mode(.TopLevelText);

    // Parse the body
    p.reparse(); // Reparse the last token since we just switched modes
    parseText(p);

    p.setMode(mode);
    p.finish_on_end = old_end;

    p.expect(.End);
    p.wrap(.FunctionDef, m);
}

/// Parses a variable declaration `let foo = 10`
fn parseLetExpr(p: *Parser) void {
    const m = p.marker();

    p.assert(.Let);
    p.expect(.Ident);
    p.expect(.Eq);
    parseExpr(p, 0, false);

    p.wrap(.LetExpr, m);
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
        preceding_whitespace: bool,
    };

    fn mode(self: *Parser) Mode {
        return self.l.mode;
    }

    fn setMode(self: *Parser, m: Mode) void {
        self.l.mode = m;
    }

    fn marker(self: Parser) Marker {
        return self.nodes.items.len;
    }

    fn eat(self: *Parser) void {
        self.stack.append(self.current.node);
        self.current = parseToken(self.l);
    }

    fn eatGet(self: *Parser) *SyntaxNode {
        self.eat();
        return self.stack.items[self.stack.items.len - 1];
    }

    fn assert(self: *Parser, kind: SyntaxKind) void {
        std.debug.assert(self.current.kind == kind);
        self.eat();
    }

    fn at(self: Parser, kind: SyntaxKind) bool {
        return self.current.kind == kind;
    }

    fn lastKind(self: *Parser) SyntaxKind {
        return if (self.stack.getLastOrNull()) |v| v.kind() else .Newline;
    }

    fn eatIf(self: *Parser, kind: SyntaxKind) bool {
        if (self.at(kind)) {
            self.eat();
            return true;
        }
        return false;
    }

    fn expect(self: *Parser, kind: SyntaxKind) void {
        const tok = self.eatGet();
        if (tok.kind() != kind) {
            tok.unexpected();
        }
    }

    fn unexpected(self: *Parser) void {
        self.eatGet().unexpected();
    }

    fn wrap(self: *Parser, kind: SyntaxKind, m: Marker) void {
        const start = self.nodes.len;
        self.nodes.appendSlice(self.stack[m..]);
        self.nodes.resize(self.stack.items.len - m);

        // TODO use indexes into nodes so that the pointers don't become invalidated

        self.nodes.append(.{ .Tree = TreeNode.tree(kind, self.nodes[start..]) });
    }

    fn isEndingKind(self: Parser, kind: SyntaxKind) bool {
        return (self.finish_on_end and kind == .End) or kind == .EOF;
    }

    fn init(t: []const u8, allocator: std.mem.Allocator) Parser {
        const lexer = Lexer.init(t);
        const token = Parser.parseToken(lexer);

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
