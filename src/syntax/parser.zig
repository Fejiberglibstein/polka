const std = @import("std");
const SyntaxNode = @import("node.zig").SyntaxNode;
const SyntaxKind = @import("node.zig").SyntaxKind;
const TreeNode = @import("node.zig").TreeNode;
const Lexer = @import("Lexer.zig");
const Mode = @import("Lexer.zig").Mode;

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
