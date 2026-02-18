pub fn parse(src: []const u8, mode: Lexer.Mode, gpa: Allocator) Allocator.Error![]const SyntaxNode {}

const Parser = struct {
    text: []const u8,
    l: Lexer,
    /// Stack of the current nodes we haven't yet parsed into a tree node.
    stack: std.ArrayList(SyntaxNode),
    /// A flattened list of the entire AST
    nodes: std.ArrayList(SyntaxNode),
    /// Current token we're looking at
    current: Token,
    /// If the parser has had an error
    has_error: bool,

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
        try self.stack.append(self.current.node);
        self.current = parseToken(&self.l);
    }

    fn assert(self: *Parser, kind: SyntaxKind) Allocator.Error!void {
        std.debug.assert(self.current.kind == kind);
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

    fn wrap(self: *Parser, kind: SyntaxKind, m: Marker) Allocator.Error!void {
        const offset = self.nodes.items.len;

        try self.nodes.appendSlice(self.stack.items[m..]);

        // Sizing down, so can't get an allocation error
        std.debug.assert(m <= self.stack.items.len);
        self.stack.resize(m) catch unreachable;

        const len = self.nodes.items.len - offset;

        try self.stack.append(SyntaxNode{
            .kind = kind,
            .data = .{ .tree = .{ .len = @intCast(len), .offset = @intCast(offset) } },
        });
    }

    fn isEndingKind(self: Parser, kind: SyntaxKind) bool {
        return self.finish_on.hasKind(kind);
    }

    fn init(t: []const u8, gpa: std.mem.Allocator) Parser {
        var lexer = Lexer.init(t);
        const token = Parser.parseToken(&lexer);

        return Parser{
            .text = t,
            .l = lexer,
            .nodes = .init(gpa),
            .stack = .init(gpa),
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

const std = @import("std");
const Allocator = std.mem.Allocator;

const Lexer = @import("Lexer.zig");
const SyntaxKind = @import("node.zig").SyntaxKind;
const SyntaxNode = @import("node.zig").SyntaxNode;
