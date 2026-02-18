pub fn parse(src: []const u8, mode: Lexer.Mode, gpa: Allocator) Allocator.Error![]const SyntaxNode {}

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
