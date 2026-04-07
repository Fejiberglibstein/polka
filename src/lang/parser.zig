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
    expected_expression,
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
        const m = try p.marker();
        try p.eat();
        p.wrap(m, .text);
    } else {
        try parseText(&p);
    }

    try p.eatExpect(.eof, .none);
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
    // Even when in a `.polka` file, all nodes are still wrapped in text
    if (p.mode() == .code_file) {
        const m = try p.marker();
        try parseCode(p);
        p.wrap(m, .text);
        return;
    }

    const m = try p.marker();
    defer p.wrap(m, .text);

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
                const m2 = try p.marker();
                defer p.wrap(m2, .code);

                p.setMode(.code_line);
                try p.eatAssert(.code_begin);
                try parseCode(p);
            },

            .codeblock_delim => {
                const m2 = try p.marker();
                defer p.wrap(m2, .code);

                p.setMode(.code_block);
                try p.eatAssert(.codeblock_delim);
                try parseCode(p);
                try p.eatExpect(.codeblock_delim, .{ .dont_eat = .newline, .error_on = .none });
            },

            // Lexer doesn't produce any other tokens
            else => {
                try p.unexpected();
                try p.eat();
            },
        }
    }
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

fn parseStatement(p: *Parser) Allocator.Error!void {
    if (try p.eatIf(.newline)) return;

    (switch (p.current.node.kind) {
        .keyword_for => parseForLoop(p),
        .keyword_if => parseConditional(p),
        .keyword_while => parseWhileLoop(p),
        .keyword_let => parseLetStatement(p),
        .keyword_export => parseExportStatement(p),
        .keyword_return => parseReturnStatement(p),
        .keyword_break, .keyword_continue => p.eat(),
        else => parseExpression(p, 0),
    }) catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ParseError => {
            // Look for the end of the statement
            const set = SyntaxSet.init(&.{ .eof, .newline });
            while (!set.contains(p.current.node.kind)) try p.eat();
        },
    };

    if (!p.at(.eof)) {
        if (!p.at(.newline)) {
            const stack_top = if (p.stack.getLastOrNull()) |n| n.kind else .eof;
            const at_start_of_line: bool = if (p.mode() == .code_line)
                stack_top == .code_begin
            else
                stack_top == .newline;
            if (!at_start_of_line) {
                // Only add this error if we are not at the beginning of the line. If we are at the
                // beginning of the line, there was already a syntax error that is most likely
                // "expected expression"
                try p.addError(.{
                    .expected_token = .{ .expected = .newline, .actual = p.current.node.kind },
                });
            }
            // Look for the actual newline
            const set = SyntaxSet.init(&.{ .eof, .newline });
            while (!set.contains(p.current.node.kind)) try p.eat();
            if (p.at(.eof)) return;
        }
        try p.eatAssert(.newline);
    }
}

fn parseExpression(p: *Parser, precedence: usize) Error!void {
    const m = try p.marker();

    if (ast.toASTNode(ast.UnaryOperator, 0, &.{p.current.node})) |op| {
        defer p.wrap(m, .unary);
        try p.eat();
        try parseExpression(p, op.precedence()); // Plus 1 since unary ops are right associative
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

        .l_brace => try parseDict(p),
        .l_bracket => try parseList(p),

        .l_paren => {
            const m2 = try p.marker();
            defer p.wrap(m2, .grouping);
            try p.eatAssert(.l_paren);
            try skipNewlines(p);
            try parseExpression(p, 0);
            try skipNewlines(p);
            try p.eatExpect(.r_paren, .{ .dont_eat = .any });
        },

        .keyword_func => try parseFunctionDefinition(p),

        .backtick => try parseMultilineString(p),

        else => {},
    }

    while (true) {
        // Parse a function call
        if (p.at(.l_paren)) {
            defer p.wrap(m, .function_call);
            try parseFunctionCallArguments(p);
            continue;
        }

        if (try p.eatIf(.l_bracket)) {
            defer p.wrap(m, .bracket_access);
            try parseExpression(p, 0);
            try p.eatExpect(.r_bracket, .{ .dont_eat = .any });
            continue;
        }

        // Parse dot access
        if (try p.eatIf(.dot)) {
            defer p.wrap(m, .dot_access);
            try p.eatExpect(.ident, .newline);
            continue;
        }

        if (ast.toASTNode(ast.BinaryOperator, 0, &.{p.current.node})) |op| {
            if (op.precedence() >= precedence) {
                defer p.wrap(m, .binary);
                const new_precedence = switch (op.associativity()) {
                    .left => op.precedence() + 1,
                    .right => op.precedence(),
                };
                try p.eat();
                try parseExpression(p, new_precedence);
                continue;
            }
        }

        if (m == try p.marker()) {
            if (p.at(.unexpected_character)) {
                try p.eat();
            } else {
                try p.addError(.expected_expression);
            }
        }
        break;
    }
}

fn parseFunctionCallArguments(p: *Parser) Error!void {
    const m = try p.marker();
    defer p.wrap(m, .function_args);

    try p.eatAssert(.l_paren);
    while (!try p.eatIf(.r_paren)) {
        try skipNewlines(p);
        try parseExpression(p, 0);
        try skipNewlines(p);
        if (!try p.eatIf(.comma)) {
            try p.eatExpect(.r_paren, .{ .dont_eat = .any });
            break;
        }
        try skipNewlines(p);
    }
}

fn parseBlock(p: *Parser) Error!void {
    const old_end = p.ending_kind;
    const old_mode = p.switchToTextMode();
    p.ending_kind.add(.keyword_end);

    p.reparse();
    try parseText(p);

    // Switch mode before eating the end so that the next token is in the correct mode.
    p.setMode(old_mode);
    p.ending_kind = old_end;

    try p.eatExpect(.keyword_end, .newline);
}

fn parseFunctionParameters(p: *Parser) Error!void {
    const m = try p.marker();
    defer p.wrap(m, .function_parameters);

    try p.eatExpect(.l_paren, .{ .dont_eat = .any });
    while (!try p.eatIf(.r_paren)) {
        try skipNewlines(p);
        try p.eatExpect(.ident, .eof);
        try skipNewlines(p);
        if (!try p.eatIf(.comma)) {
            try p.eatExpect(.r_paren, .{ .dont_eat = .any });
            break;
        }
        try skipNewlines(p);
    }
}

fn parseFunctionDefinition(p: *Parser) Error!void {
    const m = try p.marker();
    defer p.wrap(m, .function_def);

    try p.eatAssert(.keyword_func);
    _ = try p.eatIf(.ident);
    try parseFunctionParameters(p);
    if (try p.eatIf(.newline))
        try parseBlock(p)
    else
        try parseExpression(p, 0);
}

fn parseMultilineString(p: *Parser) Error!void {
    const old_mode = p.mode();
    p.setMode(.multiline_string);

    const m = try p.marker();
    defer p.wrap(m, .multiline_string);

    // Must eat backtick _after_ switching modes so that the next token is parsed in the correct
    // mode.
    try p.eatAssert(.backtick);

    var state: Parser.State = undefined;

    defer {
        p.resetToState(state);
        p.setMode(old_mode);
        p.reparse();
    }

    while (true) {
        state = p.getState();
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
                const m2 = try p.marker();
                defer p.wrap(m2, .mls_expression);

                // If the lexer yielded a `.at`, it _must be_ followed by an `.l_paren`. The lexer
                // specifically looks for the sequence "@(" to determine whether or not to return a
                // `.at`
                try p.eatAssert(.at);
                try p.eatAssert(.l_paren);

                try parseExpression(p, 0);

                try p.eatExpect(.r_paren, .{ .dont_eat = .any, .error_on = .none });

                p.setMode(.multiline_string);
                p.reparse();
            },
            else => @panic("Lexer doesn't yield any other tokens while in multiline mode"),
        }
    }
}

fn parseList(p: *Parser) Error!void {
    const m = try p.marker();
    defer p.wrap(m, .list);

    try p.eatAssert(.l_bracket);
    while (!try p.eatIf(.r_bracket)) {
        try skipNewlines(p);
        try parseExpression(p, 0);
        try skipNewlines(p);
        if (!try p.eatIf(.comma)) {
            try p.eatExpect(.r_bracket, .{ .dont_eat = .any });
            break;
        }
        try skipNewlines(p);
    }
}

fn parseDict(p: *Parser) Error!void {
    const m = try p.marker();
    defer p.wrap(m, .dict);

    try p.eatAssert(.l_brace);
    while (!try p.eatIf(.r_brace)) {
        try skipNewlines(p);
        {
            const m2 = try p.marker();
            defer p.wrap(m2, .dict_field);

            try p.eatExpect(.ident, .{ .dont_eat = comptime .init(&.{ .eq, .r_brace, .newline }) });
            try skipNewlines(p);
            try p.eatExpect(.eq, .{ .dont_eat = comptime .init(&.{ .r_brace, .newline }) });
            try skipNewlines(p);
            try parseExpression(p, 0);
        }
        try skipNewlines(p);
        if (!try p.eatIf(.comma)) {
            try p.eatExpect(.r_brace, .{ .dont_eat = .any });
            break;
        }
        try skipNewlines(p);
    }
}

fn parseConditional(p: *Parser) Error!void {
    const m = try p.marker();
    defer p.wrap(m, .conditional);

    try p.eatAssert(.keyword_if);
    try parseExpression(p, 0);
    try p.eatExpect(.keyword_then, .{ .dont_eat = .newline });
    // Switch nodes before consuming the newline so that the next token is tokenized in text mode
    const old_mode = p.switchToTextMode();
    try p.eatExpect(.newline, .eof);

    // Setup block to be parsed
    const old_end = p.ending_kind;
    p.ending_kind.add(.keyword_end);
    p.ending_kind.add(.keyword_else);
    p.ending_kind.add(.keyword_elseif);
    try parseText(p);

    defer {
        // Switch mode before eating the end so that the next token is in the correct mode.
        p.setMode(old_mode);
        p.reparse();
        p.ending_kind = old_end;
    }

    while (true) {
        switch (p.current.node.kind) {
            .keyword_elseif => {
                try p.eatAssert(.keyword_elseif);
                try parseExpression(p, 0);
                try p.eatExpect(.keyword_then, .{ .dont_eat = .newline });
                _ = p.switchToTextMode();
                try p.eatExpect(.newline, .eof);

                try parseText(p);
            },

            .keyword_else => {
                try p.eatAssert(.keyword_else);
                _ = p.switchToTextMode();
                try p.eatExpect(.newline, .eof);

                p.ending_kind.remove(.keyword_else);
                p.ending_kind.remove(.keyword_elseif);
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
}

fn parseWhileLoop(p: *Parser) Error!void {
    const m = try p.marker();
    defer p.wrap(m, .while_loop);

    try p.eatAssert(.keyword_while);
    try parseExpression(p, 0);
    try p.eatExpect(.keyword_do, .{ .dont_eat = .newline });
    try p.eatExpect(.newline, .eof);
    try parseBlock(p);
}

fn parseForLoop(p: *Parser) Error!void {
    const m = try p.marker();
    defer p.wrap(m, .for_loop);

    try p.eatAssert(.keyword_for);
    try p.eatExpect(.ident, .{ .dont_eat = .init(&.{ .newline, .keyword_in, .keyword_do }) });
    try p.eatExpect(.keyword_in, .{ .dont_eat = .init(&.{ .newline, .keyword_do }) });
    try parseExpression(p, 0);
    try p.eatExpect(.keyword_do, .{ .dont_eat = .init(&.{.newline}) });
    try p.eatExpect(.newline, .eof);
    try parseBlock(p);
}

fn parseReturnStatement(p: *Parser) Error!void {
    const m = try p.marker();
    defer p.wrap(m, .return_statement);

    try p.eatAssert(.keyword_return);
    if (!p.at(.newline)) {
        try parseExpression(p, 0);
    }
}

fn parseExportStatement(p: *Parser) Error!void {
    const m = try p.marker();
    defer p.wrap(m, .export_statement);

    try p.eatAssert(.keyword_export);
    switch (p.current.node.kind) {
        .keyword_let => try parseLetStatement(p),
        .keyword_func => try parseFunctionDefinition(p),
        else => try p.unexpected(),
    }
}

fn parseLetStatement(p: *Parser) Error!void {
    const m = try p.marker();
    defer p.wrap(m, .let_statement);

    try p.eatAssert(.keyword_let);
    try p.eatExpect(.ident, .{ .dont_eat = .newline });
    if (try p.eatIf(.eq)) try parseExpression(p, 0);
}

fn skipNewlines(p: *Parser) Error!void {
    switch (p.mode()) {
        .code_line => while (p.at(.newline)) {
            const state = p.getState();
            try p.eatAssert(.newline);
            if (!p.at(.code_begin)) {
                p.resetToState(state);
                break;
            }
            try p.eatAssert(.code_begin);
        },

        .code_file, .code_block => {
            while (p.at(.newline)) try p.eat();
        },

        inline else => |tag| @panic(
            ".skipNewlines() should not be called in " ++ @tagName(tag) ++ " mode",
        ),
    }
}

fn eatNewline(p: *Parser) !bool {
    return switch (p.mode()) {
        .code_line => blk: {
            if (try p.eatIf(.newline)) {
                try p.eatExpect(.code_begin, .{ .dont_eat = .any });
                break :blk true;
            }
            break :blk false;
        },
        .code_file,
        .code_block,
        .text,
        .multiline_string,
        => try p.eatIf(.newline),
    };
}

const Error = error{ParseError} || Allocator.Error;

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

    /// The number of markers that haven't been wrapped yet.
    active_markers: usize,

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
            .active_markers = 0,
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
        return self.ending_kind.contains(kind);
    }

    fn at(self: Parser, kind: SyntaxKind) bool {
        return self.current.node.kind == kind;
    }

    fn eat(self: *Parser) !void {
        if (self.current.node.kind == .unexpected_character) {
            try self.addError(.invalid_token);
        }

        try self.stack.append(self.gpa, self.current.node);

        // Ensure that there is enough space in .nodes to add the entire stack. This is done so that
        // Parser.wrap() will not return an error, see the comments in that function for more
        // information.
        try self.nodes.ensureTotalCapacity(
            self.gpa,
            // Ensure we have enough space for every item in the stack and every marker, since
            // markers get turned into syntax nodes
            self.nodes.items.len + self.stack.items.len + self.active_markers,
        );

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

    const ExpectOpts = struct {
        error_on: SyntaxSet = .eof,
        dont_eat: SyntaxSet,
        if_ending_kind: enum {
            none,
            eat,
            err,
            eat_and_err,
        } = .eat_and_err,

        const eof: ExpectOpts = .{ .dont_eat = .eof, .error_on = .eof };
        const none: ExpectOpts = .{ .dont_eat = .none, .error_on = .none };
        const newline: ExpectOpts = .{ .dont_eat = .newline, .error_on = .newline };
    };

    fn ExpectError(comptime opts: ExpectOpts) type {
        return if (opts.error_on.v == SyntaxSet.none.v) Allocator.Error else Error;
    }

    fn eatExpect(
        self: *Parser,
        kind: SyntaxKind,
        comptime opts: ExpectOpts,
    ) ExpectError(opts)!void {
        if (self.current.node.kind != kind) {
            try self.addError(.{
                .expected_token = .{ .expected = kind, .actual = self.current.node.kind },
            });

            const error_on_ending_kind, const eat_ending_kind = blk: {
                if (!self.isEndingKind(self.current.node.kind)) break :blk .{ false, false };
                break :blk switch (opts.if_ending_kind) {
                    .none => .{ false, false },
                    .eat => .{ false, true },
                    .err => .{ true, false },
                    .eat_and_err => .{ true, true },
                };
            };
            if (opts.error_on.contains(kind) or error_on_ending_kind) {
                return if (opts.error_on.v != SyntaxSet.none.v) error.ParseError;
            }

            if (opts.dont_eat.contains(self.current.node.kind) or eat_ending_kind) return;
        }

        try self.eat();
    }

    fn unexpected(self: *Parser) !void {
        try self.addError(.{ .unexpected_token = self.current.node.kind });
    }

    fn addError(self: *Parser, kind: SyntaxErrorKind) !void {
        try self.errors.append(self.gpa, .{
            .kind = kind,
            .position = self.current.position,
            .range = self.current.node.getLeafSource(self.text),
        });
    }

    const State = struct {
        lexer: Lexer,
        stack_len: usize,
        token: Lexer.Token,
    };

    fn getState(self: *Parser) Parser.State {
        return .{
            .stack_len = self.stack.items.len,
            .token = self.current,
            .lexer = self.l,
        };
    }

    fn resetToState(self: *Parser, state: Parser.State) void {
        assert(state.stack_len <= self.stack.items.len);
        // Sizing down so there can't be an allocation error
        self.stack.resize(self.gpa, state.stack_len) catch unreachable;
        self.l = state.lexer;
        self.current = state.token;
    }

    const Marker = enum(u32) {
        _,
    };

    fn marker(self: *Parser) !Marker {
        self.active_markers += 1;

        // Ensure the stack can contain all the markers we add to it.
        try self.stack.ensureTotalCapacity(self.gpa, self.stack.items.len + self.active_markers);
        return @enumFromInt(self.stack.items.len);
    }

    fn wrap(self: *Parser, m: Marker, kind: SyntaxKind) void {
        self.active_markers -= 1;

        assert(kind.getType() == .tree);
        const node_start_index = self.nodes.items.len;

        assert(@intFromEnum(m) <= self.stack.items.len);
        const tree_nodes = self.stack.items[@intFromEnum(m)..];

        // In Parser.eat() we ensure that self.nodes has enough capacity to add all of
        // self.stack.items.len. We must do it there rather than here so that this function does not
        // error.
        self.nodes.appendSliceAssumeCapacity(tree_nodes);

        // Size the stack back down to how it was. This _might do nothing_ if there was a syntax
        // error.
        self.stack.items.len = @intFromEnum(m);

        // In Parser.marker(), the stack is expanded to be able to hold markers.
        self.stack.appendAssumeCapacity(SyntaxNode{
            .kind = kind,
            .data = .{ .tree = .{
                .len = @intCast(tree_nodes.len),
                .offset = @intCast(node_start_index),
            } },
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
