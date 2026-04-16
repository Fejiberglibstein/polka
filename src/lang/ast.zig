//! Attribution: The way the ast is set up is very heavily based on Typst's ast.
//! Source: https://github.com/typst/typst/blob/main/crates/typst-syntax/src/ast.rs
//!
//! ---
//!
//! Provides a typed interface to access the CST (Concrete syntax tree) that `parser.zig` produces.
//!
//! Usually this would be implemented by having struct data inside each variant of the SyntaxKind.
//! However, we want the CST to remain concrete (meaning that the source code can be entirely
//! recreated by doing a traversal on the tree) so that the LSP can be better.
//!
//! The tree is rooted in a `text_node`, and each block of code, such as the inside of an if
//! statement, is also a `text_node`, even when inside a #** ... #**.
//!
//! Each ASTNode will eventually produce a text of some kind, either "" in the case of just an
//! assignment:
//! ```
//! #* let h = { foo = "hello\n" }
//! ```
//! or some actual text:
//! ```
//! #* h.foo // produces "hello\n" from the previous example
//! ```

pub fn toASTNode(comptime T: type, node_index: u32, all_nodes: []const SyntaxNode) ?T {
    const node = all_nodes[node_index];
    return switch (@typeInfo(T)) {
        .@"struct" => if (node.kind == T.kind)
            T{ .node_index = node_index }
        else
            null,
        .@"union" => |u| blk: {
            @setEvalBranchQuota(5000);
            inline for (u.fields) |field| {
                switch (node.kind) {
                    inline else => if (toASTNode(field.type, node_index, all_nodes)) |v|
                        break :blk @unionInit(T, field.name, v),
                }
            }
            break :blk null;
        },
        else => @compileError(std.fmt.comptimePrint(
            "Cannot convert {s} to ASTNode",
            .{@typeName(T)},
        )),
    };
}

/// Iterator to yield all the child nodes that coerce to an AST type
fn ASTIterator(T: type) type {
    return struct {
        index: u32,
        stop_index: u32,

        pub fn init(node_index: u32, all_nodes: []const SyntaxNode) @This() {
            assert(all_nodes[node_index].kind.getType() == .tree);

            const children = all_nodes[node_index].data.tree;
            const child_index = children.offset;

            return .{
                .index = child_index,
                .stop_index = child_index + children.len,
            };
        }

        pub fn len(iter: *@This(), all_nodes: []const SyntaxNode) u32 {
            var count: u32 = 0;
            while (iter.next(all_nodes)) |_| {
                count += 1;
            }
            return count;
        }

        pub fn skip(iter: *@This(), n: usize, all_nodes: []const SyntaxNode) void {
            for (0..n) |_| {
                _ = iter.next(all_nodes);
            }
        }

        pub fn next(iter: *@This(), all_nodes: []const SyntaxNode) ?T {
            while (iter.index < iter.stop_index) : (iter.index += 1) {
                if (toASTNode(T, iter.index, all_nodes)) |child| {
                    iter.index += 1;
                    return child;
                }
            }
            return null;
        }
    };
}

/// Used in the typed AST to get children matching a certain ASTNode type.
fn getLastChild(T: type, node_index: u32, all_nodes: []const SyntaxNode) ?T {
    assert(all_nodes[node_index].kind.getType() == .tree);
    const children = all_nodes[node_index].data.tree;

    const child_index = children.offset;
    var i = children.len;
    while (i > 0) {
        i -= 1;
        if (toASTNode(T, i + child_index, all_nodes)) |c| return c;
    }
    return null;
}

/// Used in the typed AST to get children matching a certain ASTNode type.
fn getFirstChild(T: type, node_index: u32, all_nodes: []const SyntaxNode) ?T {
    assert(all_nodes[node_index].kind.getType() == .tree);
    const children = all_nodes[node_index].data.tree;

    const child_index = children.offset;
    for (0..children.len) |i| {
        if (toASTNode(T, @as(u32, @intCast(i)) + child_index, all_nodes)) |c| return c;
    }
    return null;
}

fn nodeFn(ast_node: anytype, all_nodes: []const SyntaxNode) SyntaxNode {
    return all_nodes[ast_node.node_index];
}

pub const Text = struct {
    node_index: u32,
    const kind: SyntaxKind = .text;
    pub const node = nodeFn;

    pub fn parts(self: Text, all_nodes: []const SyntaxNode) ASTIterator(TextPart) {
        return .init(self.node_index, all_nodes);
    }
};

pub const TextPart = union(enum) {
    newline: Newline,
    text_line: TextLine,
    code: Code,

    pub fn nodeIndex(part: TextPart) u32 {
        return switch (part) {
            inline else => |v| v.node_index,
        };
    }
};

pub const TextLine = struct {
    node_index: u32,
    const kind: SyntaxKind = .text_line;
    pub const node = nodeFn;

    pub fn get(self: TextLine, src: []const u8, all_nodes: []const SyntaxNode) []const u8 {
        return self.node(all_nodes).getLeafSource(src);
    }
};

pub const Code = struct {
    node_index: u32,
    const kind: SyntaxKind = .code;
    pub const node = nodeFn;

    pub fn statements(self: Code, all_nodes: []const SyntaxNode) ASTIterator(Statement) {
        return .init(self.node_index, all_nodes);
    }
};

pub const Statement = union(enum) {
    for_loop: ForLoop,
    while_loop: WhileLoop,
    expression: Expression,
    conditional: Conditional,
    let_statement: LetStatement,
    break_statement: BreakStatement,
    return_statement: ReturnStatement,
    export_statement: ExportStatement,
    continue_statement: ContinueStatement,

    pub fn nodeIndex(stmt: Statement) u32 {
        return switch (stmt) {
            .expression => |expr| expr.nodeIndex(),
            inline else => |v| v.node_index,
        };
    }
};

pub const Expression = union(enum) {
    nil: Nil,
    list: List,
    dict: Dict,
    true: True,
    unary: Unary,
    ident: Ident,
    false: False,
    binary: Binary,
    number: Number,
    integer: Integer,
    grouping: Grouping,
    dot_access: DotAccess,
    function_def: FunctionDef,
    function_call: FunctionCall,
    static_string: StaticString,
    bracket_access: BracketAccess,
    multi_line_string: MultiLineString,

    pub fn nodeIndex(expr: Expression) u32 {
        return switch (expr) {
            inline else => |v| v.node_index,
        };
    }
};

pub const List = struct {
    node_index: u32,
    const kind: SyntaxKind = .list;
    pub const node = nodeFn;

    pub fn items(self: List, all_nodes: []const SyntaxNode) ASTIterator(Expression) {
        return .init(self.node_index, all_nodes);
    }
};

pub const Dict = struct {
    node_index: u32,
    const kind: SyntaxKind = .dict;
    pub const node = nodeFn;

    pub fn fields(self: Dict, all_nodes: []const SyntaxNode) ASTIterator(DictField) {
        return .init(self.node_index, all_nodes);
    }
};

pub const DictField = struct {
    node_index: u32,
    const kind: SyntaxKind = .dict_field;
    pub const node = nodeFn;

    pub fn key(self: DictField, all_nodes: []const SyntaxNode) Ident {
        return getFirstChild(Ident, self.node_index, all_nodes) orelse unreachable;
    }

    pub fn value(self: DictField, all_nodes: []const SyntaxNode) Expression {
        return getLastChild(Expression, self.node_index, all_nodes) orelse unreachable;
    }
};

pub const Grouping = struct {
    node_index: u32,
    const kind: SyntaxKind = .grouping;
    pub const node = nodeFn;

    pub fn inner(self: Grouping, all_nodes: []const SyntaxNode) Expression {
        return getFirstChild(Expression, self.node_index, all_nodes) orelse unreachable;
    }
};

pub const FunctionDef = struct {
    node_index: u32,
    const kind: SyntaxKind = .function_def;
    pub const node = nodeFn;

    pub const FunctionBody = union(enum) {
        text: Text,
        expression: Expression,
    };

    pub fn name(self: FunctionDef, all_nodes: []const SyntaxNode) ?Ident {
        return getFirstChild(Ident, self.node_index, all_nodes);
    }

    pub fn parameters(self: FunctionDef, all_nodes: []const SyntaxNode) FunctionParameters {
        return getFirstChild(FunctionParameters, self.node_index, all_nodes) orelse unreachable;
    }

    pub fn body(self: FunctionDef, all_nodes: []const SyntaxNode) FunctionBody {
        return getLastChild(FunctionBody, self.node_index, all_nodes) orelse unreachable;
    }
};

pub const FunctionParameters = struct {
    node_index: u32,
    const kind: SyntaxKind = .function_parameters;
    pub const node = nodeFn;

    pub fn get(self: FunctionParameters, all_nodes: []const SyntaxNode) ASTIterator(Ident) {
        return .init(self.node_index, all_nodes);
    }
};

pub const ReturnStatement = struct {
    node_index: u32,
    const kind: SyntaxKind = .return_statement;
    pub const node = nodeFn;

    pub fn returnValue(self: ReturnStatement, all_nodes: []const SyntaxNode) ?Expression {
        return getFirstChild(Expression, self.node_index, all_nodes);
    }
};

pub const LetStatement = struct {
    node_index: u32,
    const kind: SyntaxKind = .let_statement;
    pub const node = nodeFn;

    pub fn variableName(self: LetStatement, all_nodes: []const SyntaxNode) Ident {
        return getFirstChild(Ident, self.node_index, all_nodes) orelse unreachable;
    }

    pub fn initialValue(self: LetStatement, all_nodes: []const SyntaxNode) ?Expression {
        var iter: ASTIterator(Expression) = .init(self.node_index, all_nodes);
        iter.skip(1, all_nodes); // Skip past var name
        return iter.next(all_nodes);
    }
};

pub const ExportStatement = struct {
    node_index: u32,
    const kind: SyntaxKind = .export_statement;
    pub const node = nodeFn;

    const ExportInner = union(enum) {
        let_statement: LetStatement,
        function_def: FunctionDef,
    };

    pub fn inner(self: ExportStatement, all_nodes: []const SyntaxNode) ExportInner {
        return getFirstChild(ExportInner, self.node_index, all_nodes) orelse unreachable;
    }
};

pub const ForLoop = struct {
    node_index: u32,
    const kind: SyntaxKind = .for_loop;
    pub const node = nodeFn;

    // TODO
};

pub const WhileLoop = struct {
    node_index: u32,
    const kind: SyntaxKind = .while_loop;
    pub const node = nodeFn;

    pub fn condition(self: WhileLoop, all_nodes: []const SyntaxNode) Expression {
        return getFirstChild(Expression, self.node_index, all_nodes) orelse unreachable;
    }

    pub fn body(self: WhileLoop, all_nodes: []const SyntaxNode) Text {
        return getLastChild(Text, self.node_index, all_nodes) orelse unreachable;
    }
};

pub const Conditional = struct {
    node_index: u32,
    const kind: SyntaxKind = .conditional;
    pub const node = nodeFn;

    const BranchIterator = struct {
        index: u32,
        stop_index: u32,

        pub fn init(node_index: u32, all_nodes: []const SyntaxNode) @This() {
            assert(all_nodes[node_index].kind.getType() == .tree);

            const children = all_nodes[node_index].data.tree;
            const child_index = children.offset;

            return .{
                .index = child_index,
                .stop_index = child_index + children.len,
            };
        }

        const Branch = struct {
            /// Branch condition is null if the branch is an else.
            condition: ?Expression,
            body: Text,
        };

        pub fn len(iter: *BranchIterator, all_nodes: []const SyntaxNode) u32 {
            var count: u32 = 0;
            while (iter.next(all_nodes)) |_| {
                count += 1;
            }
            return count;
        }

        pub fn next(iter: *BranchIterator, all_nodes: []const SyntaxNode) ?Branch {
            // Will be null for an else branch
            var condition: ?Expression = null;

            while (iter.index < iter.stop_index) {
                defer iter.index += 1;

                if (toASTNode(Expression, iter.index, all_nodes)) |child|
                    condition = child;

                if (toASTNode(Text, iter.index, all_nodes)) |body|
                    return .{ .condition = condition, .body = body };
            }
            return null;
        }
    };

    pub fn branches(self: Conditional, all_nodes: []const SyntaxNode) BranchIterator {
        return .init(self.node_index, all_nodes);
    }
};

pub const UnaryOperator = union(enum) {
    not: Not,
    negate: Minus,

    pub fn precedence(self: UnaryOperator) usize {
        return switch (self) {
            // See BinaryOperator.precedence for more detailed precedence rules
            .not => 4,
            .negate => 10,
        };
    }
};

pub const Unary = struct {
    node_index: u32,
    const kind: SyntaxKind = .unary;
    pub const node = nodeFn;

    pub fn op(self: Unary, all_nodes: []const SyntaxNode) UnaryOperator {
        return getFirstChild(UnaryOperator, self.node_index, all_nodes) orelse unreachable;
    }

    pub fn rhs(self: Unary, all_nodes: []const SyntaxNode) Expression {
        return getLastChild(Expression, self.node_index, all_nodes) orelse unreachable;
    }
};

pub const BinaryOperator = union(enum) {
    in: In,
    @"or": Or,
    add: Plus,
    assign: Eq,
    equal: EqEq,
    @"and": And,
    less_than: Lt,
    divide: Slash,
    multiply: Star,
    subtract: Minus,
    modulo: Percent,
    greater_than: Gt,
    not_equal: NotEq,
    less_than_equal: LtEq,
    greater_than_equal: GtEq,

    pub fn precedence(self: BinaryOperator) usize {
        return switch (self) {
            .assign => 1,
            .@"or" => 2,
            .@"and" => 3,
            // Unary not => 4
            .not_equal, .equal => 5,
            .less_than, .less_than_equal, .greater_than_equal, .greater_than => 6,
            .add, .subtract => 7,
            .multiply, .divide, .modulo => 8,
            .in => 9,
            // Unary negate => 10
        };
    }

    pub const Associativity = enum { left, right };
    pub fn associativity(self: BinaryOperator) Associativity {
        return switch (self) {
            .@"or",
            .@"and",
            .not_equal,
            .equal,
            .less_than_equal,
            .less_than,
            .greater_than_equal,
            .greater_than,
            .add,
            .subtract,
            .modulo,
            .multiply,
            .divide,
            .in,
            => .left,
            .assign => .right,
        };
    }
};

pub const Binary = struct {
    node_index: u32,
    const kind: SyntaxKind = .binary;
    pub const node = nodeFn;

    pub fn lhs(self: Binary, all_nodes: []const SyntaxNode) Expression {
        return getFirstChild(Expression, self.node_index, all_nodes) orelse unreachable;
    }

    pub fn op(self: Binary, all_nodes: []const SyntaxNode) BinaryOperator {
        return getFirstChild(BinaryOperator, self.node_index, all_nodes) orelse unreachable;
    }

    pub fn rhs(self: Binary, all_nodes: []const SyntaxNode) Expression {
        return getLastChild(Expression, self.node_index, all_nodes) orelse unreachable;
    }
};

pub const FunctionCall = struct {
    node_index: u32,
    const kind: SyntaxKind = .function_call;
    pub const node = nodeFn;

    pub fn caller(self: FunctionCall, all_nodes: []const SyntaxNode) Expression {
        return getFirstChild(Expression, self.node_index, all_nodes) orelse unreachable;
    }

    pub fn arguments(self: FunctionCall, all_nodes: []const SyntaxNode) FunctionArgs {
        return getFirstChild(FunctionArgs, self.node_index, all_nodes) orelse unreachable;
    }
};

pub const FunctionArgs = struct {
    node_index: u32,
    const kind: SyntaxKind = .function_args;
    pub const node = nodeFn;

    pub fn get(self: FunctionArgs, all_nodes: []const SyntaxNode) ASTIterator(Expression) {
        return .init(self.node_index, all_nodes);
    }
};

pub const DotAccess = struct {
    node_index: u32,
    const kind: SyntaxKind = .dot_access;
    pub const node = nodeFn;

    /// What is on the right side of the access, e.g. "foo" in `foo.bar`
    pub fn lhs(self: DotAccess, all_nodes: []const SyntaxNode) Expression {
        return getFirstChild(Expression, self.node_index, all_nodes) orelse unreachable;
    }

    /// What is on the right side of the access, e.g. "bar" in `foo.bar`
    pub fn rhs(self: DotAccess, all_nodes: []const SyntaxNode) Ident {
        return getLastChild(Ident, self.node_index, all_nodes) orelse unreachable;
    }
};

pub const BracketAccess = struct {
    node_index: u32,
    const kind: SyntaxKind = .bracket_access;
    pub const node = nodeFn;

    /// What is on the right side of the access, e.g. "foo" in `foo["bar"]`
    pub fn lhs(self: BracketAccess, all_nodes: []const SyntaxNode) Expression {
        return getFirstChild(Expression, self.node_index, all_nodes) orelse unreachable;
    }

    /// What is on the right side of the access, e.g. "bar" in `foo["bar"]`
    pub fn rhs(self: BracketAccess, all_nodes: []const SyntaxNode) Expression {
        return getLastChild(Expression, self.node_index, all_nodes) orelse unreachable;
    }
};

pub const Integer = struct {
    node_index: u32,
    const kind: SyntaxKind = .integer;
    pub const node = nodeFn;

    const Error = error{Overflow};
    pub fn getAsInt(self: Integer, all_nodes: []const SyntaxNode, src: []const u8) Error!u32 {
        return std.fmt.parseInt(
            u32,
            self.node(all_nodes).getLeafSource(src),
            10,
        ) catch error.Overflow;
    }

    pub fn getAsFloat(self: Integer, all_nodes: []const SyntaxNode, src: []const u8) f64 {
        return std.fmt.parseFloat(f64, self.node(all_nodes).getLeafSource(src)) catch unreachable;
    }
};

pub const Number = struct {
    node_index: u32,
    const kind: SyntaxKind = .number;
    pub const node = nodeFn;

    pub fn get(self: Number, all_nodes: []const SyntaxNode, src: []const u8) f64 {
        return std.fmt.parseFloat(f64, self.node(all_nodes).getLeafSource(src)) catch unreachable;
    }
};

pub const StaticString = struct {
    node_index: u32,
    const kind: SyntaxKind = .static_string;
    pub const node = nodeFn;

    pub fn create(
        self: StaticString,
        all_nodes: []const SyntaxNode,
        src: []const u8,
        w: *std.Io.Writer,
    ) !void {
        const string = self.node(all_nodes).getLeafSource(src);
        const inner = string[1 .. string.len - 1]; // Trim off the quotes

        const quote = string[0];
        assert(quote == '"' or quote == '\'');

        var is_backslashed = false;
        var i: usize = 0;
        while (i < inner.len) : (i += 1) {
            const char = inner[i];

            if (std.mem.startsWith(u8, inner[i..], "@\\(")) {
                i += "@\\(".len;
                _ = try w.writeAll("@(");
                continue;
            }

            if (is_backslashed) {
                if (char == quote) try w.writeByte(quote);
                switch (char) {
                    'n' => try w.writeByte('\n'),
                    '\\' => try w.writeByte('\\'),
                    else => try w.print("\\{}", .{char}),
                }
                is_backslashed = false;
                continue;
            }

            if (char == '\\') {
                is_backslashed = true;
                continue;
            }
            try w.writeByte(char);
        }
    }
};

pub const MultiLineString = struct {
    node_index: u32,
    const kind: SyntaxKind = .multiline_string;
    pub const node = nodeFn;

    pub fn parts(self: MultiLineString, all_nodes: []const SyntaxNode) ASTIterator(MLSPart) {
        return ASTIterator(MLSPart).init(self.node_index, all_nodes);
    }
};

pub const MLSPart = union(enum) {
    newline: Newline,
    text: MLSText,
    expression: MLSExpression,

    pub fn nodeIndex(self: MLSPart) u32 {
        return switch (self) {
            inline else => |v| v.node_index,
        };
    }
};

pub const MLSExpression = struct {
    node_index: u32,
    const kind: SyntaxKind = .mls_expression;
    pub const node = nodeFn;

    pub fn get(self: MLSExpression, all_nodes: []const SyntaxNode) Expression {
        return getFirstChild(Expression, self.node_index, all_nodes) orelse unreachable;
    }
};

pub const MLSText = struct {
    node_index: u32,
    const kind: SyntaxKind = .mls_text;
    pub const node = nodeFn;

    pub fn get(self: MLSText, all_nodes: []const SyntaxNode, src: []const u8) []const u8 {
        return self.node(all_nodes).getLeafSource(src);
    }
};

pub const Ident = struct {
    node_index: u32,
    const kind: SyntaxKind = .ident;
    pub const node = nodeFn;

    pub fn get(self: Ident, all_nodes: []const SyntaxNode, src: []const u8) []const u8 {
        return self.node(all_nodes).getLeafSource(src);
    }
};

fn ASTNode(k: SyntaxKind) type {
    return struct {
        node_index: u32,
        const kind: SyntaxKind = k;
        pub const node = nodeFn;
    };
}

pub const Eq = ASTNode(.eq);
pub const Lt = ASTNode(.lt);
pub const Gt = ASTNode(.gt);
pub const Plus = ASTNode(.plus);
pub const Star = ASTNode(.star);
pub const EqEq = ASTNode(.eq_eq);
pub const LtEq = ASTNode(.lt_eq);
pub const GtEq = ASTNode(.gt_eq);
pub const Minus = ASTNode(.minus);
pub const Slash = ASTNode(.slash);
pub const NotEq = ASTNode(.not_eq);
pub const In = ASTNode(.keyword_in);
pub const Or = ASTNode(.keyword_or);
pub const Newline = ASTNode(.newline);
pub const Percent = ASTNode(.percent);
pub const Nil = ASTNode(.keyword_nil);
pub const And = ASTNode(.keyword_and);
pub const Not = ASTNode(.keyword_not);
pub const True = ASTNode(.keyword_true);
pub const False = ASTNode(.keyword_false);
pub const BreakStatement = ASTNode(.keyword_break);
pub const ContinueStatement = ASTNode(.keyword_continue);

const std = @import("std");
const assert = std.debug.assert;

const SyntaxKind = @import("node.zig").SyntaxKind;
const SyntaxNode = @import("node.zig").SyntaxNode;
