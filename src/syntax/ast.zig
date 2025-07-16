//! Provides a typed interface to access the CST (Concrete syntax tree) that `parser.zig` produces.
//!
//! Usually this would be implemented by having struct data inside each variant of the SyntaxKind.
//! However, we want the CST to remain concrete (meaning that the source code can be entirely
//! recreated by doing a traversal on the tree) so that the LSP can be better.
//!
//! The tree is rooted in a `text_node`, and each block of code, such as the inside of an if
//! statement, is also a `text_node`, even when inside a #** **#.
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
//!
//! ## Implementation
//!
//! A node of the AST has a `toTyped` method that converts a SyntaxNode into itself, and a `kind`
//! indicating what the kind of the node is. The kind is only necessary when you need to call
//! `default` on the node.
//!
//! Some derived nodes are not rooted in the CST, such as `Access`; there is a `dot_access` and
//! `bracket_access` SyntaxKind but no `access` SyntaxKind. These derived nodes simplify the AST to
//! not worry about the specific kind.

const std = @import("std");
const Allocator = std.mem.Allocator;

const SyntaxKind = @import("node.zig").SyntaxKind;
const SyntaxNode = @import("node.zig").SyntaxNode;

/// Returns a default value for an ASTNode.
///
/// Because the CST can be improperly created by having syntax errors, the ast must not error and
/// instead just use default values. If there are errors, the AST won't be evaluated so it is fine
/// that there are meaningless values
fn default(comptime T: type) T {
    return switch (@typeInfo(T)) {
        .@"struct" => T{ .v = SyntaxNode.leafNode(T.kind, "") },
        .@"union" => T.default,
        else => @compileError("`default` not implemented for non-struct nodes: " ++ @typeName(T)),
    };
}

/// Iterator to yield all the child nodes that coerce to an AST type
pub fn ASTIterator(comptime T: type) type {
    return struct {
        index: usize,
        nodes: []const SyntaxNode,

        pub fn init(node: SyntaxNode, all_nodes: []const SyntaxNode) @This() {
            return .{ .index = 0, .nodes = node.children(all_nodes) };
        }

        pub fn skip(self: *@This(), n: usize) void {
            for (0..n) |_| {
                _ = self.next();
            }
        }

        pub fn next(self: *@This()) ?T {
            while (self.index < self.nodes.len) : (self.index += 1) {
                if (T.toTyped(self.nodes[self.index])) |c| {
                    self.index += 1;
                    return c;
                }
            }
            return null;
        }
    };
}

/// Used in the typed AST to get children matching a certain ASTNode type.
fn castLastChild(node: SyntaxNode, all_nodes: []const SyntaxNode, T: type) ?T {
    const children = node.children(all_nodes);
    var i = children.len - 1;

    while (i >= 0) : (i -= 1) {
        if (T.toTyped(children[i])) |c| {
            return c;
        }
    }
    return null;
}

/// Used in the typed AST to get children matching a certain ASTNode type.
fn castFirstChild(node: SyntaxNode, all_nodes: []const SyntaxNode, T: type) ?T {
    for (node.children(all_nodes)) |child| {
        if (T.toTyped(child)) |c| {
            return c;
        }
    }
    return null;
}

fn toTypedTemplate(
    comptime T: type,
    comptime k: SyntaxKind,
) fn (SyntaxNode) callconv(.@"inline") ?T {
    return struct {
        inline fn toTyped(n: SyntaxNode) ?T {
            return if (n.kind() == k)
                T{ .v = n }
            else
                null;
        }
    }.toTyped;
}

pub const TextNode = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .text_node;
    pub const toTyped = toTypedTemplate(@This(), kind);

    /// Get all the expressions that are either a `Text`, `Newline`, or `Code`.
    pub fn text(self: TextNode, all_nodes: []const SyntaxNode) ASTIterator(TextPart) {
        return ASTIterator(TextPart).init(self.v, all_nodes);
    }
};

/// A part of a text that can be evaluated. Can be any of `Text`, `Newline`, or `Code`
pub const TextPart = union(enum(u8)) {
    text: Text,
    code: Code,
    newline: void,

    pub fn toTyped(n: SyntaxNode) ?TextPart {
        return switch (n.kind()) {
            .text => .{ .text = Text{ .v = n } },
            .code => .{ .code = Code{ .v = n } },
            .newline => .newline,

            else => null,
        };
    }
};

pub const Text = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .text;
    pub const toTyped = toTypedTemplate(@This(), kind);

    pub fn get(self: Text) []const u8 {
        // TODO make this clean the text by removing backslashes and such
        return self.v.range;
    }
};

pub const Code = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .code;
    pub const toTyped = toTypedTemplate(@This(), kind);

    pub fn statements(self: Code, all_nodes: []const SyntaxNode) ASTIterator(Statement) {
        return ASTIterator(Statement).init(self.v, all_nodes);
    }
};

pub const Newline = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .newline;
    pub const toTyped = toTypedTemplate(@This(), kind);
};

pub const Statement = union(enum(u8)) {
    expr: Expr,
    for_loop: ForLoop,
    let_expr: LetExpr,
    while_loop: WhileLoop,
    export_expr: ExportExpr,
    return_expr: ReturnExpr,
    conditional: Conditional,
    function_def: FunctionDef,

    pub inline fn toTyped(n: SyntaxNode) ?Statement {
        return switch (n.kind()) {
            .for_loop => .{ .for_loop = ForLoop{ .v = n } },
            .let_expr => .{ .let_expr = LetExpr{ .v = n } },
            .while_loop => .{ .while_loop = WhileLoop{ .v = n } },
            .export_expr => .{ .export_expr = ExportExpr{ .v = n } },
            .return_expr => .{ .return_expr = ReturnExpr{ .v = n } },
            .conditional => .{ .conditional = Conditional{ .v = n } },
            .function_def => .{ .function_def = FunctionDef{ .v = n } },

            else => if (Expr.toTyped(n)) |v| .{ .expr = v } else null,
        };
    }
};

pub const Expr = union(enum(u8)) {
    nil: Nil,
    bool: Bool,
    ident: Ident,
    number: Number,
    string: String,
    access: Access,
    unary_op: Unary,
    binary_op: Binary,
    grouping: Grouping,
    function_call: FunctionCall,

    // Default value to use when `default(Expr)` is called
    pub const default: Expr = .{ .nil = Nil{ .v = .leafNode(.nil, "") } };

    pub inline fn toTyped(n: SyntaxNode) ?Expr {
        return switch (n.kind()) {
            .nil => .{ .nil = Nil{ .v = n } },
            .bool => .{ .bool = Bool{ .v = n } },
            .ident => .{ .ident = Ident{ .v = n } },
            .number => .{ .number = Number{ .v = n } },
            .string => .{ .string = String{ .v = n } },
            .unary => .{ .unary_op = Unary{ .v = n } },
            .grouping => .{ .grouping = Grouping{ .v = n } },
            .binary => .{ .binary_op = Binary{ .v = n } },
            .function_call => .{ .function_call = FunctionCall{ .v = n } },
            .dot_access, .bracket_access => .{ .access = Access.toTyped(n) orelse unreachable },

            else => null,
        };
    }
};

pub const Nil = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .nil;
    pub const toTyped = toTypedTemplate(@This(), kind);
};

pub const Ident = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .ident;
    pub const toTyped = toTypedTemplate(@This(), kind);

    fn get(self: Ident) []const u8 {
        return self.v.range;
    }
};

pub const Number = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .number;
    pub const toTyped = toTypedTemplate(@This(), kind);

    pub fn get(self: Number) f64 {
        return std.fmt.parseFloat(f64, self.v.range) catch 0;
    }
};

pub const String = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .string;
    pub const toTyped = toTypedTemplate(@This(), kind);

    pub fn get(self: Text) []const u8 {
        // TODO make this clean the text by removing backslashes and such
        return self.v.range;
    }
};

pub const Bool = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .bool;
    pub const toTyped = toTypedTemplate(@This(), kind);

    pub fn get(self: Bool) bool {
        return std.mem.eql(u8, self.v.range, "true");
    }
};

pub const Grouping = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .grouping;
    pub const toTyped = toTypedTemplate(@This(), kind);

    pub fn get(self: Grouping, all_nodes: []const SyntaxNode) Expr {
        return castFirstChild(self.v, all_nodes, Expr) orelse default(Expr);
    }
};

pub const ExportInner = union(enum(u8)) {
    let_expr: LetExpr,
    function_def: FunctionDef,

    pub inline fn toTyped(n: SyntaxNode) ?ExportInner {
        return switch (n.kind()) {
            .let_expr => .{ .let_expr = LetExpr{ .v = n } },
            .function_def => .{ .function_def = FunctionDef{ .v = n } },

            else => null,
        };
    }
};

pub const ExportExpr = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .export_expr;
    pub const toTyped = toTypedTemplate(@This(), kind);

    pub fn getInner(self: ExportExpr, all_nodes: []const SyntaxNode) ExportInner {
        return castFirstChild(self.v, all_nodes, ExportInner) orelse default(LetExpr);
    }
};

pub const FunctionDef = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .function_def;
    pub const toTyped = toTypedTemplate(@This(), kind);

    pub fn name(self: FunctionDef, all_nodes: []const SyntaxNode) ?Ident {
        return castFirstChild(self.v, all_nodes, Ident);
    }

    pub fn params(self: FunctionDef, all_nodes: []const SyntaxNode) FunctionParameters {
        return castFirstChild(
            self.v,
            all_nodes,
            FunctionParameters,
        ) orelse default(FunctionParameters);
    }

    pub fn body(self: FunctionDef, all_nodes: []const SyntaxNode) TextNode {
        return castFirstChild(self.v, all_nodes, TextNode) orelse default(TextNode);
    }
};

pub const FunctionParameters = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .function_parameters;
    pub const toTyped = toTypedTemplate(@This(), kind);

    pub fn get(self: FunctionParameters, all_nodes: []const SyntaxNode) ASTIterator(Ident) {
        return ASTIterator(Ident).init(self.v, all_nodes);
    }
};

pub const ReturnExpr = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .return_expr;
    pub const toTyped = toTypedTemplate(@This(), kind);

    pub fn body(self: ReturnExpr, all_nodes: []const SyntaxNode) ?Expr {
        return castLastChild(self.v, all_nodes, Expr);
    }
};

pub const Conditional = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .conditional;
    pub const toTyped = toTypedTemplate(@This(), kind);

    pub fn condition(self: Conditional, all_nodes: []const SyntaxNode) Expr {
        return castFirstChild(self.v, all_nodes, Expr) orelse default(Expr);
    }

    pub fn ifBody(self: Conditional, all_nodes: []const SyntaxNode) TextNode {
        return castFirstChild(self.v, all_nodes, TextNode) orelse default(TextNode);
    }

    pub fn elseBody(self: Conditional, all_nodes: []const SyntaxNode) ?TextNode {
        var iter = ASTIterator(TextNode).init(self, all_nodes);

        // Skip past the if body
        iter.skip(1);

        return iter.next();
    }
};

pub const ForLoop = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .for_loop;
    pub const toTyped = toTypedTemplate(@This(), kind);

    pub fn binding(self: ForLoop, all_nodes: []const SyntaxNode) Ident {
        return castFirstChild(self.v, all_nodes, Ident) orelse default(Ident);
    }

    pub fn iterator(self: ForLoop, all_nodes: []const SyntaxNode) Expr {
        var iter = ASTIterator(Expr).init(self.v, all_nodes);

        // Skip past the ident binding
        iter.skip(1);

        return iter.next() orelse default(Expr);
    }

    pub fn body(self: ForLoop, all_nodes: []const SyntaxNode) TextNode {
        return castFirstChild(self.v, all_nodes, TextNode) orelse default(TextNode);
    }
};

pub const WhileLoop = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .while_loop;
    pub const toTyped = toTypedTemplate(@This(), kind);

    pub fn condition(self: WhileLoop, all_nodes: []const SyntaxNode) Expr {
        return castFirstChild(self.v, all_nodes, Expr) orelse default(Expr);
    }

    pub fn body(self: WhileLoop, all_nodes: []const SyntaxNode) TextNode {
        return castFirstChild(self.v, all_nodes, TextNode) orelse default(TextNode);
    }
};

pub const LetExpr = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .let_expr;
    pub const toTyped = toTypedTemplate(@This(), kind);

    pub fn binding(self: LetExpr, all_nodes: []const SyntaxNode) Ident {
        return castFirstChild(self.v, all_nodes, Ident) orelse default(Ident);
    }

    pub fn value(self: LetExpr, all_nodes: []const SyntaxNode) Expr {
        return castLastChild(self.v, all_nodes, Expr) orelse default(Expr);
    }
};

pub const BinaryOperator = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .plus;

    pub fn toTyped(node: SyntaxNode) ?BinaryOperator {
        return if (node.kind().isBinaryOp()) BinaryOperator{ .v = node } else null;
    }

    pub fn precedence(self: BinaryOperator) usize {
        return switch (self.getOp()) {
            .assign => 1,
            .@"or" => 2,
            .@"and" => 3,
            .not_equal, .equal => 4,
            .less_than, .less_than_equal, .greater_than_equal, .greater_than => 5,
            .add, .subtract => 6,
            .multiply, .divide, .modulo => 7,
        };
    }

    pub const Associativity = enum(u8) { left, right };
    pub fn associativity(self: BinaryOperator) BinaryOperator.Associativity {
        return switch (self.getOp()) {
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
            => .left,
            .assign => .right,
        };
    }

    pub const Op = enum(u8) {
        add,
        subtract,
        divide,
        multiply,
        modulo,
        greater_than,
        less_than,
        greater_than_equal,
        less_than_equal,
        equal,
        not_equal,
        assign,
        @"and",
        @"or",

        pub fn toString(self: Op) []const u8 {
            switch (self) {
                .add => "+",
                .subtract => "-",
                .divide => "/",
                .multiply => "*",
                .modulo => "%",
                .greater_than => ">",
                .less_than => "<",
                .greater_than_equal => ">=",
                .less_than_equal => "<=",
                .equal => "==",
                .not_equal => "!=",
                .assign => "=",
                .@"and" => "and",
                .@"or" => "or",
            }
        }
    };

    pub fn getOp(self: BinaryOperator) Op {
        return switch (self.v.kind()) {
            .@"or" => .@"or",
            .@"and" => .@"and",
            .not_eq => .not_equal,
            .eq_eq => .equal,
            .lt_eq => .less_than_equal,
            .lt => .less_than,
            .gt_eq => .greater_than_equal,
            .gt => .greater_than,
            .plus => .add,
            .minus => .subtract,
            .perc => .modulo,
            .star => .multiply,
            .slash => .divide,
            .eq => .assign,

            else => unreachable,
        };
    }
};

pub const Binary = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .binary;
    pub const toTyped = toTypedTemplate(@This(), kind);

    pub fn lhs(self: Binary, all_nodes: []const SyntaxNode) Expr {
        return castFirstChild(self.v, all_nodes, Expr) orelse default(Expr);
    }

    pub fn rhs(self: Binary, all_nodes: []const SyntaxNode) Expr {
        return castLastChild(self.v, all_nodes, Expr) orelse default(Expr);
    }

    pub fn op(self: Binary, all_nodes: []const SyntaxNode) BinaryOperator {
        return castFirstChild(self.v, all_nodes, BinaryOperator) orelse default(BinaryOperator);
    }
};

pub const UnaryOperator = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .minus;

    pub fn toTyped(node: SyntaxNode) ?UnaryOperator {
        return if (node.kind().isUnaryOp()) UnaryOperator{ .v = node } else null;
    }

    pub const Op = enum {
        negate,

        pub fn toString(self: Op) []const u8 {
            return switch (self) {
                .negate => "-",
            };
        }
    };

    pub fn getOp(self: UnaryOperator) Op {
        return switch (self.v.kind()) {
            .minus => .negate,
            else => unreachable,
        };
    }
};

pub const Unary = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .unary;
    pub const toTyped = toTypedTemplate(@This(), kind);

    pub fn rhs(self: Unary, all_nodes: []const SyntaxNode) Expr {
        return castFirstChild(self.v, all_nodes, Expr) orelse default(Expr);
    }

    pub fn op(self: Unary, all_nodes: []const SyntaxNode) UnaryOperator {
        return castFirstChild(self.v, all_nodes, UnaryOperator) orelse default(UnaryOperator);
    }
};

pub const FunctionCall = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .function_call;
    pub const toTyped = toTypedTemplate(@This(), kind);

    pub fn name(self: FunctionCall, all_nodes: []const SyntaxNode) Ident {
        return castFirstChild(self.v, all_nodes, Ident) orelse default(Ident);
    }

    pub fn arguments(self: FunctionCall, all_nodes: []const SyntaxNode) ArgumentList {
        return castFirstChild(self.v, all_nodes, ArgumentList) orelse default(ArgumentList);
    }
};

pub const ArgumentList = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .argument_list;
    pub const toTyped = toTypedTemplate(@This(), kind);

    pub fn get(self: ArgumentList, all_nodes: []const SyntaxNode) ASTIterator(Expr) {
        return ASTIterator(Expr).init(self.v, all_nodes);
    }
};

pub const Access = union(enum(u8)) {
    bracket_access: BracketAccess,
    dot_access: DotAccess,

    pub inline fn toTyped(n: SyntaxNode) ?Access {
        return switch (n.kind()) {
            .bracket_access => .{ .bracket_access = BracketAccess{ .v = n } },
            .dot_access => .{ .dot_access = DotAccess{ .v = n } },

            else => null,
        };
    }

    /// What is on the left side of the access, e.g. "foo" in `foo.bar`
    pub fn lhs(self: Access, all_nodes: []const SyntaxNode) Expr {
        return switch (self) {
            inline else => |v| castFirstChild(v.v, all_nodes, Expr) orelse default(Expr),
        };
    }

    /// What is on the right side of the access, e.g. "bar" in `foo.bar`
    pub fn rhs(self: Access, all_nodes: []const SyntaxNode) Expr {
        return switch (self) {
            inline else => |v| castLastChild(v.v, all_nodes, Expr) orelse default(Expr),
        };
    }
};

pub const DotAccess = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .dot_access;
    pub const toTyped = toTypedTemplate(@This(), kind);

    // Implementation handled in `Access`
};

pub const BracketAccess = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .bracket_access;
    pub const toTyped = toTypedTemplate(@This(), kind);

    // Implementation handled in `Access`
};
