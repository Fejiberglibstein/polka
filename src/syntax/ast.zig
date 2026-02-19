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

pub fn toASTNode(comptime T: type, node: SyntaxNode) ?T {
    return switch (@typeInfo(T)) {
        .@"struct" => if (node.kind == T.kind)
            T{ .node = node }
        else
            null,
        .@"union" => |u| blk: {
            inline for (u.fields) |field| {
                switch (node.kind) {
                    inline else => if (toASTNode(field.type, node)) |v|
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
        child_nodes: []const SyntaxNode,
        index: usize,

        pub fn init(node: SyntaxNode, all_nodes: []const SyntaxNode) @This() {
            return .{ .child_nodes = node.getTreeChildren(all_nodes), .index = 0 };
        }

        pub fn skip(self: *@This(), n: usize) void {
            for (0..n) |_| {
                _ = self.next();
            }
        }

        pub fn next(self: *@This()) ?T {
            while (self.index < self.child_nodes.len) : (self.index += 1) {
                if (toASTNode(T, self.child_nodes[self.index])) |child| {
                    self.index += 1;
                    return child;
                }
            }
            return null;
        }
    };
}

/// Used in the typed AST to get children matching a certain ASTNode type.
fn getLastChild(T: type, node: SyntaxNode, all_nodes: []const SyntaxNode) ?T {
    const children = node.getTreeChildren(all_nodes);
    var i = children.len;
    while (i > 0) {
        i -= 1;
        if (toASTNode(T, children[i])) |c| return c;
    }
    return null;
}

/// Used in the typed AST to get children matching a certain ASTNode type.
fn getFirstChild(T: type, node: SyntaxNode, all_nodes: []const SyntaxNode) ?T {
    for (node.getTreeChildren(all_nodes)) |child| {
        if (toASTNode(T, child)) |c| return c;
    }
    return null;
}

pub const Text = struct {
    node: SyntaxNode,
    const kind: SyntaxKind = .text;

    pub fn parts(self: Text, all_nodes: []const SyntaxNode) ASTIterator(TextPart) {
        return .init(self.node, all_nodes);
    }
};

pub const TextPart = union(enum) {
    newline: Newline,
    text_line: TextLine,
    code: Code,
};

pub const TextLine = struct {
    node: SyntaxNode,
    const kind: SyntaxKind = .text_line;

    pub fn get(self: TextLine, src: []const u8) []const u8 {
        return self.node.getLeafSource(src);
    }
};

pub const Code = struct {
    node: SyntaxNode,
    const kind: SyntaxKind = .code;

    pub fn statements(self: Code, all_nodes: []const SyntaxNode) ASTIterator(Statement) {
        return .init(self.node, all_nodes);
    }
};

pub const Statement = union(enum) {
    for_loop: ForLoop,
    while_loop: WhileLoop,
    expression: Expression,
    let_statement: LetStatement,
    break_statement: BreakStatement,
    return_statement: ReturnStatement,
    export_statement: ExportStatement,
    continue_statement: ContinueStatement,
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
    string: String,
    grouping: Grouping,
    conditional: Conditional,
    function_def: FunctionDef,
    function_call: FunctionCall,
};

pub const List = struct {
    node: SyntaxNode,
    const kind: SyntaxKind = .list;

    pub fn items(self: List, all_nodes: []const SyntaxNode) ASTIterator(Expression) {
        return .init(self.node, all_nodes);
    }
};

pub const Dict = struct {
    node: SyntaxNode,
    const kind: SyntaxKind = .dict;

    pub fn items(self: Dict, all_nodes: []const SyntaxNode) ASTIterator(DictField) {
        return .init(self.node, all_nodes);
    }
};

pub const DictField = struct {
    node: SyntaxNode,
    const kind: SyntaxKind = .dict_field;

    pub fn key(self: DictField, all_nodes: []const SyntaxNode) Expression {
        return getFirstChild(Expression, self.node, all_nodes) orelse unreachable;
    }

    pub fn value(self: DictField, all_nodes: []const SyntaxNode) Expression {
        return getLastChild(Expression, self.node, all_nodes) orelse unreachable;
    }
};

pub const Grouping = struct {
    node: SyntaxNode,
    const kind: SyntaxKind = .grouping;

    pub fn inner(self: Grouping, all_nodes: []const SyntaxNode) Expression {
        return getFirstChild(Expression, self.node, all_nodes) orelse unreachable;
    }
};

pub const FunctionDef = struct {
    node: SyntaxNode,
    const kind: SyntaxKind = .function_def;

    pub const FunctionBody = union(enum) {
        text: Text,
        expression: Expression,
    };

    pub fn name(self: FunctionDef, all_nodes: []const SyntaxNode) ?Ident {
        return getFirstChild(Ident, self.node, all_nodes);
    }

    pub fn parameters(self: FunctionDef, all_nodes: []const SyntaxNode) FunctionParameters {
        return getFirstChild(FunctionParameters, self.node, all_nodes) orelse unreachable;
    }

    pub fn body(self: FunctionDef, all_nodes: []const SyntaxNode) FunctionBody {
        return getLastChild(FunctionBody, self.node, all_nodes) orelse unreachable;
    }
};

pub const FunctionParameters = struct {
    node: SyntaxNode,
    const kind: SyntaxKind = .function_parameters;

    pub fn get(self: FunctionParameters, all_nodes: []const SyntaxNode) ASTIterator(Ident) {
        return .init(self.node, all_nodes);
    }
};

pub const ReturnStatement = struct {
    node: SyntaxNode,
    const kind: SyntaxKind = .return_statement;

    pub fn returnValue(self: ReturnStatement, all_nodes: []const SyntaxNode) ?Expression {
        return getFirstChild(Expression, self.node, all_nodes);
    }
};

pub const LetStatement = struct {
    node: SyntaxNode,
    const kind: SyntaxKind = .let_statement;

    pub fn variableName(self: LetStatement, all_nodes: []const SyntaxNode) Ident {
        return getFirstChild(Ident, self.node, all_nodes) orelse unreachable;
    }

    pub fn initialValue(self: LetStatement, all_nodes: []const SyntaxNode) ?Expression {
        var iter: ASTIterator(Expression) = .init(self.node, all_nodes);
        iter.skip(1); // Skip past var name
        return iter.next();
    }
};

pub const ExportStatement = struct {
    node: SyntaxNode,
    const kind: SyntaxKind = .export_statement;

    const ExportInner = union(enum) {
        let_statement: LetStatement,
        function_def: FunctionDef,
    };

    pub fn getInner(self: ExportStatement, all_nodes: []const SyntaxNode) ExportInner {
        return getFirstChild(ExportInner, self.node, all_nodes) orelse unreachable;
    }
};

pub const ForLoop = struct {
    node: SyntaxNode,
    const kind: SyntaxKind = .for_loop;

    // TODO
};

pub const WhileLoop = struct {
    node: SyntaxNode,
    const kind: SyntaxKind = .while_loop;

    pub fn condition(self: WhileLoop, all_nodes: []const SyntaxNode) Expression {
        return getFirstChild(Expression, self.node, all_nodes);
    }

    pub fn body(self: WhileLoop, all_nodes: []const SyntaxNode) Text {
        return getLastChild(Text, self.node, all_nodes);
    }
};

pub const Conditional = struct {
    node: SyntaxNode,
    const kind: SyntaxKind = .conditional;

    // TODO
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
    node: SyntaxNode,
    const kind: SyntaxKind = .unary;

    pub fn op(self: Unary, all_nodes: []const SyntaxNode) UnaryOperator {
        return getFirstChild(UnaryOperator, self.node, all_nodes) orelse unreachable;
    }

    pub fn rhs(self: Unary, all_nodes: []const SyntaxNode) Expression {
        return getLastChild(Expression, self.node, all_nodes) orelse unreachable;
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
    node: SyntaxNode,
    const kind: SyntaxKind = .binary;

    pub fn lhs(self: Binary, all_nodes: []const SyntaxNode) Expression {
        return getFirstChild(Expression, self.node, all_nodes) orelse unreachable;
    }

    pub fn op(self: Binary, all_nodes: []const SyntaxNode) BinaryOperator {
        return getFirstChild(BinaryOperator, self.node, all_nodes) orelse unreachable;
    }

    pub fn rhs(self: Binary, all_nodes: []const SyntaxNode) Expression {
        return getLastChild(Expression, self.node, all_nodes) orelse unreachable;
    }
};

pub const FunctionCall = struct {
    node: SyntaxNode,
    const kind: SyntaxKind = .function_call;

    pub fn caller(self: FunctionCall, all_nodes: []const SyntaxNode) Expression {
        return getFirstChild(Expression, self.node, all_nodes) orelse unreachable;
    }

    pub fn arguments(self: FunctionCall, all_nodes: []const SyntaxNode) FunctionArgs {
        return getFirstChild(FunctionArgs, self.node, all_nodes) orelse unreachable;
    }
};

pub const FunctionArgs = struct {
    node: SyntaxNode,
    const kind: SyntaxKind = .function_args;

    pub fn get(self: FunctionArgs, all_nodes: []const SyntaxNode) ASTIterator(Expression) {
        return .init(self.v, all_nodes);
    }
};

pub const DotAccess = struct {
    node: SyntaxNode,
    const kind: SyntaxKind = .dot_access;

    /// What is on the right side of the access, e.g. "foo" in `foo.bar`
    pub fn lhs(self: DotAccess, all_nodes: []const SyntaxNode) Expression {
        return getFirstChild(Expression, self.v, all_nodes) orelse unreachable;
    }

    /// What is on the right side of the access, e.g. "bar" in `foo.bar`
    pub fn rhs(self: DotAccess, all_nodes: []const SyntaxNode) Ident {
        return getLastChild(Ident, self.v, all_nodes) orelse unreachable;
    }
};

pub const BracketAccess = struct {
    node: SyntaxNode,
    const kind: SyntaxKind = .bracket_access;

    /// What is on the right side of the access, e.g. "foo" in `foo["bar"]`
    pub fn lhs(self: BracketAccess, all_nodes: []const SyntaxNode) Expression {
        return getFirstChild(Expression, self.v, all_nodes) orelse unreachable;
    }

    /// What is on the right side of the access, e.g. "bar" in `foo["bar"]`
    pub fn rhs(self: BracketAccess, all_nodes: []const SyntaxNode) Expression {
        return getLastChild(Expression, self.v, all_nodes) orelse unreachable;
    }
};

pub const Number = struct {
    node: SyntaxNode,
    const kind: SyntaxKind = .number;

    pub fn get(self: Number, src: []const u8) f64 {
        return std.fmt.parseFloat(f64, self.node.getLeafSource(src)) catch unreachable;
    }
};

pub const String = struct {
    node: SyntaxNode,
    const kind: SyntaxKind = .string;

    pub fn get(self: String, src: []const u8) []const u8 {
        // TODO make this handle backslashes
        const str = self.node.getLeafSource(src);
        return str[1 .. str.len - 1]; // Trim the quotes
    }
};

pub const Ident = struct {
    node: SyntaxNode,
    const kind: SyntaxKind = .ident;

    pub fn get(self: Ident, src: []const u8) []const u8 {
        return self.node.getLeafSource(src);
    }
};

fn ASTNode(k: SyntaxKind) type {
    return struct {
        node: SyntaxNode,
        const kind: SyntaxKind = k;
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

const SyntaxKind = @import("node.zig").SyntaxKind;
const SyntaxNode = @import("node.zig").SyntaxNode;
const std = @import("std");
