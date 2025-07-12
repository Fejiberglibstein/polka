const std = @import("std");
const SyntaxNode = @import("node.zig").SyntaxNode;
const SyntaxKind = @import("node.zig").SyntaxKind;

pub const ASTNode = union(enum) {
    code: Code,
    ident: Ident,
    number: Number,
    string: String,
    grouping: Grouping,
    export_expr: ExportExpr,
    function_def: FunctionDef,
    function_parameters: FunctionParameters,
    return_expr: ReturnExpr,
    conditional: Conditional,
    for_loop: ForLoop,
    while_loop: WhileLoop,
    let_expr: LetExpr,
    assignment: Assignment,
    binary: Binary,
    unary: Unary,
    function_call: FunctionCall,
    argument_list: ArgumentList,
    dot_access: DotAccess,
    bracket_access: BracketAccess,
    text_node: TextNode,

    pub fn value(self: ASTNode) SyntaxNode {
        switch (self) {
            inline else => |v| {
                return v.v;
            },
        }
    }
};

fn toTypedTemplate(comptime T: type, comptime k: SyntaxKind) fn (SyntaxNode) ?T {
    return struct {
        fn toTyped(n: SyntaxNode) ?T {
            if (n.kind() == k) {
                return T{ .v = n };
            } else {
                return null;
            }
        }
    }.toTyped;
}

pub const Code = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .code;

    pub const toTyped = toTypedTemplate(@This(), kind);
};
pub const Ident = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .ident;

    pub const toTyped = toTypedTemplate(@This(), kind);
};
pub const Number = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .number;

    pub const toTyped = toTypedTemplate(@This(), kind);
};
pub const String = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .string;

    pub const toTyped = toTypedTemplate(@This(), kind);
};
pub const Grouping = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .grouping;

    pub const toTyped = toTypedTemplate(@This(), kind);
};
pub const ExportExpr = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .export_expr;

    pub const toTyped = toTypedTemplate(@This(), kind);
};
pub const FunctionDef = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .function_def;

    pub const toTyped = toTypedTemplate(@This(), kind);
};
pub const FunctionParameters = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .function_parameters;

    pub const toTyped = toTypedTemplate(@This(), kind);
};
pub const ReturnExpr = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .return_expr;

    pub const toTyped = toTypedTemplate(@This(), kind);
};
pub const Conditional = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .conditional;

    pub const toTyped = toTypedTemplate(@This(), kind);
};
pub const ForLoop = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .for_loop;

    pub const toTyped = toTypedTemplate(@This(), kind);
};
pub const WhileLoop = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .while_loop;

    pub const toTyped = toTypedTemplate(@This(), kind);
};
pub const LetExpr = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .let_expr;

    pub const toTyped = toTypedTemplate(@This(), kind);
};
pub const Assignment = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .assignment;

    pub const toTyped = toTypedTemplate(@This(), kind);
};
pub const Binary = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .binary;

    pub const toTyped = toTypedTemplate(@This(), kind);
};
pub const Unary = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .unary;

    pub const toTyped = toTypedTemplate(@This(), kind);
};
pub const FunctionCall = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .function_call;

    pub const toTyped = toTypedTemplate(@This(), kind);
};
pub const ArgumentList = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .argument_list;

    pub const toTyped = toTypedTemplate(@This(), kind);
};
pub const DotAccess = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .dot_access;

    pub const toTyped = toTypedTemplate(@This(), kind);
};
pub const BracketAccess = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .bracket_access;

    pub const toTyped = toTypedTemplate(@This(), kind);
};
pub const TextNode = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .text_node;

    pub const toTyped = toTypedTemplate(@This(), kind);
};
