//! Provides a typed inerface to access the CST (Concrete syntax tree) that `parser.zig` produces.
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

const std = @import("std");
const Allocator = std.mem.Allocator;
const SyntaxNode = @import("node.zig").SyntaxNode;
const SyntaxKind = @import("node.zig").SyntaxKind;

pub const ASTNode = union(enum) {
    text: Text,
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

pub const Context = struct {
    /// Source code of the entire file
    src_file: []const u8,

    /// All of the parsed nodes
    all_nodes: std.ArrayList(SyntaxNode),

    inline fn nodes(self: Context) []const SyntaxNode {
        return self.all_nodes.items;
    }
};

/// Iterator to yield all the child nodes that coerce to an AST type
pub fn ASTIterator(comptime T: type) type {
    return struct {
        index: usize,
        nodes: []const SyntaxNode,

        pub fn init(node: SyntaxNode, all_nodes: []const SyntaxNode) @This() {
            return .{
                .index = 0,
                .nodes = node.children(all_nodes),
            };
        }

        pub fn next(self: *@This()) ?T {
            while (self.index < self.nodes.len) : (self.index += 1) {
                if (T.toTyped(self.nodes[self.index])) |c| {
                    return c;
                }
            }
            return null;
        }
    };
}

/// Used in the typed AST to get children matching a certain ASTNode type.
///
/// If no nodes are found, will return a generic node
fn lastChild(node: SyntaxNode, all_nodes: []const SyntaxNode, T: type) T {
    const childs = node.children(all_nodes);
    var i = childs.len - 1;

    while (i > 0) : (i -= 1) {
        if (T.toTyped(childs[i])) |c| {
            return c;
        }
    }

    return T{ .v = SyntaxNode.leafNode(T.kind, "") };
}

/// Used in the typed AST to get children matching a certain ASTNode type.
///
/// If no nodes are found, will return a generic node
fn firstChild(node: SyntaxNode, all_nodes: []const SyntaxNode, T: type) T {
    for (node.children(all_nodes)) |child| {
        if (T.toTyped(child)) |c| {
            return c;
        }
    }

    return T{ .v = SyntaxNode.leafNode(T.kind, "") };
}

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

pub const Text = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .text;
    pub const toTyped = toTypedTemplate(@This(), kind);

    pub fn get(self: Text, allocator: Allocator) Allocator.Error![]const u8 {
        return std.mem.replacementSize(u8, allocator, self.v.range, "\\`", "`");
    }
};

pub const Code = struct {
    v: SyntaxNode,
    pub const kind: SyntaxKind = .code;
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
        return self.v.range;
    }
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
