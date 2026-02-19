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
//!
//! ## Implementation
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
//! Some derived nodes are not rooted in the CST, such as `Access`; there is a `dot_access` and
//! `bracket_access` SyntaxKind but no `access` SyntaxKind. These derived nodes simplify the AST to
//! not worry about the specific kind.
//!
//! Some derived nodes are not rooted in the CST, such as `Access`; there is a `dot_access` and
//! `bracket_access` SyntaxKind but no `access` SyntaxKind. These derived nodes simplify the AST to
//! not worry about the specific kind.

pub fn toTyped(T: type, node: SyntaxNode) ?T {
    if (@typeInfo(T) == .@"struct") {
        return if (node.kind == T.kind)
            T{ .v = node }
        else
            null;
    } else if (@typeInfo(T) == .@"union") {
        // TODO check if this optimizes into a jump table
        inline for (@typeInfo(T).@"union".fields) |field| {
            if (toTyped(field.type, node)) |v| return @unionInit(T, field.name, v);
        }
        return null;
    }
}

/// Iterator to yield all the child nodes that coerce to an AST type
fn ASTIterator(T: type) type {
    return struct {
        child_nodes: []const SyntaxNode,
        index: usize,

        pub fn init(node: SyntaxNode, all_nodes: []const SyntaxNode) @This() {
            return .{ .child_nodes = node.getTreeChildren(all_nodes), .index = 0 };
        }

        pub fn next(self: *@This()) ?T {
            while (self.index < self.child_nodes.len) : (self.index += 1) {
                if (toTyped(T, self.child_nodes[self.index])) |child| {
                    self.index += 1;
                    return child;
                }
            }
            return null;
        }
    };
}

/// Used in the typed AST to get children matching a certain ASTNode type.
fn castLastChild(T: type, node: SyntaxNode, all_nodes: []const SyntaxNode) ?T {
    const children = node.getTreeChildren(all_nodes);
    if (children.len == 0) return null;

    var i = children.len - 1;
    while (i > 0) : (i -= 1) {
        if (toTyped(T, children[i])) |c| return c;
    }
    return null;
}

/// Used in the typed AST to get children matching a certain ASTNode type.
fn getFirstChild(T: type, node: SyntaxNode, all_nodes: []const SyntaxNode) ?T {
    for (node.getTreeChildren(all_nodes)) |child| {
        if (toTyped(T, child)) |c| return c;
    }
    return null;
}

pub const Text = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .text;

    pub fn parts(self: Text, all_nodes: []const SyntaxNode) ASTIterator(TextPart) {
        return .init(self.v, all_nodes);
    }
};

pub const TextPart = union(enum) {
    newline: Newline,
    text: TextLine,
    code: Code,
};

pub const TextLine = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .text_line;
};

pub const Newline = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .newline;
};

pub const List = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .list;
};

pub const Dict = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .dict;
};

pub const DictField = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .dict_field;
};

pub const Code = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .code;
};

pub const Grouping = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .grouping;
};

pub const FunctionDef = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .function_def;
};

pub const FunctionParameters = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .function_parameters;
};

pub const ReturnStatement = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .return_statement;
};

pub const LetStatement = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .let_statement;
};

pub const ExportStatement = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .export_statement;
};

pub const ForLoop = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .for_loop;
};

pub const WhileLoop = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .while_loop;
};

pub const Conditional = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .conditional;
};

pub const Unary = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .unary;
};

pub const Binary = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .binary;
};

pub const FunctionCall = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .function_call;
};

pub const FunctionArgs = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .function_args;
};

pub const DotAccess = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .dot_access;
};

pub const BracketAccess = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .bracket_access;
};

pub const Number = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .number;

    pub fn get(self: Number, src: []const u8) f64 {
        return std.fmt.parseFloat(f64, self.v.getLeafSource(src)) catch unreachable;
    }
};

pub const String = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .string;

    pub fn get(self: String, src: []const u8) []const u8 {
        // TODO make this handle backslashes
        const str = self.v.getLeafSource(src);
        return str[1 .. str.len - 1]; // Trim the quotes
    }
};

pub const Ident = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .ident;

    pub fn get(self: Ident, src: []const u8) []const u8 {
        return self.v.getLeafSource(src);
    }
};

pub const Bool = union(enum) {
    true: True,
    false: False,

    pub fn get(self: Bool) bool {
        return self.v == Bool.true;
    }
};

pub const True = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .keyword_true;
};

pub const False = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .keyword_false;
};

pub const Nil = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .keyword_nil;
};

const SyntaxKind = @import("node.zig").SyntaxKind;
const SyntaxNode = @import("node.zig").SyntaxNode;
const std = @import("std");
