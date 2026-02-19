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
//! A node of the AST has a `toTyped` function that converts a SyntaxNode into itself, and a `kind`
//! indicating what the kind of the node is. The kind is used to derive the toTyped function.
//!
//! Some derived nodes are not rooted in the CST, such as `Access`; there is a `dot_access` and
//! `bracket_access` SyntaxKind but no `access` SyntaxKind. These derived nodes simplify the AST to
//! not worry about the specific kind.
//!
//! A node of the AST has a `toTyped` function that converts a SyntaxNode into itself, and a `kind`
//! indicating what the kind of the node is. The kind is used to derive the toTyped function.
//!
//! Some derived nodes are not rooted in the CST, such as `Access`; there is a `dot_access` and
//! `bracket_access` SyntaxKind but no `access` SyntaxKind. These derived nodes simplify the AST to
//! not worry about the specific kind.

fn deriveToTyped(T: type) fn (SyntaxNode) ?T {
    return struct {
        fn toTyped(node: SyntaxNode) ?T {
            return if (node.kind == T.kind)
                T{ .v = node }
            else
                null;
        }
    }.toTyped;
}

pub const List = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .list;
    pub const toTyped = deriveToTyped(@This());
};

pub const Dict = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .dict;
    pub const toTyped = deriveToTyped(@This());
};

pub const DictField = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .dict_field;
    pub const toTyped = deriveToTyped(@This());
};

pub const Code = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .code;
    pub const toTyped = deriveToTyped(@This());
};

pub const Text = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .text;
    pub const toTyped = deriveToTyped(@This());
};

pub const Grouping = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .grouping;
    pub const toTyped = deriveToTyped(@This());
};

pub const FunctionDef = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .function_def;
    pub const toTyped = deriveToTyped(@This());
};

pub const FunctionParameters = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .function_parameters;
    pub const toTyped = deriveToTyped(@This());
};

pub const ReturnStatement = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .return_statement;
    pub const toTyped = deriveToTyped(@This());
};

pub const LetStatement = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .let_statement;
    pub const toTyped = deriveToTyped(@This());
};

pub const ExportStatement = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .export_statement;
    pub const toTyped = deriveToTyped(@This());
};

pub const ForLoop = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .for_loop;
    pub const toTyped = deriveToTyped(@This());
};

pub const WhileLoop = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .while_loop;
    pub const toTyped = deriveToTyped(@This());
};

pub const Conditional = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .conditional;
    pub const toTyped = deriveToTyped(@This());
};

pub const Unary = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .unary;
    pub const toTyped = deriveToTyped(@This());
};

pub const Binary = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .binary;
    pub const toTyped = deriveToTyped(@This());
};

pub const FunctionCall = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .function_call;
    pub const toTyped = deriveToTyped(@This());
};

pub const FunctionArgs = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .function_args;
    pub const toTyped = deriveToTyped(@This());
};

pub const DotAccess = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .dot_access;
    pub const toTyped = deriveToTyped(@This());
};

pub const BracketAccess = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .bracket_access;
    pub const toTyped = deriveToTyped(@This());
};

pub const Number = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .number;
    pub const toTyped = deriveToTyped(@This());

    pub fn get(self: Number, src: []const u8) f64 {
        return std.fmt.parseFloat(f64, self.v.getLeafSource(src)) catch unreachable;
    }
};

pub const String = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .string;
    pub const toTyped = deriveToTyped(@This());

    pub fn get(self: String, src: []const u8) []const u8 {
        // TODO make this handle backslashes
        const str = self.v.getLeafSource(src);
        return str[1 .. str.len - 1]; // Trim the quotes
    }
};

pub const Ident = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .ident;
    pub const toTyped = deriveToTyped(@This());

    pub fn get(self: Ident, src: []const u8) []const u8 {
        return self.v.getLeafSource(src);
    }
};

pub const Nil = struct {
    v: SyntaxNode,
    const kind: SyntaxKind = .keyword_nil;
    pub const toTyped = deriveToTyped(@This());
};

pub const Bool = struct {
    v: SyntaxNode,

    pub fn toTyped(node: SyntaxNode) ?Bool {
        return switch (node.kind) {
            .keyword_true => .{ .v = node },
            .keyword_false => .{ .v = node },

            else => null,
        };
    }

    pub fn get(self: Bool) bool {
        return self.v.kind == .keyword_true;
    }
};

const SyntaxKind = @import("node.zig").SyntaxKind;
const SyntaxNode = @import("node.zig").SyntaxNode;
const std = @import("std");
