pub const SyntaxKind = enum(u7) {
    // Leaf tokens

    /// End of file
    eof,
    /// Syntax error
    unexpected_character,
    /// Begins a codeline `#*`
    ///
    /// The symbol doesn't need to begin with a hash, it will use whatever the filetype's comment
    /// is. For example, in .toml it would be `#*`, but in .vimrc it would be `"*`
    code_begin,
    /// Begins/ends a codeblock `#**`
    ///
    /// The symbol doesn't need to begin with a hash, it will use whatever the filetype's comment
    /// is. For example, in .toml it would be `#*`, but in .vimrc it would be `"*`
    codeblock_delim,
    /// Newline
    newline,
    /// Line of text
    text_line,
    /// .
    dot,
    /// +
    plus,
    /// *
    star,
    /// %
    percent,
    /// -
    minus,
    /// /
    slash,
    /// ,
    comma,
    /// (
    l_paren,
    /// )
    r_paren,
    /// {
    l_brace,
    /// }
    r_brace,
    /// [
    l_bracket,
    /// ]
    r_bracket,
    /// =
    eq,
    /// ==
    eq_eq,
    /// ~=
    not_eq,
    /// <
    lt,
    /// <=
    lt_eq,
    /// >
    gt,
    /// >=
    gt_eq,
    /// Positive numerical literal: `12`, `0.284`, etc.
    number,
    /// String constant `"hello world"`
    string,
    /// Identifier, used for variables, functions, etc. `foo`
    ident,
    keyword_nil,
    keyword_true,
    keyword_false,
    keyword_in,
    keyword_or,
    keyword_and,
    keyword_not,
    keyword_for,
    keyword_while,
    keyword_do,
    keyword_if,
    keyword_then,
    keyword_else,
    keyword_elseif,
    keyword_end,
    keyword_export,
    keyword_return,
    keyword_let,
    keyword_func,
    keyword_continue,
    keyword_break,

    // Tree tokens
    //
    // EXPRESSION: ( list | dict | grouping | function_call | bracket_access | dot_access
    //               | conditional | unary | binary | number | string | ident | true | false | nil )
    //
    // STATEMENT:  ( let_statement | export_statement | for_loop | while_loop | return_statement
    //               | function_def )

    /// List literal
    /// '[' (EXPRESSION (',' EXPRESSION)* ','?)? ']'
    list,
    /// Dictionary literal
    /// '{' (dict_field (',' dict_field)* ','?)? '}'
    dict,
    /// Field in a dictionary
    /// (identifier | '[' EXPRESSION ']') '=' EXPRESSION
    dict_field,
    /// A list of code statements. This node can be formed both from sequential codelines:
    ///     #* let foo = 10
    ///     #* let h = foo - 2
    ///     #* ...
    /// or from a codeblock:
    ///     #**
    ///     let foo = 10
    ///     let h = foo - 2
    ///     ...
    ///     #**
    ///
    /// For sequential codelines, the node's children will be
    /// ('#*' STATEMENT newline)*
    ///
    /// For codeblocks, the node's children will be
    /// '#**' newline (STATEMENT newline)* '#**' newline
    code,
    /// A list of text_line, newline, and code nodes. This is the root node that parsing will yield.
    /// newline* ((text_line | code) newline+)*
    text,
    /// Expression inside parens
    /// '(' EXPRESSION ')'
    grouping,
    /// Function definition statement. Functions can be anonymous `func(a, b) ... end`, and they
    /// also come in two flavors: The first is the typical lua-style function:
    ///     #* func add(a, b)
    ///     #*   return a + b
    ///     #* end
    ///
    /// The second does not require a newline after the line with `func`, nor does it require an
    /// `end` keyword to terminate it:
    ///     #* func add(a, b) a + b
    /// This makes it consise to create simple functions that just return a single expression
    ///
    /// 'func' identifier? function_parameters (newline text 'end' | expression)
    function_def,
    /// The parameter listing inside a function definition
    /// '(' (ident (',' ident)* ','?)?  ')'
    function_parameters,
    /// Return statement
    /// 'return' EXPRESSION
    return_statement,
    /// Let statement
    /// 'let' ident '=' EXPRESSION
    let_statement,
    /// Export statement
    /// 'export' (let_statement | function_def)
    export_statement,
    /// For loop statement
    /// 'for' identifier (',' identifier)* 'in' EXPRESSION 'do' newline text 'end'
    for_loop,
    /// While loop statement
    /// 'for' identifier (',' identifier)* 'in' EXPRESSION 'do' newline text 'end'
    while_loop,
    /// If expression with an optional else at the end
    ///
    /// 'if' EXPRESSION 'then' newline text
    /// ('elseif' newline EXPRESSION text)*
    /// ('else' newline EXPRESSION)? 'end'
    conditional,
    /// Unary expression
    /// ('not' | '-') EXPRESSION
    unary,
    /// Binary expression
    /// EXPRESSION ('+' | '-' | '*' | '/' | '%' | 'in') EXPRESSION
    binary,
    /// Function call expression
    /// EXPRESSION argument_list
    function_call,
    /// Argument list in a function.
    /// '(' (EXPRESSION (',' EXPRESSION)* ','?)? ')'
    function_args,
    /// Dot access expression, used to access fields of expressions
    /// EXPRESSION '.' ident
    dot_access,
    /// Bracket access expression, used to access fields of expressions
    /// EXPRESSION '[ EXPRESSION ']'
    bracket_access,

    pub const SyntaxNodeType = enum { tree, leaf };
    pub fn getType(self: SyntaxKind) SyntaxNodeType {
        return switch (self) {
            .unexpected_character,
            .eof,
            .code_begin,
            .codeblock_delim,
            .newline,
            .text_line,
            .dot,
            .plus,
            .star,
            .percent,
            .minus,
            .slash,
            .comma,
            .l_paren,
            .r_paren,
            .l_brace,
            .r_brace,
            .l_bracket,
            .r_bracket,
            .eq,
            .eq_eq,
            .not_eq,
            .lt,
            .lt_eq,
            .gt,
            .gt_eq,
            .number,
            .string,
            .ident,
            .keyword_nil,
            .keyword_true,
            .keyword_false,
            .keyword_in,
            .keyword_or,
            .keyword_and,
            .keyword_not,
            .keyword_for,
            .keyword_while,
            .keyword_do,
            .keyword_if,
            .keyword_then,
            .keyword_else,
            .keyword_elseif,
            .keyword_end,
            .keyword_export,
            .keyword_return,
            .keyword_let,
            .keyword_func,
            .keyword_continue,
            .keyword_break,
            => .leaf,

            .list,
            .dict,
            .dict_field,
            .code,
            .text,
            .grouping,
            .function_def,
            .function_parameters,
            .return_statement,
            .let_statement,
            .export_statement,
            .for_loop,
            .while_loop,
            .conditional,
            .unary,
            .binary,
            .function_call,
            .function_args,
            .dot_access,
            .bracket_access,
            => .tree,
        };
    }
};

pub const SyntaxNode = packed struct {
    kind: SyntaxKind,
    data: packed union {
        /// A tree SyntaxNode. It is made up of a slice of children. The children of every
        /// SyntaxNode are located in a single list; .offset is the offset into that list where this
        /// node's children start, and .len is the number of children that this node has.
        tree: packed struct { offset: u32, len: u25 },
        /// A leaf SyntaxNode. It is made up of a slice of characters of the range this node takes
        /// up in the source string. .offset is the offset into that source string where this node's
        /// range starts, and .len is the length of the node's range.
        leaf: packed struct { offset: u32, len: u25 },
    },

    pub fn getLeafSource(self: SyntaxNode, src: []const u8) []const u8 {
        assert(self.kind.getType() == .leaf);
        const range = self.data.leaf;
        return src[range.offset .. range.offset + range.len];
    }

    pub fn getTreeChildren(self: SyntaxNode, all_nodes: []const SyntaxNode) []const SyntaxNode {
        assert(self.kind.getType() == .tree);
        const children = self.data.tree;
        return all_nodes[children.offset .. children.offset + children.len];
    }

    pub fn print(
        self: SyntaxNode,
        all_nodes: []const SyntaxNode,
        src: []const u8,
        depth: usize,
        writer: *std.Io.Writer,
    ) !void {
        for (0..depth) |_| {
            try writer.print("  ", .{});
        }

        switch (self.kind.getType()) {
            .leaf => {
                // const range = if (self.kind == .newline) "" else self.getLeafSource(src);
                try writer.print("{s},\n", .{@tagName(self.kind)});
            },
            .tree => {
                try writer.print("{s} [\n", .{@tagName(self.kind)});
                const children = self.getTreeChildren(all_nodes);
                for (children) |child| {
                    try child.print(all_nodes, src, depth + 1, writer);
                }

                for (0..depth) |_| {
                    try writer.print("  ", .{});
                }
                try writer.print("],\n", .{});
            },
        }
    }
};

const std = @import("std");
const assert = std.debug.assert;
