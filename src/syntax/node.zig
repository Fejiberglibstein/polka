pub const SyntaxKind = enum(u7) {
    // Leaf tokens

    /// End of file
    eof,
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
    perc,
    /// -
    minus,
    /// /
    slash,
    /// ,
    comma,
    /// (
    left_paren,
    /// )
    right_paren,
    /// {
    left_brace,
    /// }
    right_brace,
    /// [
    left_bracket,
    /// ]
    right_bracket,
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
    nil,
    /// `true` keyword; the true constant
    true,
    /// `false` keyword; the false constant
    false,
    //// `in` keyword, used for iterables and as a binary operator `h in list`
    in,
    /// `or` keyword, used as a binary operator or
    @"or",
    /// `and` keyword, used as a binary operator and
    @"and",
    /// `not` keyword, used as unary operator not
    not,
    /// `for` keyword, used for for loops
    @"for",
    /// `while` keyword, used in while loops
    @"while",
    /// `do` keyword, used in for loops (for i in list do)
    do,
    /// `if` keyword, used in conditional expressions
    @"if",
    /// `then` keyword, used in conditional expressions
    then,
    /// `else` keyword, used in conditional expressions
    @"else",
    /// `elseif` keyword, used in conditional expressions
    elseif,
    /// `end` keyword, used to terminate a block
    end,
    /// `nil` keyword; the nil constant
    /// `export` keyword, used to declare a variable or function as global outside the file
    @"export",
    /// `return` keyword, used to return from a function
    @"return",
    /// `let` keyword, used for variable declarations
    let,
    /// `func` keyword, used to declare a function
    func,
    /// `continue` keyword, used to continue to the next iteration in a loop
    @"continue",
    /// `break` keyword, used to break out of loops
    @"break",

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
    /// Function definition statement. Functions can be anonymous `func(a, b) ... end`, and they also come in
    /// two flavors: The first is the typical lua-style function:
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
    argument_list,
    /// Dot access expression, used to access fields of expressions
    /// EXPRESSION '.' ident
    dot_access,
    /// Bracket access expression, used to access fields of expressions
    /// EXPRESSION '[ EXPRESSION ']'
    bracket_access,

    pub const SyntaxNodeType = enum { tree, leaf };
    pub fn getType(self: SyntaxKind) SyntaxNodeType {
        return switch (self) {
            .eof,
            .code_begin,
            .codeblock_delim,
            .newline,
            .text_line,
            .dot,
            .plus,
            .star,
            .perc,
            .minus,
            .slash,
            .comma,
            .left_paren,
            .right_paren,
            .left_brace,
            .right_brace,
            .left_bracket,
            .right_bracket,
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
            .nil,
            .true,
            .false,
            .in,
            .@"or",
            .@"and",
            .not,
            .@"for",
            .@"while",
            .do,
            .@"if",
            .then,
            .@"else",
            .elseif,
            .end,
            .@"export",
            .@"return",
            .let,
            .func,
            .@"continue",
            .@"break",
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
            .argument_list,
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
};

const std = @import("std");
const assert = std.debug.assert;
