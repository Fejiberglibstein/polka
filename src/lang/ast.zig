/// Attribution: The way the ast is set up is very heavily based on Typst's and Rust Analyzer's ast.
///
/// ---
///
/// Provides a typed interface to access the untyped CST (Concrete syntax tree) that `parser.zig`
/// produces.
///
/// The untyped CST is simply a `[]const SyntaxNode`. The topmost node in that is _always_ a .text
/// node. Each SyntaxNode with children contains a offset index into the CST and the length of its
/// children nodes; more information is in the doc comment for SyntaxNode.
///
/// In order to provide typed-ergonomics, most SyntaxKinds have respective structs with methods like
/// .op(), .rhs(), .lhs() for a Binary Expression. These nodes may be one of 
/// - A struct containing a `kind: SyntaxKind` decl and a 32 bit index into the CST where its node
///   lives
/// - A tagged union made up of other node types
///
/// These nodes can be constructed in a few ways:
/// - ASTIterator() iterates over the children of a node and returns all child nodes that match a
///   particular type
/// - get{First,Last}Child() gets the first/last child of a node that coerce to a certain type, if
///   any.
/// - `ast.toASTNode()` takes in the index of a node in the CST and returns a typed value if the
///   types match, though this needn't be used very often.
const ast = @This();

pub fn toASTNode(comptime T: type, index: NodeIndex, all_nodes: []const SyntaxNode) ?T {
    const node = all_nodes[@intFromEnum(index)];
    return switch (@typeInfo(T)) {
        .@"struct" => if (node.kind == T.kind)
            T{ .index = index }
        else
            null,
        .@"union" => |info| blk: {
            // The amount of inlining done here is to help the compiler do its best to optimize this
            // into a jump table
            @setEvalBranchQuota(@typeInfo(SyntaxKind).@"enum".fields.len * info.fields.len);
            inline for (info.fields) |field| {
                switch (node.kind) {
                    inline else => if (toASTNode(field.type, index, all_nodes)) |v|
                        break :blk @unionInit(T, field.name, v),
                }
            }
            break :blk null;
        },
        else => @compileError("Cannot convert " ++ @typeName(T) ++ " to ASTNode"),
    };
}

/// Iterator to yield all the child nodes that coerce to an AST type
fn ASTIterator(T: type) type {
    return struct {
        index: u32,
        stop_index: u32,

        pub fn init(index: NodeIndex, all_nodes: []const SyntaxNode) @This() {
            assert(all_nodes[@intFromEnum(index)].kind.getType() == .tree);

            const children = all_nodes[@intFromEnum(index)].data.tree;
            const child_index = children.offset;

            return .{
                .index = child_index,
                .stop_index = child_index + children.len,
            };
        }

        pub fn len(iter: *@This(), all_nodes: []const SyntaxNode) u32 {
            var count: u32 = 0;
            while (iter.next(all_nodes)) |_| count += 1;
            return count;
        }

        pub fn skip(iter: *@This(), all_nodes: []const SyntaxNode, n: usize) void {
            for (0..n) |_|
                _ = iter.next(all_nodes);
        }

        pub fn next(iter: *@This(), all_nodes: []const SyntaxNode) ?T {
            while (iter.index < iter.stop_index) {
                defer iter.index += 1;
                if (toASTNode(T, @enumFromInt(iter.index), all_nodes)) |child| return child;
            }
            return null;
        }
    };
}

/// Used in the typed AST to get children matching a certain ASTNode type.
fn getLastChild(T: type, index: NodeIndex, all_nodes: []const SyntaxNode) ?T {
    assert(all_nodes[@intFromEnum(index)].kind.getType() == .tree);
    const children = all_nodes[@intFromEnum(index)].data.tree;
    const child_index = children.offset;
    var i = children.len;
    while (i > 0) {
        i -= 1;
        if (toASTNode(T, @enumFromInt(i + child_index), all_nodes)) |child| return child;
    }
    return null;
}

/// Used in the typed AST to get children matching a certain ASTNode type.
fn getFirstChild(T: type, index: NodeIndex, all_nodes: []const SyntaxNode) ?T {
    assert(all_nodes[@intFromEnum(index)].kind.getType() == .tree);
    const children = all_nodes[@intFromEnum(index)].data.tree;
    const child_index = children.offset;
    for (0..children.len) |i| {
        if (toASTNode(T, @enumFromInt(i + child_index), all_nodes)) |child| return child;
    }
    return null;
}

fn nodeFn(ast_node: anytype, all_nodes: []const SyntaxNode) SyntaxNode {
    return all_nodes[@intFromEnum(ast_node.index)];
}

pub const Text = struct {
    index: NodeIndex,
    pub const kind: SyntaxKind = .text;
    pub const node = nodeFn;

    pub fn parts(self: Text, all_nodes: []const SyntaxNode) ASTIterator(TextPart) {
        return .init(self.index, all_nodes);
    }
};

pub const TextPart = union(enum) {
    newline: Newline,
    text_line: TextLine,
    code: Code,

    pub fn index(part: TextPart) NodeIndex {
        return switch (part) {
            inline else => |v| v.index,
        };
    }
};

pub const TextLine = struct {
    index: NodeIndex,
    pub const kind: SyntaxKind = .text_line;
    pub const node = nodeFn;

    pub fn get(self: TextLine, src: []const u8, all_nodes: []const SyntaxNode) []const u8 {
        return self.node(all_nodes).getLeafSource(src);
    }
};

pub const Code = struct {
    index: NodeIndex,
    pub const kind: SyntaxKind = .code;
    pub const node = nodeFn;

    pub fn statements(self: Code, all_nodes: []const SyntaxNode) ASTIterator(Statement) {
        return .init(self.index, all_nodes);
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

    pub fn index(stmt: Statement) NodeIndex {
        return switch (stmt) {
            .expression => |expr| expr.index(),
            inline else => |v| v.index,
        };
    }
};

pub const Expression = union(enum) {
    nil: Nil,
    list: List,
    dict: Dict,
    true: True,
    color: Color,
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

    pub fn index(expr: Expression) NodeIndex {
        return switch (expr) {
            inline else => |v| v.index,
        };
    }
};

pub const List = struct {
    index: NodeIndex,
    pub const kind: SyntaxKind = .list;
    pub const node = nodeFn;

    pub fn items(self: List, all_nodes: []const SyntaxNode) ASTIterator(Expression) {
        return .init(self.index, all_nodes);
    }
};

pub const Dict = struct {
    index: NodeIndex,
    pub const kind: SyntaxKind = .dict;
    pub const node = nodeFn;

    pub fn fields(self: Dict, all_nodes: []const SyntaxNode) ASTIterator(DictField) {
        return .init(self.index, all_nodes);
    }
};

pub const DictField = struct {
    index: NodeIndex,
    pub const kind: SyntaxKind = .dict_field;
    pub const node = nodeFn;

    pub fn key(self: DictField, all_nodes: []const SyntaxNode) Ident {
        return getFirstChild(Ident, self.index, all_nodes) orelse unreachable;
    }

    pub fn value(self: DictField, all_nodes: []const SyntaxNode) Expression {
        return getLastChild(Expression, self.index, all_nodes) orelse unreachable;
    }
};

pub const Grouping = struct {
    index: NodeIndex,
    pub const kind: SyntaxKind = .grouping;
    pub const node = nodeFn;

    pub fn inner(self: Grouping, all_nodes: []const SyntaxNode) Expression {
        return getFirstChild(Expression, self.index, all_nodes) orelse unreachable;
    }
};

pub const FunctionDef = struct {
    index: NodeIndex,
    pub const kind: SyntaxKind = .function_def;
    pub const node = nodeFn;

    pub const FunctionBody = union(enum) {
        text: Text,
        expression: Expression,
    };

    pub fn name(self: FunctionDef, all_nodes: []const SyntaxNode) ?Ident {
        return getFirstChild(Ident, self.index, all_nodes);
    }

    pub fn parameters(self: FunctionDef, all_nodes: []const SyntaxNode) FunctionParameters {
        return getFirstChild(FunctionParameters, self.index, all_nodes) orelse unreachable;
    }

    pub fn body(self: FunctionDef, all_nodes: []const SyntaxNode) FunctionBody {
        return getLastChild(FunctionBody, self.index, all_nodes) orelse unreachable;
    }
};

pub const FunctionParameters = struct {
    index: NodeIndex,
    pub const kind: SyntaxKind = .function_parameters;
    pub const node = nodeFn;

    pub fn get(self: FunctionParameters, all_nodes: []const SyntaxNode) ASTIterator(Ident) {
        return .init(self.index, all_nodes);
    }
};

pub const ReturnStatement = struct {
    index: NodeIndex,
    pub const kind: SyntaxKind = .return_statement;
    pub const node = nodeFn;

    pub fn returnValue(self: ReturnStatement, all_nodes: []const SyntaxNode) ?Expression {
        return getFirstChild(Expression, self.index, all_nodes);
    }
};

pub const LetStatement = struct {
    index: NodeIndex,
    pub const kind: SyntaxKind = .let_statement;
    pub const node = nodeFn;

    pub fn variableName(self: LetStatement, all_nodes: []const SyntaxNode) Ident {
        return getFirstChild(Ident, self.index, all_nodes) orelse unreachable;
    }

    pub fn initialValue(self: LetStatement, all_nodes: []const SyntaxNode) ?Expression {
        var iter: ASTIterator(Expression) = .init(self.index, all_nodes);
        iter.skip(all_nodes, 1); // Skip past var name
        return iter.next(all_nodes);
    }
};

pub const ExportStatement = struct {
    index: NodeIndex,
    pub const kind: SyntaxKind = .export_statement;
    pub const node = nodeFn;

    const ExportInner = union(enum) {
        let_statement: LetStatement,
        function_def: FunctionDef,
    };

    pub fn inner(self: ExportStatement, all_nodes: []const SyntaxNode) ExportInner {
        return getFirstChild(ExportInner, self.index, all_nodes) orelse unreachable;
    }
};

pub const ForLoop = struct {
    index: NodeIndex,
    pub const kind: SyntaxKind = .for_loop;
    pub const node = nodeFn;

    // TODO
};

pub const WhileLoop = struct {
    index: NodeIndex,
    pub const kind: SyntaxKind = .while_loop;
    pub const node = nodeFn;

    pub fn condition(self: WhileLoop, all_nodes: []const SyntaxNode) Expression {
        return getFirstChild(Expression, self.index, all_nodes) orelse unreachable;
    }

    pub fn body(self: WhileLoop, all_nodes: []const SyntaxNode) Text {
        return getLastChild(Text, self.index, all_nodes) orelse unreachable;
    }
};

pub const Conditional = struct {
    index: NodeIndex,
    pub const kind: SyntaxKind = .conditional;
    pub const node = nodeFn;

    const BranchIterator = struct {
        index: u32,
        stop_index: u32,

        pub fn init(index: NodeIndex, all_nodes: []const SyntaxNode) @This() {
            assert(all_nodes[@intFromEnum(index)].kind == .conditional);

            const children = all_nodes[@intFromEnum(index)].data.tree;
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

        pub fn next(iter: *BranchIterator, all_nodes: []const SyntaxNode) ?Branch {
            // Will be null for an else branch
            var condition: ?Expression = null;

            while (iter.index < iter.stop_index) {
                defer iter.index += 1;

                if (toASTNode(Expression, @enumFromInt(iter.index), all_nodes)) |cond|
                    condition = cond;

                if (toASTNode(Text, @enumFromInt(iter.index), all_nodes)) |body|
                    return .{ .condition = condition, .body = body };
            }
            return null;
        }
    };

    pub fn branches(self: Conditional, all_nodes: []const SyntaxNode) BranchIterator {
        return .init(self.index, all_nodes);
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

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.writeAll(switch (self) {
            .not => "not",
            .negate => "-",
        });
    }
};

pub const Unary = struct {
    index: NodeIndex,
    pub const kind: SyntaxKind = .unary;
    pub const node = nodeFn;

    pub fn op(self: Unary, all_nodes: []const SyntaxNode) UnaryOperator {
        return getFirstChild(UnaryOperator, self.index, all_nodes) orelse unreachable;
    }

    pub fn rhs(self: Unary, all_nodes: []const SyntaxNode) Expression {
        return getLastChild(Expression, self.index, all_nodes) orelse unreachable;
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

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.writeAll(switch (self) {
            .add => "+",
            .in => "in",
            .@"or" => "or",
            .equal => "==",
            .modulo => "%",
            .divide => "/",
            .assign => "=",
            .@"and" => "and",
            .subtract => "-",
            .multiply => "*",
            .less_than => "<",
            .not_equal => "!=",
            .greater_than => ">",
            .less_than_equal => "<=",
            .greater_than_equal => ">=",
        });
    }
};

pub const Binary = struct {
    index: NodeIndex,
    pub const kind: SyntaxKind = .binary;
    pub const node = nodeFn;

    pub fn lhs(self: Binary, all_nodes: []const SyntaxNode) Expression {
        return getFirstChild(Expression, self.index, all_nodes) orelse unreachable;
    }

    pub fn op(self: Binary, all_nodes: []const SyntaxNode) BinaryOperator {
        return getFirstChild(BinaryOperator, self.index, all_nodes) orelse unreachable;
    }

    pub fn rhs(self: Binary, all_nodes: []const SyntaxNode) Expression {
        return getLastChild(Expression, self.index, all_nodes) orelse unreachable;
    }
};

pub const FunctionCall = struct {
    index: NodeIndex,
    pub const kind: SyntaxKind = .function_call;
    pub const node = nodeFn;

    pub fn caller(self: FunctionCall, all_nodes: []const SyntaxNode) Expression {
        return getFirstChild(Expression, self.index, all_nodes) orelse unreachable;
    }

    pub fn arguments(self: FunctionCall, all_nodes: []const SyntaxNode) FunctionArgs {
        return getFirstChild(FunctionArgs, self.index, all_nodes) orelse unreachable;
    }
};

pub const FunctionArgs = struct {
    index: NodeIndex,
    pub const kind: SyntaxKind = .function_args;
    pub const node = nodeFn;

    pub fn get(self: FunctionArgs, all_nodes: []const SyntaxNode) ASTIterator(Expression) {
        return .init(self.index, all_nodes);
    }
};

pub const DotAccess = struct {
    index: NodeIndex,
    pub const kind: SyntaxKind = .dot_access;
    pub const node = nodeFn;

    /// What is on the right side of the access, e.g. "foo" in `foo.bar`
    pub fn lhs(self: DotAccess, all_nodes: []const SyntaxNode) Expression {
        return getFirstChild(Expression, self.index, all_nodes) orelse unreachable;
    }

    /// What is on the right side of the access, e.g. "bar" in `foo.bar`
    pub fn rhs(self: DotAccess, all_nodes: []const SyntaxNode) Ident {
        return getLastChild(Ident, self.index, all_nodes) orelse unreachable;
    }
};

pub const BracketAccess = struct {
    index: NodeIndex,
    pub const kind: SyntaxKind = .bracket_access;
    pub const node = nodeFn;

    /// What is on the right side of the access, e.g. "foo" in `foo["bar"]`
    pub fn lhs(self: BracketAccess, all_nodes: []const SyntaxNode) Expression {
        return getFirstChild(Expression, self.index, all_nodes) orelse unreachable;
    }

    /// What is on the right side of the access, e.g. "bar" in `foo["bar"]`
    pub fn rhs(self: BracketAccess, all_nodes: []const SyntaxNode) Expression {
        return getLastChild(Expression, self.index, all_nodes) orelse unreachable;
    }
};

pub const Color = struct {
    index: NodeIndex,
    pub const kind: SyntaxKind = .color;
    pub const node = nodeFn;

    pub const ParsedColor = struct {
        r: u8,
        g: u8,
        b: u8,
        alpha: ?u8,
    };

    pub fn get(self: Color, all_nodes: []const SyntaxNode, src: []const u8) ParsedColor {
        const node_src = self.node(all_nodes).getLeafSource(src);
        assert(node_src[0] == '#');

        // Parsing the numbers cannot fail; it was verified during lexing.
        errdefer unreachable;
        return .{
            .r = try std.fmt.parseInt(u8, node_src[1..2], 16),
            .g = try std.fmt.parseInt(u8, node_src[3..4], 16),
            .b = try std.fmt.parseInt(u8, node_src[5..6], 16),
            .alpha = if (node_src.len == 9)
                try std.fmt.parseInt(u8, node_src[7..8], 16)
            else
                null,
        };
    }
};

pub const Integer = struct {
    index: NodeIndex,
    pub const kind: SyntaxKind = .integer;
    pub const node = nodeFn;

    const Error = error{Overflow};
    pub fn getAsInt(self: Integer, all_nodes: []const SyntaxNode, src: []const u8) Error!NodeIndex {
        return std.fmt.parseInt(
            NodeIndex,
            self.node(all_nodes).getLeafSource(src),
            10,
        ) catch error.Overflow;
    }

    pub fn getAsFloat(self: Integer, all_nodes: []const SyntaxNode, src: []const u8) f64 {
        return std.fmt.parseFloat(f64, self.node(all_nodes).getLeafSource(src)) catch unreachable;
    }
};

pub const Number = struct {
    index: NodeIndex,
    pub const kind: SyntaxKind = .number;
    pub const node = nodeFn;

    pub fn get(self: Number, all_nodes: []const SyntaxNode, src: []const u8) f64 {
        return std.fmt.parseFloat(f64, self.node(all_nodes).getLeafSource(src)) catch unreachable;
    }
};

pub const StaticString = struct {
    index: NodeIndex,
    pub const kind: SyntaxKind = .static_string;
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
    index: NodeIndex,
    pub const kind: SyntaxKind = .multiline_string;
    pub const node = nodeFn;

    pub fn parts(self: MultiLineString, all_nodes: []const SyntaxNode) ASTIterator(MLSPart) {
        return .init(self.index, all_nodes);
    }
};

pub const MLSPart = union(enum) {
    newline: Newline,
    text: MLSText,
    expression: MLSExpression,

    pub fn index(self: MLSPart) NodeIndex {
        return switch (self) {
            inline else => |v| v.index,
        };
    }
};

pub const MLSExpression = struct {
    index: NodeIndex,
    pub const kind: SyntaxKind = .mls_expression;
    pub const node = nodeFn;

    pub fn get(self: MLSExpression, all_nodes: []const SyntaxNode) Expression {
        return getFirstChild(Expression, self.index, all_nodes) orelse unreachable;
    }
};

pub const MLSText = struct {
    index: NodeIndex,
    pub const kind: SyntaxKind = .mls_text;
    pub const node = nodeFn;

    pub fn get(self: MLSText, all_nodes: []const SyntaxNode, src: []const u8) []const u8 {
        return self.node(all_nodes).getLeafSource(src);
    }
};

pub const Ident = struct {
    index: NodeIndex,
    pub const kind: SyntaxKind = .ident;
    pub const node = nodeFn;

    pub fn get(self: Ident, all_nodes: []const SyntaxNode, src: []const u8) []const u8 {
        return self.node(all_nodes).getLeafSource(src);
    }
};

fn ASTNode(k: SyntaxKind) type {
    return struct {
        index: NodeIndex,
        pub const kind: SyntaxKind = k;
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

comptime {
    const SyntaxSet = @import("SyntaxSet.zig");

    // Contains all the kinds of nodes covered by the AST.
    //
    // Initialized to all the nodes that are not explicitly not in the AST
    var ast_nodes: SyntaxSet = .init(&.{
        .at,
        .eof,
        .dot,
        .comma,
        .l_paren,
        .r_paren,
        .l_brace,
        .r_brace,
        .backtick,
        .l_bracket,
        .r_bracket,
        .code_begin,
        .invalid_color,
        .codeblock_delim,
        .unexpected_character,

        .keyword_do,
        .keyword_if,
        .keyword_for,
        .keyword_end,
        .keyword_let,
        .keyword_then,
        .keyword_else,
        .keyword_func,
        .keyword_while,
        .keyword_break,
        .keyword_elseif,
        .keyword_export,
        .keyword_return,
        .keyword_continue,
    });

    const decls = @typeInfo(@This()).@"struct".decls;
    for (decls) |decl| {
        const DeclType = @field(@This(), decl.name);
        if (@TypeOf(DeclType) != type) continue;
        if (@typeInfo(DeclType) != .@"struct") continue;

        if (@hasDecl(DeclType, "kind")) {
            ast_nodes.add(DeclType.kind);
        }
    }

    const kinds = @typeInfo(SyntaxKind).@"enum".fields;
    for (kinds) |kind| {
        if (!ast_nodes.contains(@enumFromInt(kind.value))) {
            @compileError("No ast node constructed for " ++ kind.name);
        }
    }
}

pub const NodeIndex = enum(u32) { _ };

const SyntaxKind = @import("node.zig").SyntaxKind;
const SyntaxNode = @import("node.zig").SyntaxNode;
