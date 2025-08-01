const ValueType = @import("value.zig").ValueType;
const Value = @import("value.zig").Value;
const ast = @import("../syntax/ast.zig");

pub const RuntimeErrorPayload = union(enum(u8)) {
    /// Invalid operands to binary data
    /// hint: <lhs> <op> <rhs> is not allowed
    invalid_binary_operands: struct {
        lhs: Value,
        rhs: Value,
        op: ast.BinaryOperator.Op,
    },
    /// The denominator of the modulus operator must be > 0. (got <rhs>)
    modulo_error: struct { rhs: f64 },
    /// Invalid operand to unary <op>
    invalid_unary_operands: struct {
        rhs: Value,
        op: ast.UnaryOperator.Op,
    },

    /// Invalid left-hand-side to assignment
    invalid_assignment: void,

    /// Use of undeclared identifier `<name>`
    undeclared_ident: []const u8,

    /// When the value stack exceeds its limit
    ///
    /// Stack overflow
    stack_overflow: void,

    /// When the heap runs out of space
    ///
    /// Out of memory
    out_of_memory,
};

pub const RuntimeError = error{Error};
