const ValueType = @import("../runtime/value.zig").ValueType;
const Value = @import("../runtime/value.zig").Value;
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
};

pub const RuntimeError = error{Error};
