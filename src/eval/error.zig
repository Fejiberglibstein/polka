const ValueType = @import("value.zig").ValueType;
const Value = @import("value.zig").Value;
const ast = @import("../syntax/ast.zig");

pub const RuntimeErrorPayload = union(enum(u8)) {
    /// Invalid operands to binary operation
    /// hint: <lhs> <op> <rhs> is not allowed
    invalid_binary_operands: struct {
        lhs: Value,
        rhs: Value,
        op: ast.BinaryOperator.Op,
    },
    /// The rhs of the modulus operator must be > 0. (got <rhs>)
    modulo_error: struct { rhs: f64 },
    /// Invalid operand to unary <op> (got <rhs>)
    invalid_unary_operands: struct {
        rhs: Value,
        op: ast.UnaryOperator.Op,
    },

    /// Cannot call <callee>
    bad_function: Value,

    /// Function called with too many parameters. Expected <arity>
    function_bad_args: usize,

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
    heap_oom,

    /// An internal allocation error
    allocation_error,
};

pub const RuntimeError = error{Error};
