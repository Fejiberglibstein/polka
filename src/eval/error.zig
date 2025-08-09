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
    /// Function declarations must be named
    unnamed_function,

    /// Cannout have return outside of function
    misplaced_return,
    /// Cannout have break outside of loop
    misplaced_break,
    /// Cannout have continue outside of loop
    misplaced_continue,

    /// Invalid left-hand-side to assignment
    invalid_assignment,

    /// Use of undeclared identifier `<name>`
    undeclared_ident: []const u8,

    /// Cannot index a dict with `<Value type name>`
    invalid_dict_access: Value,
    /// Cannot index a list with `<Value type name>`
    invalid_list_access: Value,
    /// Used in smth like `12[10]`
    ///
    /// Cannot index a <Value type name>
    invalid_access: Value,

    /// When the value stack exceeds its limit
    ///
    /// Stack overflow
    stack_overflow,

    /// When the heap runs out of space
    ///
    /// Out of memory
    heap_oom,

    /// An internal allocation error
    allocation_error,
};

/// Used as the return values inside all eval* functions in `./nodes.zig`.
///
/// Represents the flow of control throughout the program execution.
pub const ControlFlow = error{
    /// A continue statement has occured
    Continue,
    /// A break statement has occured
    Break,
    /// A
    Return,
} || RuntimeError;

pub const RuntimeError = error{Error};
