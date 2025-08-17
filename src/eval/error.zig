const ValueType = @import("value.zig").ValueType;
const Value = @import("value.zig").Value;
const ast = @import("../syntax/ast.zig");

pub const RuntimeErrorPayload = union(enum(u8)) {
    /// Invalid operands to binary operation. <lhs> <op> <rhs> is not allowed
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

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try switch (self) {
            .invalid_binary_operands => |v| writer.print(
                \\Invalid operands to binary operation. {s} {} {s} is not allowed
            , .{ v.lhs.typeName(), v.op, v.rhs.typeName() }),
            .modulo_error => |v| writer.print(
                \\The rhs of the modulus operator must be > 0. (got {})
            , .{v.rhs}),
            .invalid_unary_operands => |v| writer.print(
                \\Invalid operand to unary {} (got {})
            , .{ v.op, v.rhs }),
            .bad_function => |v| writer.print(
                \\Cannot call {s}
            , .{v.typeName()}),
            .function_bad_args => |v| writer.print(
                \\Function called with too many parameters. Expected {}
            , .{v}),
            .unnamed_function => writer.print(
                \\Function declarations must be named
            , .{}),
            .misplaced_return => writer.print(
                \\Cannout have return outside of function
            , .{}),
            .misplaced_break => writer.print(
                \\Cannout have break outside of loop
            , .{}),
            .misplaced_continue => writer.print(
                \\Cannout have continue outside of loop
            , .{}),
            .invalid_assignment => writer.print(
                \\Invalid left-hand-side to assignment
            , .{}),
            .undeclared_ident => |v| writer.print(
                \\Use of undeclared identifier `{s}`
            , .{v}),
            .invalid_dict_access => |v| writer.print(
                \\Cannot index a dict with a {s}
            , .{v.typeName()}),
            .invalid_list_access => |v| writer.print(
                \\Cannot index a list with a {s}
            , .{v.typeName()}),
            .invalid_access => |v| writer.print(
                \\Cannot index a {s}
            , .{v.typeName()}),
            .stack_overflow => writer.print(
                \\Stack overflow
            , .{}),
            .heap_oom => writer.print(
                \\Out of memory
            , .{}),
            .allocation_error => writer.print(
                \\An internal allocation error
            , .{}),
        };
    }
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

pub const std = @import("std");
