const ast = @import("../syntax/ast.zig");
const SyntaxNode = @import("../syntax/node.zig");
const Value = @import("../runtime/value.zig").Value;
const Vm = @import("Vm.zig");
const RuntimeErrorPayload = @import("error.zig").RuntimeErrorPayload;
const RuntimeError = @import("error.zig").RuntimeError;

const std = @import("std");

pub fn evalTextNode(node: ast.TextNode, vm: *Vm) !void {
    var text_parts = node.text(vm.nodes);

    // if the last node visited was a code node
    var last_was_code = false;
    while (text_parts.next()) |part| {
        switch (part) {
            .code => |c| {
                try evalCode(c, vm);
                last_was_code = true;
            },
            .text => |t| {
                try vm.content.print("{s}", .{t.get()});
                last_was_code = false;
            },
            .newline => if (!last_was_code)
                try vm.content.print("\n", .{})
            else {
                last_was_code = false;
            },
        }
    }
}

pub fn evalCode(node: ast.Code, vm: *Vm) !void {
    var statements = node.statements(vm.nodes);
    while (statements.next()) |stmt| {
        switch (stmt) {
            .conditional => |_| @panic("TODO"),
            .export_expr => @panic("TODO"),
            .for_loop => @panic("TODO"),
            .function_def => @panic("TODO"),
            .return_expr => @panic("TODO"),
            .expr => |v| try vm.writeValue(try evalExpr(v, vm)),
            .let_expr => |_| @panic("TODO"),
            .while_loop => |_| @panic("TODO"),
        }
    }
}

pub fn evalExpr(node: ast.Expr, vm: *Vm) !Value {
    return switch (node) {
        .nil => .nil,
        .bool => |v| Value{ .bool = v.get() },
        .number => |v| Value{ .number = v.get() },
        .grouping => |v| try evalExpr(v.get(vm.nodes), vm),
        .binary_op => |v| try evalBinary(v, vm),
        .unary_op => |v| try evalUnary(v, vm),
        .string => |_| @panic("TODO"),
        .access => |_| @panic("TODO"),
        .ident => |_| @panic("TODO"),
        .function_call => |_| @panic("TODO"),
    };
}

pub fn evalUnary(node: ast.Unary, vm: *Vm) RuntimeError!Value {
    const rhs = try evalExpr(node.rhs(vm.nodes), vm);
    const op = node.op(vm.nodes).getOp();

    return switch (op) {
        .negate => switch (rhs) {
            .number => |r| Value{ .number = -r },
            else => try vm.setError(.{
                .invalid_unary_operands = .{ .rhs = rhs, .op = op },
            }),
        },
    };
}

pub fn evalBinary(node: ast.Binary, vm: *Vm) RuntimeError!Value {
    // don't eval these nodes yet so that we can do short circuit evaluation on `and`/`or`
    const lhs = node.lhs(vm.nodes);
    const rhs = node.rhs(vm.nodes);
    const op = node.op(vm.nodes).getOp();

    // Lazily calculate the potential error.
    //
    // Since this function evaluates the lhs and rhs, it cannot be called until we definitely have
    // an error since we need to do short circuit evaluation
    const invalidOpError = struct {
        pub fn err(
            lhs_expr: ast.Expr,
            rhs_expr: ast.Expr,
            operator: ast.BinaryOperator.Op,
            inner_vm: *Vm,
        ) RuntimeError!RuntimeErrorPayload {
            return .{
                .invalid_binary_operands = .{
                    .lhs = try evalExpr(lhs_expr, inner_vm),
                    .rhs = try evalExpr(rhs_expr, inner_vm),
                    .op = operator,
                },
            };
        }
    }.err;

    return switch (op) {
        .@"and" => blk: {
            const l = try evalExpr(lhs, vm);
            // Return l if l is false.
            if (!l.isTruthy()) {
                break :blk l;
            }
            // otherwise return r
            break :blk try evalExpr(rhs, vm);
        },
        .@"or" => blk: {
            const l = try evalExpr(lhs, vm);
            // Return l if l is true.
            if (l.isTruthy()) {
                break :blk l;
            }
            // otherwise return r
            break :blk try evalExpr(rhs, vm);
        },
        .add => switch (try evalExpr(lhs, vm)) {
            .number => |l| switch (try evalExpr(rhs, vm)) {
                .number => |r| Value{ .number = l + r },
                else => try vm.setError(try invalidOpError(lhs, rhs, op, vm)),
            },
            .bool => try vm.setError(try invalidOpError(lhs, rhs, op, vm)),
            .nil => try vm.setError(try invalidOpError(lhs, rhs, op, vm)),
            .string => @panic("TODO"),
            .list => @panic("TODO"),
        },
        .subtract => switch (try evalExpr(lhs, vm)) {
            .number => |l| switch (try evalExpr(rhs, vm)) {
                .number => |r| Value{ .number = l - r },
                else => try vm.setError(try invalidOpError(lhs, rhs, op, vm)),
            },
            else => try vm.setError(try invalidOpError(lhs, rhs, op, vm)),
        },
        .multiply => switch (try evalExpr(lhs, vm)) {
            .number => |l| switch (try evalExpr(rhs, vm)) {
                .number => |r| Value{ .number = l * r },
                else => try vm.setError(try invalidOpError(lhs, rhs, op, vm)),
            },
            else => try vm.setError(try invalidOpError(lhs, rhs, op, vm)),
        },
        .divide => switch (try evalExpr(lhs, vm)) {
            .number => |l| switch (try evalExpr(rhs, vm)) {
                .number => |r| Value{ .number = l / r },
                else => try vm.setError(try invalidOpError(lhs, rhs, op, vm)),
            },
            else => try vm.setError(try invalidOpError(lhs, rhs, op, vm)),
        },
        .modulo => switch (try evalExpr(lhs, vm)) {
            .number => |l| switch (try evalExpr(rhs, vm)) {
                .number => |r| if (r > 0)
                    Value{ .number = @rem(l, r) }
                else
                    try vm.setError(.{ .modulo_error = .{ .rhs = r } }),
                else => try vm.setError(try invalidOpError(lhs, rhs, op, vm)),
            },
            else => try vm.setError(try invalidOpError(lhs, rhs, op, vm)),
        },
        .greater_than => switch (try evalExpr(lhs, vm)) {
            .number => |l| switch (try evalExpr(rhs, vm)) {
                .number => |r| Value{ .bool = l > r },
                else => try vm.setError(try invalidOpError(lhs, rhs, op, vm)),
            },
            else => try vm.setError(try invalidOpError(lhs, rhs, op, vm)),
        },
        .greater_than_equal => switch (try evalExpr(lhs, vm)) {
            .number => |l| switch (try evalExpr(rhs, vm)) {
                .number => |r| Value{ .bool = l >= r },
                else => try vm.setError(try invalidOpError(lhs, rhs, op, vm)),
            },
            else => try vm.setError(try invalidOpError(lhs, rhs, op, vm)),
        },
        .less_than => switch (try evalExpr(lhs, vm)) {
            .number => |l| switch (try evalExpr(rhs, vm)) {
                .number => |r| Value{ .bool = l < r },
                else => try vm.setError(try invalidOpError(lhs, rhs, op, vm)),
            },
            else => try vm.setError(try invalidOpError(lhs, rhs, op, vm)),
        },
        .less_than_equal => switch (try evalExpr(lhs, vm)) {
            .number => |l| switch (try evalExpr(rhs, vm)) {
                .number => |r| Value{ .bool = l <= r },
                else => try vm.setError(try invalidOpError(lhs, rhs, op, vm)),
            },
            else => try vm.setError(try invalidOpError(lhs, rhs, op, vm)),
        },
        else => @panic("TODO"),
    };
}
