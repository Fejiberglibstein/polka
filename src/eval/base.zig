const ast = @import("../syntax/ast.zig");
const SyntaxNode = @import("../syntax/node.zig");
const Value = @import("value.zig").Value;
const Vm = @import("Vm.zig");
const RuntimeErrorPayload = @import("error.zig").RuntimeErrorPayload;
const RuntimeError = @import("error.zig").RuntimeError;

const std = @import("std");

pub fn evalTextNode(node: ast.TextNode, vm: *Vm) !void {
    vm.locals.pushScope();
    defer vm.locals.popScope();

    // if the last node visited was a code node
    var last_was_code = false;

    var text_parts = node.text(vm.nodes);
    while (text_parts.next()) |part| {
        switch (part) {
            .code => |c| {
                const len = vm.output.items.len;
                try evalCode(c, vm);
                if (len != vm.output.items.len) {
                    try vm.outputPrint("\n", .{});
                }
                last_was_code = true;
            },
            .text => |t| {
                try vm.outputPrint("{s}", .{t.get()});
                last_was_code = false;
            },
            .newline => if (!last_was_code)
                try vm.outputPrint("\n", .{})
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
            .expr => |v| {
                try evalExpr(v, vm);
            },
            .for_loop => @panic("TODO"),
            .let_expr => @panic("TODO"),
            .while_loop => @panic("TODO"),
            .return_expr => @panic("TODO"),
            .conditional => @panic("TODO"),
            .export_expr => @panic("TODO"),
            .function_def => @panic("TODO"),
        }
    }
}

pub fn evalExpr(node: ast.Expr, vm: *Vm) !void {
    switch (node) {
        .number => |v| try vm.stackPush(.{ .number = v.get() }),
        .bool => |v| try vm.stackPush(.{ .bool = v.get() }),
        .nil => |_| try vm.stackPush(.nil),
        .binary_op => |v| try evalBinary(v, vm),
        .ident => @panic("TODO"),
        .string => @panic("TODO"),
        .access => @panic("TODO"),
        .grouping => @panic("TODO"),
        .unary_op => |v| try evalUnary(v, vm),
        .function_call => @panic("TODO"),
    }
}

pub fn evalUnary(node: ast.Unary, vm: *Vm) RuntimeError!void {
    try evalExpr(node.rhs(vm.nodes), vm);
    const op = node.op(vm.nodes).getOp();

    switch (op) {
        .negate => switch (vm.stackPop()) {
            .number => |v| try vm.stackPush(Value{ .number = -v }),
            else => |v| try vm.setError(.{ .invalid_unary_operands = .{ .rhs = v, .op = op } }),
        },
    }
}

pub fn evalBinary(node: ast.Binary, vm: *Vm) RuntimeError!void {
    const lhs = node.lhs(vm.nodes);
    const rhs = node.rhs(vm.nodes);
    const op = node.op(vm.nodes).getOp();

    const invalidOpError = struct {
        pub fn func(operator: ast.BinaryOperator.Op, virtual_machine: *Vm) RuntimeError!noreturn {
            try virtual_machine.setError(.{ .invalid_binary_operands = .{
                .rhs = virtual_machine.stackPop(),
                .lhs = virtual_machine.stackPop(),
                .op = operator,
            } });
        }
    }.func;
    // Lhs will be pushed first, then rhs

    try evalExpr(lhs, vm);
    try vm.stackPush(switch (op) {
        .assign => @panic("TODO"),
        .@"and" => @panic("TODO"),
        .@"or" => @panic("TODO"),
        .add => blk: {
            try evalExpr(rhs, vm);
            switch (vm.stackPeek(1)) { // switch on lhs
                .number => |l| switch (vm.stackPeek(0)) { // switch on rhs
                    .number => |r| {
                        _ = vm.stackPop(); // Pop rhs
                        _ = vm.stackPop(); // Pop lhs
                        break :blk Value{ .number = l + r };
                    },
                    else => try invalidOpError(op, vm),
                },
                else => @panic("TODO: add strings"),
            }
        },
        .divide => blk: {
            try evalExpr(rhs, vm);
            switch (vm.stackPeek(1)) { // switch on lhs
                .number => |l| switch (vm.stackPeek(0)) { // switch on rhs
                    .number => |r| {
                        _ = vm.stackPop(); // Pop rhs
                        _ = vm.stackPop(); // Pop lhs
                        break :blk Value{ .number = l / r };
                    },
                    else => try invalidOpError(op, vm),
                },
                else => {
                    try evalExpr(rhs, vm);
                    try invalidOpError(op, vm);
                },
            }
        },
        .subtract => blk: {
            try evalExpr(rhs, vm);
            switch (vm.stackPeek(1)) { // switch on lhs
                .number => |l| switch (vm.stackPeek(0)) { // switch on rhs
                    .number => |r| {
                        _ = vm.stackPop(); // Pop rhs
                        _ = vm.stackPop(); // Pop lhs
                        break :blk Value{ .number = l - r };
                    },
                    else => try invalidOpError(op, vm),
                },
                else => {
                    try evalExpr(rhs, vm);
                    try invalidOpError(op, vm);
                },
            }
        },
        .greater_than => blk: {
            try evalExpr(rhs, vm);
            switch (vm.stackPeek(1)) { // switch on lhs
                .number => |l| switch (vm.stackPeek(0)) { // switch on rhs
                    .number => |r| {
                        _ = vm.stackPop(); // Pop rhs
                        _ = vm.stackPop(); // Pop lhs
                        break :blk Value{ .bool = l > r };
                    },
                    else => try invalidOpError(op, vm),
                },
                else => {
                    try evalExpr(rhs, vm);
                    try invalidOpError(op, vm);
                },
            }
        },
        .greater_than_equal => blk: {
            try evalExpr(rhs, vm);
            switch (vm.stackPeek(1)) { // switch on lhs
                .number => |l| switch (vm.stackPeek(0)) { // switch on rhs
                    .number => |r| {
                        _ = vm.stackPop(); // Pop rhs
                        _ = vm.stackPop(); // Pop lhs
                        break :blk Value{ .bool = l >= r };
                    },
                    else => try invalidOpError(op, vm),
                },
                else => {
                    try evalExpr(rhs, vm);
                    try invalidOpError(op, vm);
                },
            }
        },
        .less_than => blk: {
            try evalExpr(rhs, vm);
            switch (vm.stackPeek(1)) { // switch on lhs
                .number => |l| switch (vm.stackPeek(0)) { // switch on rhs
                    .number => |r| {
                        _ = vm.stackPop(); // Pop rhs
                        _ = vm.stackPop(); // Pop lhs
                        break :blk Value{ .bool = l < r };
                    },
                    else => try invalidOpError(op, vm),
                },
                else => {
                    try evalExpr(rhs, vm);
                    try invalidOpError(op, vm);
                },
            }
        },
        .less_than_equal => blk: {
            try evalExpr(rhs, vm);
            switch (vm.stackPeek(1)) { // switch on lhs
                .number => |l| switch (vm.stackPeek(0)) { // switch on rhs
                    .number => |r| {
                        _ = vm.stackPop(); // Pop rhs
                        _ = vm.stackPop(); // Pop lhs
                        break :blk Value{ .bool = l <= r };
                    },
                    else => try invalidOpError(op, vm),
                },
                else => {
                    try evalExpr(rhs, vm);
                    try invalidOpError(op, vm);
                },
            }
        },
        .modulo => blk: {
            try evalExpr(rhs, vm);
            switch (vm.stackPeek(1)) { // switch on lhs
                .number => |l| switch (vm.stackPeek(0)) { // switch on rhs
                    .number => |r| if (r > 0) {
                        _ = vm.stackPop(); // Pop rhs
                        _ = vm.stackPop(); // Pop lhs
                        break :blk Value{ .number = @rem(l, r) };
                    } else {
                        try vm.setError(.{ .modulo_error = .{ .rhs = r } });
                    },
                    else => try invalidOpError(op, vm),
                },
                else => {
                    try evalExpr(rhs, vm);
                    try invalidOpError(op, vm);
                },
            }
        },
        .multiply => blk: {
            try evalExpr(rhs, vm);
            switch (vm.stackPeek(1)) { // switch on lhs
                .number => |l| switch (vm.stackPeek(0)) { // switch on rhs
                    .number => |r| {
                        _ = vm.stackPop(); // Pop rhs
                        _ = vm.stackPop(); // Pop lhs
                        break :blk Value{ .number = l * r };
                    },
                    else => try invalidOpError(op, vm),
                },
                else => {
                    try evalExpr(rhs, vm);
                    try invalidOpError(op, vm);
                },
            }
        },
        .equal => @panic("TODO"),
        .not_equal => @panic("TODO"),
    });
}
