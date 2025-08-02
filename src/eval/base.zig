const ast = @import("../syntax/ast.zig");
const SyntaxNode = @import("../syntax/node.zig");
const Value = @import("value.zig").Value;
const Vm = @import("Vm.zig");
const StackRef = Vm.StackRef;
const RuntimeErrorPayload = @import("error.zig").RuntimeErrorPayload;
const RuntimeError = @import("error.zig").RuntimeError;

const std = @import("std");

pub fn evalTextNode(node: ast.TextNode, vm: *Vm) !void {
    vm.pushScope();
    defer vm.popScope();

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

    var i: usize = 0;
    var output_len = vm.output.items.len;
    while (statements.next()) |stmt| {
        if (output_len != vm.output.items.len) {
            try vm.outputPrint(" ", .{});
        }
        output_len = vm.output.items.len;

        switch (stmt) {
            .expr => |v| {
                try evalExpr(v, vm);
                const value = vm.stackPop();

                // nil shouldn't be printed under normal circumstances
                if (value != .nil) {
                    try vm.output.writer(vm.allocator).print("{any}", .{value});
                }
            },
            .for_loop => @panic("TODO"),
            .let_expr => |v| {
                vm.pushVar(v.binding(vm.nodes).get());
                try evalExpr(v.value(vm.nodes), vm);
            },
            .while_loop => @panic("TODO"),
            .return_expr => @panic("TODO"),
            .conditional => @panic("TODO"),
            .export_expr => @panic("TODO"),
            .function_def => @panic("TODO"),
        }
        i += 1;
    }
}

pub fn evalExpr(node: ast.Expr, vm: *Vm) !void {
    switch (node) {
        .nil => |_| try vm.stackPush(.nil),
        .bool => |v| try vm.stackPush(.{ .bool = v.get() }),
        .ident => |v| try vm.stackPush(try vm.getVar(v.get())),
        .number => |v| try vm.stackPush(.{ .number = v.get() }),
        .string => |s| try vm.stackPush(try vm.allocateString("{s}", .{s.get()})),

        .unary_op => |v| try evalUnary(v, vm),
        .grouping => |v| try evalExpr(v.get(vm.nodes), vm),
        .binary_op => |v| try evalBinary(v, vm),
        .access => @panic("TODO"),
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

    switch (op) {
        .assign => {
            const var_name = switch (lhs) {
                .ident => |v| v.get(),
                else => {
                    // Push lhs and rhs so that invalidOpError can pop the right values off
                    try evalExpr(lhs, vm);
                    try evalExpr(rhs, vm);
                    try invalidOpError(.assign, vm);
                },
            };

            try evalExpr(rhs, vm);

            try vm.setVar(var_name, vm.stackPop());
            try vm.stackPush(.nil); // assignment operator returns nil
        },
        .@"and" => {
            try evalExpr(lhs, vm);
            if (!vm.stackPeek(0).isTruthy()) {
                // lhs is already on top of the stack, so nothing needs to be done.
            } else {
                // Pop off lhs
                _ = vm.stackPop();

                // Push rhs onto the stack.
                try evalExpr(rhs, vm);
            }
        },
        .@"or" => {
            try evalExpr(lhs, vm);
            if (vm.stackPeek(0).isTruthy()) {
                // lhs is already on top of the stack, so nothing needs to be done.
            } else {
                // Pop off lhs
                _ = vm.stackPop();

                // Push rhs onto the stack
                try evalExpr(rhs, vm);
            }
        },
        .add => {
            try evalExpr(lhs, vm);
            try evalExpr(rhs, vm);
            switch (vm.stackPeek(1)) { // switch on lhs
                .number => |l| switch (vm.stackPeek(0)) { // switch on rhs
                    .number => |r| {
                        const res = Value{ .number = l + r };
                        _ = vm.stackPop(); // Pop rhs
                        _ = vm.stackPop(); // Pop lhs
                        try vm.stackPush(res);
                    },
                    else => try invalidOpError(op, vm),
                },
                .object => |o| switch (o.tag) {
                    .string => {
                        const res = try vm.allocateString("{any}{any}", .{
                            StackRef.init(1, vm),
                            StackRef.init(0, vm),
                        });

                        _ = vm.stackPop(); // Pop rhs
                        _ = vm.stackPop(); // Pop lhs
                        try vm.stackPush(res);
                    },
                    else => @panic("TODO"),
                },
                else => try invalidOpError(op, vm),
            }
        },
        .divide => {
            try evalExpr(lhs, vm);
            try evalExpr(rhs, vm);
            switch (vm.stackPeek(1)) { // switch on lhs
                .number => |l| switch (vm.stackPeek(0)) { // switch on rhs
                    .number => |r| {
                        const res = Value{ .number = l / r };
                        _ = vm.stackPop(); // Pop rhs
                        _ = vm.stackPop(); // Pop lhs
                        try vm.stackPush(res);
                    },
                    else => try invalidOpError(op, vm),
                },
                else => try invalidOpError(op, vm),
            }
        },
        .subtract => {
            try evalExpr(lhs, vm);
            try evalExpr(rhs, vm);
            switch (vm.stackPeek(1)) { // switch on lhs
                .number => |l| switch (vm.stackPeek(0)) { // switch on rhs
                    .number => |r| {
                        const res = Value{ .number = l - r };
                        _ = vm.stackPop(); // Pop rhs
                        _ = vm.stackPop(); // Pop lhs
                        try vm.stackPush(res);
                    },
                    else => try invalidOpError(op, vm),
                },
                else => try invalidOpError(op, vm),
            }
        },
        .greater_than => {
            try evalExpr(lhs, vm);
            try evalExpr(rhs, vm);
            switch (vm.stackPeek(1)) { // switch on lhs
                .number => |l| switch (vm.stackPeek(0)) { // switch on rhs
                    .number => |r| {
                        const res = Value{ .bool = l > r };
                        _ = vm.stackPop(); // Pop rhs
                        _ = vm.stackPop(); // Pop lhs
                        try vm.stackPush(res);
                    },
                    else => try invalidOpError(op, vm),
                },
                else => try invalidOpError(op, vm),
            }
        },
        .greater_than_equal => {
            try evalExpr(lhs, vm);
            try evalExpr(rhs, vm);
            switch (vm.stackPeek(1)) { // switch on lhs
                .number => |l| switch (vm.stackPeek(0)) { // switch on rhs
                    .number => |r| {
                        const res = Value{ .bool = l >= r };
                        _ = vm.stackPop(); // Pop rhs
                        _ = vm.stackPop(); // Pop lhs
                        try vm.stackPush(res);
                    },
                    else => try invalidOpError(op, vm),
                },
                else => try invalidOpError(op, vm),
            }
        },
        .less_than => {
            try evalExpr(lhs, vm);
            try evalExpr(rhs, vm);
            switch (vm.stackPeek(1)) { // switch on lhs
                .number => |l| switch (vm.stackPeek(0)) { // switch on rhs
                    .number => |r| {
                        const res = Value{ .bool = l < r };
                        _ = vm.stackPop(); // Pop rhs
                        _ = vm.stackPop(); // Pop lhs
                        try vm.stackPush(res);
                    },
                    else => try invalidOpError(op, vm),
                },
                else => try invalidOpError(op, vm),
            }
        },
        .less_than_equal => {
            try evalExpr(lhs, vm);
            try evalExpr(rhs, vm);
            switch (vm.stackPeek(1)) { // switch on lhs
                .number => |l| switch (vm.stackPeek(0)) { // switch on rhs
                    .number => |r| {
                        const res = Value{ .bool = l <= r };
                        _ = vm.stackPop(); // Pop rhs
                        _ = vm.stackPop(); // Pop lhs
                        try vm.stackPush(res);
                    },
                    else => try invalidOpError(op, vm),
                },
                else => try invalidOpError(op, vm),
            }
        },
        .modulo => {
            try evalExpr(lhs, vm);
            try evalExpr(rhs, vm);
            switch (vm.stackPeek(1)) { // switch on lhs
                .number => |l| switch (vm.stackPeek(0)) { // switch on rhs
                    .number => |r| if (r > 0) {
                        const res = Value{ .number = @rem(l, r) };
                        _ = vm.stackPop(); // Pop rhs
                        _ = vm.stackPop(); // Pop lhs
                        try vm.stackPush(res);
                    } else {
                        try vm.setError(.{ .modulo_error = .{ .rhs = r } });
                    },
                    else => try invalidOpError(op, vm),
                },
                else => try invalidOpError(op, vm),
            }
        },
        .multiply => {
            try evalExpr(lhs, vm);
            try evalExpr(rhs, vm);
            switch (vm.stackPeek(1)) { // switch on lhs
                .number => |l| switch (vm.stackPeek(0)) { // switch on rhs
                    .number => |r| {
                        const res = Value{ .number = l * r };
                        _ = vm.stackPop(); // Pop rhs
                        _ = vm.stackPop(); // Pop lhs
                        try vm.stackPush(res);
                    },
                    else => try invalidOpError(op, vm),
                },
                else => try invalidOpError(op, vm),
            }
        },
        .equal => @panic("TODO"),
        .not_equal => @panic("TODO"),
    }
}
