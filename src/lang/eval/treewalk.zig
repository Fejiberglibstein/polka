pub fn evalText(vm: *Vm, node: ast.Text) RuntimeError!void {
    var parts = node.parts(vm.all_nodes);
    while (parts.next(vm.all_nodes)) |part| {
        switch (part) {
            .newline => |nl| _ = vm.output.write("\n") catch
                try vm.setError(nl.node_index, .write_failure),
            .text_line => |line| _ = vm.output.write(line.get(vm.src, vm.all_nodes)) catch
                try vm.setError(line.node_index, .write_failure),
            .code => |code| try evalCode(vm, code),
        }
    }
}

pub fn evalCode(vm: *Vm, node: ast.Code) !void {
    var statements = node.statements(vm.all_nodes);
    while (statements.next(vm.all_nodes)) |statement| {
        switch (statement) {
            .for_loop => {},
            .while_loop => {},
            .expression => |expr| try evalExpression(vm, expr),
            .let_statement => {},
            .break_statement => {},
            .return_statement => {},
            .export_statement => {},
            .continue_statement => {},
            .conditional => |conditional| try evalConditional(vm, conditional),
        }
    }
}

pub fn evalConditional(vm: *Vm, node: ast.Conditional) RuntimeError!void {
    var branches = node.branches(vm.all_nodes);

    while (branches.next(vm.all_nodes)) |branch| {
        if (branch.condition) |condition| {
            try evalExpression(vm, condition);
            if (!vm.stackPop().isTruthy()) {
                continue;
            }
        }
        try evalText(vm, branch.branch);
        break;
    }
}

pub fn evalExpression(vm: *Vm, node: ast.Expression) RuntimeError!void {
    switch (node) {
        .nil => |n| try vm.stackPush(n.node_index, Value.nil),
        .list => {},
        .dict => {},
        .true => |n| try vm.stackPush(n.node_index, Value.boolean(true)),
        .unary => {},
        .ident => {},
        .false => |n| try vm.stackPush(n.node_index, Value.boolean(false)),
        .binary => |binary| try evalBinary(vm, binary),
        .number => |num| {
            const n = num.get(vm.all_nodes, vm.src);
            try vm.stackPush(num.node_index, Value.number(n));
        },
        .string => {},
        .integer => |num| {
            const n = num.get(vm.all_nodes, vm.src) catch {
                try vm.setError(num.node_index, .number_too_large);
            };
            try vm.stackPush(num.node_index, Value.number(@floatFromInt(n)));
        },
        .grouping => |group| try evalExpression(vm, group.inner(vm.all_nodes)),
        .function_def => {},
        .function_call => {},
    }
}

pub fn evalBinary(vm: *Vm, node: ast.Binary) !void {
    try evalExpression(vm, node.lhs(vm.all_nodes));
    try evalExpression(vm, node.rhs(vm.all_nodes));

    const lhs = vm.stackPeek(1);
    const rhs = vm.stackPeek(0);

    const result = switch (node.op(vm.all_nodes)) {
        .@"or" => @panic("TODO"),
        .@"and" => @panic("TODO"),
        .assign => @panic("TODO"),
        .in => Value.Operators.in(lhs, rhs),
        .add => Value.Operators.add(lhs, rhs),
        .equal => Value.Operators.equal(lhs, rhs),
        .divide => Value.Operators.divide(lhs, rhs),
        .modulo => Value.Operators.modulo(lhs, rhs),
        .multiply => Value.Operators.multiply(lhs, rhs),
        .subtract => Value.Operators.subtract(lhs, rhs),
        .less_than => Value.Operators.less_than(lhs, rhs),
        .not_equal => Value.Operators.not_equal(lhs, rhs),
        .greater_than => Value.Operators.greater_than(lhs, rhs),
        .less_than_equal => Value.Operators.less_than_equal(lhs, rhs),
        .greater_than_equal => Value.Operators.greater_than_equal(lhs, rhs),
    } catch try vm.setError(
        node.node_index,
        .{ .invalid_binary_operands = .{ .lhs = lhs, .rhs = rhs } },
    );

    // Discard lhs and rhs
    _ = vm.stackPop();
    _ = vm.stackPop();

    try vm.stackPush(node.node_index, result);
}

const std = @import("std");

const ast = @import("../syntax/ast.zig");
const Value = @import("value.zig").Value;
const Vm = @import("Vm.zig");
const RuntimeError = Vm.RuntimeError;
