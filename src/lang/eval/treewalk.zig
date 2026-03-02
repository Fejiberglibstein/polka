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
            .expression => |expr| switch (expr) {
                inline else => |v| {
                    const res = try evalExpression(vm, expr);

                    if (res.getObject()) |obj| {
                        if (obj.getString()) |str| {
                            vm.output.print("{s}", .{str.slice()}) catch
                                try vm.setError(v.node_index, .write_failure);
                        }
                    }

                    try vm.setError(v.node_index, .{ .cannot_print_value = .{ .value = res } });
                },
            },
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
            const cond = try evalExpression(vm, condition);
            if (!cond.isTruthy()) {
                continue;
            }
        }
        try evalText(vm, branch.branch);
        break;
    }
}

pub fn evalExpression(vm: *Vm, node: ast.Expression) RuntimeError!Value {
    return switch (node) {
        .nil => Value.nil,
        .list => @panic("TODO"),
        .dict => @panic("TODO"),
        .true => Value.boolean(true),
        .unary => @panic("TODO"),
        .ident => @panic("TODO"),
        .false => Value.boolean(false),
        .binary => |binary| try evalBinary(vm, binary),
        .string => @panic("TODO"),
        .number => |num| Value.number(num.get(vm.all_nodes, vm.src)),
        .grouping => |group| try evalExpression(vm, group.inner(vm.all_nodes)),
        .integer => |num| Value.number(@floatFromInt(num.get(vm.all_nodes, vm.src) catch {
            try vm.setError(num.node_index, .number_too_large);
        })),
        .function_def => @panic("TODO"),
        .function_call => @panic("TODO"),
    };
}

pub fn evalBinary(vm: *Vm, node: ast.Binary) !Value {
    const op = node.op(vm.all_nodes);

    const lhs = try evalExpression(vm, node.lhs(vm.all_nodes));

    if (op == .@"and" or op == .@"or") {
        if (lhs.isTruthy() == (op == .@"or")) {
            return lhs;
        } else {
            return try evalExpression(vm, node.rhs(vm.all_nodes));
        }
    }

    const rhs = try evalExpression(vm, node.rhs(vm.all_nodes));

    return switch (node.op(vm.all_nodes)) {
        .@"or" => unreachable,
        .@"and" => unreachable,
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
}

const std = @import("std");

const ast = @import("../syntax/ast.zig");
const Value = @import("value.zig").Value;
const Vm = @import("Vm.zig");
const RuntimeError = Vm.RuntimeError;
