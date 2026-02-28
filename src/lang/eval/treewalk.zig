pub fn evalText(vm: *Vm, node: ast.Text) !void {
    var parts = node.parts(vm.all_nodes);
    while (parts.next(vm.all_nodes)) |part| {
        switch (part) {
            .newline => _ = try vm.output.write("\n"),
            .text_line => |line| _ = try vm.output.write(line.get(vm.src, vm.all_nodes)),
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
        }
    }
}

pub fn evalExpression(vm: *Vm, node: ast.Expression) !void {
    switch (node) {
        .nil => |n| try vm.stackPush(n.node_index, Value.nil),
        .list => {},
        .dict => {},
        .true => |n| try vm.stackPush(n.node_index, Value.boolean(true)),
        .unary => {},
        .ident => {},
        .false => |n| try vm.stackPush(n.node_index, Value.boolean(false)),
        .binary => {},
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
        .grouping => {},
        .conditional => |conditional| try evalConditional(vm, conditional),
        .function_def => {},
        .function_call => {},
    }
}

pub fn evalConditional(vm: *Vm, node: ast.Conditional) RuntimeError!void {
    var branches = node.branches(vm.all_nodes);

    while (branches.next(vm.all_nodes)) |branch| {
        if (branch.condition) |condition| {
            try evalExpression(vm, condition);
            if (vm.stackPop().isTruthy()) {}
        }
    }
}

const std = @import("std");

const ast = @import("../syntax/ast.zig");
const Value = @import("value.zig").Value;
const Vm = @import("Vm.zig");
const RuntimeError = Vm.RuntimeError;
