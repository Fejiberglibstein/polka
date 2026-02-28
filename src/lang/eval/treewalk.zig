pub fn evalText(vm: *Vm, node: ast.Text) !void {
    var parts = node.parts(vm.all_nodes);
    while (parts.next()) |part| {
        switch (part) {
            .newline => _ = try vm.output.write("\n"),
            .text_line => |line| _ = try vm.output.write(line.get(vm.src)),
            .code => |code| try evalCode(vm, code),
        }
    }
}

pub fn evalCode(vm: *Vm, node: ast.Code) !void {
    var statements = node.statements(vm.all_nodes);
    while (statements.next()) |statement| {
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
        .nil => |n| try vm.stackPush(n.node, Value.nil),
        .list => {},
        .dict => {},
        .true => |n| try vm.stackPush(n.node, Value.boolean(true)),
        .unary => {},
        .ident => {},
        .false => |n| try vm.stackPush(n.node, Value.boolean(false)),
        .binary => {},
        .number => |num| try vm.stackPush(num.node, Value.number(num.get(vm.src))),
        .string => {},
        .integer => |num| {
            const n = num.get(vm.src) catch try vm.setError(num.node, .number_too_large);
            try vm.stackPush(num.node, Value.number(@floatFromInt(n)));
        },
        .grouping => {},
        .conditional => |conditional| try evalConditional(vm, conditional),
        .function_def => {},
        .function_call => {},
    }
}

pub fn evalConditional(vm: *Vm, node: ast.Conditional) RuntimeError!void {
    var branches = node.branches(vm.all_nodes);

    while (branches.next()) |branch| {
        const condition, const text = branch;
        if (condition) |cond| {
            try evalExpression(vm, cond);
            if (vm.stackPop().isTruthy()) {
            }
        }
    }
}

const std = @import("std");

const ast = @import("../syntax/ast.zig");
const Value = @import("value.zig").Value;
const Vm = @import("Vm.zig");
const RuntimeError = Vm.RuntimeError;
