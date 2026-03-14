pub fn evalText(vm: *Vm, node: ast.Text) ControlFlow!void {
    var parts = node.parts(vm.all_nodes);
    vm.pushScope();
    while (parts.next(vm.all_nodes)) |part| {
        switch (part) {
            .newline => |nl| _ = vm.output.write("\n") catch
                try vm.setError(nl.node_index, .write_failure),
            .text_line => |line| _ = vm.output.write(line.get(vm.src, vm.all_nodes)) catch
                try vm.setError(line.node_index, .write_failure),
            .code => |code| try evalCode(vm, code),
        }
    }
    vm.popScope();
}

pub fn evalCode(vm: *Vm, node: ast.Code) ControlFlow!void {
    var statements = node.statements(vm.all_nodes);
    while (statements.next(vm.all_nodes)) |statement| {
        switch (statement) {
            .for_loop => {},
            .export_statement => {},
            .break_statement => return ControlFlow.Break,
            .continue_statement => return ControlFlow.Continue,
            .let_statement => |stmt| try evalLetStatement(vm, stmt),
            .return_statement => |ret| try evalReturnStatement(vm, ret),
            .while_loop => |while_loop| try evalWhileLoop(vm, while_loop),
            .conditional => |conditional| try evalConditional(vm, conditional),
            .expression => |expr| switch (expr) {
                // Inline else so that the node's node_index may be used
                inline else => |v| {
                    const res = try evalExpression(vm, expr);
                    if (res.getObject()) |obj| if (obj.getString()) |str| {
                        vm.output.print("{s}", .{vm.intern_pool.getString(str.slice)}) catch
                            try vm.setError(v.node_index, .write_failure);
                        return;
                    };

                    if (!res.isNil()) {
                        try vm.setError(v.node_index, .{ .cannot_print_value = .{ .value = res } });
                    }
                },
            },
        }
    }
}

pub fn evalReturnStatement(vm: *Vm, node: ast.ReturnStatement) ControlFlow!void {
    if (node.returnValue(vm.all_nodes)) |ret|
        vm.function_return_value = try evalExpression(vm, ret);
    return ControlFlow.Return;
}

pub fn evalLetStatement(vm: *Vm, node: ast.LetStatement) RuntimeError!void {
    const initial_value = if (node.initialValue(vm.all_nodes)) |expr|
        try evalExpression(vm, expr)
    else
        Value.nil;

    const var_name = node.variableName(vm.all_nodes).get(vm.all_nodes, vm.src);
    vm.bindVariable(var_name, initial_value) catch
        try vm.setError(node.node_index, .too_many_variables);
}

pub fn evalWhileLoop(vm: *Vm, node: ast.WhileLoop) ControlFlow!void {
    const body = node.body(vm.all_nodes);
    const condition = node.condition(vm.all_nodes);

    while ((try evalExpression(vm, condition)).isTruthy()) {
        evalText(vm, body) catch |err| switch (err) {
            ControlFlow.Break => break,
            ControlFlow.Continue => continue,
            else => return err,
        };
    }
}

pub fn evalConditional(vm: *Vm, node: ast.Conditional) ControlFlow!void {
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
        .list => @panic("TODO"),
        .dict => @panic("TODO"),
        .dot_access => @panic("TODO"),
        .bracket_access => @panic("TODO"),
        .nil => Value.nil,
        .true => Value.boolean(true),
        .false => Value.boolean(false),
        .unary => |unary| try evalUnary(vm, unary),
        .binary => |binary| try evalBinary(vm, binary),
        .function_def => |def| evalFunctionDef(vm, def),
        .function_call => |call| evalFunctionCall(vm, call),
        .ident => |variable| try evalVariable(vm, variable),
        .number => |num| Value.number(num.get(vm.all_nodes, vm.src)),
        .grouping => |group| try evalExpression(vm, group.inner(vm.all_nodes)),
        .integer => |num| Value.number(@floatFromInt(num.get(vm.all_nodes, vm.src) catch {
            try vm.setError(num.node_index, .number_too_large);
        })),
        .static_string => |str| try evalStaticString(vm, str),
    };
}

pub fn evalStaticString(vm: *Vm, node: ast.StaticString) RuntimeError!Value {
    const m, const writer = vm.intern_pool.createString();
    node.create(vm.all_nodes, vm.src, writer) catch
        try vm.setError(node.node_index, .write_failure);
    const slice = vm.intern_pool.internString(m, vm.gpa) catch
        try vm.setError(node.node_index, .internal_oom);

    return Value.object(Object.String.init(vm.valueAllocator(), slice) catch
        try vm.setError(node.node_index, .value_oom));
}

pub fn evalVariable(vm: *Vm, node: ast.Ident) RuntimeError!Value {
    const ident = node.get(vm.all_nodes, vm.src);
    return vm.getVariable(ident) catch {
        // TODO add builtin function & variables here like sys, range(), etc.
        try vm.setError(node.node_index, .undeclared_variable);
    };
}

pub fn evalFunctionDef(vm: *Vm, node: ast.FunctionDef) RuntimeError!Value {
    var parameters = node.parameters(vm.all_nodes).get(vm.all_nodes);
    const len = parameters.len(vm.all_nodes);
    return Value.object(Object.Function.initRuntime(
        vm.valueAllocator(),
        node.node_index,
        len,
    ) catch try vm.setError(node.node_index, .value_oom));
}

pub fn evalFunctionCall(vm: *Vm, node: ast.FunctionCall) RuntimeError!Value {
    const function = blk: {
        const caller = try evalExpression(vm, node.caller(vm.all_nodes));
        if (caller.getObject()) |obj| {
            if (obj.getFunction()) |func| break :blk func;
        }
        try vm.setError(node.node_index, .{ .cannot_call_value = .{ .value = caller } });
    };

    return switch (function.func) {
        .runtime => try callRuntimeFunction(vm, function, node),
        .builtin => |_| @panic("TODO"),
    };
}

pub fn callRuntimeFunction(
    vm: *Vm,
    function: *Object.Function,
    callsite: ast.FunctionCall,
) RuntimeError!Value {
    const func_node = function.func.runtime;

    const old_state = vm.setupFunctionCall();
    defer vm.endFunctioncall(old_state);

    const func = ast.toASTNode(ast.FunctionDef, func_node.definition_index, vm.all_nodes).?;

    const arguments = callsite.arguments(vm.all_nodes);

    var arg_iter = arguments.get(vm.all_nodes);
    var param_iter = func.parameters(vm.all_nodes).get(vm.all_nodes);
    var i: u32 = 0;
    while (true) {
        defer i += 1;

        const arg = arg_iter.next(vm.all_nodes);
        const param = param_iter.next(vm.all_nodes);

        if (arg == null and param == null) break;
        if (arg == null) try vm.setError(arguments.node_index, .{
            .invalid_function_args = .{
                // .len() will get the number of remaining parameters.
                .expected_num = i + param_iter.len(vm.all_nodes),
                .actual_num = i,
            },
        });
        if (param == null) try vm.setError(arguments.node_index, .{
            .invalid_function_args = .{
                .expected_num = i,
                // .len() will get the number of remaining arguments.
                .actual_num = i + arg_iter.len(vm.all_nodes),
            },
        });

        vm.bindVariable(param.?.get(vm.all_nodes, vm.src), try evalExpression(vm, arg.?)) catch
            try vm.setError(arguments.node_index, .too_many_variables);
    }

    if (func.name(vm.all_nodes)) |name| {
        vm.bindVariable(name.get(vm.all_nodes, vm.src), Value.object(&function.base)) catch
            try vm.setError(name.node_index, .too_many_variables);
    }

    const body = ast.toASTNode(ast.Text, func_node.definition_index, vm.all_nodes).?;
    evalText(vm, body) catch |err| switch (err) {
        ControlFlow.Error => return ControlFlow.Error,
        ControlFlow.Continue => @panic("TODO"),
        ControlFlow.Break => @panic("TODO"),
        ControlFlow.Return => {},
    };

    const return_value = vm.function_return_value orelse Value.nil;
    vm.function_return_value = null;

    return return_value;
}

pub fn evalUnary(vm: *Vm, node: ast.Unary) RuntimeError!Value {
    const op = node.op(vm.all_nodes);
    const rhs = try evalExpression(vm, node.rhs(vm.all_nodes));
    return switch (op) {
        .not => Value.Operators.not(rhs),
        .negate => Value.Operators.negate(rhs),
    } catch try vm.setError(
        node.node_index,
        .{ .invalid_unary_operands = .{ .rhs = rhs } },
    );
}

pub fn evalBinary(vm: *Vm, node: ast.Binary) RuntimeError!Value {
    const op = node.op(vm.all_nodes);

    if (op == .assign) {
        const lhs = node.lhs(vm.all_nodes);
        if (lhs == .ident) {
            const value = try evalExpression(vm, node.rhs(vm.all_nodes));
            vm.setVariable(lhs.ident.get(vm.all_nodes, vm.src), value) catch
                try vm.setError(lhs.ident.node_index, .undeclared_variable);
            return Value.nil;
        } else try vm.setError(node.node_index, .cannot_assign_to_non_variable);
    }

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
        .assign => unreachable,
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
const Object = @import("value.zig").Object;
const Vm = @import("Vm.zig");
const RuntimeError = Vm.RuntimeError;
const ControlFlow = Vm.ControlFlow;
