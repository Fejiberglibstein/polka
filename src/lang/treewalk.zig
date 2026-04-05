pub fn evalText(vm: *Vm, node: ast.Text) ControlFlow!void {
    var parts = node.parts(vm.all_nodes);
    const old_scope = vm.pushScope();
    defer vm.popScope(old_scope);

    const out = vm.out();
    while (parts.next(vm.all_nodes)) |part| {
        switch (part) {
            .newline => |nl| _ = out.write("\n") catch
                try vm.setError(nl.node_index, .write_failure),
            .text_line => |line| _ = out.write(line.get(vm.src, vm.all_nodes)) catch
                try vm.setError(line.node_index, .write_failure),
            .code => |code| {
                try evalCode(vm, code);
            },
        }
    }
}

pub fn evalCode(vm: *Vm, node: ast.Code) ControlFlow!void {
    var statements = node.statements(vm.all_nodes);
    const out = vm.out();
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
                    if (res.getString()) |str| {
                        out.print("{s}", .{vm.string_builder.pool.get(str)}) catch
                            try vm.setError(v.node_index, .write_failure);
                        continue;
                    }

                    if (!res.isNil()) {
                        try vm.setError(v.node_index, .{ .cannot_print_value = res });
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
            if (!cond.isTruthy()) continue;
        }
        try evalText(vm, branch.body);
        break;
    }
}

pub fn evalExpression(vm: *Vm, node: ast.Expression) RuntimeError!Value {
    return switch (node) {
        .dot_access => @panic("TODO"),
        .bracket_access => @panic("TODO"),
        .nil => Value.nil,
        .true => Value.boolean(true),
        .false => Value.boolean(false),
        .list => |list| try evalList(vm, list),
        .dict => |dict| try evalDict(vm, dict),
        .unary => |unary| try evalUnary(vm, unary),
        .binary => |binary| try evalBinary(vm, binary),
        .function_def => |def| evalFunctionDef(vm, def),
        .function_call => |call| evalFunctionCall(vm, call),
        .ident => |variable| try evalVariable(vm, variable),
        .static_string => |str| try evalStaticString(vm, str),
        .multi_line_string => |str| try evalMultiLineString(vm, str),
        .number => |num| Value.number(num.get(vm.all_nodes, vm.src)),
        .integer => |num| Value.number(num.getAsFloat(vm.all_nodes, vm.src)),
        .grouping => |group| try evalExpression(vm, group.inner(vm.all_nodes)),
    };
}

pub fn evalStaticString(vm: *Vm, node: ast.StaticString) RuntimeError!Value {
    var sb = &vm.string_builder;
    const m = sb.begin();
    node.create(vm.all_nodes, vm.src, &sb.w.writer) catch
        try vm.setError(node.node_index, .internal_oom);

    return Value.string(sb.finish(m) catch
        try vm.setError(node.node_index, .internal_oom));
}

pub fn evalMultiLineString(vm: *Vm, node: ast.MultiLineString) RuntimeError!Value {
    var sb = &vm.string_builder;
    const m = sb.begin();

    var parts = node.parts(vm.all_nodes);
    while (parts.next(vm.all_nodes)) |part| {
        const node_index = switch (part) {
            inline else => |v| v.node_index,
        };

        (switch (part) {
            .newline => |_| sb.w.writer.print("\n", .{}),
            .text => |text| sb.w.writer.print("{s}", .{text.get(vm.all_nodes, vm.src)}),
            .expression => |expr| blk: {
                const v = try evalExpression(vm, expr.get(vm.all_nodes));
                v.print(vm.string_builder.pool, &sb.w.writer) catch |err| switch (err) {
                    error.ValueError => try vm.setError(node_index, .{ .cannot_print_value = v }),
                    error.WriteFailed => break :blk error.WriteFailed,
                };
            },
        }) catch try vm.setError(node_index, .internal_oom);
    }
    sb.w.writer.print("\n", .{}) catch try vm.setError(node.node_index, .internal_oom);

    return Value.string(sb.finish(m) catch
        try vm.setError(node.node_index, .internal_oom));
}

pub fn evalDict(vm: *Vm, node: ast.Dict) RuntimeError!Value {
    const object = Object.Dict.init(vm.valueAllocator()) catch
        try vm.setError(node.node_index, .value_oom);
    const dict = object.asDict();

    var sb = &vm.string_builder;

    var fields = node.fields(vm.all_nodes);
    while (fields.next(vm.all_nodes)) |field| {
        const key = key: {
            const m = sb.begin();
            const key_node = field.key(vm.all_nodes);
            sb.w.writer.print("{s}", .{key_node.get(vm.all_nodes, vm.src)}) catch
                try vm.setError(node.node_index, .internal_oom);
            break :key sb.finish(m) catch
                try vm.setError(key_node.node_index, .internal_oom);
        };

        const value = try evalExpression(vm, field.value(vm.all_nodes));

        dict.items.putContext(vm.valueAllocator(), key, value, .{ .pool = sb.pool }) catch
            try vm.setError(field.node_index, .value_oom);
    }

    return Value.object(object);
}

pub fn evalList(vm: *Vm, node: ast.List) RuntimeError!Value {
    const object = Object.List.init(vm.valueAllocator()) catch
        try vm.setError(node.node_index, .value_oom);
    const list = object.asList();

    var items = node.items(vm.all_nodes);
    while (items.next(vm.all_nodes)) |item| {
        list.items.append(vm.valueAllocator(), try evalExpression(vm, item)) catch
            try vm.setError(node.node_index, .value_oom);
    }

    return Value.object(object);
}

pub fn evalVariable(vm: *Vm, node: ast.Ident) RuntimeError!Value {
    const ident = node.get(vm.all_nodes, vm.src);
    return vm.getVariable(ident, vm.scope) catch {
        // TODO add builtin function & variables here like sys, range(), etc.
        try vm.setError(node.node_index, .undeclared_variable);
    };
}

pub fn evalFunctionDef(vm: *Vm, node: ast.FunctionDef) RuntimeError!Value {
    var parameters = node.parameters(vm.all_nodes).get(vm.all_nodes);
    const len = parameters.len(vm.all_nodes);

    const function = Value.object(Object.Function.initRuntime(
        vm.valueAllocator(),
        node.node_index,
        len,
    ) catch try vm.setError(node.node_index, .value_oom));

    if (node.name(vm.all_nodes)) |fn_name| {
        vm.bindVariable(fn_name.get(vm.all_nodes, vm.src), function) catch
            try vm.setError(node.node_index, .too_many_variables);
        return Value.nil;
    }

    return function;
}

pub fn evalFunctionCall(vm: *Vm, node: ast.FunctionCall) RuntimeError!Value {
    const function = blk: {
        const caller = try evalExpression(vm, node.caller(vm.all_nodes));
        if (caller.getObject()) |obj| {
            if (obj.getFunction()) |func| break :blk func;
        }
        try vm.setError(node.node_index, .{ .cannot_call_value = caller });
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

    const func = ast.toASTNode(ast.FunctionDef, func_node.definition_index, vm.all_nodes).?;

    const arguments = callsite.arguments(vm.all_nodes);

    var arg_iter = arguments.get(vm.all_nodes);
    var param_iter = func.parameters(vm.all_nodes).get(vm.all_nodes);
    var i: u32 = 0;

    const caller_scope = vm.pushFunctionScope();
    defer vm.popScope(caller_scope);

    while (true) {
        defer i += 1;

        const arg = arg_iter.next(vm.all_nodes);
        const param = param_iter.next(vm.all_nodes);
        if (arg == null and param == null)
            break;

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

        // The value needs to be evaluated in the old scope so that variables bound outside the
        // function call can be passed as arguments to the function.
        const function_scope = vm.scope;
        vm.scope = caller_scope;
        const value = try evalExpression(vm, arg.?);
        vm.scope = function_scope;

        vm.bindVariable(param.?.get(vm.all_nodes, vm.src), value) catch
            try vm.setError(arguments.node_index, .too_many_variables);
    }

    if (func.name(vm.all_nodes)) |name| {
        vm.bindVariable(name.get(vm.all_nodes, vm.src), Value.object(&function.base)) catch
            try vm.setError(name.node_index, .too_many_variables);
    }

    // Call the function
    const return_value = switch (func.body(vm.all_nodes)) {
        .text => |body| return_value: {
            const m = vm.string_builder.begin();

            evalText(vm, body) catch |err| switch (err) {
                ControlFlow.Error => return ControlFlow.Error,
                ControlFlow.Continue => @panic("TODO"),
                ControlFlow.Break => @panic("TODO"),
                ControlFlow.Return => {},
            };

            const function_text = if (m != vm.string_builder.begin())
                Value.string(vm.string_builder.finish(m) catch
                    try vm.setError(func.node_index, .internal_oom))
            else
                null;

            const return_value = vm.function_return_value;
            vm.function_return_value = null;

            if (return_value) |_| if (function_text) |_| {
                try vm.setError(func.node_index, .function_return_and_text);
            };

            break :return_value return_value orelse function_text orelse Value.nil;
        },
        .expression => |expr| try evalExpression(vm, expr),
    };

    return return_value;
}

pub fn evalUnary(vm: *Vm, node: ast.Unary) RuntimeError!Value {
    const op = node.op(vm.all_nodes);
    const rhs = try evalExpression(vm, node.rhs(vm.all_nodes));
    return switch (op) {
        .not => Value.operators.not(rhs),
        .negate => Value.operators.negate(rhs),
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
        .in => Value.operators.in(lhs, rhs),
        .add => Value.operators.add(vm, lhs, rhs),
        .equal => Value.operators.equal(lhs, rhs),
        .divide => Value.operators.divide(lhs, rhs),
        .modulo => Value.operators.modulo(lhs, rhs),
        .multiply => Value.operators.multiply(lhs, rhs),
        .subtract => Value.operators.subtract(lhs, rhs),
        .less_than => Value.operators.less_than(lhs, rhs),
        .not_equal => Value.operators.not_equal(lhs, rhs),
        .greater_than => Value.operators.greater_than(lhs, rhs),
        .less_than_equal => Value.operators.less_than_equal(lhs, rhs),
        .greater_than_equal => Value.operators.greater_than_equal(lhs, rhs),
    } catch |err| try vm.setError(node.node_index, switch (err) {
        error.WriteFailed => .internal_oom,
        error.OutOfMemory => .value_oom,
        error.ValueError, error.InvalidOperands => .{
            .invalid_binary_operands = .{ .lhs = lhs, .rhs = rhs },
        },
    });
}

const std = @import("std");
const assert = std.debug.assert;

const ast = @import("ast.zig");
const Value = @import("value.zig").Value;
const Object = @import("value.zig").Object;
const Vm = @import("Vm.zig");
const RuntimeError = Vm.RuntimeError;
const ControlFlow = Vm.ControlFlow;
