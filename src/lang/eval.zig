pub fn evalText(vm: *Vm, node: ast.Text) ControlFlow!void {
    const old_scope = vm.pushScope();
    defer vm.popScope(old_scope);

    const out = vm.out();
    var parts = node.parts(vm.nodes);
    while (parts.next(vm.nodes)) |part| {
        (switch (part) {
            .newline => out.writeAll("\n"),
            .code => |code| try evalCode(vm, code),
            .text_line => |line| out.writeAll(line.get(vm.src, vm.nodes)),
        }) catch |err| switch (err) {
            error.WriteFailed => try vm.setError(part.index(), .write_failure),
            inline else => |e| return e,
        };
    }
}

pub fn evalCode(vm: *Vm, code: ast.Code) ControlFlow!void {
    var statements = code.statements(vm.nodes);
    while (statements.next(vm.nodes)) |statement| {
        try switch (statement) {
            .for_loop => @panic("TODO"),
            .export_statement => @panic("TODO"),
            .while_loop => |node| evalWhileLoop(vm, node),
            .conditional => |node| evalConditional(vm, node),
            .let_statement => |node| evalLetStatement(vm, node),
            .expression => |node| evalExpressionStatement(vm, node),
            .break_statement => |node| evalBreakStatement(vm, node),
            .return_statement => |node| evalReturnStatement(vm, node),
            .continue_statement => |node| evalContinueStatement(vm, node),
        };
    }
}

pub fn evalContinueStatement(vm: *Vm, node: ast.ContinueStatement) ControlFlow!void {
    vm.control_flow_node = node.index;
    return ControlFlow.Continue;
}

pub fn evalBreakStatement(vm: *Vm, node: ast.BreakStatement) ControlFlow!void {
    vm.control_flow_node = node.index;
    return ControlFlow.Break;
}

pub fn evalReturnStatement(vm: *Vm, node: ast.ReturnStatement) ControlFlow!void {
    if (node.returnValue(vm.nodes)) |ret|
        vm.function_return_value = try evalExpression(vm, ret);
    vm.control_flow_node = node.index;
    return ControlFlow.Return;
}

pub fn evalLetStatement(vm: *Vm, node: ast.LetStatement) RuntimeError!void {
    const initial_value = if (node.initialValue(vm.nodes)) |expr|
        try evalExpression(vm, expr)
    else
        Value.nil;

    const var_name = node.variableName(vm.nodes).get(vm.nodes, vm.src);
    vm.bindVariable(var_name, initial_value) catch
        try vm.setError(node.index, .too_many_variables);
}

pub fn evalWhileLoop(vm: *Vm, node: ast.WhileLoop) ControlFlow!void {
    const body = node.body(vm.nodes);
    const condition = node.condition(vm.nodes);

    while ((try evalExpression(vm, condition)).isTruthy()) {
        evalText(vm, body) catch |err| switch (err) {
            ControlFlow.Break => break,
            ControlFlow.Continue => continue,
            else => return err,
        };
        vm.control_flow_node = null;
    }
}

pub fn evalConditional(vm: *Vm, node: ast.Conditional) ControlFlow!void {
    var branches = node.branches(vm.nodes);

    while (branches.next(vm.nodes)) |branch| {
        if (branch.condition) |condition| {
            const cond = try evalExpression(vm, condition);
            if (!cond.isTruthy()) continue;
        }
        try evalText(vm, branch.body);
        break;
    }
}

pub fn evalExpressionStatement(vm: *Vm, node: ast.Expression) RuntimeError!void {
    const out = vm.out();
    const res = try evalExpression(vm, node);
    switch (res.taggedValue()) {
        .nil => {},
        .string => |str| {
            out.writeAll(vm.string_builder.pool.get(str)) catch
                try vm.setError(node.index(), .write_failure);
        },
        else => try vm.setError(node.index(), .{ .cannot_print_value = res }),
    }
}

pub fn evalExpression(vm: *Vm, expr: ast.Expression) RuntimeError!Value {
    return switch (expr) {
        .nil => Value.nil,
        .true => Value.new(.boolean, true),
        .false => Value.new(.boolean, false),
        .number => |node| Value.new(.number, node.get(vm.nodes, vm.src)),
        .integer => |node| Value.new(.number, node.getAsFloat(vm.nodes, vm.src)),

        .list => |node| evalList(vm, node),
        .dict => |node| evalDict(vm, node),
        .color => |node| evalColor(vm, node),
        .unary => |node| evalUnary(vm, node),
        .binary => |node| evalBinary(vm, node),
        .ident => |node| evalVariable(vm, node),
        .dot_access => |node| evalDotAccess(vm, node),
        .function_def => |node| evalFunctionDef(vm, node),
        .static_string => |node| evalStaticString(vm, node),
        .function_call => |node| evalFunctionCall(vm, node),
        .bracket_access => |node| evalBracketAccess(vm, node),
        .multi_line_string => |node| evalMultiLineString(vm, node),
        .grouping => |node| evalExpression(vm, node.inner(vm.nodes)),
    };
}

pub fn evalColor(vm: *Vm, node: ast.Color) RuntimeError!Value {
    const color = node.get(vm.nodes, vm.src);
    return Value.new(.color, .{
        .r = color.r,
        .g = color.g,
        .b = color.b,
        .alpha = color.alpha orelse 0xFF,
    });
}

pub fn evalStaticString(vm: *Vm, node: ast.StaticString) RuntimeError!Value {
    var sb = &vm.string_builder;
    const m = sb.begin();

    node.create(vm.nodes, vm.src, &sb.w.writer) catch
        try vm.setError(node.index, .internal_oom);

    return Value.new(.string, sb.finish(m) catch
        try vm.setError(node.index, .internal_oom));
}

pub fn evalMultiLineString(vm: *Vm, node: ast.MultiLineString) RuntimeError!Value {
    var sb = &vm.string_builder;
    const m = sb.begin();

    var parts = node.parts(vm.nodes);
    while (parts.next(vm.nodes)) |part| {
        (switch (part) {
            .newline => sb.w.writer.writeByte('\n'),
            .text => |text| sb.w.writer.writeAll(text.get(vm.nodes, vm.src)),
            .expression => |expr| blk: {
                const v = try evalExpression(vm, expr.get(vm.nodes));
                v.print(vm.string_builder.pool, &sb.w.writer) catch |err| switch (err) {
                    error.ValueError => try vm.setError(part.index(), .{ .cannot_print_value = v }),
                    error.WriteFailed => break :blk error.WriteFailed,
                };
            },
        }) catch try vm.setError(part.index(), .internal_oom);
    }
    sb.w.writer.writeByte('\n') catch try vm.setError(node.index, .internal_oom);

    return Value.new(.string, sb.finish(m) catch try vm.setError(node.index, .internal_oom));
}

pub fn evalDict(vm: *Vm, node: ast.Dict) RuntimeError!Value {
    const object = Value.Object.Dict.init(vm.valueAllocator(), vm.string_builder.pool) catch
        try vm.setError(node.index, .value_oom);
    const dict = object.as(.dict);

    var string_pool = vm.string_builder.pool;

    var fields = node.fields(vm.nodes);
    while (fields.next(vm.nodes)) |field| {
        const key = key: {
            const key = field.key(vm.nodes).get(vm.nodes, vm.src);
            break :key string_pool.put(key) catch try vm.setError(node.index, .internal_oom);
        };

        const value = try evalExpression(vm, field.value(vm.nodes));

        dict.map.put(vm.valueAllocator(), key, value) catch
            try vm.setError(field.index, .value_oom);
    }

    return Value.new(.object, object);
}

pub fn evalList(vm: *Vm, node: ast.List) RuntimeError!Value {
    const object = Value.Object.List.init(vm.valueAllocator()) catch
        try vm.setError(node.index, .value_oom);
    const list = object.as(.list);

    var items = node.items(vm.nodes);
    while (items.next(vm.nodes)) |item| {
        list.array.append(vm.valueAllocator(), try evalExpression(vm, item)) catch
            try vm.setError(node.index, .value_oom);
    }

    return Value.new(.object, object);
}

pub fn evalAccess(
    vm: *Vm,
    lhs: Value,
    rhs: Value,
    node_indices: struct { lhs: ast.NodeIndex, rhs: ast.NodeIndex, node: ast.NodeIndex },
) RuntimeError!Value {
    return switch (lhs.taggedValue()) {
        .list => |list| ret: {
            const index: f64 = try vm.expectType(node_indices.rhs, rhs, .number);

            if (index < 0) break :ret Value.nil;
            if (@as(usize, @intFromFloat(index)) >= list.array.items.len) break :ret Value.nil;

            break :ret list.array.items[@intFromFloat(index)];
        },
        .dict => |dict| ret: {
            const field: Value.String = try vm.expectType(node_indices.rhs, rhs, .string);
            break :ret dict.map.get(field) orelse Value.nil;
        },
        else => try vm.setError(
            node_indices.lhs,
            .{ .mismatched_types = .{ .exp = .list, .act = lhs } },
        ),
    };
}

pub fn evalDotAccess(vm: *Vm, node: ast.DotAccess) RuntimeError!Value {
    const lhs_node = node.lhs(vm.nodes);
    const lhs = try evalExpression(vm, lhs_node);

    const rhs_node = node.rhs(vm.nodes);
    const rhs_name = rhs_node.get(vm.nodes, vm.src);

    // If the rhs is the name of a method, we can just get the function off of it.
    //
    // NOTE: this will have weird semantics in this situation:
    //
    // #* let list = []
    // #* list.append(4) -- Append 4 to the list
    // #* list.append = 3
    // #* list.append(2) -- This will still append 2 to the list
    // #* list["append"] -- This results in 3
    //
    // NOTE 2: You are also able to create bound functions:
    //
    // #* let list = []
    // #* let fn = list.append
    // #* fn(3)
    // #* assert(list[0] == 3)
    //
    // This works fine, but I dont really it.
    const method = builtin.methods.get(lhs, vm.valueAllocator(), rhs_name) catch
        try vm.setError(rhs_node.index, .value_oom);
    if (method) |m| return Value.new(.object, m);

    const rhs = Value.new(.string, blk: {
        const sb = &vm.string_builder;
        const m = sb.begin();
        sb.w.writer.writeAll(rhs_name) catch |err| break :blk err;
        break :blk sb.finish(m);
    } catch try vm.setError(rhs_node.index, .internal_oom));

    return evalAccess(vm, lhs, rhs, .{
        .rhs = rhs_node.index,
        .lhs = lhs_node.index(),
        .node = node.index,
    });
}

pub fn evalBracketAccess(vm: *Vm, node: ast.BracketAccess) RuntimeError!Value {
    const lhs_node = node.lhs(vm.nodes);
    const rhs_node = node.rhs(vm.nodes);

    const lhs = try evalExpression(vm, lhs_node);
    const rhs = try evalExpression(vm, rhs_node);

    return evalAccess(vm, lhs, rhs, .{
        .rhs = rhs_node.index(),
        .lhs = lhs_node.index(),
        .node = node.index,
    });
}

pub fn evalVariable(vm: *Vm, node: ast.Ident) RuntimeError!Value {
    const ident = node.get(vm.nodes, vm.src);

    const variable = vm.getVariable(ident, vm.scope) catch {
        if (vm.constants.map.get(ident)) |constant| {
            return constant;
        }

        // If it's neither a variable nor constant, then its an error.
        try vm.setError(node.index, .undeclared_variable);
    };

    return variable.*;
}

pub fn evalFunctionDef(vm: *Vm, node: ast.FunctionDef) RuntimeError!Value {
    var parameters = node.parameters(vm.nodes).get(vm.nodes);
    const len = parameters.len(vm.nodes);

    const function = Value.new(.object, Value.Object.Function.initRuntime(
        vm.valueAllocator(),
        node.index,
        len,
    ) catch try vm.setError(node.index, .value_oom));

    if (node.name(vm.nodes)) |fn_name| {
        vm.bindVariable(fn_name.get(vm.nodes, vm.src), function) catch
            try vm.setError(node.index, .too_many_variables);
        return Value.nil;
    }

    return function;
}

pub fn evalFunctionCall(vm: *Vm, node: ast.FunctionCall) RuntimeError!Value {
    const caller = try evalExpression(vm, node.caller(vm.nodes));
    const function: *Value.Object.Function = try vm.expectType(node.index, caller, .function);
    return switch (function.func) {
        .runtime => callRuntimeFunction(vm, function, node),
        .builtin => callBuiltinFunction(vm, function, node),
    };
}

pub fn callBuiltinFunction(
    vm: *Vm,
    function: *Value.Object.Function,
    callsite: ast.FunctionCall,
) RuntimeError!Value {
    const builtin_fn = function.func.builtin;

    var arguments: [Value.Object.Function.max_args]Value = @splat(Value.nil);
    var args_iter = callsite.arguments(vm.nodes).get(vm.nodes);

    var total_args: usize = 0;
    while (args_iter.next(vm.nodes)) |arg_node| : (total_args += 1) {
        const arg = try evalExpression(vm, arg_node);

        // Continue evaluating function args
        //
        // TODO: there should probably be an error emitted somewhere when you call a function with
        // too many arguments
        if (total_args >= Value.Object.Function.max_args)
            continue;

        arguments[total_args] = arg;
    }

    return builtin_fn.func(.{
        .vm = vm,
        .caller_index = callsite.index,
        .self = builtin_fn.self,
    }, arguments[0..total_args]);
}

pub fn callRuntimeFunction(
    vm: *Vm,
    function: *Value.Object.Function,
    callsite: ast.FunctionCall,
) RuntimeError!Value {
    const func_node = function.func.runtime;

    const func = ast.toASTNode(ast.FunctionDef, func_node.definition_index, vm.nodes).?;
    var param_iter = func.parameters(vm.nodes).get(vm.nodes);

    const arguments = callsite.arguments(vm.nodes);
    var arg_iter = arguments.get(vm.nodes);

    const caller_scope = vm.pushFunctionScope();
    defer vm.popScope(caller_scope);

    while (true) {
        const arg = arg_iter.next(vm.nodes);
        const param = param_iter.next(vm.nodes);
        if (arg == null and param == null)
            break;

        // The value needs to be evaluated in the old scope so that variables bound outside the
        // function call can be passed as arguments to the function.
        const value = value: {
            const function_scope = vm.scope;
            vm.scope = caller_scope;
            defer vm.scope = function_scope;

            break :value if (arg) |a| try evalExpression(vm, a) else Value.nil;
        };

        // this should be placed after evaluating the argument, to have the same behavior as lua
        // does.
        if (param == null)
            continue;

        vm.bindVariable(param.?.get(vm.nodes, vm.src), value) catch
            try vm.setError(arguments.index, .too_many_variables);
    }

    if (func.name(vm.nodes)) |name| {
        vm.bindVariable(name.get(vm.nodes, vm.src), Value.new(.object, &function.base)) catch
            try vm.setError(name.index, .too_many_variables);
    }

    // Call the function
    const return_value = switch (func.body(vm.nodes)) {
        .text => |body| return_value: {
            const m = vm.string_builder.begin();

            evalText(vm, body) catch |err| switch (err) {
                ControlFlow.Return => {}, // Ignore returns
                error.RuntimeError => return error.RuntimeError,
                ControlFlow.Break => try vm.setError(vm.control_flow_node.?, .misplaced_break),
                ControlFlow.Continue => try vm.setError(vm.control_flow_node.?, .misplaced_continue),
            };
            vm.control_flow_node = null;

            const function_text = if (m != vm.string_builder.begin())
                Value.new(.string, vm.string_builder.finish(m) catch
                    try vm.setError(func.index, .internal_oom))
            else
                null;

            const return_value = vm.function_return_value;
            vm.function_return_value = null;

            if (return_value) |_| if (function_text) |_| {
                try vm.setError(func.index, .function_return_and_text);
            };

            break :return_value return_value orelse function_text orelse Value.nil;
        },
        .expression => |expr| try evalExpression(vm, expr),
    };

    return return_value;
}

pub fn evalUnary(vm: *Vm, node: ast.Unary) RuntimeError!Value {
    const op = node.op(vm.nodes);
    const rhs = try evalExpression(vm, node.rhs(vm.nodes));
    return switch (op) {
        .not => Value.operators.not(rhs),
        .negate => Value.operators.negate(rhs),
    } catch try vm.setError(
        node.index,
        .{ .invalid_unary_operands = .{ .rhs = rhs } },
    );
}

const LValue = union(enum) {
    variable: ast.Ident,
    dot_access: ast.DotAccess,
    bracket_access: ast.BracketAccess,
};

pub fn evalAccessAssignment(
    vm: *Vm,
    lvalue: Value,
    lvalue_field: Value,
    rvalue: Value,
    node_indices: struct { lhs: ast.NodeIndex, rhs: ast.NodeIndex, node: ast.NodeIndex },
) RuntimeError!void {
    if (lvalue.is(.object)) |obj| if (obj.constant) {
        try vm.setError(node_indices.lhs, .cannot_mutate_constant);
    };

    switch (lvalue.taggedValue()) {
        .list => |list| {
            const lvalue_num: f64 = try vm.expectType(node_indices.rhs, lvalue_field, .number);
            const index: usize = index: {
                if (lvalue_num < 0)
                    break :index error.ValueError;
                if (@as(usize, @intFromFloat(lvalue_num)) >= list.array.items.len)
                    break :index error.ValueError;
                break :index @as(usize, @intFromFloat(lvalue_num));
            } catch try vm.setError(node_indices.rhs, .{ .array_index_out_of_bounds = .{
                .index = Value.new(.number, lvalue_num),
                .length = Value.new(.number, @floatFromInt(list.array.items.len)),
            } });

            list.array.items[index] = rvalue;
        },
        .dict => |dict| {
            const field: Value.String = try vm.expectType(node_indices.rhs, lvalue_field, .string);
            dict.map.put(vm.valueAllocator(), field, rvalue) catch
                try vm.setError(node_indices.node, .value_oom);
        },
        else => try vm.setError(node_indices.lhs, .{
            .mismatched_types = .{ .exp = .list, .act = lvalue },
        }),
    }
}

pub fn evalAssignment(vm: *Vm, lvalue: LValue, rvalue: Value) !void {
    switch (lvalue) {
        .variable => |node| {
            vm.setVariable(node.get(vm.nodes, vm.src), rvalue, vm.scope) catch
                try vm.setError(node.index, .undeclared_variable);
        },
        .dot_access => |node| {
            const lhs_node = node.lhs(vm.nodes);
            const lhs = try evalExpression(vm, lhs_node);

            const rhs_node = node.rhs(vm.nodes);
            const rhs_name = rhs_node.get(vm.nodes, vm.src);
            const rhs = Value.new(.string, blk: {
                const sb = &vm.string_builder;
                const m = sb.begin();
                sb.w.writer.writeAll(rhs_name) catch |err| break :blk err;
                break :blk sb.finish(m);
            } catch try vm.setError(rhs_node.index, .internal_oom));

            try evalAccessAssignment(vm, lhs, rhs, rvalue, .{
                .rhs = rhs_node.index,
                .lhs = lhs_node.index(),
                .node = node.index,
            });
        },
        .bracket_access => |node| {
            const lhs_node = node.lhs(vm.nodes);
            const rhs_node = node.rhs(vm.nodes);

            const lhs = try evalExpression(vm, lhs_node);
            const rhs = try evalExpression(vm, rhs_node);

            return evalAccessAssignment(vm, lhs, rhs, rvalue, .{
                .rhs = rhs_node.index(),
                .lhs = lhs_node.index(),
                .node = node.index,
            });
        },
    }
}

pub fn evalBinary(vm: *Vm, node: ast.Binary) RuntimeError!Value {
    const op = node.op(vm.nodes);

    if (op == .assign) {
        const lhs = node.lhs(vm.nodes);

        const lvalue: LValue = switch (lhs) {
            .ident => |variable| .{ .variable = variable },
            .dot_access => |dot_access| .{ .dot_access = dot_access },
            .bracket_access => |bracket_access| .{ .bracket_access = bracket_access },
            else => try vm.setError(lhs.index(), .cannot_assign_to_non_variable),
        };

        const value = try evalExpression(vm, node.rhs(vm.nodes));
        try evalAssignment(vm, lvalue, value);

        return Value.nil;
    }

    const lhs = try evalExpression(vm, node.lhs(vm.nodes));

    if (op == .@"and" or op == .@"or") {
        if (lhs.isTruthy() == (op == .@"or")) {
            return lhs;
        } else {
            return try evalExpression(vm, node.rhs(vm.nodes));
        }
    }

    const rhs = try evalExpression(vm, node.rhs(vm.nodes));

    return switch (node.op(vm.nodes)) {
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
    } catch |err| try vm.setError(node.index, switch (err) {
        error.WriteFailed => .internal_oom,
        error.OutOfMemory => .value_oom,

        error.ValueError,
        error.InvalidOperands,
        => .{ .invalid_binary_operands = .{ .lhs = lhs, .rhs = rhs } },
    });
}

const std = @import("std");
const assert = std.debug.assert;

const ast = @import("ast.zig");
const Value = @import("value.zig").Value;
const Vm = @import("Vm.zig");
const RuntimeError = Vm.RuntimeError;
const ControlFlow = Vm.ControlFlow;
const builtin = @import("builtin.zig");
