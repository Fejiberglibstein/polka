pub fn evalText(vm: *Vm, node: ast.Text) ControlFlow!void {
    var parts = node.parts(vm.all_nodes);
    const old_scope = vm.pushScope();
    defer vm.popScope(old_scope);

    const out = vm.out();
    while (parts.next(vm.all_nodes)) |part| {
        (switch (part) {
            .newline => out.writeAll("\n"),
            .code => |code| try evalCode(vm, code),
            .text_line => |line| out.writeAll(line.get(vm.src, vm.all_nodes)),
        }) catch |err| switch (err) {
            error.WriteFailed => try vm.setError(part.nodeIndex(), .write_failure),
            inline else => |e| return e,
        };
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
            .expression => |expr| {
                const res = try evalExpression(vm, expr);
                if (res.getString()) |str| {
                    const bytes = vm.string_builder.pool.get(str);
                    out.writeAll(bytes) catch
                        try vm.setError(expr.nodeIndex(), .write_failure);
                    continue;
                }

                if (!res.isNil()) {
                    try vm.setError(expr.nodeIndex(), .{ .cannot_print_value = res });
                }
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
        .nil => Value.nil,
        .true => Value.newBoolean(true),
        .false => Value.newBoolean(false),
        .list => |list| evalList(vm, list),
        .dict => |dict| evalDict(vm, dict),
        .color => |color| evalColor(vm, color),
        .unary => |unary| evalUnary(vm, unary),
        .binary => |binary| evalBinary(vm, binary),
        .ident => |variable| evalVariable(vm, variable),
        .function_def => |def| evalFunctionDef(vm, def),
        .static_string => |str| evalStaticString(vm, str),
        .function_call => |call| evalFunctionCall(vm, call),
        .multi_line_string => |str| evalMultiLineString(vm, str),
        .dot_access => |dot_access| evalDotAccess(vm, dot_access),
        .bracket_access => |access| evalBracketAccess(vm, access),
        .number => |num| Value.newNumber(num.get(vm.all_nodes, vm.src)),
        .grouping => |group| evalExpression(vm, group.inner(vm.all_nodes)),
        .integer => |num| Value.newNumber(num.getAsFloat(vm.all_nodes, vm.src)),
    };
}

pub fn evalColor(vm: *Vm, node: ast.Color) RuntimeError!Value {
    const color = node.get(vm.all_nodes, vm.src);
    return Value.newColor(.{
        .r = color.r,
        .g = color.g,
        .b = color.b,
        .alpha = color.alpha orelse 0xFF,
    });
}

pub fn evalStaticString(vm: *Vm, node: ast.StaticString) RuntimeError!Value {
    var sb = &vm.string_builder;
    const m = sb.begin();
    node.create(vm.all_nodes, vm.src, &sb.w.writer) catch
        try vm.setError(node.node_index, .internal_oom);

    return Value.newString(sb.finish(m) catch
        try vm.setError(node.node_index, .internal_oom));
}

pub fn evalMultiLineString(vm: *Vm, node: ast.MultiLineString) RuntimeError!Value {
    var sb = &vm.string_builder;
    const m = sb.begin();

    var parts = node.parts(vm.all_nodes);
    while (parts.next(vm.all_nodes)) |part| {
        (switch (part) {
            .newline => sb.w.writer.print("\n", .{}),
            .text => |text| sb.w.writer.print("{s}", .{text.get(vm.all_nodes, vm.src)}),
            .expression => |expr| blk: {
                const v = try evalExpression(vm, expr.get(vm.all_nodes));
                v.print(vm.string_builder.pool, &sb.w.writer) catch |err| switch (err) {
                    error.ValueError => try vm.setError(
                        part.nodeIndex(),
                        .{ .cannot_print_value = v },
                    ),
                    error.WriteFailed => break :blk error.WriteFailed,
                };
            },
        }) catch try vm.setError(part.nodeIndex(), .internal_oom);
    }
    sb.w.writer.print("\n", .{}) catch try vm.setError(node.node_index, .internal_oom);

    return Value.newString(sb.finish(m) catch
        try vm.setError(node.node_index, .internal_oom));
}

pub fn evalDict(vm: *Vm, node: ast.Dict) RuntimeError!Value {
    const object = Value.Object.Dict.init(vm.valueAllocator()) catch
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

        dict.map.putContext(vm.valueAllocator(), key, value, .{ .pool = sb.pool }) catch
            try vm.setError(field.node_index, .value_oom);
    }

    return Value.newObject(object);
}

pub fn evalList(vm: *Vm, node: ast.List) RuntimeError!Value {
    const object = Value.Object.List.init(vm.valueAllocator()) catch
        try vm.setError(node.node_index, .value_oom);
    const list = object.asList();

    var items = node.items(vm.all_nodes);
    while (items.next(vm.all_nodes)) |item| {
        list.array.append(vm.valueAllocator(), try evalExpression(vm, item)) catch
            try vm.setError(node.node_index, .value_oom);
    }

    return Value.newObject(object);
}

pub fn evalAccess(
    vm: *Vm,
    lhs: Value,
    rhs: Value,
    node_indices: struct { lhs: u32, rhs: u32, node: u32 },
) RuntimeError!Value {
    return switch (lhs.taggedValue()) {
        .list => |list| ret: {
            if (!rhs.isNumber()) try vm.setError(node_indices.rhs, .{
                .invalid_type = .{ .exp = .number, .act = rhs },
            });

            if (rhs.asNumber() < 0) break :ret Value.nil;
            const index: usize = @intFromFloat(rhs.asNumber());
            if (index > list.array.items.len) break :ret Value.nil;

            break :ret list.array.items[index];
        },
        .dict => |dict| ret: {
            if (!rhs.isString()) try vm.setError(node_indices.rhs, .{
                .invalid_type = .{ .exp = .string, .act = rhs },
            });

            const field = rhs.asString();
            const sb = &vm.string_builder;
            break :ret dict.map.getContext(field, .{ .pool = sb.pool }) orelse Value.nil;
        },
        else => try vm.setError(
            node_indices.lhs,
            .{ .invalid_type = .{ .exp = .list, .act = lhs } },
        ),
    };
}

pub fn evalDotAccess(vm: *Vm, node: ast.DotAccess) RuntimeError!Value {
    const lhs_node = node.lhs(vm.all_nodes);
    const lhs = try evalExpression(vm, lhs_node);

    const rhs_node = node.rhs(vm.all_nodes);
    const rhs_name = rhs_node.get(vm.all_nodes, vm.src);

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
        try vm.setError(rhs_node.node_index, .value_oom);
    if (method) |m| return Value.newObject(m);

    const rhs = Value.newString(blk: {
        const sb = &vm.string_builder;
        const m = sb.begin();
        sb.w.writer.writeAll(rhs_name) catch |err| break :blk err;
        break :blk sb.finish(m);
    } catch try vm.setError(rhs_node.node_index, .internal_oom));

    return evalAccess(vm, lhs, rhs, .{
        .rhs = rhs_node.node_index,
        .lhs = lhs_node.nodeIndex(),
        .node = node.node_index,
    });
}

pub fn evalBracketAccess(vm: *Vm, node: ast.BracketAccess) RuntimeError!Value {
    const lhs_node = node.lhs(vm.all_nodes);
    const rhs_node = node.rhs(vm.all_nodes);

    const lhs = try evalExpression(vm, lhs_node);
    const rhs = try evalExpression(vm, rhs_node);

    return evalAccess(vm, lhs, rhs, .{
        .rhs = rhs_node.nodeIndex(),
        .lhs = lhs_node.nodeIndex(),
        .node = node.node_index,
    });
}

pub fn evalVariable(vm: *Vm, node: ast.Ident) RuntimeError!Value {
    const ident = node.get(vm.all_nodes, vm.src);

    const variable = vm.getVariable(ident, vm.scope) catch {
        if (builtin.functions.get(ident)) |function| {
            return Value.newObject(function);
        } else {
            try vm.setError(node.node_index, .undeclared_variable);
        }
    };

    return variable.*;
}

pub fn evalFunctionDef(vm: *Vm, node: ast.FunctionDef) RuntimeError!Value {
    var parameters = node.parameters(vm.all_nodes).get(vm.all_nodes);
    const len = parameters.len(vm.all_nodes);

    const function = Value.newObject(Value.Object.Function.initRuntime(
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
    var args_iter = callsite.arguments(vm.all_nodes).get(vm.all_nodes);

    var total_args: usize = 0;
    while (args_iter.next(vm.all_nodes)) |arg_node| : (total_args += 1) {
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
        .caller_node_index = callsite.node_index,
        .self = builtin_fn.self,
    }, arguments[0..total_args]);
}

pub fn callRuntimeFunction(
    vm: *Vm,
    function: *Value.Object.Function,
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

        vm.bindVariable(param.?.get(vm.all_nodes, vm.src), value) catch
            try vm.setError(arguments.node_index, .too_many_variables);
    }

    if (func.name(vm.all_nodes)) |name| {
        vm.bindVariable(name.get(vm.all_nodes, vm.src), Value.newObject(&function.base)) catch
            try vm.setError(name.node_index, .too_many_variables);
    }

    // Call the function
    const return_value = switch (func.body(vm.all_nodes)) {
        .text => |body| return_value: {
            const m = vm.string_builder.begin();

            evalText(vm, body) catch |err| switch (err) {
                error.RuntimeError => return error.RuntimeError,
                ControlFlow.Continue => @panic("TODO"),
                ControlFlow.Break => @panic("TODO"),
                ControlFlow.Return => {},
            };

            const function_text = if (m != vm.string_builder.begin())
                Value.newString(vm.string_builder.finish(m) catch
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
    node_indices: struct { lhs: u32, rhs: u32, node: u32 },
) RuntimeError!void {
    switch (lvalue.taggedValue()) {
        .list => |list| {
            if (!lvalue_field.isNumber()) try vm.setError(node_indices.rhs, .{
                .invalid_type = .{ .exp = .number, .act = lvalue_field },
            });

            const index: usize = index: {
                const index = lvalue_field.asNumber();
                if (index < 0)
                    break :index error.ValueError;
                if (@as(usize, @intFromFloat(index)) > list.array.items.len)
                    break :index error.ValueError;
                break :index @as(usize, @intFromFloat(lvalue_field.asNumber()));
            } catch try vm.setError(node_indices.rhs, .array_access_out_of_bounds);

            list.array.items[index] = rvalue;
        },
        .dict => |dict| {
            if (!lvalue_field.isString()) try vm.setError(node_indices.rhs, .{
                .invalid_type = .{ .exp = .string, .act = lvalue_field },
            });

            const field = lvalue_field.asString();
            const gop = dict.map.getOrPutContext(
                vm.valueAllocator(),
                field,
                .{ .pool = vm.string_builder.pool },
            ) catch try vm.setError(node_indices.node, .value_oom);

            gop.value_ptr.* = rvalue;
        },
        else => try vm.setError(node_indices.lhs, .{
            .invalid_type = .{ .exp = .list, .act = lvalue },
        }),
    }
}

pub fn evalAssignment(vm: *Vm, lvalue: LValue, rvalue: Value) !void {
    switch (lvalue) {
        .variable => |node| {
            const variable = vm.getVariable(node.get(vm.all_nodes, vm.src), vm.scope) catch
                try vm.setError(node.node_index, .undeclared_variable);
            variable.* = rvalue;
        },
        .dot_access => |node| {
            const lhs_node = node.lhs(vm.all_nodes);
            const lhs = try evalExpression(vm, lhs_node);

            const rhs_node = node.rhs(vm.all_nodes);
            const rhs_name = rhs_node.get(vm.all_nodes, vm.src);
            const rhs = Value.newString(blk: {
                const sb = &vm.string_builder;
                const m = sb.begin();
                sb.w.writer.writeAll(rhs_name) catch |err| break :blk err;
                break :blk sb.finish(m);
            } catch try vm.setError(rhs_node.node_index, .internal_oom));

            try evalAccessAssignment(vm, lhs, rhs, rvalue, .{
                .rhs = rhs_node.node_index,
                .lhs = lhs_node.nodeIndex(),
                .node = node.node_index,
            });
        },
        .bracket_access => |node| {
            const lhs_node = node.lhs(vm.all_nodes);
            const rhs_node = node.rhs(vm.all_nodes);

            const lhs = try evalExpression(vm, lhs_node);
            const rhs = try evalExpression(vm, rhs_node);

            return evalAccessAssignment(vm, lhs, rhs, rvalue, .{
                .rhs = rhs_node.nodeIndex(),
                .lhs = lhs_node.nodeIndex(),
                .node = node.node_index,
            });
        },
    }
}

pub fn evalBinary(vm: *Vm, node: ast.Binary) RuntimeError!Value {
    const op = node.op(vm.all_nodes);

    if (op == .assign) {
        const lhs = node.lhs(vm.all_nodes);

        const lvalue: LValue = switch (lhs) {
            .ident => |variable| .{ .variable = variable },
            .dot_access => |dot_access| .{ .dot_access = dot_access },
            .bracket_access => |bracket_access| .{ .bracket_access = bracket_access },
            else => try vm.setError(node.node_index, .cannot_assign_to_non_variable),
        };

        const value = try evalExpression(vm, node.rhs(vm.all_nodes));
        try evalAssignment(vm, lvalue, value);

        return Value.nil;
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
const Vm = @import("Vm.zig");
const RuntimeError = Vm.RuntimeError;
const ControlFlow = Vm.ControlFlow;
const builtin = @import("builtin.zig");
