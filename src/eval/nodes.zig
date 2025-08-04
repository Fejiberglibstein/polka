const ast = @import("../syntax/ast.zig");
const SyntaxNode = @import("../syntax/node.zig");
const Value = @import("value.zig").Value;
const Vm = @import("Vm.zig");
const StackRef = Vm.StackRef;
const RuntimeErrorPayload = @import("error.zig").RuntimeErrorPayload;
const ControlFlow = @import("error.zig").ControlFlow;
const RuntimeError = @import("error.zig").RuntimeError;

const std = @import("std");
const assert = std.debug.assert;

pub fn evalTextNode(node: ast.TextNode, vm: *Vm) ControlFlow!void {
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

pub fn evalCode(node: ast.Code, vm: *Vm) ControlFlow!void {
    var statements = node.statements(vm.nodes);

    var i: usize = 0;
    var output_len = vm.output.items.len;
    while (statements.next()) |stmt| {
        if (output_len != vm.output.items.len and vm.output.getLast() != '\n') {
            try vm.outputPrint(" ", .{});
        }
        output_len = vm.output.items.len;

        switch (stmt) {
            .expr => |v| {
                try evalExpr(v, vm);
                const value = vm.stackPop();

                // nil shouldn't be printed under normal circumstances
                if (value != .nil) {
                    try vm.outputPrint("{any}", .{value});
                }
            },
            .for_loop => @panic("TODO"),
            .let_expr => |v| {
                try evalExpr(v.value(vm.nodes), vm);
                vm.pushVar(v.binding(vm.nodes).get());
            },
            .while_loop => |v| {
                while (blk: {
                    try evalExpr(v.condition(vm.nodes), vm);
                    break :blk vm.stackPop().isTruthy();
                }) {
                    try evalTextNode(v.body(vm.nodes), vm);
                }
            },
            .return_expr => |v| {
                if (v.body(vm.nodes)) |ret| {
                    try evalExpr(ret, vm);
                } else {
                    try vm.stackPush(.nil);
                }

                return ControlFlow.Return;
            },
            .conditional => |v| {
                try evalExpr(v.condition(vm.nodes), vm);
                if (vm.stackPop().isTruthy()) {
                    try evalTextNode(v.ifBody(vm.nodes), vm);
                } else if (v.elseBody(vm.nodes)) |else_body| {
                    try evalTextNode(else_body, vm);
                }
            },
            .export_expr => @panic("TODO"),
            .function_def => |v| {
                if (v.name(vm.nodes)) |name| {
                    try vm.stackPush(try vm.allocateClosure(v));
                    vm.pushVar(name.get());
                } else {
                    try vm.setError(.unnamed_function);
                }
            },
        }
        i += 1;
    }
}

pub fn evalExpr(node: ast.Expr, vm: *Vm) RuntimeError!void {
    switch (node) {
        .nil => |_| try vm.stackPush(.nil),
        .bool => |v| try vm.stackPush(.{ .bool = v.get() }),
        .ident => |v| try vm.stackPush(try vm.getVar(v.get())),
        .number => |v| try vm.stackPush(.{ .number = v.get() }),
        .function_def => |v| try vm.stackPush(try vm.allocateClosure(v)),
        .string => |v| try vm.stackPush(try vm.allocateString("{s}", .{v.get()})),

        .unary_op => |v| try evalUnary(v, vm),
        .binary_op => |v| try evalBinary(v, vm),
        .function_call => |v| try evalFunctionCall(v, vm),
        .grouping => |v| try evalExpr(v.get(vm.nodes), vm),
        .access => @panic("TODO"),
    }
}

pub fn evalFunctionCall(node: ast.FunctionCall, vm: *Vm) RuntimeError!void {
    try evalExpr(node.caller(vm.nodes), vm);

    // because pushing arguments onto the stack could cause garbage collection, we cannot pop the
    // closure off the stack until we finish calling it.
    var closure = switch (vm.stackPeek(0)) {
        .object => |o| if (o.getClosure()) |v|
            v
        else
            try vm.setError(.{ .bad_function = vm.stackPop() }),
        else => try vm.setError(.{ .bad_function = vm.stackPop() }),
    };

    // Push a dummy variable onto the stack. This accounts for the fact that the closure is still on
    // the stack, offsetting the number of local variables.
    //
    // This variable will essentially be set equal to the closure, e.g. _ = <closure>
    vm.pushVar("_");
    const closure_stack_ref = vm.stack.len - 1; // The location on the stack where the closure is

    const function_def = ast.FunctionDef.toTyped(closure.function) orelse unreachable;

    // Save the current depth
    const scope_depth = vm.locals.scope_depth;
    const old_locals_count = vm.locals.count;
    const old_stack_len = vm.stack.len;
    vm.locals.function_depth += 1;
    vm.locals.scope_depth = 0;
    errdefer {
        // Ensure that things are cleaned up properly even if there's an error
        vm.locals.scope_depth = scope_depth;
        vm.locals.function_depth -= 1;
    }

    var args = node.arguments(vm.nodes).get(vm.nodes);
    var params = function_def.params(vm.nodes).get(vm.nodes);
    var arity: u32 = 0;

    // Push the arguments onto the stack
    while (true) {
        const arg = args.next();
        const param = params.next();

        if (param == null) {
            if (arg == null) {
                break;
            } else {
                // arguments exceed the amount of parameters
                try vm.setError(.{ .function_bad_args = arity });
            }
        }

        if (arg) |v| {
            try evalExpr(v, vm);
        } else {
            try vm.stackPush(.nil);
        }

        vm.pushVar(param.?.get());
        arity += 1;
    }

    // Because evaluating the functions arguments could have caused garbage to be collected, we need
    // to get a new reference to the closure since it would have been moved.
    closure = vm.stack.get(closure_stack_ref).object.asClosure();

    var captures_length: u32 = 0;
    // Push the closure captures onto the stack.
    //
    // None of this will allocate memory on the heap, thus causing garbage to be collected. So, we
    // don't need to worry about closure becoming invalidated again.
    if (function_def.captures(vm.nodes)) |captures| {
        var iter = captures.get(vm.nodes);
        iter = captures.get(vm.nodes);

        for (closure.getCaptures()) |capture_value| {
            captures_length += 1;
            const capture_name = iter.next().?.get();

            try vm.stackPush(capture_value);
            vm.pushVar(capture_name);
        }
    }

    // Call the function
    evalTextNode(function_def.body(vm.nodes), vm) catch |e| switch (e) {
        ControlFlow.Return => {},

        ControlFlow.Error => return RuntimeError.Error,
        ControlFlow.Break => try vm.setError(.misplaced_break),
        ControlFlow.Continue => try vm.setError(.misplaced_continue),
    };

    // The return value of the function
    const result = if (vm.stack.len == vm.locals.count)
        // If the function didn't return anything, it should be nil
        .nil
    else blk: {
        const result = vm.stackPop();
        // Make sure that the amount of local variables is equal to the amount of values on the
        // stack
        assert(vm.stack.len == vm.locals.count);
        break :blk result;
    };

    // Reset the stack to how it was before the function call
    vm.stack.len -= arity + captures_length;
    vm.locals.count -= arity + captures_length;

    assert(vm.stack.len == old_stack_len);
    assert(vm.locals.count == old_locals_count);
    vm.locals.function_depth -= 1;
    vm.locals.scope_depth = scope_depth;

    // Pop off the closure
    assert(vm.stackPop().object.getClosure() != null);
    assert(std.mem.eql(u8, vm.locals.items[vm.locals.count - 1].name, "_"));
    vm.locals.count -= 1; // Remove the dummy variable we set before

    try vm.stackPush(result);
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

pub fn binaryOp(
    vm: *Vm,
    lhs: ast.Expr,
    rhs: ast.Expr,
    op: ast.BinaryOperator.Op,
    calculate: fn (f64, f64, *Vm) RuntimeError!Value,
) RuntimeError!void {
    try evalExpr(lhs, vm);
    try evalExpr(rhs, vm);
    switch (vm.stackPeek(1)) { // switch on lhs
        .number => |l| switch (vm.stackPeek(0)) { // switch on rhs
            .number => |r| {
                const res = try calculate(l, r, vm);
                _ = vm.stackPop(); // Pop rhs
                _ = vm.stackPop(); // Pop lhs
                try vm.stackPush(res);
            },
            else => try Ops.invalidOpError(op, vm),
        },
        else => try Ops.invalidOpError(op, vm),
    }
}

const Ops = struct {
    pub fn divide(l: f64, r: f64, _: *Vm) !Value {
        return Value{ .number = l - r };
    }
    pub fn subtract(l: f64, r: f64, _: *Vm) !Value {
        return Value{ .number = l - r };
    }
    pub fn greater_than(l: f64, r: f64, _: *Vm) !Value {
        return Value{ .bool = l > r };
    }
    pub fn greater_than_equal(l: f64, r: f64, _: *Vm) !Value {
        return Value{ .bool = l >= r };
    }
    pub fn less_than(l: f64, r: f64, _: *Vm) !Value {
        return Value{ .bool = l < r };
    }
    pub fn less_than_equal(l: f64, r: f64, _: *Vm) !Value {
        return Value{ .bool = l <= r };
    }
    pub fn modulo(l: f64, r: f64, _vm: *Vm) !Value {
        return if (r > 0)
            Value{ .number = @rem(l, r) }
        else
            try _vm.setError(.{ .modulo_error = .{ .rhs = r } });
    }
    pub fn multiply(l: f64, r: f64, _: *Vm) !Value {
        return Value{ .number = l * r };
    }

    pub fn invalidOpError(operator: ast.BinaryOperator.Op, vm: *Vm) RuntimeError!noreturn {
        try vm.setError(.{
            .invalid_binary_operands = .{
                .rhs = vm.stackPop(),
                .lhs = vm.stackPop(),
                .op = operator,
            },
        });
    }
};

pub fn evalBinary(node: ast.Binary, vm: *Vm) RuntimeError!void {
    const lhs = node.lhs(vm.nodes);
    const rhs = node.rhs(vm.nodes);
    const op = node.op(vm.nodes).getOp();

    // Lhs will be pushed first, then rhs

    switch (op) {
        .assign => {
            const var_name = switch (lhs) {
                .ident => |v| v.get(),
                else => {
                    // Push lhs and rhs so that invalidOpError can pop the right values off
                    try evalExpr(lhs, vm);
                    try evalExpr(rhs, vm);
                    try Ops.invalidOpError(.assign, vm);
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
                    else => try Ops.invalidOpError(op, vm),
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
                else => try Ops.invalidOpError(op, vm),
            }
        },
        .equal => {
            try evalExpr(lhs, vm);
            try evalExpr(rhs, vm);

            try vm.stackPush(Value{
                .bool = vm.stackPop().equal(vm.stackPop()),
            });
        },
        .not_equal => {
            try evalExpr(lhs, vm);
            try evalExpr(rhs, vm);

            try vm.stackPush(Value{
                .bool = !vm.stackPop().equal(vm.stackPop()),
            });
        },
        .modulo => try binaryOp(vm, lhs, rhs, op, Ops.modulo),
        .divide => try binaryOp(vm, lhs, rhs, op, Ops.divide),
        .multiply => try binaryOp(vm, lhs, rhs, op, Ops.multiply),
        .subtract => try binaryOp(vm, lhs, rhs, op, Ops.subtract),
        .less_than => try binaryOp(vm, lhs, rhs, op, Ops.less_than),
        .greater_than => try binaryOp(vm, lhs, rhs, op, Ops.greater_than),
        .less_than_equal => try binaryOp(vm, lhs, rhs, op, Ops.less_than_equal),
        .greater_than_equal => try binaryOp(vm, lhs, rhs, op, Ops.greater_than_equal),
    }
}
