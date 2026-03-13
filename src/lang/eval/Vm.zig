const Variable = struct {
    /// The name of the variable
    name: []const u8,
    /// The value bound to the variable
    value: Value,
    /// The scope level of the variable. Starts at 0 for the outermost block, and increases by one
    /// for every inner block. When a block ends, all scopes are popped.
    scope_level: u32,
    /// The function depth of the variable. Every function call will increase the depth by one, and
    /// returning from a function will pop all variables off
    function_depth: u32,
};

src: []const u8,
all_nodes: []const SyntaxNode,
output: *std.Io.Writer,
gpa: std.mem.Allocator,
/// Allocator specifically used to allocate values. For now it's just an arena, but it may be more
/// complex in the future.
value_allocator: *std.heap.ArenaAllocator,

err: ?RuntimeErrorPayload,
variables: std.ArrayList(Variable),
scope_level: u32,
function_depth: u32,
function_return_value: ?Value,

const Vm = @This();

pub fn init(
    all_nodes: []const SyntaxNode,
    src: []const u8,
    gpa: std.mem.Allocator,
    value_arena: *std.heap.ArenaAllocator,
    output: *std.Io.Writer,
) !Vm {
    return .{
        .gpa = gpa,
        .src = src,
        .err = null,
        .output = output,
        .scope_level = 0,
        .function_depth = 0,
        .all_nodes = all_nodes,
        .function_return_value = null,
        .value_allocator = value_arena,
        .variables = try .initCapacity(gpa, 512),
    };
}

pub fn deinit(self: *Vm) void {
    self.value_allocator.deinit();
    self.variables.deinit(self.gpa);
}

pub fn eval(self: *Vm) ?RuntimeErrorPayload {
    if (self.all_nodes.len == 0) return null;

    const root = ast.toASTNode(ast.Text, @intCast(self.all_nodes.len - 1), self.all_nodes) orelse unreachable;
    treewalk.evalText(self, root) catch |err| {
        (switch (err) {
            ControlFlow.Error => RuntimeError.Error,
            ControlFlow.Break => self.setError(root.node_index, .misplaced_break),
            ControlFlow.Return => self.setError(root.node_index, .misplaced_return),
            ControlFlow.Continue => self.setError(root.node_index, .misplaced_continue),
        }) catch return self.err;
    };

    assert(self.variables.items.len == 0);
    return null;
}

pub fn valueAllocator(self: *Vm) std.mem.Allocator {
    return self.value_allocator.allocator();
}

pub fn setError(self: *Vm, node_index: u32, kind: RuntimeErrorPayload.Kind) RuntimeError!noreturn {
    self.err = .{ .node_index = node_index, .kind = kind };
    return RuntimeError.Error;
}

pub fn setVariable(self: *Vm, ident: []const u8, value: Value) !void {
    var i: usize = self.variables.items.len;

    while (i > 0) {
        i = i - 1;
        const variable = &self.variables.items[i];

        if (variable.function_depth != self.function_depth)
            break;

        if (std.mem.eql(u8, variable.name, ident)) {
            variable.value = value;
            return;
        }
    }

    return error.UndeclaredVariable;
}

pub fn getVariable(self: *Vm, ident: []const u8) !Value {
    var i: usize = self.variables.items.len;

    while (i > 0) {
        i = i - 1;
        const variable = self.variables.items[i];

        if (variable.function_depth != self.function_depth)
            break;

        if (std.mem.eql(u8, variable.name, ident)) {
            return variable.value;
        }
    }

    return error.UndeclaredVariable;
}

pub fn bindVariable(self: *Vm, ident: []const u8, value: Value) !void {
    assert(self.scope_level > 0);
    try self.variables.appendBounded(.{
        .name = ident,
        .value = value,
        .scope_level = self.scope_level,
        .function_depth = self.function_depth,
    });
}

pub fn pushScope(self: *Vm) void {
    self.scope_level += 1;
}

pub fn popScope(self: *Vm) void {
    self.scope_level -= 1;

    while (self.variables.items.len > 0) {
        const variable = self.variables.getLast();
        if (variable.scope_level > self.scope_level and
            variable.function_depth == self.function_depth)
        {
            _ = self.variables.pop();
        } else {
            break;
        }
    }
}

pub const StackState = struct {
    scope_level: u32,
    function_depth: u32,
    variables_len: usize,
};

pub fn setupFunctionCall(self: *Vm) StackState {
    const scope_level = self.scope_level;
    const function_depth = self.function_depth;
    self.scope_level = 0;
    self.function_depth += 1;
    return .{
        .scope_level = scope_level,
        .function_depth = function_depth,
        .variables_len = self.variables.items.len,
    };
}

pub fn endFunctioncall(self: *Vm, old_state: StackState) void {
    self.scope_level = old_state.scope_level;
    self.function_depth = old_state.function_depth;
    self.variables.items.len = old_state.variables_len;
}

const RuntimeErrorPayload = struct {
    /// The index of the node that caused the error
    node_index: u32,
    kind: Kind,
    const Kind = union(enum) {
        /// Out of memory for allocating values
        value_oom,
        /// Out of memory for allocating internal interpreter things
        internal_oom,
        /// Integer literal is too large
        number_too_large,
        /// Could not write to .output because of a std.Io.Writer.Error
        write_failure,
        /// Too many variables bound to a scope
        too_many_variables,
        /// Variable not declared in the current scope
        undeclared_variable,
        cannot_assign_to_non_variable,
        /// Invalid operands to binary operator. <lhs> node.op <rhs> is not allowed.
        invalid_binary_operands: struct { lhs: Value, rhs: Value },
        /// Invalid operands to unary operator. node.op <rhs> is not allowed.
        invalid_unary_operands: struct { rhs: Value },
        cannot_print_value: struct { value: Value },
        cannot_call_value: struct { value: Value },
        invalid_function_args: struct { expected_num: u32, actual_num: u32 },
        misplaced_break,
        misplaced_continue,
        misplaced_return,
    };
};

pub const RuntimeError = error{Error};
pub const ControlFlow = error{
    /// A break statement has occurred
    Break,
    /// A continue statement has occurred
    Continue,
    /// A return statement has occurred.
    Return,
} || RuntimeError;

const std = @import("std");
const assert = std.debug.assert;

const ast = @import("../syntax/ast.zig");
const SyntaxNode = @import("../syntax/node.zig").SyntaxNode;
const treewalk = @import("treewalk.zig");
const Value = @import("value.zig").Value;
