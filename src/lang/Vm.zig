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

pub const Scope = struct {
    level: u32,
    function_depth: u32,
    /// The index in .variables of the top of this stack
    top: u32,

    pub const init: Scope = .{ .level = 0, .function_depth = 0, .top = 0 };
};

src: []const u8,
all_nodes: []const SyntaxNode,
err: ?RuntimeErrorPayload,

/// Use Vm.out() to write text to the output file, since that will handle functions as well
output_file: *std.Io.Writer,
gpa: Allocator,
/// Allocator specifically used to allocate values. For now it's just an arena, but it may be more
/// complex in the future.
value_allocator: *std.heap.ArenaAllocator,
string_builder: String.Builder,

variables: std.ArrayList(Variable),

scope: Scope,
function_return_value: ?Value,

const Vm = @This();

pub fn init(
    all_nodes: []const SyntaxNode,
    src: []const u8,
    gpa: Allocator,
    value_arena: *std.heap.ArenaAllocator,
    string_pool: *String.Pool,
    output: *std.Io.Writer,
) !Vm {
    return .{
        .gpa = gpa,
        .src = src,
        .err = null,
        .output_file = output,
        .scope = .init,
        .all_nodes = all_nodes,
        .function_return_value = null,
        .value_allocator = value_arena,
        .variables = try .initCapacity(gpa, 512),
        .string_builder = try .init(gpa, string_pool),
    };
}

pub fn deinit(self: *Vm) void {
    _ = self.value_allocator.reset(.retain_capacity);
    self.variables.deinit(self.gpa);
    self.string_builder.deinit();
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

pub fn out(self: *Vm) *std.Io.Writer {
    return if (self.inFunction())
        &self.string_builder.w.writer
    else
        self.output_file;
}

pub fn valueAllocator(self: *Vm) Allocator {
    return self.value_allocator.allocator();
}

pub fn setError(self: *Vm, node_index: u32, kind: RuntimeErrorPayload.Kind) RuntimeError!noreturn {
    self.err = .{ .node_index = node_index, .kind = kind };
    return RuntimeError.Error;
}

pub fn inFunction(self: *const Vm) bool {
    return self.scope.function_depth > 0;
}

pub fn setVariable(self: *Vm, ident: []const u8, value: Value) !void {
    var i: usize = self.variables.items.len;

    while (i > 0) {
        i = i - 1;
        const variable = &self.variables.items[i];

        if (variable.function_depth < self.scope.function_depth)
            break;

        assert(variable.function_depth == self.scope.function_depth);
        if (std.mem.eql(u8, variable.name, ident)) {
            variable.value = value;
            return;
        }
    }

    return error.UndeclaredVariable;
}

pub fn getVariable(self: *Vm, ident: []const u8, scope: Scope) !Value {
    var i: usize = scope.top;

    while (i > 0) {
        i = i - 1;
        const variable = self.variables.items[i];

        if (variable.function_depth < self.scope.function_depth)
            break;

        // We can assert this because we started at the top of the scope so no variables have a
        // function depth greater than the scope's.
        assert(variable.function_depth == self.scope.function_depth);

        if (std.mem.eql(u8, variable.name, ident)) {
            return variable.value;
        }
    }

    return error.UndeclaredVariable;
}

pub fn bindVariable(self: *Vm, ident: []const u8, value: Value) !void {
    self.scope.top += 1;
    try self.variables.appendBounded(.{
        .name = ident,
        .value = value,
        .scope_level = self.scope.level,
        .function_depth = self.scope.function_depth,
    });
}

pub fn pushScope(self: *Vm) Scope {
    const old_scope = self.scope;
    self.scope.level += 1;
    return old_scope;
}

pub fn popScope(self: *Vm, old_scope: Scope) void {
    self.scope = old_scope;
    self.variables.items.len = @intCast(old_scope.top);
}

pub fn pushFunctionScope(self: *Vm) Scope {
    const old_scope = self.scope;
    self.scope.level = 0;
    self.scope.function_depth += 1;
    return old_scope;
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
        cannot_print_value: Value,
        cannot_call_value: Value,
        invalid_function_args: struct { expected_num: u32, actual_num: u32 },
        misplaced_break,
        misplaced_continue,
        misplaced_return,
        function_return_and_text,
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
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const SyntaxNode = @import("node.zig").SyntaxNode;
const Object = @import("value.zig").Object;
const treewalk = @import("treewalk.zig");
const Value = @import("value.zig").Value;
const String = @import("value.zig").String;
