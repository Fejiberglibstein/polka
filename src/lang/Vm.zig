const Variable = struct {
    /// The name of the variable
    name: []const u8,
    /// The value bound to the variable
    value: Value,

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
nodes: []const SyntaxNode,
err: ?RuntimeErrorPayload,

gpa: Allocator,
/// Allocator specifically used to allocate values. For now it's just an arena, but it may be more
/// complex in the future.
value_allocator: *std.heap.ArenaAllocator,

/// Since emitting text inside functions is legal and will write the text into a string rather than
/// the output file, it is usually better to use Vm.out() to output text.
output_file: *std.Io.Writer,
string_builder: StringBuilder,
constants: builtins.Constants,
config: *polka.Config,

variables: std.ArrayList(Variable),

scope: Scope,
function_return_value: ?Value,

/// A file-local string builder, this is used to create strings using .begin() & .finish(). A
/// finished string may be placed in the StringPool if it's not already in the pool.
pub const StringBuilder = struct {
    pool: *String.Pool,
    w: std.Io.Writer.Allocating,

    pub fn init(gpa: Allocator, pool: *String.Pool) !StringBuilder {
        return .{
            .pool = pool,
            .w = .init(gpa),
        };
    }

    pub fn deinit(builder: *StringBuilder) void {
        builder.w.deinit();
        builder.* = undefined;
    }

    pub const Marker = enum(u32) { _ };

    pub fn begin(b: *@This()) Marker {
        return @enumFromInt(b.w.written().len);
    }

    pub fn finish(b: *@This(), m: Marker) !String {
        const str = b.w.written()[@intFromEnum(m)..];
        const result = b.pool.put(str);
        b.w.shrinkRetainingCapacity(@intFromEnum(m));
        return result;
    }
};

const Vm = @This();

pub const InitOptions = struct {
    nodes: []const SyntaxNode,
    src: []const u8,

    gpa: Allocator,
    string_pool: *String.Pool,
    value_arena: *std.heap.ArenaAllocator,

    config: *polka.Config,
    output: *std.Io.Writer,
    constants: builtins.Constants,
};

pub fn init(opts: InitOptions) !Vm {
    var string_builder: StringBuilder = try .init(opts.gpa, opts.string_pool);
    errdefer string_builder.deinit();

    var variables: std.ArrayList(Variable) = try .initCapacity(opts.gpa, 512);
    errdefer variables.deinit(opts.gpa);

    return .{
        .err = null,
        .scope = .init,
        .gpa = opts.gpa,
        .src = opts.src,
        .nodes = opts.nodes,
        .config = opts.config,
        .variables = variables,
        .output_file = opts.output,
        .function_return_value = null,
        .string_builder = string_builder,
        .value_allocator = opts.value_arena,
        .constants = opts.constants,
    };
}

pub fn deinit(vm: *Vm) void {
    _ = vm.value_allocator.reset(.retain_capacity);
    vm.variables.deinit(vm.gpa);
    vm.string_builder.deinit();
}

pub fn run(vm: *Vm) ?RuntimeErrorPayload {
    if (vm.nodes.len == 0) return null;

    const root = ast.toASTNode(ast.Text, @intCast(vm.nodes.len - 1), vm.nodes) orelse unreachable;
    eval.evalText(vm, root) catch |err| {
        (switch (err) {
            ControlFlow.Break => vm.setError(root.node_index, .misplaced_break),
            ControlFlow.Return => vm.setError(root.node_index, .misplaced_return),
            ControlFlow.Continue => vm.setError(root.node_index, .misplaced_continue),
            ControlFlow.RuntimeError => error.RuntimeError,
        }) catch return vm.err;
    };

    assert(vm.variables.items.len == 0);
    return null;
}

pub fn out(vm: *Vm) *std.Io.Writer {
    return if (vm.inFunction())
        &vm.string_builder.w.writer
    else
        vm.output_file;
}

pub fn valueAllocator(vm: *Vm) Allocator {
    return vm.value_allocator.allocator();
}

pub fn setError(vm: *Vm, node_index: u32, kind: RuntimeErrorPayload.Kind) RuntimeError!noreturn {
    vm.err = .{ .node_index = node_index, .kind = kind };
    return error.RuntimeError;
}

pub fn inFunction(vm: *const Vm) bool {
    return vm.scope.function_depth > 0;
}

pub fn setVariable(vm: *Vm, ident: []const u8, value: Value, scope: Scope) !void {
    const variable = try getVariable(vm, ident, scope);
    variable.* = value;
}

pub fn getVariable(vm: *Vm, ident: []const u8, scope: Scope) !*Value {
    var i: usize = scope.top;

    while (i > 0) {
        i = i - 1;
        const variable = &vm.variables.items[i];

        if (variable.function_depth < vm.scope.function_depth) {
            @branchHint(.unlikely);
            break;
        }

        // We can assert this because we started at the top of the scope so no variables have a
        // function depth greater than the scope's.
        assert(variable.function_depth == vm.scope.function_depth);

        if (std.mem.eql(u8, variable.name, ident)) {
            return &variable.value;
        }
    }

    return error.UndeclaredVariable;
}

pub fn bindVariable(vm: *Vm, ident: []const u8, value: Value) !void {
    vm.scope.top += 1;
    try vm.variables.appendBounded(.{
        .name = ident,
        .value = value,
        .function_depth = vm.scope.function_depth,
    });
}

pub fn pushScope(vm: *Vm) Scope {
    const old_scope = vm.scope;
    vm.scope.level += 1;
    return old_scope;
}

pub fn popScope(vm: *Vm, old_scope: Scope) void {
    vm.scope = old_scope;
    vm.variables.items.len = @intCast(old_scope.top);
}

pub fn pushFunctionScope(vm: *Vm) Scope {
    const old_scope = vm.scope;
    vm.scope.level = 0;
    vm.scope.function_depth += 1;
    return old_scope;
}

pub const RuntimeErrorPayload = struct {
    /// The index of the node that caused the error
    node_index: u32,
    kind: Kind,
    const Kind = union(enum) {
        /// Out of memory for allocating values
        value_oom,
        /// Out of memory for allocating internal interpreter things
        internal_oom,
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
        invalid_type: struct { exp: Value.Type, act: Value },
        cannot_print_value: Value,
        cannot_call_value: Value,
        invalid_function_args: struct { expected_num: u32, actual_num: u32 },
        misplaced_break,
        misplaced_continue,
        misplaced_return,
        function_return_and_text,
        array_access_out_of_bounds,
        cannot_mutate_constant,
    };
};

pub const RuntimeError = error{RuntimeError};
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
const eval = @import("eval.zig");
const SyntaxNode = @import("node.zig").SyntaxNode;
const Value = @import("value.zig").Value;
const String = Value.String;
const builtins = @import("builtins.zig");
const polka = @import("../polka.zig");
