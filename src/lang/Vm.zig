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
constants: builtin.Constants,
config: *polka.Config,

variables: std.ArrayList(Variable),

scope: Scope,
function_return_value: ?Value,
/// The index of the node that started a ControlFlow error to be tried.
control_flow_node: ?ast.NodeIndex,

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
    constants: builtin.Constants,
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
        .constants = opts.constants,
        .function_return_value = null,
        .control_flow_node = null,
        .string_builder = string_builder,
        .value_allocator = opts.value_arena,
    };
}

pub fn deinit(vm: *Vm) void {
    _ = vm.value_allocator.reset(.retain_capacity);
    vm.variables.deinit(vm.gpa);
    vm.string_builder.deinit();
}

pub fn run(vm: *Vm) ?RuntimeErrorPayload {
    if (vm.nodes.len == 0) return null;

    const root = ast.toASTNode(ast.Text, @enumFromInt(vm.nodes.len - 1), vm.nodes) orelse unreachable;
    eval.evalText(vm, root) catch |err| {
        (switch (err) {
            ControlFlow.Break => vm.setError(vm.control_flow_node.?, .misplaced_break),
            ControlFlow.Return => vm.setError(vm.control_flow_node.?, .misplaced_return),
            ControlFlow.Continue => vm.setError(vm.control_flow_node.?, .misplaced_continue),
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

pub fn setFormattedError(
    vm: *Vm,
    index: ast.NodeIndex,
    comptime fmt: []const u8,
    args: anytype,
) RuntimeError!noreturn {
    const err_string = blk: {
        const sb = &vm.string_builder;
        const m = sb.begin();
        sb.w.writer.print(fmt, args) catch |err| break :blk err;
        break :blk sb.finish(m);
    } catch try vm.setError(index, .internal_oom);

    vm.err = .{ .index = index, .kind = .{ .any = err_string } };
    return error.RuntimeError;
}

pub fn setError(vm: *Vm, index: ast.NodeIndex, kind: RuntimeErrorPayload.Kind) RuntimeError!noreturn {
    vm.err = .{ .index = index, .kind = kind };
    return error.RuntimeError;
}

fn TaggedType(comptime ty: Value.Type) type {
    return @typeInfo(Value.TaggedValue).@"union".fields[@intFromEnum(ty)].type;
}

pub fn expectType(
    vm: *Vm,
    index: ast.NodeIndex,
    value: Value,
    comptime expected_type: Value.Type,
) RuntimeError!TaggedType(expected_type) {
    if (value.typ() != expected_type)
        try vm.setError(index, .{ .mismatched_types = .{ .exp = expected_type, .act = value } });
    return @field(value.taggedValue(), @tagName(expected_type));
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
    index: ast.NodeIndex,
    kind: Kind,
    const Kind = union(enum) {
        /// Out of memory for allocating values
        value_oom,
        /// Out of memory for allocating internal interpreter things
        internal_oom,
        /// Could not write to .output_file
        write_failure,
        /// Too many variables bound to a scope
        too_many_variables,
        /// Variable not declared in the current scope
        undeclared_variable,
        /// Attempted to mutate non-variable
        cannot_assign_to_non_variable,
        /// Invalid operands to binary operator. <lhs> node.op <rhs> is not allowed.
        invalid_binary_operands: struct { lhs: Value, rhs: Value },
        /// Invalid operands to unary operator. node.op <rhs> is not allowed.
        invalid_unary_operands: struct { rhs: Value },
        /// Expected type <exp>, got <act.type()>
        mismatched_types: struct { exp: Value.Type, act: Value },
        /// Cannot print value
        cannot_print_value: Value,
        /// Break statement outside of loop
        misplaced_break,
        /// Continue statement outside of loop
        misplaced_continue,
        /// Return statemenout outside of function
        misplaced_return,
        /// Function returns a value and outputs text.
        // TODO make name clearer
        function_return_and_text,
        /// Array access was out of bounds
        array_index_out_of_bounds: struct { index: Value, length: Value },
        /// Cannot mutate constant value
        cannot_mutate_constant,

        any: String,
    };

    pub const FormatWrapper = struct {
        err: RuntimeErrorPayload,

        nodes: []const SyntaxNode,
        pool: *String.Pool,
        src: []const u8,

        pub fn format(
            self: @This(),
            w: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try switch (self.err.kind) {
                .internal_oom => w.writeAll("internal error; oom"),
                .write_failure => w.writeAll("internal error; write failure"),
                .value_oom => w.writeAll(
                    \\Too many values allocated at once.
                    \\
                    \\Hint:
                    \\  Try optimizing your templates memory usage
                    \\  or rerun with TODO flag to increase memory
                ),
                .too_many_variables => w.writeAll(
                    \\Too many variables in scope
                    \\
                    \\Hint:
                    \\  rerun with TODO flag to increase variable limit
                ),
                .undeclared_variable => w.print(
                    \\Use of undeclared identifier `{s}`
                , .{
                    ast.toASTNode(
                        ast.Ident,
                        self.err.index,
                        self.nodes,
                    ).?.get(self.nodes, self.src),
                }),
                .cannot_assign_to_non_variable => w.writeAll(
                    \\Invalid left-hand side to assignment
                ),
                .invalid_binary_operands => |v| w.print(
                    \\Invalid operands to binary operator. {[lhs]f} {[op]f} {[rhs]f} is not allowed
                , .{
                    .lhs = v.lhs.typ(),
                    .op = ast.toASTNode(
                        ast.BinaryOperator,
                        self.err.index,
                        self.nodes,
                    ).?,
                    .rhs = v.rhs.typ(),
                }),
                .invalid_unary_operands => |v| w.print(
                    \\Invalid operands to unary operator. {[op]f} {[rhs]f} is not allowed
                , .{
                    .op = ast.toASTNode(
                        ast.UnaryOperator,
                        self.err.index,
                        self.nodes,
                    ).?,
                    .rhs = v.rhs.typ(),
                }),
                .mismatched_types => |v| w.print(
                    \\Invalid type. Expected {[exp]f}, got {[act]f}
                , .{ .exp = v.exp, .act = v.act.typ() }),
                .cannot_print_value => w.writeAll(
                    // TODO i dont like the word "print" in this error message
                    \\Cannot print {}
                    \\
                    \\Hint:
                    \\  Use `debug()` to log an expression
                ),
                .misplaced_break => w.writeAll(
                    \\Break statement outside of loop
                ),
                .misplaced_continue => w.writeAll(
                    \\Continue statement outside of loop
                ),
                .misplaced_return => w.writeAll(
                    \\Return statement outside of function body
                ),
                .function_return_and_text => w.writeAll(
                    // TODO i dont like this error message
                    \\Functions may not both emit text and return a value.
                ),
                .array_index_out_of_bounds => |v| w.print(
                    \\Array index out of bounds (index: {[index]}, len: {[len]})
                , .{
                    .index = v.index.asNumber(),
                    .len = v.index.asNumber(),
                }),
                .cannot_mutate_constant => w.writeAll(
                    \\Cannot mutate a constant
                ),
                .any => |v| w.writeAll(self.pool.get(v)),
            };
        }
    };
    pub fn formatWith(
        self: RuntimeErrorPayload,
        nodes: []const SyntaxNode,
        src: []const u8,
        pool: *String.Pool,
    ) FormatWrapper {
        return .{
            .src = src,
            .err = self,
            .pool = pool,
            .nodes = nodes,
        };
    }
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
const builtin = @import("builtin.zig");
const polka = @import("../polka.zig");
