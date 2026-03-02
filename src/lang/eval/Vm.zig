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
gpa: std.mem.Allocator,
output: *std.Io.Writer,

errors: std.ArrayList(RuntimeErrorPayload),
variables: std.ArrayList(Variable),
scope_level: u32,
function_depth: u32,

const Vm = @This();

pub fn init(
    all_nodes: []const SyntaxNode,
    src: []const u8,
    gpa: std.mem.Allocator,
    output: *std.Io.Writer,
) !Vm {
    return .{
        .gpa = gpa,
        .src = src,
        .errors = .empty,
        .output = output,
        .scope_level = 0,
        .function_depth = 0,
        .all_nodes = all_nodes,
        .variables = try .initCapacity(gpa, 512),
    };
}

pub fn setError(self: *Vm, node_index: u32, kind: RuntimeErrorPayload.Kind) RuntimeError!noreturn {
    try self.errors.append(self.gpa, .{ .node_index = node_index, .kind = kind });
    return RuntimeError.Error;
}

pub fn bindVariable(self: *Vm, ident: []const u8, value: Value) !void {
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
        if (self.variables.getLast().scope_level > self.scope_level) {
            self.variables.pop();
        } else {
            break;
        }
    }
}

const RuntimeErrorPayload = struct {
    /// The index of the node that caused the error
    node_index: u32,
    kind: Kind,
    const Kind = union(enum) {
        /// Integer literal is too large
        number_too_large,
        /// Could not write to .output because of a std.Io.Writer.Error
        write_failure,
        stack_overflow,
        /// Invalid operands to binary operator. <lhs> node.op <rhs> is not allowed.
        invalid_binary_operands: struct { lhs: Value, rhs: Value },
        cannot_print_value: struct { value: Value },
    };
};

pub const RuntimeError = error{
    Error,
} || std.mem.Allocator.Error;

const std = @import("std");
const SyntaxNode = @import("../syntax/node.zig").SyntaxNode;
const ast = @import("../syntax/ast.zig");
const Value = @import("value.zig").Value;
