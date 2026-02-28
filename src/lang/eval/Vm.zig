const Variable = struct {
    /// The name of the variable
    name: []const u8,
    stack_position: usize,
};

src: []const u8,
all_nodes: []const SyntaxNode,
gpa: std.mem.Allocator,
output: std.Io.Writer,

errors: std.ArrayList(RuntimeErrorPayload),
stack: std.ArrayList(Value),
variables: std.ArrayList(Variable),

const Vm = @This();
const stack_size = 1024;

pub fn init(
    all_nodes: []const SyntaxNode,
    src: []const u8,
    gpa: std.mem.Allocator,
    output: std.Io.Writer,
) !Vm {
    return .{
        .gpa = gpa,
        .src = src,
        .errors = .empty,
        .output = output,
        .all_nodes = all_nodes,
        .stack = try .initCapacity(gpa, stack_size),
        .variables = try .initCapacity(gpa, stack_size),
    };
}

pub fn setError(self: *Vm, node: SyntaxNode, kind: RuntimeErrorPayload.Kind) !noreturn {
    try self.errors.append(self.gpa, .{ .node = node, .kind = kind });
    return RuntimeError.Error;
}

pub fn stackPush(self: *Vm, ctx: SyntaxNode, v: Value) !void {
    self.stack.appendBounded(v) catch try self.setError(ctx, .stack_overflow);
}
pub fn stackPop(self: *Vm) Value {
    return self.stack.pop() catch unreachable;
}
pub fn stackPeek(self: *Vm, back: usize) Value {
    return self.stack[self.stack.items.len - back - 1];
}

const RuntimeErrorPayload = struct {
    /// The node that caused the error
    node: SyntaxNode,
    kind: Kind,
    const Kind = union(enum) {
        /// Integer literal is too large
        number_too_large,
        stack_overflow,
    };
};

pub const RuntimeError = error{
    Error,
} || std.mem.Allocator.Error;

const std = @import("std");
const SyntaxNode = @import("../syntax/node.zig").SyntaxNode;
const ast = @import("../syntax/ast.zig");
const Value = @import("value.zig").Value;
