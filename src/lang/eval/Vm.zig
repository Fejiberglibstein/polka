const Variable = struct {
    /// The name of the variable
    name: []const u8,
    /// The index into the stack frame where this variable lives.
    frame_index: u32,
};

src: []const u8,
all_nodes: []const SyntaxNode,
gpa: std.mem.Allocator,

variables: std.ArrayList(Variable),
errors: std.ArrayList(RuntimeErrorPayload),

const Vm = @This();

pub fn init(all_nodes: []const SyntaxNode, src: []const u8, gpa: std.mem.Allocator) Vm {
    return .{
        .all_nodes = all_nodes,
        .errors = .empty,
        .gpa = gpa,
        .src = src,
        .variables = .empty,
    };
}

pub fn getVariable(self: *Vm, var_name: []const u8) ?Variable {
    for (self.variables.items) |variable| {
        if (variable.name == var_name) return variable;
    }
    return null;
}

pub fn setError(self: *Vm, node: SyntaxNode, kind: RuntimeErrorPayload.Kind) !noreturn {
    try self.errors.append(self.gpa, .{ .node = node, .kind = kind });
    return RuntimeError.Error;
}

const RuntimeErrorPayload = struct {
    /// The node that caused the error
    node: SyntaxNode,
    kind: Kind,
    const Kind = union(enum) {
        /// Integer literal is too large
        number_too_large,
    };
};

const RuntimeError = error{
    Error,
} || std.mem.Allocator.Error;

const std = @import("std");
const SyntaxNode = @import("syntax//node.zig").SyntaxNode;
const ast = @import("syntax/ast.zig");
