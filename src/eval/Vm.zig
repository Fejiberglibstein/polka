const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("../runtime/value.zig").Value;
const ast = @import("../syntax/ast.zig");
const RuntimeErrorPayload = @import("error.zig").RuntimeErrorPayload;
const RuntimeError = @import("error.zig").RuntimeError;

const SyntaxNode = @import("../syntax/node.zig").SyntaxNode;
const base = @import("base.zig");

const Vm = @This();

/// List of all nodes in the program
nodes: []const SyntaxNode,
/// The resulting text that the language produces
content: Content,
err: ?RuntimeErrorPayload,

pub fn init(allocator: Allocator, all_nodes: []const SyntaxNode) Vm {
    return Vm{
        .nodes = all_nodes,
        .content = Content{ .v = .init(allocator) },
        .err = null,
    };
}

pub fn deinit(self: Vm) void {
    self.content.v.deinit();
}

pub fn eval(self: *Vm, start_node: SyntaxNode) ![]const u8 {
    const root = ast.TextNode.toTyped(start_node).?;
    try base.evalTextNode(root, self);

    return self.content.v.items;
}

pub fn setError(self: *Vm, err: RuntimeErrorPayload) RuntimeError!noreturn {
    if (self.err != null) self.err = err;
    return RuntimeError.Error;
}

pub fn writeValue(self: *Vm, value: Value) !void {
    switch (value) {
        .bool => |v| try self.content.print("{} ", .{v}),
        .number => |v| try self.content.print("{d} ", .{v}),
        .nil => try self.content.print("nil ", .{}),
        else => unreachable, // TODO
    }
}

/// A heap allocated string buffer
const Content = struct {
    v: std.ArrayList(u8),

    pub fn print(self: *Content, comptime fmt: []const u8, args: anytype) Allocator.Error!void {
        try self.v.writer().print(fmt, args);
    }
};
