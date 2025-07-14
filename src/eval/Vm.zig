const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("../runtime/value.zig").Value;
const ast = @import("../syntax/ast.zig");

const SyntaxNode = @import("../syntax/node.zig").SyntaxNode;
const base = @import("base.zig");

const Vm = @This();

/// List of all nodes in the program
nodes: []const SyntaxNode,
// A string of the entire content of the finished file
result_content: std.ArrayList(u8),

pub fn init(allocator: Allocator, all_nodes: []const SyntaxNode) Vm {
    return Vm{
        .nodes = all_nodes,
        .result_content = .init(allocator),
    };
}

pub fn deinit(self: Vm) void {
    self.result_content.deinit();
}

pub fn writeContent(self: *Vm, comptime fmt: []const u8, args: anytype) Allocator.Error!void {
    try self.result_content.writer().print(fmt, args);
}

pub fn eval(self: *Vm, start_node: SyntaxNode) ![]const u8 {
    const root = ast.TextNode.toTyped(start_node).?;
    try base.evalTextNode(root, self);

    return self.result_content.items;
}

pub fn writeValue(self: *Vm, value: Value) !void {
    switch (value) {
        .bool => |v| try self.writeContent("{b}", .{v}),
        .number => |v| try self.writeContent("{f}", .{v}),
        .nil => try self.writeContent("nil", .{}),
        else => unreachable, // TODO
    }
}
