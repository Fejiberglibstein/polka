/// List of all nodes in the program
nodes: []const SyntaxNode,
/// The resulting text that the language produces
content: Content,
err: ?RuntimeErrorPayload,
stack: Stack,

pub fn init(allocator: Allocator, all_nodes: []const SyntaxNode) Vm {
    return Vm{
        .nodes = all_nodes,
        .content = Content{ .v = .init(allocator) },
        .stack = Stack.init(allocator),
        .err = null,
    };
}

pub fn deinit(self: Vm) void {
    self.content.v.deinit();
    self.stack.deinit();
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

/// Set a local variable on the stack.
///
/// If the variable name does not exist on the stack, nothing will happen
pub fn setVar(self: *Vm, var_name: []const u8, value: Value) void {
    // Most programs only use variables in the top-most stack, so iterate backwards.
    var i = self.stack.locals.items.len;
    while (i >= 0) {
        i -= 1;
        const local = &self.stack.locals.items[i];

        if (std.mem.eql(u8, var_name, local.name)) {
            local.value = value;
            return;
        }
    }
    // Do nothing if we didn't find any variable that matched
}

pub fn getVar(self: *Vm, var_name: []const u8) RuntimeError!Value {
    // Most programs only use variables in the top-most stack, so iterate backwards.
    var i = self.stack.locals.items.len;
    while (i >= 0) {
        i -= 1;
        const local = &self.stack.locals.items[i];

        if (std.mem.eql(u8, var_name, local.name)) {
            return local.value;
        }
    }

    try self.setError(.{ .undeclared_ident = var_name });
}

/// Local variable in a scope
const Local = struct {
    name: []const u8,
    value: Value,
    scope_depth: usize,
};

const Stack = struct {
    /// Stack of all local variables
    ///
    /// We create a new stack every time we visit a `TextNode`
    locals: std.ArrayList(Local),
    scope_depth: usize,

    pub fn pushVar(self: *Stack, var_name: []const u8, value: Value) Allocator.Error!void {
        try self.locals.append(Local{
            .scope_depth = self.scope_depth,
            .value = value,
            .name = var_name,
        });
    }

    pub fn pushScope(self: *Stack) void {
        self.scope_depth += 1;
    }

    pub fn popScope(self: *Stack) void {
        self.scope_depth -= 1;
        // Pop off all locals that have a scope greater than the new scope depth
        while (self.locals.pop()) |v| {
            if (v.scope_depth <= self.scope_depth) {
                break;
            }
        }
    }

    pub fn init(allocator: Allocator) Stack {
        return .{
            .locals = .init(allocator),
            .scope_depth = 0,
        };
    }
    pub fn deinit(self: Stack) void {
        self.locals.deinit();
    }
};

const Vm = @This();

const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("../runtime/value.zig").Value;
const ast = @import("../syntax/ast.zig");
const RuntimeErrorPayload = @import("error.zig").RuntimeErrorPayload;
const RuntimeError = @import("error.zig").RuntimeError;

const SyntaxNode = @import("../syntax/node.zig").SyntaxNode;
const base = @import("base.zig");
