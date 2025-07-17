/// List of all nodes in the program
nodes: []const SyntaxNode,

/// The resulting text that the entire source code will produce. This is what goes inside the file
/// after program execution finishes
output: std.ArrayList(u8),
err: ?RuntimeErrorPayload,
locals: LocalVariables,

pub fn init(allocator: Allocator, all_nodes: []const SyntaxNode) Vm {
    return Vm{
        .nodes = all_nodes,
        .output = .init(allocator),
        .locals = LocalVariables.init,
        .err = null,
    };
}

pub fn deinit(self: Vm) void {
    self.output.deinit();
    self.locals.deinit();
}

pub fn eval(self: *Vm, start_node: SyntaxNode) ![]const u8 {
    const root = ast.TextNode.toTyped(start_node).?;
    try base.evalTextNode(root, self);

    return self.output.items;
}

pub fn setError(self: *Vm, err: RuntimeErrorPayload) RuntimeError!noreturn {
    if (self.err != null) self.err = err;
    return RuntimeError.Error;
}

pub fn outputPrint(self: *Vm, comptime fmt: []const u8, args: anytype) Allocator.Error!void {
    try self.output.writer().print(fmt, args);
}

pub fn writeValue(self: *Vm, value: Value) !void {
    switch (value) {
        .bool => |v| try self.outputPrint("{} ", .{v}),
        .number => |v| try self.outputPrint("{d} ", .{v}),
        .nil => try self.outputPrint("nil ", .{}),
        else => unreachable, // TODO
    }
}

/// Set a local variable on the stack.
///
/// If the variable name does not exist on the stack, nothing will happen
pub fn setVar(self: *Vm, var_name: []const u8, value: Value) void {
    // Most programs only use variables in the top-most stack, so iterate backwards.
    var i = self.locals.count;
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

/// Stack of all local variables
///
/// A new scope is created every time a `TextNode` is visited.
const LocalVariables = struct {
    items: [256]Local,
    scope_depth: u8,
    count: u8,

    /// Local variable in a scope
    const Local = struct {
        name: []const u8,
        scope_depth: usize,
    };

    pub fn pushVar(self: *LocalVariables, var_name: []const u8) void {
        self.items[self.count] = Local{
            .scope_depth = self.scope_depth,
            .name = var_name,
        };
        self.count += 1;
    }

    pub fn pushScope(self: *LocalVariables) void {
        self.scope_depth += 1;
    }

    pub fn popScope(self: *LocalVariables) void {
        self.scope_depth -= 1;
        // Pop off all locals that have a scope greater than the new scope depth
        while (self.count > 0) {
            const v = self.items[self.count - 1];
            if (v.scope_depth > self.scope_depth) {
                // Pop off the top local
                self.count -= 1;
            } else {
                break;
            }
        }
    }

    pub const init = LocalVariables{
        .locals = undefined,
        .locals_count = 0,
        .scope_depth = 0,
    };
};

const Vm = @This();

const std = @import("std");
const Allocator = std.mem.Allocator;

const ast = @import("../syntax/ast.zig");
const SyntaxNode = @import("../syntax/node.zig").SyntaxNode;
const base = @import("base.zig");
const RuntimeError = @import("error.zig").RuntimeError;
const RuntimeErrorPayload = @import("error.zig").RuntimeErrorPayload;
const Value = @import("value.zig").Value;
