/// List of all nodes in the program
nodes: []const SyntaxNode,

/// The resulting text that the entire source code will produce. This is what goes inside the file
/// after program execution finishes
output: std.ArrayList(u8),
err: ?RuntimeErrorPayload,
stack: Stack,
locals: LocalVariables,

pub fn init(allocator: Allocator, all_nodes: []const SyntaxNode) Vm {
    return Vm{
        .nodes = all_nodes,
        .output = .init(allocator),
        .locals = LocalVariables.init,
        .err = null,
        .stack = .init,
    };
}

pub fn deinit(self: Vm) void {
    self.output.deinit();
}

pub fn stackPush(self: *Vm, value: Value) RuntimeError!void {
    if (self.stack.count >= Stack.MAX_STACK_SIZE) {
        try self.setError(.stack_overflow);
    }

    self.stack.items[self.stack.count] = value;
    self.stack.count += 1;
}

pub fn stackPop(self: *Vm) void {
    self.stack.count -= 1;
    return self.stack.items[self.stack.count];
}

pub fn stackPeek(self: *Vm, back: usize) Value {
    return self.stack.items[self.stack.count - back - 1];
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

/// Stack containing all values currently in scope
const Stack = struct {
    items: [MAX_STACK_SIZE]Value,
    count: u8,

    pub const MAX_STACK_SIZE = 256;
    pub const init = Stack{
        .items = undefined,
        .count = 0,
    };
};

/// All the local variable names.
///
/// Each local variable matches up with a value on the stack, e.g.
///
/// ```
/// #* let x = 6
/// #* let b = 4
/// ```
///
/// LOCALS: ["x"]["b"]
/// STACK:  [ 6 ][ 4 ]
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
        .items = undefined,
        .count = 0,
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
