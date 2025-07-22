/// List of all nodes in the program
nodes: []const SyntaxNode,

/// The resulting text that the entire source code will produce. This is what goes inside the file
/// after program execution finishes
output: std.ArrayList(u8),
err: ?RuntimeErrorPayload,
stack: Stack,
locals: LocalVariables,
heap: Heap,
strings: std.HashMap(*String, void, HashContext, 80),

const HashContext = struct {
    pub fn eql(self: HashContext, k1: *String, k2: *String) bool {
        _ = self;

        return std.mem.eql(u8, k1, k2);
    }

    pub fn hash(self: HashContext, k: *String) u64 {
        _ = self;
        return k.hash;
    }
};

pub fn init(gpa: Allocator, all_nodes: []const SyntaxNode) !Vm {
    return Vm{
        .nodes = all_nodes,
        .output = .init(gpa),
        .locals = .init,
        .err = null,
        .stack = .init,
        .heap = try .init(gpa),
        .strings = .init(gpa),
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

pub fn stackPop(self: *Vm) Value {
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

pub fn pushVar(self: *Vm, var_name: []const u8) void {
    self.locals.items[self.locals.count] = LocalVariables.Local{
        .scope_depth = self.locals.scope_depth,
        .name = var_name,
    };
    self.locals.count += 1;
}

pub fn pushScope(self: *Vm) void {
    self.locals.scope_depth += 1;
}

pub fn popScope(self: *Vm) void {
    self.locals.scope_depth -= 1;
    // Pop off all locals that have a scope greater than the new scope depth
    while (self.locals.count > 0) {
        const v = self.locals.items[self.locals.count - 1];
        if (v.scope_depth > self.locals.scope_depth) {
            // Pop off the top local
            self.locals.count -= 1;
            self.stack.count -= 1;
        } else {
            break;
        }
    }
}

pub fn setVar(self: *Vm, var_name: []const u8, value: Value) RuntimeError!void {
    var i = self.locals.count - 1;
    while (i >= 0) : (i -= 1) {
        const variable = self.locals.items[i];
        if (std.mem.eql(u8, var_name, variable.name)) {
            self.stack.items[i] = value;
            return;
        }
    }
}

pub fn getVar(self: *Vm, var_name: []const u8) RuntimeError!Value {
    var i = self.locals.count - 1;
    while (i >= 0) : (i -= 1) {
        const variable = self.locals.items[i];
        if (std.mem.eql(u8, var_name, variable.name)) {
            return self.stack.items[i];
        }
    }

    try self.setError(.{ .undeclared_ident = var_name });
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

    pub const init = LocalVariables{
        .items = undefined,
        .count = 0,
        .scope_depth = 0,
    };
};

/// There are two heaps used in this vm. This is so that the GC can be a moving GC, where every time
/// garbage is collected, the in-use heap has all of its remaining alive objects moved over to the
/// other heap.
pub const Heap = struct {
    heaps: [HEAPS]std.ArrayListUnmanaged(u8),
    current_heap: u8,
    buffers: [HEAPS][]u8,

    const HEAPS = 2;
    const HEAP_SIZE = std.math.pow(usize, 2, 24);

    pub fn init(arena: std.mem.Allocator) Allocator.Error!Heap {
        const heaps: [HEAPS]std.ArrayListUnmanaged(u8) = undefined;
        const buffers: [HEAPS][]u8 = undefined;

        for (0..HEAPS) |i| {
            const buf = arena.alloc(u8, HEAP_SIZE);
            const heap = std.ArrayListUnmanaged(u8).initBuffer(buf);

            buffers[i] = buf;
            heaps[i] = heap;
        }

        return Heap{
            .current_heap = 0,
            .buffers = buffers,
            .heaps = heaps,
        };
    }
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
const String = @import("value.zig").String;
const Object = @import("value.zig").Object;
