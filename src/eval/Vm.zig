/// List of all nodes in the program
nodes: []const SyntaxNode,

/// The resulting text that the entire source code will produce. This is what goes inside the file
/// after program execution finishes
output: std.ArrayListUnmanaged(u8),
err: ?RuntimeErrorPayload,
stack: std.BoundedArray(Value, stack_size),
locals: LocalVariables,
heap: Heap,
strings: std.HashMapUnmanaged(*String, void, HashContext, 80),
allocator: Allocator,

const stack_size = 256;

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
        .output = try .initCapacity(gpa, 0),
        .locals = .init,
        .err = null,
        .stack = try .init(0),
        .heap = try .init(gpa),
        .strings = .empty,
        .allocator = gpa,
    };
}

pub fn deinit(self: *Vm) void {
    self.output.deinit(self.allocator);
    self.strings.deinit(self.allocator);
    self.heap.deinit(self.allocator);
}

pub fn eval(self: *Vm, start_node: SyntaxNode) ![]const u8 {
    const root = ast.TextNode.toTyped(start_node).?;
    try base.evalTextNode(root, self);

    return self.output.items;
}

pub fn allocateString(self: *Vm, comptime fmt: []const u8, args: anytype) !Value {
    const string = String.init(fmt, args);

    const length = string.length + @sizeOf(String);

    const writer = try self.heap.allocate(self, length);
    const heapString = self.heap.as(String);
    // Any potential overflow errors won't happen since we already checked if the heap has enough
    // room for the entire string

    writer.writeStruct(string) catch unreachable;

    writer.print(fmt, args) catch unreachable;
    heapString.computeHash();

    return Value{ .object = @ptrCast(@alignCast(heapString)) };
}

pub fn stackPush(self: *Vm, value: Value) RuntimeError!void {
    self.stack.append(value) catch {
        try self.setError(.stack_overflow);
    };
}

pub fn stackPop(self: *Vm) Value {
    return self.stack.pop() orelse unreachable;
}

pub fn stackPeek(self: *Vm, back: usize) Value {
    return self.stack.get(self.stack.len - back - 1);
}

pub fn setError(self: *Vm, err: RuntimeErrorPayload) RuntimeError!noreturn {
    if (self.err != null) self.err = err;
    return RuntimeError.Error;
}

pub fn outputPrint(self: *Vm, comptime fmt: []const u8, args: anytype) Allocator.Error!void {
    try self.output.writer(self.allocator).print(fmt, args);
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
            self.stack.len -= 1;
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
            self.stack.buffer[i] = value;
            return;
        }
    }
}

pub fn getVar(self: *Vm, var_name: []const u8) RuntimeError!Value {
    var i = self.locals.count - 1;
    while (i >= 0) : (i -= 1) {
        const variable = self.locals.items[i];
        if (std.mem.eql(u8, var_name, variable.name)) {
            return self.stack.get(i);
        }
    }

    try self.setError(.{ .undeclared_ident = var_name });
}

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
    heaps: [total_heaps]*Buffer,
    current_heap: u8,

    const Buffer = std.BoundedArrayAligned(u8, 8, heap_size);
    const total_heaps = 2;
    const heap_size = std.math.pow(usize, 2, 24);

    pub fn init(arena: std.mem.Allocator) Allocator.Error!Heap {
        var heaps: [total_heaps]*Buffer = undefined;

        for (0..total_heaps) |i| {
            heaps[i] = try arena.create(Buffer);
            heaps[i].len = 0;
        }

        return Heap{
            .current_heap = 0,
            .heaps = heaps,
        };
    }

    pub fn deinit(self: Heap, arena: std.mem.Allocator) void {
        for (self.heaps) |heap| {
            arena.destroy(heap);
        }
    }

    pub fn allocate(self: *Heap, vm: *Vm, length: u64) RuntimeError!Buffer.Writer {
        const alignment = 8 - self.getCurrentHeap().len % 8;

        // // For testing purposes
        // self.collectGarbage(vm);

        if (self.getCurrentHeap().len + length + alignment > heap_size) {
            self.collectGarbage(vm);

            // If it still exceeds, OOM
            if (self.getCurrentHeap().len + length > heap_size) {
                try vm.setError(.out_of_memory);
            }
        }
        // Fix alignment
        self.getCurrentHeap().appendNTimes(undefined, alignment) catch unreachable;

        return self.getCurrentHeap().writer();
    }

    inline fn getCurrentHeap(self: Heap) *Buffer {
        return self.heaps[self.current_heap];
    }

    fn collectGarbage(self: *Heap, vm: *Vm) void {
        const old_heap = self.getCurrentHeap();
        self.current_heap = (self.current_heap + 1) % total_heaps;
        const new_heap = self.getCurrentHeap();

        for (vm.stack.slice()) |*item| {
            switch (@as(Value, item.*)) {
                .object => |o| {
                    switch (o.tag) {
                        .freed => item.object = o.asFreed().new_ptr,
                        .string => {
                            item.object = @ptrCast(@alignCast(move(new_heap, o.asString())));
                        },
                        else => @panic("TODO"),
                    }
                },
                else => {},
            }
        }

        // Clear out the old heap.
        old_heap.len = 0;
    }

    /// Reinterprets the current spot in the heap as the type
    pub fn as(self: Heap, comptime T: type) *align(8) T {
        return @ptrCast(@alignCast(&self.getCurrentHeap().buffer[self.getCurrentHeap().len]));
    }

    fn move(new_heap: *Buffer, value: anytype) @TypeOf(value) {
        // Fix alignment
        new_heap.appendNTimes(undefined, 8 - new_heap.len % 8) catch unreachable;
        const ptr = &new_heap.buffer[new_heap.len];

        // Cannot overflow since value was already on the other heap.
        new_heap.appendSlice(value.asBytes()) catch unreachable;

        return @ptrCast(@alignCast(ptr));
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
