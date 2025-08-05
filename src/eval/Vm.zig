/// List of all nodes in the program
nodes: []const SyntaxNode,

/// The resulting text that the entire source code will produce. This is what goes inside the file
/// after program execution finishes
output: std.ArrayListUnmanaged(u8),
err: ?RuntimeErrorPayload,
stack: std.BoundedArray(Value, stack_size),
locals: LocalVariables,
globals: std.StringHashMapUnmanaged(Value),
heap: Heaps,
strings: InternPool,
allocator: Allocator,

const stack_size = 256;

const InternPool = std.HashMapUnmanaged(*String, void, HashContext, 80);
const HashContext = struct {
    pub fn eql(self: HashContext, k1: *String, k2: *String) bool {
        _ = self;

        return std.mem.eql(u8, k1.get(), k2.get());
    }

    pub fn hash(self: HashContext, k: *String) u64 {
        _ = self;
        return k.hash;
    }
};

pub fn init(gpa: Allocator, all_nodes: []const SyntaxNode) !Vm {
    return Vm{
        .err = null,
        .locals = .init,
        .allocator = gpa,
        .output = .empty,
        .strings = .empty,
        .globals = .empty,
        .nodes = all_nodes,
        .stack = try .init(0),
        .heap = try .init(gpa),
    };
}

pub fn deinit(self: *Vm) void {
    self.heap.deinit(self.allocator);
    self.output.deinit(self.allocator);
    self.strings.deinit(self.allocator);
    self.globals.deinit(self.allocator);
}

pub fn eval(self: *Vm, start_node: *const SyntaxNode) RuntimeError![]const u8 {
    const root = ast.TextNode.toTyped(start_node) orelse unreachable;

    base.evalTextNode(root, self) catch |e| switch (e) {
        ControlFlow.Error => return RuntimeError.Error,
        ControlFlow.Break => try self.setError(.misplaced_break),
        ControlFlow.Return => try self.setError(.misplaced_return),
        ControlFlow.Continue => try self.setError(.misplaced_continue),
    };
    assert(self.stack.len == 0);
    assert(self.locals.count == 0);

    if (gc_logging) {
        try self.heap.collectGarbage(self);
        assert(self.heap.getCurrentHeap().len == 0);
    }

    return self.output.items;
}

pub const StackRef = struct {
    offset: usize,
    vm: *Vm,

    pub fn init(offset: usize, vm: *Vm) StackRef {
        return StackRef{
            .offset = offset,
            .vm = vm,
        };
    }

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{any}", .{self.vm.stackPeek(self.offset)});
    }
};

pub fn allocateString(self: *Vm, comptime fmt: []const u8, args: anytype) !Value {
    const string = String.init(fmt, args);

    const length = string.length + @sizeOf(String);
    const writer, const heap_string = try self.heap.allocate(self, length, String);
    // Save the index so we can reset the heap if we have an interned string already.
    const old_heap_length = self.heap.getCurrentHeap().len;

    // Any potential overflow errors won't happen since we already checked if the heap has enough
    // room for the entire string
    writer.writeStruct(string) catch unreachable;
    writer.print(fmt, args) catch unreachable;

    heap_string.computeHash();

    if (self.strings.getEntry(heap_string)) |interned_string| {
        // Because we had interned the string already, we don't need this memory allocated anymore
        //
        // TODO: Perhaps try to do interning without allocating memory?
        self.heap.getCurrentHeap().len = old_heap_length;

        return Value{ .object = @ptrCast(@alignCast(interned_string.key_ptr.*)) };
    }

    self.strings.put(self.allocator, heap_string, undefined) catch {
        try self.setError(.allocation_error);
    };
    return Value{ .object = @ptrCast(@alignCast(heap_string)) };
}

/// Called after garbage collection occurs to fix the intern pool, since after garbage collection
/// all the valid strings will have been moved to the new heap and thus have new pointers.
pub fn reinternStrings(self: *Vm) Allocator.Error!void {
    // We can't modify the hashmap since that invalidates iterators, but we can create a new hashmap
    // and move everything over.
    var new_map = InternPool.empty;
    try new_map.ensureTotalCapacity(self.allocator, self.strings.size);

    var keys = self.strings.keyIterator();
    while (keys.next()) |key| {
        if (key.*.base.tag == .moved) {
            try new_map.put(
                self.allocator,
                @ptrCast(@alignCast(key.*.base.asMoved().new_ptr)),
                undefined,
            );
        }
    }
    self.strings.deinit(self.allocator);
    self.strings = new_map;
}

pub fn allocateList(self: *Vm, length: usize) RuntimeError!Value {
    const size = @sizeOf(List) + length * @sizeOf(Value);

    _, const list = try self.heap.allocate(self, size, List);
    list.* = List{
        .base = Object{ .tag = .list },
        .length = length,
    };

    return Value {.object = @ptrCast(list)};
}

pub fn allocateClosure(self: *Vm, function_def: ast.FunctionDef) RuntimeError!Value {
    const captures_len = if (function_def.captures(self.nodes)) |v| blk: {
        var iter = v.get(self.nodes);
        break :blk iter.count();
    } else 0;

    const length = @sizeOf(Closure) + captures_len * @sizeOf(Value);
    const writer, const closure = try self.heap.allocate(self, length, Closure);

    // Any potential overflow errors won't happen since we already checked if the heap has enough
    // room for the entire string
    writer.writeStruct(Closure{
        .base = Object{ .tag = .closure },
        .function = function_def.v,
        .function_name = .init(
            if (function_def.name(self.nodes)) |ident| ident.get() else null,
        ),
        .length = captures_len,
    }) catch unreachable;

    if (function_def.captures(self.nodes)) |captures| {
        var iter = captures.get(self.nodes);

        while (iter.next()) |capture| {
            const value = try self.getVar(capture.get());
            const bytes = std.mem.asBytes(&value);

            const written = writer.write(bytes) catch unreachable;
            assert(written == bytes.len);
        }
    }

    return Value{ .object = @ptrCast(@alignCast(closure)) };
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

pub fn outputPrint(self: *Vm, comptime fmt: []const u8, args: anytype) RuntimeError!void {
    self.output.writer(self.allocator).print(fmt, args) catch try self.setError(.allocation_error);
}

pub fn pushVar(self: *Vm, var_name: []const u8) void {
    self.locals.items[self.locals.count] = LocalVariables.Local{
        .scope_depth = self.locals.scope_depth,
        .function_depth = self.locals.function_depth,
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
    //
    // Since functions push their return value(s) onto the stack, we need to make sure that after
    // popping off all locals and their respective values, we put the return values back on the top
    // of the stack.
    const depth = self.locals.function_depth;

    const original_locals = self.locals.count;
    while (self.locals.count > 0) {
        const new_count = self.locals.count - 1;

        const v = self.locals.items[new_count];
        if (v.scope_depth > self.locals.scope_depth and v.function_depth == depth) {
            // Pop off the top local
            self.locals.count = new_count;
        } else {
            break;
        }
    }

    const locals_dif: usize = original_locals - self.locals.count;
    const locals_count = self.locals.count;
    const return_values = self.stack.len - original_locals;

    // Fix the return values so that they're in the right place on the stack
    for (0..return_values) |i| {
        self.stack.buffer[locals_count + i] = self.stack.buffer[original_locals + i];
    }
    self.stack.len -= locals_dif;
}

pub fn setGlobalVar(self: *Vm, var_name: []const u8, value: Value) RuntimeError!void {
    self.globals.put(self.allocator, var_name, value) catch try self.setError(.allocation_error);
}

pub fn setVar(self: *Vm, var_name: []const u8, value: Value) RuntimeError!void {
    var i = self.locals.count - 1;

    const depth = self.locals.function_depth;
    while (i >= 0 and self.locals.items[i].function_depth == depth) : (i -= 1) {
        const variable = self.locals.items[i];
        if (std.mem.eql(u8, var_name, variable.name)) {
            self.stack.buffer[i] = value;
            return;
        }
    }
}

pub fn getVar(self: *Vm, var_name: []const u8) RuntimeError!Value {
    if (self.locals.count == 0) {
        try self.setError(.{ .undeclared_ident = var_name });
    }

    var i = self.locals.count - 1;

    const depth = self.locals.function_depth;
    while (i >= 0 and self.locals.items[i].function_depth == depth) : (i -= 1) {
        const variable = self.locals.items[i];
        if (std.mem.eql(u8, var_name, variable.name)) {
            return self.stack.get(i);
        }
    }

    if (self.globals.get(var_name)) |val| {
        return val;
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
    items: [stack_size]Local,
    count: u32,

    scope_depth: u16,
    function_depth: u16,

    /// Local variable in a scope
    const Local = struct {
        name: []const u8,
        scope_depth: u16,
        function_depth: u16,
    };

    pub const init = LocalVariables{
        .count = 0,
        .scope_depth = 0,
        .items = undefined,
        .function_depth = 0,
    };
};

/// There are two heaps used in this vm. This is so that the GC can be a moving GC, where every time
/// garbage is collected, the in-use heap has all of its remaining alive objects moved over to the
/// other heap.
pub const Heaps = struct {
    heaps: [total_heaps]*Heap,
    current_heap: u8,

    const Heap = std.BoundedArrayAligned(u8, 8, heap_size);
    const total_heaps = 2;
    const heap_size = std.math.pow(usize, 2, 24);

    pub fn init(arena: std.mem.Allocator) Allocator.Error!Heaps {
        var heaps: [total_heaps]*Heap = undefined;

        for (0..total_heaps) |i| {
            heaps[i] = try arena.create(Heap);
            heaps[i].len = 0;
        }

        return Heaps{
            .current_heap = 0,
            .heaps = heaps,
        };
    }

    pub fn deinit(self: Heaps, arena: std.mem.Allocator) void {
        for (self.heaps) |heap| {
            arena.destroy(heap);
        }
    }

    pub fn allocate(self: *Heaps, vm: *Vm, length: u64, comptime T: type) RuntimeError!struct {
        Heap.Writer,
        *T,
    } {
        if (gc_logging) {
            try self.collectGarbage(vm);
        }

        var alignment = fixAlignment(self.getCurrentHeap().len, 8);

        if (self.getCurrentHeap().len + length + alignment > heap_size) {
            try self.collectGarbage(vm);

            // Recalculate alignment since it's on a different heap now
            alignment = fixAlignment(self.getCurrentHeap().len, 8);
            // If it still exceeds, OOM
            if (self.getCurrentHeap().len + length + alignment > heap_size) {
                try vm.setError(.heap_oom);
            }
        }

        // Fix alignment
        self.getCurrentHeap().appendNTimes(undefined, alignment) catch unreachable;

        return .{ self.getCurrentHeap().writer(), self.as(T) };
    }

    inline fn getCurrentHeap(self: Heaps) *Heap {
        return self.heaps[self.current_heap];
    }

    /// Reinterprets the current spot in the heap as the type
    pub fn as(self: Heaps, comptime T: type) *align(8) T {
        return @ptrCast(@alignCast(&self.getCurrentHeap().buffer[self.getCurrentHeap().len]));
    }

    pub fn collectGarbage(self: *Heaps, vm: *Vm) RuntimeError!void {
        const old_heap = self.getCurrentHeap();
        self.current_heap = (self.current_heap + 1) % total_heaps;
        const new_heap = self.getCurrentHeap();

        // Collect garbage on the stack
        for (vm.stack.slice()) |*item| {
            collectValue(item, new_heap);
        }

        vm.reinternStrings() catch try vm.setError(.allocation_error);

        if (gc_logging) {
            logGarbage(old_heap);
        }

        // Clear out the old heap.
        @memset(old_heap.slice(), undefined);
        old_heap.len = 0;
    }

    fn collectValue(item: *Value, new_heap: *Heap) void {
        switch (item.*) {
            .object => |o| {
                switch (o.tag) {
                    .moved => item.object = o.asMoved().new_ptr,
                    .string => {
                        item.object = @ptrCast(
                            @alignCast(move(new_heap, String, o.asString())),
                        );
                    },
                    .closure => {
                        for (o.asClosure().getCaptures()) |*capture| {
                            collectValue(capture, new_heap);
                        }
                        item.object = @ptrCast(
                            @alignCast(move(new_heap, Closure, o.asClosure())),
                        );
                    },
                    else => @panic("TODO"),
                }
            },
            else => {},
        }
    }

    fn move(new_heap: *Heap, comptime T: type, value: *T) *T {
        // Fix alignment
        new_heap.appendNTimes(undefined, fixAlignment(new_heap.len, 8)) catch unreachable;
        const ptr: *T = @ptrCast(@alignCast(&new_heap.buffer[new_heap.len]));

        // Cannot overflow since value was already on the other heap.
        new_heap.appendSlice(value.asBytes()) catch unreachable;

        // Set a `Moved` where the value was on the old heap
        @as(*Moved, @ptrCast(value)).* = .{
            .base = Object{ .tag = .moved },
            .old_size = value.asBytes().len,
            .new_ptr = @ptrCast(ptr),
        };

        return ptr;
    }

    fn logGarbage(old_heap: *Heap) void {
        std.debug.print(">begin garbage collection\n", .{});

        var iterator = HeapIterator.init(old_heap);

        while (iterator.next()) |obj| {
            switch (obj.tag) {
                .moved => {},
                else => std.debug.print(" `{}` was freed\n", .{Value{ .object = obj }}),
            }
        }
    }

    const HeapIterator = struct {
        index: usize = 0,
        heap: *Heap,

        pub fn init(heap: *Heap) HeapIterator {
            return .{ .heap = heap };
        }

        pub fn next(self: *HeapIterator) ?*Object {
            // Fix alignment
            self.index += fixAlignment(self.index, 8);

            if (self.index >= self.heap.len) {
                return null;
            }

            const obj: *Object = @ptrCast(&self.heap.buffer[self.index]);

            // Move past the object that used to be here.
            self.index += switch (obj.tag) {
                .moved => obj.asMoved().old_size,
                .string => obj.asString().asBytes().len,
                .closure => obj.asClosure().asBytes().len,
                else => @panic("TODO"),
            };

            return obj;
        }
    };
};

inline fn fixAlignment(x: usize, alignment: usize) usize {
    // Equivalent to
    // ```
    // if (x % 8 == 0) {
    //     return 0;
    // } else {
    //     return 8 - (x % 8);
    // }
    // ```
    return (~x +% 1) & (alignment - 1);
}

const Vm = @This();

const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const gc_logging = @import("build_options").gc_logging;

const ast = @import("../syntax/ast.zig");
const SyntaxNode = @import("../syntax/node.zig").SyntaxNode;
const base = @import("nodes.zig");
const Moved = @import("value.zig").Moved;
const Object = @import("value.zig").Object;
const RuntimeError = @import("error.zig").RuntimeError;
const ControlFlow = @import("error.zig").ControlFlow;
const RuntimeErrorPayload = @import("error.zig").RuntimeErrorPayload;
const String = @import("value.zig").String;
const Closure = @import("value.zig").Closure;
const List = @import("value.zig").List;
const Value = @import("value.zig").Value;
