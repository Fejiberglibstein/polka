const std = @import("std");
const Allocator = std.mem.Allocator;
const Vm = @import("Vm.zig");
const SyntaxNode = @import("../syntax/node.zig").SyntaxNode;
const assert = std.debug.assert;

pub const ValueType = enum(u8) {
    nil,
    bool,
    number,
    object,
};

pub const Value = union(ValueType) {
    nil,
    bool: bool,
    number: f64,
    object: *Object,

    pub fn isTruthy(self: Value) bool {
        // Lua-like truthy values.
        return switch (self) {
            .nil => false,
            .bool => |b| b,
            else => true,
        };
    }

    pub fn equal(a: Value, b: Value) bool {
        if (@intFromEnum(a) != @intFromEnum(b)) {
            return false;
        }

        return switch (a) {
            .object => |o| switch (o.tag) {
                .string => o == b.object,
                .moved => unreachable,
                else => @panic("TODO"),
            },
            inline else => |v, tag| @field(b, @tagName(tag)) == v,
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

        switch (self) {
            .nil => |_| try writer.writeAll("<nil>"),
            .bool => |v| try writer.print("{}", .{v}),
            .number => |v| try writer.print("{d}", .{v}),
            .object => |o| switch (o.tag) {
                .string => try writer.print("{s}", .{o.asString().get()}),
                .closure => try writer.print("<function@{x}>", .{@intFromPtr(o)}),
                .moved => unreachable,
                else => @panic("TODO"),
            },
        }
    }
};

pub const ObjectType = enum(u8) {
    /// If the object has already been moved to the new heap by the garbage collector during
    /// collection.
    moved,
    string,
    list,
    dict,
    closure,
};

/// Represents a dynamically allocated value on the heap.
///
/// Can be any one of
/// - String
/// - List
/// - Dict
/// - Closure
///
/// Each different object will be implemented through struct inheritance.
pub const Object = extern struct {
    // Any potential header information that may need to exist
    tag: ObjectType,

    pub inline fn asList(self: *Object) *List {
        assert(self.tag == .list);
        return @ptrCast(@alignCast(self));
    }

    pub inline fn asDict(self: *Object) *Dict {
        assert(self.tag == .dict);
        return @ptrCast(@alignCast(self));
    }

    pub inline fn asMoved(self: *Object) *Moved {
        assert(self.tag == .moved);
        return @ptrCast(@alignCast(self));
    }

    pub inline fn asString(self: *Object) *String {
        assert(self.tag == .string);
        return @ptrCast(@alignCast(self));
    }

    pub inline fn asClosure(self: *Object) *Closure {
        assert(self.tag == .closure);
        return @ptrCast(@alignCast(self));
    }

    pub inline fn getList(self: *Object) ?*List {
        return if (self.tag == .list) self.asList() else null;
    }

    pub inline fn getDict(self: *Object) ?*Dict {
        return if (self.tag == .dict) self.asDict() else null;
    }

    pub inline fn getMoved(self: *Object) ?*Moved {
        return if (self.tag == .moved) self.asMoved() else null;
    }

    pub inline fn getString(self: *Object) ?*String {
        return if (self.tag == .string) self.asString() else null;
    }

    pub inline fn getClosure(self: *Object) ?*Closure {
        return if (self.tag == .closure) self.asClosure() else null;
    }
};

/// While garbage is being collected, a pointer to the same object can be on the stack in two
/// different spots.
///
/// To prevent the object from being moved to the new heap twice, when an object is moved the first
/// time, the value on the old heap is set to `Moved`. This way, when the other pointer is reached
/// on the stack, it can find the new ptr and update itself to point to the object on the new heap.
///
/// The `old_length` is the total size of the object that used to be on the heap, including the
/// struct header in addition to the data.
pub const Moved = extern struct {
    base: Object,
    /// The location of the object on the new heap
    new_ptr: *Object,
    /// The size of the object that used to be in this spot
    old_size: u64,
};

pub const Closure = extern struct {
    base: Object,
    /// The syntax node for the function. The type of the node is `FunctionDef`
    function: *const SyntaxNode,
    /// Name of the function
    function_name: Name,
    /// Length of all the captures
    length: usize,
    /// Start of the flexible length array of Values for the captures
    captures: void = undefined,

    /// Done because you cannot have slices in extern structs
    const Name = extern struct {
        ptr: ?[*]const u8,
        len: usize,

        pub fn init(_s: ?[]const u8) Name {
            return if (_s) |s|
                Name{ .ptr = s.ptr, .len = s.len }
            else
                Name{ .ptr = null, .len = 0 };
        }

        pub fn slice(self: Name) ?[]const u8 {
            return if (self.ptr) |ptr|
                ptr[0..self.len]
            else
                null;
        }
    };

    pub fn getCaptures(self: *Closure) []Value {
        const ptr: [*]Value = @ptrCast(&self.captures);
        return ptr[0..self.length];
    }

    pub fn asBytes(self: *const Closure) []align(8) const u8 {
        const ptr: [*]const u8 = @ptrCast(self);
        const size = (self.length * @sizeOf(Value)) + @sizeOf(Closure);
        return @alignCast(ptr[0..size]);
    }
};

pub const List = extern struct {
    base: Object,
    length: u64,
    /// Start of the flexible length Value array for the list
    items: void = undefined,

    pub fn getValues(self: *List) []Value {
        const ptr: [*]Value = @ptrCast(&self.items);
        return ptr[0..self.length];
    }

    pub fn asBytes(self: *const List) []align(8) const u8 {
        const ptr: [*]const u8 = @ptrCast(self);
        const size = (self.length * @sizeOf(Value)) + @sizeOf(List);
        return @alignCast(ptr[0..size]);
    }
};

pub const Dict = extern struct {
    base: Object,
    length: u64,
    /// Start of the flexible length Value array for the list
    items: void = undefined,

    pub const KeyPair = struct {
        key: *String,
        value: Value,
    };

    pub fn getKeyPairs(self: *List) []KeyPair {
        const ptr: [*]Value = @ptrCast(&self.items);
        return ptr[0..self.length];
    }

    pub fn asBytes(self: *const List) []align(8) const u8 {
        const ptr: [*]const u8 = @ptrCast(self);
        const size = (self.length * @sizeOf(KeyPair)) + @sizeOf(Dict);
        return @alignCast(ptr[0..size]);
    }
};

pub const String = extern struct {
    base: Object,
    length: u64,
    /// Pre-computed hash of the string
    hash: u64,
    /// Start of the flexible length character array for the string
    chars: void = undefined,

    /// Create a string with the length and base set. Neither the hash nor the character array is
    /// created.
    pub fn init(comptime fmt: []const u8, args: anytype) String {
        const length = std.fmt.count(fmt, args);

        return String{
            .base = .{ .tag = .string },
            .length = length,
            // Hash will be computed later, after the string has its character array added.
            .hash = 0,
        };
    }

    /// Computes the hash for the string. This function should only be called after the string has
    /// had its character array added to it.
    pub fn computeHash(self: *String) void {
        self.hash = std.hash.Wyhash.hash(0, self.get());
    }

    /// Get the characters of the string
    pub fn get(self: *const String) []const u8 {
        const ptr: [*]const u8 = @ptrCast(&self.chars);
        return ptr[0..self.length];
    }

    /// Get the bytes of the entire struct, including characters.
    pub fn asBytes(self: *const String) []align(8) const u8 {
        const ptr: [*]const u8 = @ptrCast(self);
        return @alignCast(ptr[0 .. self.length + @sizeOf(String)]);
    }
};
