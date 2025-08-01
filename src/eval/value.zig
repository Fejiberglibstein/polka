const std = @import("std");
const Allocator = std.mem.Allocator;
const Vm = @import("Vm.zig");
const assert = std.debug.assert;

pub const ValueType = enum(u8) {
    nil,
    bool,
    number,
    object,
};

pub const Value = union(ValueType) {
    nil: void,
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

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .nil => try writer.writeAll("<nil>"),
            .bool => |v| try writer.print("{}", .{v}),
            .number => |v| try writer.print("{d}", .{v}),
            .object => |o| switch (o.tag) {
                .string => try writer.print("{s}", .{o.asString().get()}),
                .freed => unreachable,
                else => @panic("TODO"),
            },
        }
    }

};

pub const ObjectType = enum(u8) {
    /// If the object has already been freed by the garbage collector when cleaning up
    freed,
    string,
    list,
    dict,
};

/// Represents a dynamically allocated value on the heap.
///
/// Can be any one of
/// - String
/// - List
/// - Dict
///
/// Each different object will be implemented through struct inheritance.
pub const Object = extern struct {
    // Any potential header information that may need to exist
    tag: ObjectType,

    pub fn asFreed(self: *Object) *Freed {
        assert(self.tag == .freed);
        return @ptrCast(@alignCast(self));
    }

    pub fn asString(self: *Object) *String {
        assert(self.tag == .string);
        return @ptrCast(@alignCast(self));
    }
};

/// While garbage is being collected, a pointer to the same object can be on the stack in two
/// different spots.
///
/// To prevent the object from being moved to the new heap twice, when an object is moved the first
/// time, the value on the old heap is set to `Freed`. This way, when the other pointer is reached
/// on the stack, it can find the new ptr and update itself to point to the object on the new heap.
///
/// The `old_length` is the total size of the object that used to be on the heap, including the
/// struct header in addition to the data.
pub const Freed = extern struct {
    base: Object,
    /// The location of the object on the new heap
    new_ptr: *Object,
    /// The size of the object that used to be in this spot
    old_size: u64,
};

pub const String = extern struct {
    base: Object,
    length: u64,
    /// Pre-computed hash of the string
    hash: u64,
    /// Start of the flexible length character array for the string
    body: void = undefined,

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
    pub fn get(self: *String) []const u8 {
        const ptr: [*]u8 = @ptrCast(&self.body);
        return ptr[0..self.length];
    }

    /// Get the bytes of the entire struct, including characters.
    pub fn asBytes(self: *String) []align(8) const u8 {
        const ptr: [*]u8 = @ptrCast(self);
        return @alignCast(ptr[0 .. self.length + @sizeOf(String)]);
    }
};
