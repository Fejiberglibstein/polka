const std = @import("std");
const Allocator = std.mem.Allocator;
const Vm = @import("Vm.zig");

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
    object: Object,

    pub fn isTruthy(self: Value) bool {
        // Lua-like truthy values.
        return switch (self) {
            .nil => false,
            .bool => |b| b,
            else => true,
        };
    }

    pub fn toString(self: Value, buf: std.ArrayList(u8).Writer) !void {
        switch (self) {
            .nil => try buf.print("<nil>", .{}),
            .bool => |v| try buf.print("{}", .{v}),
            .number => |v| try buf.print("{d}", .{v}),
            else => @panic("TODO"),
        }
    }
};

pub const ObjectType = enum(u8) {
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
};

pub const String = extern struct {
    base: Object,
    length: u32,
    /// Pre-computed hash of the string
    hash: u32,
    // The characters of the string are stored after the length. This struct is the equivalent of
    // ```
    // struct foo {
    //     size_t length;
    //     char rest[];
    // };
    // ```
    // in c
};
