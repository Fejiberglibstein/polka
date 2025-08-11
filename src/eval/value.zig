const std = @import("std");
const Allocator = std.mem.Allocator;
const Vm = @import("Vm.zig");
const SyntaxNode = @import("../syntax/node.zig").SyntaxNode;
const assert = std.debug.assert;

pub const Tag = enum(u3) {
    // zig fmt: off
    nan    = 0b000,
    nil    = 0b001,
    true   = 0b010,
    false  = 0b011,
    object = 0b100,
    // zig fmt: on
};

// zig fmt: off
const nan_mask: u64     = 0x7FF8000000000000;
const tag_mask: u64     = 0x0007000000000000;
const payload_mask: u64 = 0x0000FFFFFFFFFFFF;
// zig fmt: on

const nil_value = nan_mask & (@as(u64, @intFromEnum(Tag.nil)) << 48);
const true_value = nan_mask & (@as(u64, @intFromEnum(Tag.true)) << 48);
const false_value = nan_mask & (@as(u64, @intFromEnum(Tag.false)) << 48);

pub const ValueType = enum(u8) {
    // The values of the enum are based on the values that the `Tag` enum has.
    // zig fmt: off
    number  = 0b000,
    nil     = 0b001,
    boolean = 0b010,
    object  = 0b100,
    // zig fmt: on
};

/// `Value` is the primitive type. It supports nils, booleans, numbers, and object pointers.
///
/// Value is NaN-boxed so that it fits in 8 bytes. The details of the implementation are based on
/// https://github.com/SimonMeskens/zig-nan-boxing/blob/main/src/lib.zig and
/// https://craftinginterpreters.com/optimization.html#nan-boxing.
///
/// The bits of a value when it is not a number look like
///         +--- NAN MASK ----+ TAG +-------------------- PAYLOAD ---------------------+
///         | 0_11111111111_1 | xxx | yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy |
///         +-----------------+-----+--------------------------------------------------+
pub const Value = packed union {
    float: f64,
    /// Raw bits of the value
    bits: u64,
    tagged: packed struct {
        bits: u48,
        tag: Tag,
        nan_mask: u13 = 0b0_11111111111_1,
    },

    pub fn isNaN(self: Value) bool {
        return self.bits == nan_mask;
    }
    pub fn isNumber(self: Value) bool {
        return self.tagged.nan_mask != 0b0_11111111111_1 or self.isNaN();
    }
    pub fn isBoolean(self: Value) bool {
        return self.bits == true_value or self.bits == false_value;
    }
    pub fn isNil(self: Value) bool {
        return self.bits == nil_value;
    }
    pub fn isObject(self: Value) bool {
        return !self.isNumber() and self.tagged.tag == Tag.object;
    }

    pub fn getNumber(self: Value) ?f64 {
        return if (self.isNumber()) self.float else null;
    }
    pub fn getObject(self: Value) ?*Object {
        return if (self.isObject()) @ptrFromInt(self.bits & payload_mask) else null;
    }
    pub fn getBoolean(self: Value) ?bool {
        return if (self.isBoolean()) self.bits == true_value else null;
    }

    pub fn asNumber(self: Value) f64 {
        return self.float;
    }
    pub fn asObject(self: Value) *Object {
        return @ptrFromInt(self.bits & payload_mask);
    }
    pub fn asBoolean(self: Value) bool {
        return self.bits == true_value;
    }

    pub fn nil() Value {
        return Value{ .bits = nil_value };
    }
    pub fn object(o: *Object) Value {
        return Value{ .tagged = .{
            .bits = @truncate(@intFromPtr(o)),
            .tag = Tag.object,
        } };
    }
    pub fn boolean(b: bool) Value {
        return Value{ .bits = if (b) true_value else false_value };
    }
    pub fn number(n: f64) Value {
        return Value{ .float = n };
    }

    pub fn tag(self: Value) ValueType {
        if (self.isNumber()) {
            return ValueType.number;
        }
        if (self.isBoolean()) {
            return ValueType.boolean;
        }

        return @enumFromInt(@as(u8, @intFromEnum(self.tagged.tag)));
    }

    pub fn isTruthy(self: Value) bool {
        return self.bits != nil_value and self.bits != false_value;
    }

    pub fn equal(a: Value, b: Value) bool {
        return a.bits == b.bits;
    }

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self.tag()) {
            .nil => |_| try writer.writeAll("<nil>"),
            .boolean => try writer.print("{}", .{self.asBoolean()}),
            .number => try writer.print("{d}", .{self.asNumber()}),
            .object => {
                const o = self.asObject();
                switch (self.asObject().tag) {
                    .string => try writer.print("{s}", .{o.asString().get()}),
                    .list => try writer.print("<list@{x}>", .{@intFromPtr(o)}),
                    .dict => try writer.print("<dict@{x}>", .{@intFromPtr(o)}),
                    .closure => try writer.print("<function@{x}>", .{@intFromPtr(o)}),
                    .moved => unreachable,
                }
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
    /// We need to make the size of this struct at least 24 bytes, since that's the size of `Moved`
    _padding: [8]u8 = undefined,
    /// Start of the flexible length Value array for the list
    items: void = undefined,

    pub fn get(self: *List, index: i64) ?Value {
        return if (index > self.length or index < 0)
            null
        else
            self.getValues()[@as(u32, @intCast(index))];
    }

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
    /// We need to make the size of this struct at least 24 bytes, since that's the size of `Moved`
    _padding: [8]u8 = undefined,
    /// Start of the flexible length Value array for the list
    items: void = undefined,

    pub const KeyPair = struct {
        key: *String,
        value: Value,
    };

    pub fn get(self: *Dict, index: *String) ?Value {
        for (self.getKeyPairs()) |pair| {
            if (pair.key.equals(index)) {
                return pair.value;
            }
        }
        return null;
    }

    pub fn getKeyPairs(self: *Dict) []KeyPair {
        const ptr: [*]KeyPair = @ptrCast(&self.items);
        return ptr[0..self.length];
    }

    pub fn asBytes(self: *const Dict) []align(8) const u8 {
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

    pub fn equals(self: *const String, other: *const String) bool {
        // Because we do string interning, all strings with the same value have the same address.
        return self == other;
    }

    /// Get the bytes of the entire struct, including characters.
    pub fn asBytes(self: *const String) []align(8) const u8 {
        const ptr: [*]const u8 = @ptrCast(self);
        return @alignCast(ptr[0 .. self.length + @sizeOf(String)]);
    }
};
