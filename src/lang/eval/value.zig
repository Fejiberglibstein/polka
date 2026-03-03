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
const sign_mask: u64    = 0x8000000000000000;
const payload_mask: u64 = 0x0000FFFFFFFFFFFF;
// zig fmt: on

const nil_value = nan_mask | (@as(u64, @intFromEnum(Tag.nil)) << 48);
const true_value = nan_mask | (@as(u64, @intFromEnum(Tag.true)) << 48);
const false_value = nan_mask | (@as(u64, @intFromEnum(Tag.false)) << 48);

pub const ValueType = enum(u8) {
    // The values of the enum are based on the values that the `Tag` enum has.
    // zig fmt: off
    number  = 0b000,
    nil     = 0b001,
    boolean = 0b010,
    object  = 0b011,
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

    pub fn isNan(self: Value) bool {
        return self.bits & ~sign_mask == nan_mask;
    }
    pub fn isNumber(self: Value) bool {
        return self.tagged.nan_mask != 0b0_11111111111_1 or self.isNan();
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

    pub fn object(o: *const Object) Value {
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
    pub const nil: Value = .{ .bits = nil_value };

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

    pub const Operators = struct {
        pub fn equal(a: Value, b: Value) Value {
            return Value.boolean(a.bits == b.bits);
        }
        pub fn not_equal(a: Value, b: Value) Value {
            return Value.boolean(!Operators.equal(a, b).asBoolean());
        }
        pub fn add(a: Value, b: Value) !Value {
            if (a.isNumber() and b.isNumber()) {
                return Value.number(a.asNumber() + b.asNumber());
            }
            return error.InvalidOperands;
        }
        pub fn subtract(a: Value, b: Value) !Value {
            if (a.isNumber() and b.isNumber()) {
                return Value.number(a.asNumber() - b.asNumber());
            }
            return error.InvalidOperands;
        }
        pub fn modulo(a: Value, b: Value) !Value {
            if (a.isNumber() and b.isNumber()) {
                return Value.number(@mod(a.asNumber(), b.asNumber()));
            }
            return error.InvalidOperands;
        }
        pub fn multiply(a: Value, b: Value) !Value {
            if (a.isNumber() and b.isNumber()) {
                return Value.number(a.asNumber() * b.asNumber());
            }
            return error.InvalidOperands;
        }
        pub fn divide(a: Value, b: Value) !Value {
            if (a.isNumber() and b.isNumber()) {
                return Value.number(a.asNumber() / b.asNumber());
            }
            return error.InvalidOperands;
        }
        pub fn less_than(a: Value, b: Value) !Value {
            if (a.isBoolean() and b.isBoolean()) {
                return Value.boolean(a.asNumber() < b.asNumber());
            }
            return error.InvalidOperands;
        }
        pub fn less_than_equal(a: Value, b: Value) !Value {
            if (a.isBoolean() and b.isBoolean()) {
                return Value.boolean(a.asNumber() <= b.asNumber());
            }
            return error.InvalidOperands;
        }
        pub fn greater_than(a: Value, b: Value) !Value {
            if (a.isBoolean() and b.isBoolean()) {
                return Value.boolean(a.asNumber() > b.asNumber());
            }
            return error.InvalidOperands;
        }
        pub fn greater_than_equal(a: Value, b: Value) !Value {
            if (a.isBoolean() and b.isBoolean()) {
                return Value.boolean(a.asNumber() >= b.asNumber());
            }
            return error.InvalidOperands;
        }
        pub fn in(a: Value, b: Value) !Value {
            _ = a;
            _ = b;
            @panic("TODO");
            // return error.InvalidOperands;
        }

        pub fn negate(a: Value) !Value {
            if (a.getNumber()) |num| return Value.number(-num);
            return error.InvalidOperand;
        }
        pub fn not(a: Value) !Value {
            if (a.getBoolean()) |b| return Value.boolean(!b);
            return error.InvalidOperand;
        }
    };

    pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self.tag()) {
            .nil => |_| try writer.writeAll("<nil>"),
            .boolean => try writer.print("{}", .{self.asBoolean()}),
            .number => try writer.print("{d}", .{self.asNumber()}),
            .object => {
                const o = self.asObject();
                switch (o.tag) {
                    .string => try writer.print("{s}", .{o.asString().slice()}),
                }
            },
        }
    }

    /// Represents a dynamically allocated value on the heap.
    ///
    /// Can be any one of
    /// - String
    ///
    /// Each different object will be implemented through struct inheritance.
    pub const Object = extern struct {
        // Any potential header information that may need to exist
        tag: Object.Kind,

        pub const Kind = enum(usize) {
            string,
        };

        pub fn from(obj: anytype) *Object {
            return @ptrCast(@alignCast(obj));
        }

        pub fn asString(self: *Object) *String {
            assert(self.tag == .string);
            return @ptrCast(@alignCast(self));
        }

        pub fn getString(self: *Object) ?*String {
            return if (self.tag == .string) self.asString() else null;
        }
    };

    pub const String = extern struct {
        base: Object = .{ .tag = .string },
        len: usize,
        ptr: [*]const u8,

        pub fn slice(self: *const String) []const u8 {
            return self.ptr[0..self.len];
        }

        pub fn init(gpa: Allocator, str: []const u8) !*Object {
            var ret = try gpa.create(String);
            ret.len = str.len;
            ret.ptr = str.ptr;
            return @ptrCast(@alignCast(ret));
        }
    };
};

test "Value numbers" {
    const numbers = [_]f64{
        0.0,               -0.0,
        0.5,               -0.5,
        1.0,               -1.0,
        std.math.nan(f64), -std.math.nan(f64),
        std.math.inf(f64), -std.math.inf(f64),
    };

    for (numbers) |num| {
        const value = Value.number(num);

        try expect(value.isNumber());
        try expect(!value.isObject());
        try expect(!value.isBoolean());
        try expect(!value.isNil());
        try expect(value.isTruthy());

        try expectEqual(std.math.isNan(num), value.isNan());
        if (!value.isNan()) {
            // IEEE 754 says nans can't be compared
            try expectEqual(value.asNumber(), num);
        }
    }
}

test "Value booleans" {
    const booleans = [_]bool{ true, false };

    for (booleans) |boolean| {
        const value = Value.boolean(boolean);

        try expect(value.isBoolean());
        try expect(!value.isObject());
        try expect(!value.isNumber());
        try expect(!value.isNil());
        try expect(value.isTruthy() == boolean);

        try expectEqual(value.asBoolean(), boolean);
    }
}

test "Value nil" {
    const value = Value.nil;

    try expect(value.isNil());
    try expect(!value.isObject());
    try expect(!value.isNumber());
    try expect(!value.isBoolean());
    try expect(!value.isTruthy());
}

test "Value object" {
    const object: Value.Object = .{ .tag = .string };
    const value = Value.object(&object);

    try expectEqual(@intFromPtr(&object), @intFromPtr(value.asObject()));

    try expect(value.isObject());
    try expect(!value.isNumber());
    try expect(!value.isBoolean());
    try expect(!value.isNil());
    try expect(value.isTruthy());
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
