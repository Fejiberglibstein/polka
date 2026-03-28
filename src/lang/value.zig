pub const Tag = enum(u3) {
    // zig fmt: off
    nan    = 0b000,
    nil    = 0b001,
    true   = 0b010,
    false  = 0b011,
    string = 0b100,
    object = 0b101,
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
    string  = 0b100,
    object  = 0b101,
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
    pub fn isString(self: Value) bool {
        return !self.isNumber() and self.tagged.tag == Tag.string;
    }
    pub fn isObject(self: Value) bool {
        return !self.isNumber() and self.tagged.tag == Tag.object;
    }

    pub fn getNumber(self: Value) ?f64 {
        return if (self.isNumber()) self.float else null;
    }
    pub fn getBoolean(self: Value) ?bool {
        return if (self.isBoolean()) self.bits == true_value else null;
    }
    pub fn getString(self: Value) ?StringPool.String {
        return if (self.isString()) self.asString() else null;
    }
    pub fn getObject(self: Value) ?*Object {
        return if (self.isObject()) self.asObject() else null;
    }

    pub fn asNumber(self: Value) f64 {
        return self.float;
    }
    pub fn asBoolean(self: Value) bool {
        return self.bits == true_value;
    }
    pub fn asString(self: Value) StringPool.String {
        return @enumFromInt(self.bits & payload_mask);
    }
    pub fn asObject(self: Value) *Object {
        return @ptrFromInt(self.bits & payload_mask);
    }

    pub fn object(o: *const Object) Value {
        return .{ .tagged = .{
            .bits = @truncate(@intFromPtr(o)),
            .tag = Tag.object,
        } };
    }
    pub fn string(s: StringPool.String) Value {
        return .{ .tagged = .{
            .bits = @intFromEnum(s),
            .tag = Tag.string,
        } };
    }
    pub fn boolean(b: bool) Value {
        return .{ .bits = if (b) true_value else false_value };
    }
    pub fn number(n: f64) Value {
        return .{ .float = n };
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

    pub const operators = struct {
        pub fn equal(a: Value, b: Value) Value {
            return Value.boolean(a.bits == b.bits);
        }
        pub fn not_equal(a: Value, b: Value) Value {
            return Value.boolean(!operators.equal(a, b).asBoolean());
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
            if (a.isNumber() and b.isNumber()) {
                return Value.boolean(a.asNumber() < b.asNumber());
            }
            return error.InvalidOperands;
        }
        pub fn less_than_equal(a: Value, b: Value) !Value {
            if (a.isNumber() and b.isNumber()) {
                return Value.boolean(a.asNumber() <= b.asNumber());
            }
            return error.InvalidOperands;
        }
        pub fn greater_than(a: Value, b: Value) !Value {
            if (a.isNumber() and b.isNumber()) {
                return Value.boolean(a.asNumber() > b.asNumber());
            }
            return error.InvalidOperands;
        }
        pub fn greater_than_equal(a: Value, b: Value) !Value {
            if (a.isNumber() and b.isNumber()) {
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

    /// Note that this should *not* be done with a format() function. This is because this function
    /// needs the *Vm so that it can print out strings correctly.
    pub fn print(
        self: Value,
        vm: *Vm,
        w: *std.Io.Writer,
    ) error{ WriteFailed, ValueError }!void {
        return switch (self.tag()) {
            .nil => error.ValueError,
            .number => w.print("{}", .{self.asNumber()}),
            .boolean => w.print("{}", .{self.asBoolean()}),
            .string => w.print("{s}", .{
                vm.string_pool.getString(self.asString()),
            }),
            .object => error.ValueError,
        };
    }
};

/// Represents a dynamically allocated value on the heap.
///
/// Can be any one of
/// - String
///
/// Each different object will be implemented through struct inheritance.
pub const Object = struct {
    tag: Kind,

    const Kind = enum {
        list,
        function,
    };

    pub fn asList(self: *Object) *List {
        assert(self.tag == .list);
        return @alignCast(@fieldParentPtr("base", self));
    }
    pub fn getList(self: *Object) ?*List {
        return if (self.tag == .list) self.asList() else null;
    }

    pub fn asFunction(self: *Object) *Function {
        assert(self.tag == .function);
        return @alignCast(@fieldParentPtr("base", self));
    }
    pub fn getFunction(self: *Object) ?*Function {
        return if (self.tag == .function) self.asFunction() else null;
    }

    pub const List = struct {
        base: Object = .{ .tag = .list },
        items: std.ArrayList(Value),

        pub fn init(alloc: Allocator) !*Object {
            var ret = try alloc.create(@This());
            ret.* = .{
                .base = .{ .tag = .list },
                .items = .empty,
            };
            return &ret.base;
        }
    };

    pub const Function = struct {
        base: Object = .{ .tag = .function },

        func: union(enum) {
            builtin: union(enum) {
                f1: *const fn (*Vm, Value) RuntimeError!Value,
                f2: *const fn (*Vm, Value, Value) RuntimeError!Value,
                f3: *const fn (*Vm, Value, Value, Value) RuntimeError!Value,
                f4: *const fn (*Vm, Value, Value, Value, Value) RuntimeError!Value,
            },
            runtime: struct {
                /// Index into the list of all nodes of this function's definition
                definition_index: u32,
                /// Number of parameters this function expects
                arity: u32,
            },
        },

        pub fn initBuiltin(func: anytype) *Object {
            const FuncType = @TypeOf(func);

            var ret: Function = .{
                .func = .{
                    .builtin = if (FuncType == fn (*Vm, Value) RuntimeError!Value)
                        .{ .f1 = &func }
                    else if (FuncType == fn (*Vm, Value, Value) RuntimeError!Value)
                        .{ .f2 = &func }
                    else if (FuncType == fn (*Vm, Value, Value, Value) RuntimeError!Value)
                        .{ .f3 = &func }
                    else if (FuncType == fn (*Vm, Value, Value, Value, Value) RuntimeError!Value)
                        .{ .f4 = &func }
                    else
                        @compileError("Invalid builtin function type " ++ @typeName(FuncType)),
                },
            };
            return &ret.base;
        }

        pub fn initRuntime(gpa: Allocator, body_index: u32, arity: u32) !*Object {
            const ret = try gpa.create(@This());
            ret.* = .{
                .base = .{ .tag = .function },
                .func = .{
                    .runtime = .{
                        .definition_index = body_index,
                        .arity = arity,
                    },
                },
            };
            return &ret.base;
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

        try expect(value.tag() == .number);
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

        try expect(value.tag() == .boolean);
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
    const object: Object = .{ .tag = .list };
    const value = Value.object(&object);

    try expectEqual(@intFromPtr(&object), @intFromPtr(value.asObject()));

    try expect(value.tag() == .object);
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

const Vm = @import("Vm.zig");
const StringPool = Vm.StringPool;
const RuntimeError = Vm.RuntimeError;
