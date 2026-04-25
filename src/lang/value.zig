// zig fmt: off
const nan_mask:     u64 = 0x7FF8000000000000;
const tag_mask:     u64 = 0x0007000000000000;
const sign_mask:    u64 = 0x8000000000000000;
const payload_mask: u64 = 0x0000FFFFFFFFFFFF;
// zig fmt: on

const tag_offset = @ctz(tag_mask);

/// `Value` is the primitive type in the language. It can be
/// - nil
/// - true
/// - false
/// - any 64 bit floating point value
/// - A string
/// - Pointer to an allocated object
pub const Value = packed union {
    // Value is NaN-boxed so that it fits in 8 bytes. The details of the implementation are based on
    // https://github.com/SimonMeskens/zig-nan-boxing/blob/main/src/lib.zig and
    // https://craftinginterpreters.com/optimization.html#nan-boxing.
    //
    // The bits of a value when it is not a number look like
    //         +--- NAN MASK ----+ TAG +-------------------- PAYLOAD ---------------------+
    //         | 0_11111111111_1 | xxx | yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy |
    //         +-----------------+-----+--------------------------------------------------+

    /// Prefer to use the `.number()` function instead.
    float: f64,
    /// Raw bits of the value
    bits: u64,
    tagged: packed struct(u64) {
        bits: u48,
        tag: Tag,
        // This must never change for tagged types.
        nan_mask: u13 = 0b0_11111111111_1,

        const Tag = enum(u3) {
            nan,
            nil,
            true,
            false,
            color,
            string,
            object,
        };
    },

    const true_value: Value = .{ .tagged = .{ .bits = 0, .tag = .true } };
    const false_value: Value = .{ .tagged = .{ .bits = 0, .tag = .false } };
    pub const nil: Value = .{ .tagged = .{ .bits = 0, .tag = .nil } };

    pub fn isNan(value: Value) bool {
        return value.bits & ~sign_mask == nan_mask;
    }
    pub fn isNumber(value: Value) bool {
        return value.tagged.nan_mask != 0b0_11111111111_1 or value.isNan();
    }
    pub fn isBoolean(value: Value) bool {
        return value == true_value or value == false_value;
    }
    pub fn isNil(value: Value) bool {
        return value == Value.nil;
    }
    pub fn isString(value: Value) bool {
        return !value.isNumber() and value.tagged.tag == .string;
    }
    pub fn isObject(value: Value) bool {
        return !value.isNumber() and value.tagged.tag == .object;
    }
    pub fn isColor(value: Value) bool {
        return !value.isNumber() and value.tagged.tag == .color;
    }

    pub fn getNumber(value: Value) ?f64 {
        return if (value.isNumber()) value.asNumber() else null;
    }
    pub fn getBoolean(value: Value) ?bool {
        return if (value.isBoolean()) value.asBoolean() else null;
    }
    pub fn getString(value: Value) ?String {
        return if (value.isString()) value.asString() else null;
    }
    pub fn getObject(value: Value) ?*Object {
        return if (value.isObject()) value.asObject() else null;
    }
    pub fn getColor(value: Value) ?Color {
        return if (value.isObject()) value.asColor() else null;
    }

    pub fn asNumber(value: Value) f64 {
        return value.float;
    }
    pub fn asBoolean(value: Value) bool {
        return value == true_value;
    }
    pub fn asString(value: Value) String {
        return @enumFromInt(value.tagged.bits);
    }
    pub fn asColor(value: Value) Color {
        return @bitCast(@as(u32, @truncate(value.tagged.bits)));
    }
    pub fn asObject(value: Value) *Object {
        return @ptrFromInt(value.tagged.bits);
    }

    pub fn newObject(o: *const Object) Value {
        return .{ .tagged = .{
            .bits = @truncate(@intFromPtr(o)),
            .tag = .object,
        } };
    }
    pub fn newColor(c: Color) Value {
        return .{ .tagged = .{
            .bits = @as(u32, @bitCast(c)),
            .tag = .string,
        } };
    }
    pub fn newString(s: String) Value {
        return .{ .tagged = .{
            .bits = @intFromEnum(s),
            .tag = .string,
        } };
    }
    pub fn newBoolean(b: bool) Value {
        return if (b) true_value else false_value;
    }
    pub fn newNumber(n: f64) Value {
        return .{ .float = n };
    }

    pub const Type = enum(u8) {
        nil,
        number,
        color,
        boolean,
        string,
        list,
        dict,
        function,
    };
    pub fn typ(value: Value) Type {
        return if (value.isNil())
            .nil
        else if (value.isNumber())
            .number
        else if (value.isBoolean())
            .boolean
        else if (value.isString())
            .string
        else if (value.isColor())
            .color
        else if (value.getObject()) |obj| switch (obj.tag) {
            inline else => |t| std.meta.stringToEnum(Type, @tagName(t)) orelse unreachable,
        } else unreachable;
    }

    /// Type-aware wrapper around `Value` to make it easy to switch on and access its fields
    pub const TaggedValue = union(Type) {
        nil: void,
        number: f64,
        color: Color,
        boolean: bool,
        string: String,
        list: *Object.List,
        dict: *Object.Dict,
        function: *Object.Function,
    };
    pub fn taggedValue(value: Value) TaggedValue {
        return switch (value.typ()) {
            .nil => .nil,
            .color => .{ .color = value.asColor() },
            .number => .{ .number = value.asNumber() },
            .string => .{ .string = value.asString() },
            .boolean => .{ .boolean = value.asBoolean() },
            .list => .{ .list = value.asObject().asList() },
            .dict => .{ .dict = value.asObject().asDict() },
            .function => .{ .function = value.asObject().asFunction() },
        };
    }

    pub fn isTruthy(value: Value) bool {
        return value != Value.nil and value != false_value;
    }

    pub const operators = struct {
        pub fn equal(a: Value, b: Value) Value {
            return Value.newBoolean(a.bits == b.bits);
        }
        pub fn not_equal(a: Value, b: Value) Value {
            return Value.newBoolean(!operators.equal(a, b).asBoolean());
        }
        pub fn add(vm: *Vm, a: Value, b: Value) !Value {
            if (a.isNumber() and b.isNumber()) {
                return Value.newNumber(a.asNumber() + b.asNumber());
            }

            if (a.isString()) {
                const pool = vm.string_builder.pool;
                var builder = vm.string_builder;
                const m = builder.begin();
                try print(a, pool, &builder.w.writer);
                try print(b, pool, &builder.w.writer);
                const str = try builder.finish(m);
                return Value.newString(str);
            }

            return error.InvalidOperands;
        }
        pub fn subtract(a: Value, b: Value) !Value {
            if (a.isNumber() and b.isNumber()) {
                return Value.newNumber(a.asNumber() - b.asNumber());
            }
            return error.InvalidOperands;
        }
        pub fn modulo(a: Value, b: Value) !Value {
            if (a.isNumber() and b.isNumber()) {
                return Value.newNumber(@mod(a.asNumber(), b.asNumber()));
            }
            return error.InvalidOperands;
        }
        pub fn multiply(a: Value, b: Value) !Value {
            if (a.isNumber() and b.isNumber()) {
                return Value.newNumber(a.asNumber() * b.asNumber());
            }
            return error.InvalidOperands;
        }
        pub fn divide(a: Value, b: Value) !Value {
            if (a.isNumber() and b.isNumber()) {
                return Value.newNumber(a.asNumber() / b.asNumber());
            }
            return error.InvalidOperands;
        }
        pub fn less_than(a: Value, b: Value) !Value {
            if (a.isNumber() and b.isNumber()) {
                return Value.newBoolean(a.asNumber() < b.asNumber());
            }
            return error.InvalidOperands;
        }
        pub fn less_than_equal(a: Value, b: Value) !Value {
            if (a.isNumber() and b.isNumber()) {
                return Value.newBoolean(a.asNumber() <= b.asNumber());
            }
            return error.InvalidOperands;
        }
        pub fn greater_than(a: Value, b: Value) !Value {
            if (a.isNumber() and b.isNumber()) {
                return Value.newBoolean(a.asNumber() > b.asNumber());
            }
            return error.InvalidOperands;
        }
        pub fn greater_than_equal(a: Value, b: Value) !Value {
            if (a.isNumber() and b.isNumber()) {
                return Value.newBoolean(a.asNumber() >= b.asNumber());
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
            if (a.getNumber()) |num| return Value.newNumber(-num);
            return error.InvalidOperand;
        }
        pub fn not(a: Value) !Value {
            if (a.getBoolean()) |b| return Value.newBoolean(!b);
            return error.InvalidOperand;
        }
    };

    /// Note that this should *not* be done with a format() function. This is because this function
    /// needs the *String.Pool so that it can print out strings correctly.
    pub fn print(
        value: Value,
        strings: *String.Pool,
        w: *std.Io.Writer,
    ) error{ WriteFailed, ValueError }!void {
        return switch (value.taggedValue()) {
            .number => |n| w.print("{}", .{n}),
            .boolean => |b| w.print("{}", .{b}),
            .string => |str| w.print("{s}", .{strings.get(str)}),
            else => error.ValueError,
        };
    }

    pub fn format(
        value: @This(),
        w: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        return switch (value.taggedValue()) {
            .nil => w.writeAll("<nil>"),
            .number => |n| w.print("{}", .{n}),
            .boolean => |b| w.print("{}", .{b}),
            .list => |list| w.print("<list@{x:0>12}>", .{@intFromPtr(list)}),
            .dict => |dict| w.print("<dict@{x:0>12}>", .{@intFromPtr(dict)}),
            .string => |str| w.print("<string@{x:0>12}>", .{str}),
            .function => |function| w.print("<function@{x:0>12}>", .{@intFromPtr(function)}),
            .color => |c| w.print("#{x}{x}{x}{x}", .{ c.r, c.g, c.b, c.alpha }),
        };
    }

    pub fn debugPrint(
        value: Value,
        strings: *String.Pool,
        w: *std.Io.Writer,
    ) error{WriteFailed}!void {
        return switch (value.taggedValue()) {
            .nil => w.writeAll("<nil>"),
            .number => |n| w.print("{}", .{n}),
            .boolean => |b| w.print("{}", .{b}),
            .list => |list| w.print("<list@{*}>", .{list}),
            .dict => |dict| w.print("<dict@{*}>", .{dict}),
            .string => |str| w.print("{s}", .{strings.get(str)}),
            .function => |function| w.print("<function@{*}>", .{function}),
            .color => |c| w.print("#{x}{x}{x}{x}", .{ c.r, c.g, c.b, c.alpha }),
        };
    }

    pub const Color = packed struct(u32) {
        r: u8,
        g: u8,
        b: u8,
        alpha: u8,
    };

    pub const String = @import("value/string.zig").String;

    /// Represents a dynamically allocated value on the heap.
    ///
    /// Can be any one of
    /// - List
    /// - Dict
    /// - Function
    ///
    /// Each different object will be implemented through struct inheritance.
    pub const Object = struct {
        /// The type of object this is
        tag: Kind,

        /// If the object may be mutated. This only matters in userspace; Objects that have
        /// .constant = true may still be modified by the vm, but attempting to mutate a constant
        /// object in user code will result in a runtime error.
        constant: bool = false,

        const Kind = enum {
            list,
            dict,
            function,
        };

        pub fn asList(obj: *Object) *List {
            assert(obj.tag == .list);
            return @alignCast(@fieldParentPtr("base", obj));
        }
        pub fn getList(obj: *Object) ?*List {
            return if (obj.tag == .list) obj.asList() else null;
        }

        pub fn asDict(obj: *Object) *Dict {
            assert(obj.tag == .dict);
            return @alignCast(@fieldParentPtr("base", obj));
        }
        pub fn getDict(obj: *Object) ?*Dict {
            return if (obj.tag == .dict) obj.asDict() else null;
        }

        pub fn asFunction(obj: *Object) *Function {
            assert(obj.tag == .function);
            return @alignCast(@fieldParentPtr("base", obj));
        }
        pub fn getFunction(obj: *Object) ?*Function {
            return if (obj.tag == .function) obj.asFunction() else null;
        }

        pub const List = struct {
            base: Object,
            array: std.ArrayList(Value),

            pub fn init(alloc: Allocator) !*Object {
                var ret = try alloc.create(@This());
                ret.* = .{
                    .base = .{ .tag = .list },
                    .array = .empty,
                };
                return &ret.base;
            }
        };

        pub const Dict = struct {
            base: Object,
            map: String.HashMap(Value),

            pub fn init(alloc: Allocator, pool: *String.Pool) !*Object {
                var ret = try alloc.create(@This());
                ret.* = .{
                    .base = .{ .tag = .dict },
                    .map = .init(pool),
                };
                return &ret.base;
            }
        };

        pub const Function = struct {
            base: Object,

            func: union(enum) {
                builtin: struct {
                    self: Value,
                    func: BuiltinFn,
                },
                runtime: struct {
                    /// Index into the list of all nodes of this function's definition
                    definition_index: u32,

                    /// Number of parameters this function expects
                    arity: u32,
                },
            },

            pub const BuiltinFn = *const fn (ctx: CallCtx, args: []const Value) RuntimeError!Value;

            pub const CallCtx = struct {
                vm: *Vm,
                self: Value,
                caller_node_index: u32,
            };

            pub const max_args = 32;

            pub fn initBuiltin(gpa: Allocator, func: BuiltinFn, self: Value) !*Object {
                const ret = try gpa.create(@This());
                ret.* = .{
                    .base = .{ .tag = .function },
                    .func = .{ .builtin = .{
                        .func = func,
                        .self = self,
                    } },
                };
                return &ret.base;
            }

            pub fn initRuntime(gpa: Allocator, body_index: u32, arity: u32) !*Object {
                const ret = try gpa.create(@This());
                ret.* = .{
                    .base = .{ .tag = .function },
                    .func = .{ .runtime = .{
                        .definition_index = body_index,
                        .arity = arity,
                    } },
                };
                return &ret.base;
            }
        };
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
        const value = Value.newNumber(num);

        try std.testing.expect(value.typ() == .number);
        try std.testing.expect(value.isNumber());
        try std.testing.expect(!value.isObject());
        try std.testing.expect(!value.isBoolean());
        try std.testing.expect(!value.isNil());
        try std.testing.expect(value.isTruthy());

        try std.testing.expectEqual(std.math.isNan(num), value.isNan());
        if (!value.isNan()) {
            // IEEE 754 says nans can't be compared
            try std.testing.expectEqual(value.asNumber(), num);
        }
    }
}

test "Value booleans" {
    const booleans = [_]bool{ true, false };

    for (booleans) |boolean| {
        const value = Value.newBoolean(boolean);

        try std.testing.expect(value.typ() == .boolean);
        try std.testing.expect(value.isBoolean());
        try std.testing.expect(!value.isObject());
        try std.testing.expect(!value.isNumber());
        try std.testing.expect(!value.isNil());
        try std.testing.expect(value.isTruthy() == boolean);

        try std.testing.expectEqual(value.asBoolean(), boolean);
    }
}

test "Value nil" {
    const value = Value.nil;

    try std.testing.expect(value.isNil());
    try std.testing.expect(!value.isObject());
    try std.testing.expect(!value.isNumber());
    try std.testing.expect(!value.isBoolean());
    try std.testing.expect(!value.isTruthy());
}

test "Value object" {
    const object: Value.Object = .{ .tag = .list };
    const value = Value.newObject(&object);

    try std.testing.expectEqual(@intFromPtr(&object), @intFromPtr(value.asObject()));

    try std.testing.expect(value.typ() == .list);
    try std.testing.expect(value.isObject());
    try std.testing.expect(!value.isNumber());
    try std.testing.expect(!value.isBoolean());
    try std.testing.expect(!value.isNil());
    try std.testing.expect(value.isTruthy());
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const max_load_percentage = std.hash_map.default_max_load_percentage;

const Vm = @import("Vm.zig");
const RuntimeError = Vm.RuntimeError;
