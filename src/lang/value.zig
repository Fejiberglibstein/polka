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

    fn isNan(value: Value) bool {
        return value.bits & ~sign_mask == nan_mask;
    }
    fn toObject(value: Value) *Object {
        assert(value.tagged.tag == .object);
        return @ptrFromInt(value.tagged.bits);
    }

    const true_value: Value = .{ .tagged = .{ .bits = 0, .tag = .true } };
    const false_value: Value = .{ .tagged = .{ .bits = 0, .tag = .false } };
    pub const nil: Value = .{ .tagged = .{ .bits = 0, .tag = .nil } };

    pub const Type = @typeInfo(TaggedValue).@"union".tag_type.?;
    /// TaggedValue is a combination of all the types that an object can be, plus the types listed
    /// below in Base
    pub const TaggedValue = ty: {
        const Base = union(enum) {
            nil: void,
            number: f64,
            color: Color,
            boolean: bool,
            string: String,
            object: *Object,
        };

        var field_names: []const []const u8 = &.{};
        var field_types: []const type = &.{};

        for (@typeInfo(Base).@"union".fields) |field| {
            field_names = field_names ++ .{field.name};
            field_types = field_types ++ .{field.type};
        }

        for (@typeInfo(Object.TaggedKind).@"union".fields) |field| {
            field_names = field_names ++ .{field.name};
            field_types = field_types ++ .{field.type};
        }

        const Enum = @Enum(u8, .exhaustive, field_names, &std.simd.iota(u8, field_names.len));
        break :ty @Union(.auto, Enum, field_names, @ptrCast(field_types), &@splat(.{}));
    };

    pub fn TypeOf(comptime ty: Type) type {
        return @typeInfo(TaggedValue).@"union".fields[@intFromEnum(ty)].type;
    }
    pub fn is(value: Value, comptime ty: Type) ?TypeOf(ty) {
        const is_number = value.tagged.nan_mask != 0b0_11111111111_1 or value.isNan();
        const is_ty = switch (ty) {
            .number => is_number,
            .nil => value == Value.nil,
            .boolean => value == true_value or value == false_value,

            .color => !is_number and value.tagged.tag == .color,
            .string => !is_number and value.tagged.tag == .string,
            .object => !is_number and value.tagged.tag == .object,

            inline else => {
                const kind = comptime std.meta.stringToEnum(Object.Kind, @tagName(ty)).?;
                if (value.is(.object)) |obj| return obj.is(kind);
                return null;
            },
        };

        return if (is_ty) value.as(ty) else null;
    }
    pub fn as(value: Value, comptime ty: Type) TypeOf(ty) {
        return switch (ty) {
            .nil => void{},
            .number => value.float,
            .boolean => value == true_value,
            .color => @bitCast(@as(u32, @truncate(value.tagged.bits))),
            .string => @enumFromInt(value.tagged.bits),
            .object => @ptrFromInt(value.tagged.bits),
            inline else => {
                assert(value.tagged.tag == .object);
                const object: *Object = @ptrFromInt(value.tagged.bits);
                return object.as(std.meta.stringToEnum(Object.Kind, @tagName(ty)).?);
            },
        };
    }

    pub fn newObject(o: *const Object) Value {
        return .{ .tagged = .{
            .bits = @truncate(@intFromPtr(o)),
            .tag = .object,
        } };
    }
    pub fn newColor(c: Color) Value {
        return .{ .tagged = .{
            .bits = @as(@typeInfo(Color).@"struct".backing_integer.?, @bitCast(c)),
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

    pub fn typ(value: Value) Type {
        return if (value.is(.nil)) |_|
            .nil
        else if (value.is(.number)) |_|
            .number
        else if (value.is(.boolean)) |_|
            .boolean
        else if (value.is(.string)) |_|
            .string
        else if (value.is(.color)) |_|
            .color
        else switch (value.toObject().kind) {
            inline else => |t| std.meta.stringToEnum(Type, @tagName(t)) orelse unreachable,
        };
    }

    /// Type-aware wrapper around `Value` to make it easy to switch on and access its fields
    pub fn taggedValue(value: Value) TaggedValue {
        switch (value.typ()) {
            inline else => |ty| {
                const field = @typeInfo(TaggedValue).@"union".fields[@intFromEnum(ty)];
                return @unionInit(TaggedValue, field.name, value.as(ty));
            },
        }
    }

    pub fn isTruthy(value: Value) bool {
        return value != Value.nil and value != false_value;
    }

    pub const operators = struct {
        pub fn equal(a: Value, b: Value) Value {
            return Value.newBoolean(a.bits == b.bits);
        }
        pub fn not_equal(a: Value, b: Value) Value {
            return Value.newBoolean(!operators.equal(a, b).as(.boolean));
        }
        pub fn add(vm: *Vm, a: Value, b: Value) !Value {
            if (a.is(.number)) |n1| if (b.is(.number)) |n2| {
                return Value.newNumber(n1 + n2);
            };

            if (a.is(.string)) |_| {
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
            if (a.is(.number)) |n1| if (b.is(.number)) |n2| {
                return Value.newNumber(n1 - n2);
            };
            return error.InvalidOperands;
        }
        pub fn modulo(a: Value, b: Value) !Value {
            if (a.is(.number)) |n1| if (b.is(.number)) |n2| {
                return Value.newNumber(@mod(n1, n2));
            };
            return error.InvalidOperands;
        }
        pub fn multiply(a: Value, b: Value) !Value {
            if (a.is(.number)) |n1| if (b.is(.number)) |n2| {
                return Value.newNumber(n1 * n2);
            };
            return error.InvalidOperands;
        }
        pub fn divide(a: Value, b: Value) !Value {
            if (a.is(.number)) |n1| if (b.is(.number)) |n2| {
                return Value.newNumber(n1 / n2);
            };
            return error.InvalidOperands;
        }
        pub fn less_than(a: Value, b: Value) !Value {
            if (a.is(.number)) |n1| if (b.is(.number)) |n2| {
                return Value.newBoolean(n1 < n2);
            };
            return error.InvalidOperands;
        }
        pub fn less_than_equal(a: Value, b: Value) !Value {
            if (a.is(.number)) |n1| if (b.is(.number)) |n2| {
                return Value.newBoolean(n1 <= n2);
            };
            return error.InvalidOperands;
        }
        pub fn greater_than(a: Value, b: Value) !Value {
            if (a.is(.number)) |n1| if (b.is(.number)) |n2| {
                return Value.newBoolean(n1 > n2);
            };
            return error.InvalidOperands;
        }
        pub fn greater_than_equal(a: Value, b: Value) !Value {
            if (a.is(.number)) |n1| if (b.is(.number)) |n2| {
                return Value.newBoolean(n1 >= n2);
            };
            return error.InvalidOperands;
        }
        pub fn in(a: Value, b: Value) !Value {
            _ = a;
            _ = b;
            @panic("TODO");
            // return error.InvalidOperands;
        }

        pub fn negate(a: Value) !Value {
            if (a.is(.number)) |num| return Value.newNumber(-num);
            return error.InvalidOperand;
        }
        pub fn not(a: Value) !Value {
            if (a.is(.boolean)) |b| return Value.newBoolean(!b);
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
        kind: Kind,

        /// If the object may be mutated. This only matters in userspace; Objects that have
        /// .constant = true may still be modified by the vm, but attempting to mutate a constant
        /// object in user code will result in a runtime error.
        constant: bool = false,

        pub const Kind = std.meta.Tag(TaggedKind);
        const TaggedKind = union(enum) {
            list: *List,
            dict: *Dict,
            function: *Function,
        };
        fn KindType(comptime kind: Kind) type {
            return @typeInfo(TaggedKind).@"union".fields[@intFromEnum(kind)].type;
        }

        pub fn as(obj: *Object, comptime kind: Kind) KindType(kind) {
            assert(obj.kind == kind);
            return @alignCast(@fieldParentPtr("base", obj));
        }

        pub fn is(obj: *Object, comptime kind: Kind) ?KindType(kind) {
            if (obj.kind == kind) return obj.as(kind);
            return null;
        }

        pub const List = struct {
            base: Object,
            array: std.ArrayList(Value),

            pub fn init(alloc: Allocator) !*Object {
                var ret = try alloc.create(@This());
                ret.* = .{
                    .base = .{ .kind = .list },
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
                    .base = .{ .kind = .dict },
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
                    /// Function definition node; is an ast.FunctionDef
                    definition_index: ast.NodeIndex,

                    /// Number of parameters this function expects
                    arity: u32,
                },
            },

            pub const BuiltinFn = *const fn (ctx: CallCtx, args: []const Value) RuntimeError!Value;

            pub const CallCtx = struct {
                vm: *Vm,
                self: Value,
                caller_index: ast.NodeIndex,
            };

            pub const max_args = 32;

            pub fn initBuiltin(gpa: Allocator, func: BuiltinFn, self: Value) !*Object {
                const ret = try gpa.create(@This());
                ret.* = .{
                    .base = .{ .kind = .function },
                    .func = .{ .builtin = .{
                        .func = func,
                        .self = self,
                    } },
                };
                return &ret.base;
            }

            pub fn initRuntime(gpa: Allocator, body_index: ast.NodeIndex, arity: u32) !*Object {
                const ret = try gpa.create(@This());
                ret.* = .{
                    .base = .{ .kind = .function },
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
        try std.testing.expect(value.is(.number) != null);

        try std.testing.expect(value.is(.nil) == null);
        try std.testing.expect(value.is(.list) == null);
        try std.testing.expect(value.is(.dict) == null);
        try std.testing.expect(value.is(.object) == null);
        try std.testing.expect(value.is(.boolean) == null);
        try std.testing.expect(value.is(.function) == null);
        try std.testing.expect(value.isTruthy());

        try std.testing.expectEqual(std.math.isNan(num), value.isNan());
        if (!value.isNan()) {
            // IEEE 754 says nans can't be compared
            try std.testing.expectEqual(value.as(.number), num);
        }
    }
}

test "Value booleans" {
    const booleans = [_]bool{ true, false };

    for (booleans) |boolean| {
        const value = Value.newBoolean(boolean);

        try std.testing.expect(value.typ() == .boolean);
        try std.testing.expect(value.is(.boolean) == boolean);

        try std.testing.expect(value.is(.nil) == null);
        try std.testing.expect(value.is(.list) == null);
        try std.testing.expect(value.is(.dict) == null);
        try std.testing.expect(value.is(.object) == null);
        try std.testing.expect(value.is(.number) == null);
        try std.testing.expect(value.is(.function) == null);
        try std.testing.expect(value.isTruthy() == boolean);
    }
}

test "Value nil" {
    const value = Value.nil;

    try std.testing.expect(value.typ() == .nil);
    try std.testing.expect(value.is(.nil) != null);

    try std.testing.expect(value.is(.list) == null);
    try std.testing.expect(value.is(.dict) == null);
    try std.testing.expect(value.is(.object) == null);
    try std.testing.expect(value.is(.number) == null);
    try std.testing.expect(value.is(.boolean) == null);
    try std.testing.expect(value.is(.function) == null);
    try std.testing.expect(!value.isTruthy());
}

test "Value object" {
    const list: Value.Object.List = .{ .base = .{ .kind = .list }, .array = undefined };
    const value = Value.newObject(&list.base);

    try std.testing.expect(value.typ() == .list);
    try std.testing.expect(value.is(.list) == &list);
    try std.testing.expect(value.is(.object) != null);
    try std.testing.expect(value.is(.object) == &list.base);

    try std.testing.expect(value.is(.nil) == null);
    try std.testing.expect(value.is(.dict) == null);
    try std.testing.expect(value.is(.number) == null);
    try std.testing.expect(value.is(.boolean) == null);
    try std.testing.expect(value.is(.function) == null);
    try std.testing.expect(value.isTruthy());
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const max_load_percentage = std.hash_map.default_max_load_percentage;

const Vm = @import("Vm.zig");
const RuntimeError = Vm.RuntimeError;
const ast = @import("ast.zig");
