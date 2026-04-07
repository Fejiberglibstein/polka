const Tag = enum(u3) {
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

const tag_offset = @ctz(tag_mask);
const nil_value = nan_mask | (@as(u64, @intFromEnum(Tag.nil)) << tag_offset);
const true_value = nan_mask | (@as(u64, @intFromEnum(Tag.true)) << tag_offset);
const false_value = nan_mask | (@as(u64, @intFromEnum(Tag.false)) << tag_offset);

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
    tagged: packed struct(u64) {
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
    pub fn getString(self: Value) ?String {
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
    pub fn asString(self: Value) String {
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
    pub fn string(s: String) Value {
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

    pub const Type = enum(u8) {
        nil,
        number,
        boolean,
        string,
        list,
        dict,
        function,
    };

    pub const Tagged = union(Value.Type) {
        nil: void,
        number: f64,
        boolean: bool,
        string: String,
        list: *Object.List,
        dict: *Object.Dict,
        function: *Object.Function,
    };

    pub fn tag(self: Value) Value.Type {
        return if (self.isNil())
            .nil
        else if (self.isNumber())
            .number
        else if (self.isBoolean())
            .boolean
        else if (self.isString())
            .string
        else if (self.getObject()) |obj| switch (obj.tag) {
            inline else => |t| std.meta.stringToEnum(Value.Type, @tagName(t)) orelse unreachable,
        } else unreachable;
    }

    pub fn taggedValue(self: Value) Tagged {
        return switch (self.tag()) {
            .nil => .nil,
            .number => .{ .number = self.asNumber() },
            .boolean => .{ .boolean = self.asBoolean() },
            .string => .{ .string = self.asString() },
            .list => .{ .list = self.asObject().asList() },
            .dict => .{ .dict = self.asObject().asDict() },
            .function => .{ .function = self.asObject().asFunction() },
        };
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
        pub fn add(vm: *Vm, a: Value, b: Value) !Value {
            if (a.isNumber() and b.isNumber()) {
                return Value.number(a.asNumber() + b.asNumber());
            }

            if (a.isString()) {
                const pool = vm.string_builder.pool;
                var builder = vm.string_builder;
                const m = builder.begin();
                try print(a, pool, &builder.w.writer);
                try print(b, pool, &builder.w.writer);
                const str = try builder.finish(m);
                return Value.string(str);
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
    /// needs the *String.Pool so that it can print out strings correctly.
    pub fn print(
        self: Value,
        strings: *String.Pool,
        w: *std.Io.Writer,
    ) error{ WriteFailed, ValueError }!void {
        return switch (self.taggedValue()) {
            .nil => error.ValueError,
            .number => |n| w.print("{}", .{n}),
            .boolean => |b| w.print("{}", .{b}),
            .string => |str| w.print("{s}", .{strings.get(str)}),
            else => error.ValueError,
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
        dict,
        function,
    };

    pub fn asList(self: *Object) *List {
        assert(self.tag == .list);
        return @alignCast(@fieldParentPtr("base", self));
    }
    pub fn getList(self: *Object) ?*List {
        return if (self.tag == .list) self.asList() else null;
    }

    pub fn asDict(self: *Object) *Dict {
        assert(self.tag == .dict);
        return @alignCast(@fieldParentPtr("base", self));
    }
    pub fn getDict(self: *Object) ?*Dict {
        return if (self.tag == .dict) self.asDict() else null;
    }

    pub fn asFunction(self: *Object) *Function {
        assert(self.tag == .function);
        return @alignCast(@fieldParentPtr("base", self));
    }
    pub fn getFunction(self: *Object) ?*Function {
        return if (self.tag == .function) self.asFunction() else null;
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

        pub const methods = struct {
            pub fn append(ctx: Function.CallCtx, args: []const Value) RuntimeError!Value {
                assert(args.len > 1);
                const self = args[0].asObject().asList();
                for (args[1..]) |arg| {
                    self.array.append(ctx.vm.valueAllocator(), arg) catch
                        try ctx.vm.setError(ctx.caller_node_index, .value_oom);
                }
            }
        };
    };

    pub const Dict = struct {
        base: Object,
        map: std.HashMapUnmanaged(String, Value, String.Pool.Context, max_load_percentage),

        pub fn init(alloc: Allocator) !*Object {
            var ret = try alloc.create(@This());
            ret.* = .{
                .base = .{ .tag = .dict },
                .map = .empty,
            };

            return &ret.base;
        }

        pub const methods = struct {
            pub fn get(ctx: Function.CallCtx, args: []const Value) RuntimeError!Value {
                assert(args.len > 0);
                const self = args[0].asObject().asDict();

                const key_val: Value = if (args.len > 1) args[1] else .nil;
                const key = key_val.getString() orelse
                    try ctx.vm.setError(ctx.caller_node_index, .{ .invalid_type = .{
                        .exp = .string,
                        .act = key_val,
                    } });

                const sb = &ctx.vm.string_builder;
                return self.map.getContext(key, .{ .pool = sb.pool }) orelse .nil;
            }
        };
    };

    pub const Function = struct {
        base: Object,

        func: union(enum) {
            builtin: struct {
                func: *const fn (ctx: CallCtx, args: []const Value) RuntimeError!Value,
            },
            runtime: struct {
                /// Index into the list of all nodes of this function's definition
                definition_index: u32,
            },
        },

        /// Number of parameters this function expects
        arity: u32,

        const CallCtx = struct {
            vm: *Vm,
            caller_node_index: u32,
        };

        pub fn initBuiltin(comptime func: anytype) *Object {
            const FuncType = @TypeOf(func);

            var ret: Function = .{
                .base = .{ .tag = .function },
                .func = .{
                    .builtin = if (FuncType == fn (Function.CallCtx, []const Value) RuntimeError!Value)
                        .{ .f1 = &func }
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
                .func = .{ .runtime = .{
                    .definition_index = body_index,
                } },
                .arity = arity,
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

    try expect(value.tag() == .list);
    try expect(value.isObject());
    try expect(!value.isNumber());
    try expect(!value.isBoolean());
    try expect(!value.isNil());
    try expect(value.isTruthy());
}

pub const String = enum(u32) {
    _,

    /// A global pool of every string across all files.
    ///
    /// One instance of this will usually be made at a time, so that all files intern into a single
    /// pool to increase the likelihood of intern hits.
    pub const Pool = struct {
        gpa: Allocator,
        bytes: std.ArrayList(u8),
        map: std.HashMapUnmanaged(String, void, Context, max_load_percentage),

        pub fn init(gpa: Allocator) String.Pool {
            return .{
                .bytes = .empty,
                .map = .empty,
                .gpa = gpa,
            };
        }

        pub fn deinit(pool: *@This()) void {
            pool.bytes.deinit(pool.gpa);
            pool.map.deinit(pool.gpa);
        }

        pub fn get(pool: *const @This(), index: String) []const u8 {
            return std.mem.sliceTo(pool.bytes.items[@intFromEnum(index)..], 0);
        }

        const Context = struct {
            pool: *const Pool,
            pub fn hash(ctx: @This(), key: String) u64 {
                return std.hash_map.hashString(ctx.pool.get(key));
            }

            pub fn eql(_: @This(), a: String, b: String) bool {
                return a == b;
            }
        };

        const ContextAdapted = struct {
            pool: *const Pool,

            pub fn hash(_: @This(), key: []const u8) u64 {
                assert(std.mem.indexOfScalar(u8, key, 0) == null);
                return std.hash_map.hashString(key);
            }

            pub fn eql(ctx: @This(), a: []const u8, b: String) bool {
                return std.mem.eql(u8, a, ctx.pool.get(b));
            }
        };
    };

    /// A file-local string builder, this is used to create strings using .begin() & .finish(). A
    /// finished string may be placed in the String.Pool if it doesn't already exist in the pool.
    pub const Builder = struct {
        pool: *String.Pool,
        w: std.Io.Writer.Allocating,

        pub fn init(gpa: Allocator, pool: *String.Pool) !Builder {
            return .{
                .pool = pool,
                .w = .init(gpa),
            };
        }

        pub fn deinit(self: *@This()) void {
            self.w.deinit();
            self.* = undefined;
        }

        pub const Marker = enum(u32) { _ };

        pub fn begin(b: *@This()) Marker {
            return @enumFromInt(b.w.written().len);
        }

        pub fn finish(b: *@This(), m: Marker) !String {
            var pool = b.pool;

            const str = b.w.written()[@intFromEnum(m)..];
            const gop = try pool.map.getOrPutContextAdapted(
                pool.gpa,
                str,
                String.Pool.ContextAdapted{ .pool = pool },
                String.Pool.Context{ .pool = pool },
            );

            if (!gop.found_existing) {
                try pool.bytes.ensureUnusedCapacity(pool.gpa, str.len + 1);
                const index: String = @enumFromInt(pool.bytes.items.len);
                pool.bytes.appendSliceAssumeCapacity(str);
                pool.bytes.appendAssumeCapacity(0);
                gop.key_ptr.* = index;
            }
            b.w.shrinkRetainingCapacity(@intFromEnum(m));

            return gop.key_ptr.*;
        }
    };
};

const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const max_load_percentage = std.hash_map.default_max_load_percentage;

const Vm = @import("Vm.zig");
const RuntimeError = Vm.RuntimeError;
