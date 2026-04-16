// zig fmt: off
const nan_mask: u64     = 0x7FF8000000000000;
const tag_mask: u64     = 0x0007000000000000;
const sign_mask: u64    = 0x8000000000000000;
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
            string,
            object,
        };
    },

    const true_value: Value = .{ .tagged = .{ .bits = 0, .tag = .true } };
    const false_value: Value = .{ .tagged = .{ .bits = 0, .tag = .false } };
    pub const nil: Value = .{ .tagged = .{ .bits = 0, .tag = .nil } };

    pub fn isNan(self: Value) bool {
        return self.bits & ~sign_mask == nan_mask;
    }
    pub fn isNumber(self: Value) bool {
        return self.tagged.nan_mask != 0b0_11111111111_1 or self.isNan();
    }
    pub fn isBoolean(self: Value) bool {
        return self == true_value or self == false_value;
    }
    pub fn isNil(self: Value) bool {
        return self == Value.nil;
    }
    pub fn isString(self: Value) bool {
        return !self.isNumber() and self.tagged.tag == .string;
    }
    pub fn isObject(self: Value) bool {
        return !self.isNumber() and self.tagged.tag == .object;
    }

    pub fn getNumber(self: Value) ?f64 {
        return if (self.isNumber()) self.asNumber() else null;
    }
    pub fn getBoolean(self: Value) ?bool {
        return if (self.isBoolean()) self.asBoolean() else null;
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
        return self == true_value;
    }
    pub fn asString(self: Value) String {
        return @enumFromInt(self.bits & payload_mask);
    }
    pub fn asObject(self: Value) *Object {
        return @ptrFromInt(self.bits & payload_mask);
    }

    pub fn newObject(o: *const Object) Value {
        return .{ .tagged = .{
            .bits = @truncate(@intFromPtr(o)),
            .tag = .object,
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
        boolean,
        string,
        list,
        dict,
        function,
    };
    pub fn typ(self: Value) Type {
        return if (self.isNil())
            .nil
        else if (self.isNumber())
            .number
        else if (self.isBoolean())
            .boolean
        else if (self.isString())
            .string
        else if (self.getObject()) |obj| switch (obj.tag) {
            inline else => |t| std.meta.stringToEnum(Type, @tagName(t)) orelse unreachable,
        } else unreachable;
    }

    /// Type-aware wrapper around `Value` to make it easy to switch on and access its fields
    pub const TaggedValue = union(Type) {
        nil: void,
        number: f64,
        boolean: bool,
        string: String,
        list: *Object.List,
        dict: *Object.Dict,
        function: *Object.Function,
    };
    pub fn taggedValue(self: Value) TaggedValue {
        return switch (self.typ()) {
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
        return self != Value.nil and self != false_value;
    }

    pub fn methods(self: Value) std.StaticStringMap(Object.Function.BuiltinFn) {
        const vtable = comptime vtables: {
            const tagged_fields = @typeInfo(Value.TaggedValue).@"union".fields;
            const VTable = std.StaticStringMap(Object.Function.BuiltinFn);
            var vtables: [tagged_fields.len]VTable = @splat(.initComptime(.{}));

            for (tagged_fields, 0..) |field, i| {
                const FieldType = if (@typeInfo(field.type) == .pointer)
                    @typeInfo(field.type).pointer.child
                else
                    field.type;

                if (hasDecl(FieldType, "methods")) {
                    const methods_namespace = @field(FieldType, "methods");
                    const decls = std.meta.declarations(methods_namespace);

                    const MapField = struct { []const u8, Object.Function.BuiltinFn };
                    var functions: [decls.len]MapField = undefined;
                    for (decls, 0..) |decl, j| {
                        functions[j] = .{ decl.name, &@field(methods_namespace, decl.name) };
                    }

                    const map: VTable = .initComptime(functions);
                    vtables[i] = map;
                }
            }

            break :vtables vtables;
        };

        return vtable[@intFromEnum(self.typ())];
    }

    pub fn getMethod(
        self: Value,
        gpa: Allocator,
        function_name: []const u8,
    ) !?*Object {
        return if (self.methods().get(function_name)) |function|
            try Object.Function.initBuiltin(gpa, function, self)
        else
            null;
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
        self: Value,
        strings: *String.Pool,
        w: *std.Io.Writer,
    ) error{ WriteFailed, ValueError }!void {
        return switch (self.taggedValue()) {
            .number => |n| w.print("{}", .{n}),
            .boolean => |b| w.print("{}", .{b}),
            .string => |str| w.print("{s}", .{strings.get(str)}),
            else => error.ValueError,
        };
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
                pub fn append(
                    ctx: Function.CallCtx,
                    args: *const [Function.max_args]Value,
                ) RuntimeError!Value {
                    assert(args.len > 1);
                    const self = args[0].asObject().asList();

                    for (args[1..]) |arg| {
                        self.array.append(ctx.vm.valueAllocator(), arg) catch
                            try ctx.vm.setError(ctx.caller_node_index, .value_oom);
                    }

                    return args[0];
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
                pub fn get(
                    ctx: Function.CallCtx,
                    args: *const [Function.max_args]Value,
                ) RuntimeError!Value {
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
                    this_ptr: Value,
                    func: BuiltinFn,
                },
                runtime: struct {
                    /// Index into the list of all nodes of this function's definition
                    definition_index: u32,

                    /// Number of parameters this function expects
                    arity: u32,
                },
            },

            const BuiltinFn = *const fn (
                ctx: CallCtx,
                args: *const [max_args]Value,
            ) RuntimeError!Value;

            const CallCtx = struct {
                vm: *Vm,
                this_ptr: Value,
                caller_node_index: u32,
            };

            pub const max_args = 32;

            pub fn initBuiltin(gpa: Allocator, func: BuiltinFn, this_ptr: Value) !*Object {
                const ret = try gpa.create(@This());
                ret.* = .{
                    .base = .{ .tag = .function },
                    .func = .{ .builtin = .{
                        .func = func,
                        .this_ptr = this_ptr,
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

/// @hasDecl is not permissive enough; It needs to return false on invalid types rather than emit a
/// compiler error.
fn hasDecl(comptime T: type, name: []const u8) bool {
    return switch (@typeInfo(T)) {
        .@"struct",
        .@"union",
        .@"enum",
        .@"opaque",
        => @hasDecl(T, name),
        .pointer => |p| hasDecl(p.child, name),
        else => false,
    };
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const max_load_percentage = std.hash_map.default_max_load_percentage;

const Vm = @import("Vm.zig");
const RuntimeError = Vm.RuntimeError;
