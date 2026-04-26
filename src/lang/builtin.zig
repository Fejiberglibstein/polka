//! Contains the builtin functions and methods on Values that may be called at runtime
//!
//! All builtin functions have the type
//! `fn(Function.CallCtx, []const Value) RuntimeError!Value`
const builtin = @This();

/// `methods` is a namespace containing namespaces for all the builtin methods that may be called on
/// each type at runtime, e.g. list.append()
///
/// In order to resolve a method on a Value, `methods.get()` may be called, which will bind return a
/// Function Object that has the value bound to it.
pub const methods = struct {
    // All of the following namespaces must be named identically to their respective field in the
    // enum Value.Type.

    const list = struct {
        pub fn len(ctx: Function.CallCtx, args: []const Value) RuntimeError!Value {
            _ = args;
            const self = ctx.self.asObject().asList();
            return Value.newNumber(@floatFromInt(self.array.items.len));
        }

        pub fn append(ctx: Function.CallCtx, args: []const Value) RuntimeError!Value {
            const self = ctx.self.asObject().asList();
            for (args[0..]) |arg| {
                self.array.append(ctx.vm.valueAllocator(), arg) catch
                    try ctx.vm.setError(ctx.caller_node_index, .value_oom);
            }
            return Value.nil;
        }
    };

    /// Get a method from a value, binding the value to that method.
    ///
    /// The returned Object is a Function.
    pub fn get(
        value: Value,
        gpa: Allocator,
        method_name: []const u8,
    ) !?*Value.Object {
        return if (table[@intFromEnum(value.typ())].get(method_name)) |function|
            try Function.initBuiltin(gpa, function, value)
        else
            null;
    }

    /// List of the methods belonging to each type. The ith element in this table contains all the
    /// methods for the ith field in the `Value.Type` enum.
    pub const table = table: {
        const types = @typeInfo(Value.Type).@"enum".fields;
        const MethodTable = std.StaticStringMap(Function.BuiltinFn);
        var methods_table: [types.len]MethodTable = @splat(.initComptime(.{}));

        for (types, 0..) |typ, i| {
            if (@hasDecl(methods, typ.name)) {
                const type_namespace = @field(methods, typ.name);
                const decls = std.meta.declarations(type_namespace);

                const Entry = struct { []const u8, Function.BuiltinFn };
                var entries: [decls.len]Entry = undefined;
                for (decls, 0..) |decl, j| {
                    entries[j] = .{ decl.name, &@field(type_namespace, decl.name) };
                }

                const map: MethodTable = .initComptime(entries);
                methods_table[i] = map;
            }
        }

        break :table methods_table;
    };

    comptime {
        assert(table[@intFromEnum(Value.Type.list)].get("len") != null);
    }
};

pub const Constants = struct {
    map: std.StaticStringMap(Value),
    arena: std.heap.ArenaAllocator,

    pub const empty: Constants = .{
        .map = .initComptime(.{}),
        .arena = undefined,
    };

    pub fn init(io: Io, gpa: Allocator, pool: *StringPool) !Constants {
        _ = io;

        const Entry = struct { []const u8, Value };
        const initial_capacity = functions.list.len + 1;
        // TODO this could probably be just a static array since its always going to be a fixed
        // size.
        var entries: std.ArrayList(Entry) = try .initCapacity(gpa, initial_capacity);
        defer entries.deinit(gpa);

        var arena = std.heap.ArenaAllocator.init(gpa);
        const sys = sys: {
            const object = try Object.Dict.init(arena.allocator(), pool);
            const dict = object.asDict();
            _ = dict;
            break :sys Value.newObject(object);
        };

        try entries.append(gpa, .{ "sys", sys });

        for (&functions.list) |*func| {
            try entries.append(gpa, .{ func.@"0", Value.newObject(&func.@"1".base) });
        }

        return .{
            .map = try .init(entries.items, gpa),
            .arena = arena,
        };
    }

    pub fn deinit(constants: Constants, gpa: Allocator) void {
        std.log.debug("{}", .{constants.map.kvs.len});
        if (constants.map.kvs.len == 0) return;

        constants.map.deinit(gpa);
        constants.arena.deinit();
    }
};

/// `functions` contains all of the builtin functions that may be called at runtime.
///
/// All builtin functions in this namespace have the type
/// `fn(Function.CallCtx, []const Value) RuntimeError!Value`
pub const functions = struct {
    pub fn polka(ctx: Function.CallCtx, args: []const Value) RuntimeError!Value {
        _ = ctx;
        _ = args;

        std.log.debug("hiii", .{});
        return Value.nil;
    }

    pub fn get(function_name: []const u8) ?*const Value.Object {
        const map: std.StaticStringMap(Function) = .initComptime(list);
        // List has a static lifetime so it's fine to return a pointer here.
        return if (map.get(function_name)) |f| &f.base else null;
    }

    /// Contains all of the builtin functions. Note that unlike methods.table, this is a string map
    /// of `Functions`, not `BuiltinFn`. These functions are created statically at compile time, so
    /// that `functions.get()` may return a reference to them to avoid heap allocating.
    const Entry = struct { []const u8, Function };

    pub const list = list: {
        const decls = std.meta.declarations(functions);

        var decl_count = 0;
        for (decls) |decl| {
            // Skip past this decl to avoid dependency loop since it hasn't been resolved yet.
            if (std.mem.eql(u8, decl.name, "list")) continue;
            if (@TypeOf(&@field(functions, decl.name)) == Function.BuiltinFn) decl_count += 1;
        }

        var entries: [decl_count]Entry = undefined;
        var len = 0;

        for (decls) |decl| {
            // Skip past this decl to avoid dependency loop since it hasn't been resolved yet.
            if (std.mem.eql(u8, decl.name, "list")) continue;

            const field = &@field(functions, decl.name);
            if (@TypeOf(field) == Function.BuiltinFn) {

                // not using the Value.initBuiltin here because that requires an allocator and this
                // should be constructed at compiletime.
                entries[len] = .{ decl.name, Function{
                    .base = .{ .tag = .function, .constant = true },
                    .func = .{ .builtin = .{
                        .func = field,
                        .self = Value.nil,
                    } },
                } };
                len += 1;
            }
        }

        var shortened_entries: [len]Entry = undefined;
        @memcpy(&shortened_entries, entries[0..len]);

        break :list shortened_entries;
    };

    comptime {
        assert(get("polka") != null);
    }
};

const std = @import("std");
const Io = std.Io;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const Value = @import("value.zig").Value;
const Object = Value.Object;
const StringPool = Value.String.Pool;
const Function = Object.Function;
const Vm = @import("Vm.zig");
const RuntimeError = Vm.RuntimeError;
const ControlFlow = Vm.ControlFlow;
