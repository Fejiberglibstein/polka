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

/// `functions` contains all of the builtin functions that may be called at runtime.
///
/// All builtin functions in this namespace have the type
/// `fn(Function.CallCtx, []const Value) RuntimeError!Value`
pub const functions = struct {
    pub fn polka(ctx: Function.CallCtx, args: []const Value) RuntimeError!Value {
        _ = ctx;
        _ = args;
        return Value.nil;
    }

    pub fn get(function_name: []const u8) ?*const Value.Object {
        // List has a static lifetime so it's fine to return a pointer here.
        return if (list.get(function_name)) |f| &f.base else null;
    }

    /// Contains all of the builtin functions. Note that unlike methods.table, this is a string map
    /// of `Functions`, not `BuiltinFn`. These functions are created statically at compile time, so
    /// that `functions.get()` may return a reference to them to avoid heap allocating.
    pub const list: std.StaticStringMap(Function) = list: {
        const decls = std.meta.declarations(functions);

        const Entry = struct { []const u8, Function };
        var entries: [decls.len]Entry = undefined;
        var len = 0;

        for (decls) |decl| {
            // Skip past this decl to avoid dependency loop since it hasn't been resolved yet.
            if (std.mem.eql(u8, decl.name, "list")) continue;

            const field = &@field(functions, decl.name);
            if (@TypeOf(field) == Function.BuiltinFn) {

                // not using the Value.initBuiltin here because that requires an allocator and this
                // should be constructed at compiletime.
                entries[len] = .{ decl.name, Function{
                    .base = .{ .tag = .function },
                    .func = .{ .builtin = .{
                        .func = field,
                        .self = Value.nil,
                    } },
                } };
                len += 1;
            }
        }

        break :list std.StaticStringMap(Function).initComptime(entries[0..len]);
    };

    comptime {
        assert(list.get("polka") != null);
    }
};

const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const Value = @import("value.zig").Value;
const Function = Value.Object.Function;
const Vm = @import("Vm.zig");
const RuntimeError = Vm.RuntimeError;
const ControlFlow = Vm.ControlFlow;
