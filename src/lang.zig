pub const EvaluateOpts = struct {
    src: []const u8,

    string_pool: *Value.String.Pool,
    value_arena: *std.heap.ArenaAllocator,
    output: *std.Io.Writer,
    constants: ?builtins.Constants = null,
};

pub const EvaluateResult = struct {
    nodes: []const SyntaxNode,
    result: union(enum) {
        success: void,
        parse_fail: []const parser.SyntaxError,
        runtime_error: Vm.RuntimeErrorPayload,
    },

    pub fn deinit(self: EvaluateResult, gpa: Allocator) void {
        gpa.free(self.nodes);
        if (self.result == .parse_fail) {
            gpa.free(self.result.parse_fail);
        }
    }
};

pub fn evaluate(gpa: Allocator, opts: EvaluateOpts) !EvaluateResult {
    const parsed = try parser.parse(opts.src, opts.mode, gpa);
    if (parsed.errors.len != 0)
        return .{
            .nodes = parsed.nodes,
            .result = .{
                .parse_fail = parsed.errors,
            },
        };

    const vm: Vm = .init(.{
        .gpa = gpa,
        .nodes = parsed.nodes,
        .src = opts.src,
        .output = opts.output,
        .constants = opts.constants,
        .string_pool = opts.string_pool,
        .value_arena = opts.value_arena,
    });
    defer vm.deinit();

    vm.run();
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const Io = std.Io;

pub const ast = @import("lang/ast.zig");
pub const builtins = @import("lang/builtins.zig");
pub const eval = @import("lang/eval.zig");
pub const parser = @import("lang/parser.zig");
pub const Value = @import("lang/value.zig").Value;
pub const Vm = @import("lang/Vm.zig");
pub const SyntaxNode = @import("lang/node.zig").SyntaxNode;
pub const SyntaxKind = @import("lang/node.zig").SyntaxKind;
