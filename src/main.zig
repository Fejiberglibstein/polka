pub fn main() !void {
    var dbg_allocator: std.heap.DebugAllocator(.{}) = .init;
    const allocator = if (builtin.mode == .Debug)
        dbg_allocator.allocator()
    else
        std.heap.smp_allocator;
    defer if (builtin.mode == .Debug) std.debug.assert(dbg_allocator.deinit());

    const stderr = std.io.getStdErr().writer();
    const stdout = std.io.getStdOut().writer();
    _ = stdout;

    const params = comptime clap.parseParamsComptime(
        \\-h, --help    Display this help and exit.
        \\-r, --dry-run  Print out the files that will be changed instead of overwriting them.
        \\--init     Initialize polka in the current directory
        \\<str>...
        \\
    );

    var diag: clap.Diagnostic = .{};
    var res = clap.parse(clap.Help, &params, clap.parsers.default, .{
        .diagnostic = &diag,
        .allocator = allocator,
    }) catch |err| {
        try diag.report(stderr, err);
        std.posix.exit(1);
    };
    defer res.deinit();

    if (res.args.help != 0) {
        return clap.help(stderr, clap.Help, &params, .{});
    }
}

test "all" {
    std.testing.refAllDeclsRecursive(@This());
    _ = syntax_tests;
    _ = eval_test;
}

const std = @import("std");

const ast = @import("syntax/ast.zig");
const SyntaxNode = @import("syntax/node.zig").SyntaxNode;
const SyntaxKind = @import("syntax/node.zig").SyntaxKind;
const parse = @import("syntax/parser.zig");
const Scanner = @import("syntax/Scanner.zig");
const syntax_tests = @import("syntax/test.zig");
const eval_test = @import("eval/test.zig");
const Vm = @import("eval/Vm.zig");
const clap = @import("clap");
const builtin = @import("builtin");
