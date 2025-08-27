pub fn main() !void {
    var dbg_allocator: std.heap.DebugAllocator(.{}) = .init;
    const allocator = if (builtin.mode == .Debug)
        dbg_allocator.allocator()
    else
        std.heap.smp_allocator;
    defer if (builtin.mode == .Debug) std.debug.assert(dbg_allocator.deinit() == .ok);

    const stderr = std.io.getStdErr().writer();
    const stdout = std.io.getStdOut().writer();
    _ = stdout;

    const params = comptime clap.parseParamsComptime(
        \\-h, --help     Display this help and exit.
        \\-r, --dry-run  Print out the files that will be changed instead of overwriting them.
        \\--init         Initialize polka in the current directory
        \\<str>...
        \\
    );
    var diag: clap.Diagnostic = .{};
    var res = clap.parse(clap.Help, &params, clap.parsers.default, .{
        .diagnostic = &diag,
        .allocator = allocator,
    }) catch |err| {
        try diag.report(stderr, err);
        std.process.exit(1);
    };
    defer res.deinit();

    if (res.args.help != 0) {
        return clap.help(stderr, clap.Help, &params, .{ .spacing_between_parameters = 0 });
    }

    if (res.args.init != 0) {
        return cli.init();
    }

    if (res.positionals[0].len == 0) fatal(
        \\Usage: polka [option]... [file]...
        \\Try 'polka --help' for more information.
        \\
    , .{});

    cli.run(res.positionals[0], std.fs.cwd(), res.args, allocator);
}

pub fn fatal(comptime fmt: []const u8, args: anytype) noreturn {
    std.debug.print(fmt, args);
    std.process.exit(1);
}

test "all" {
    std.testing.refAllDeclsRecursive(@This());
    _ = syntax_tests;
    _ = eval_test;
}

const std = @import("std");
const builtin = @import("builtin");

const clap = @import("clap");

const eval_test = @import("eval/test.zig");
const Vm = @import("eval/Vm.zig");
const cli = @import("cli.zig");
const ast = @import("syntax/ast.zig");
const SyntaxNode = @import("syntax/node.zig").SyntaxNode;
const SyntaxKind = @import("syntax/node.zig").SyntaxKind;
const parse = @import("syntax/parser.zig");
const Scanner = @import("syntax/Scanner.zig");
const syntax_tests = @import("syntax/test.zig");
