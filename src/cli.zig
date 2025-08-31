pub const run_cmd = @import("cli/run.zig");
pub const init_cmd = @import("cli/init.zig");

pub fn oom() noreturn {
    fatal("out of memory", .{});
}

pub fn fatal(comptime fmt: []const u8, args: anytype) noreturn {
    std.debug.print("polka: " ++ fmt ++ "\n", args);
    std.process.exit(1);
}

const std = @import("std");
