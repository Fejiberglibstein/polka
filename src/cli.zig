pub const run_cmd = @import("cli/run.zig");
pub const init_cmd = @import("cli/init.zig");

pub fn oom() noreturn {
    fatal("out of memory", .{});
}

pub fn fatal(comptime fmt: []const u8, args: anytype) noreturn {
    std.debug.print("polka: " ++ fmt ++ "\n", args);
    std.process.exit(1);
}

pub const Colors = struct {
    pub const reset = "\x1b[0m";
    pub const black = "\x1b[30m";
    pub const black_bg = "\x1b[40m";
    pub const red = "\x1b[31m";
    pub const red_bg = "\x1b[41m";
    pub const green = "\x1b[32m";
    pub const green_bg = "\x1b[42m";
    pub const yellow = "\x1b[33m";
    pub const yellow_bg = "\x1b[43m";
    pub const blue = "\x1b[34m";
    pub const blue_bg = "\x1b[44m";
    pub const magenta = "\x1b[35m";
    pub const magenta_bg = "\x1b[45m";
    pub const cyan = "\x1b[36m";
    pub const cyan_bg = "\x1b[46m";
    pub const white = "\x1b[37m";
    pub const white_bg = "\x1b[47m";
    pub const default = "\x1b[39m";
    pub const default_bg = "\x1b[49m";
};

const std = @import("std");
