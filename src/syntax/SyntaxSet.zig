//! A syntax set is used as a quick way to lookup if a kind is of a certain type.
//!
//! It is made up of a bitfield with 1s for each SyntaxKind in the set
const SyntaxSet = @This();

const BackingInt = @Type(std.builtin.Type{
    .int = .{
        .signedness = .unsigned,
        .bits = @typeInfo(SyntaxKind).@"enum".fields.len,
    },
});

v: BackingInt,

pub fn init(kinds: []const SyntaxKind) SyntaxSet {
    var self: SyntaxSet = .{ .v = 0 };

    for (kinds) |kind| {
        self.addKind(kind);
    }

    return self;
}

pub fn hasKind(self: SyntaxSet, kind: SyntaxKind) bool {
    return self.v & (@as(BackingInt, 1) << @intFromEnum(kind)) != 0;
}

pub fn addKind(self: *SyntaxSet, kind: SyntaxKind) void {
    self.v |= (@as(BackingInt, 1) << @intFromEnum(kind));
}

pub fn removeKind(self: *SyntaxSet, kind: SyntaxKind) void {
    self.v &= ~(@as(BackingInt, 1) << @intFromEnum(kind));
}

const SyntaxKind = @import("node.zig").SyntaxKind;
const std = @import("std");
