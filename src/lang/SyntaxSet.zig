//! A syntax set is used as a quick way to lookup if a kind is of a certain type.
//!
//! It is made up of a bitfield with 1s for each SyntaxKind in the set
const SyntaxSet = @This();

const BackingInt = @Int(.unsigned, @typeInfo(SyntaxKind).@"enum".fields.len);

v: BackingInt,

pub fn init(kinds: []const SyntaxKind) SyntaxSet {
    var self: SyntaxSet = .{ .v = 0 };

    for (kinds) |kind| {
        _ = self.add(kind);
    }

    return self;
}

pub fn contains(self: SyntaxSet, kind: SyntaxKind) bool {
    return self.v & (@as(BackingInt, 1) << @intFromEnum(kind)) != 0;
}

pub fn add(self: *SyntaxSet, kind: SyntaxKind) void {
    self.v |= (@as(BackingInt, 1) << @intFromEnum(kind));
}

pub fn combine(self: *SyntaxSet, other: SyntaxSet) void {
    self.v |= other.v;
}

pub fn remove(self: *SyntaxSet, kind: SyntaxKind) void {
    self.v &= ~(@as(BackingInt, 1) << @intFromEnum(kind));
}

pub const newline: SyntaxSet = .init(&.{.newline});
pub const eof: SyntaxSet = .init(&.{.eof});
pub const any: SyntaxSet = .{ .v = ~@as(BackingInt, 0) }; // Every bit is 1
pub const none: SyntaxSet = .init(&.{});

const SyntaxKind = @import("node.zig").SyntaxKind;
const std = @import("std");
