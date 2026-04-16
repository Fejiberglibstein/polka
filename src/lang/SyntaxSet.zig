//! A syntax set is used as a quick way to lookup if a kind is of a certain type.
//!
//! It is made up of a bitfield with 1s for each SyntaxKind in the set
const SyntaxSet = @This();

const BackingInt = @Int(.unsigned, @typeInfo(SyntaxKind).@"enum".fields.len);

v: BackingInt,

pub fn init(kinds: []const SyntaxKind) SyntaxSet {
    var set: SyntaxSet = .{ .v = 0 };

    for (kinds) |kind| {
        _ = set.add(kind);
    }

    return set;
}

pub fn contains(set: SyntaxSet, kind: SyntaxKind) bool {
    return set.v & (@as(BackingInt, 1) << @intFromEnum(kind)) != 0;
}

pub fn add(set: *SyntaxSet, kind: SyntaxKind) void {
    set.v |= (@as(BackingInt, 1) << @intFromEnum(kind));
}

pub fn combine(a: *SyntaxSet, b: SyntaxSet) void {
    a.v |= b.v;
}

pub fn remove(set: *SyntaxSet, kind: SyntaxKind) void {
    set.v &= ~(@as(BackingInt, 1) << @intFromEnum(kind));
}

pub const newline: SyntaxSet = .init(&.{.newline});
pub const eof: SyntaxSet = .init(&.{.eof});
pub const any: SyntaxSet = .{ .v = ~@as(BackingInt, 0) }; // Every bit is 1
pub const none: SyntaxSet = .init(&.{});

const std = @import("std");

const SyntaxKind = @import("node.zig").SyntaxKind;
