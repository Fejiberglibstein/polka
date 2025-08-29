//! A syntax set is used as a quick way to lookup if a kind is of a certain type.
//!
//! It is made up of a bitfield with 1s for each SyntaxKind in the set
const SyntaxSet = @This();

v: u256,

pub fn init(kinds: []const SyntaxKind) SyntaxSet {
    var self: SyntaxSet = .{ .v = 0 };

    for (kinds) |kind| {
        self.addKind(kind);
    }

    return self;
}

pub fn hasKind(self: SyntaxSet, kind: SyntaxKind) bool {
    return self.v & (@as(u256, 1) << @as(u8, @intCast(@intFromEnum(kind)))) != 0;
}

pub fn addKind(self: *SyntaxSet, kind: SyntaxKind) void {
    self.v |= (@as(u256, 1) << @as(u8, @intCast(@intFromEnum(kind))));
}

pub fn removeKind(self: *SyntaxSet, kind: SyntaxKind) void {
    self.v &= ~(@as(u256, 1) << @as(u8, @intCast(@intFromEnum(kind))));
}

const SyntaxKind = @import("node.zig").SyntaxKind;
const SyntaxNode = @import("node.zig").SyntaxNode;
const std = @import("std");
