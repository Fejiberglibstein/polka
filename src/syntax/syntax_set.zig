//! A syntax set is used as a quick way to lookup if a kind is of a certain type.
//!
//! It is made up of a bitfield with 1s for each SyntaxKind in the set

const SyntaxKind = @import("node.zig").SyntaxKind;
const SyntaxNode = @import("node.zig").SyntaxNode;
const std = @import("std");

const KindSet = u256;

fn hasKind(self: KindSet, kind: SyntaxKind) bool {
    return self & (@as(u256, 1) << @as(u8, @intCast(@intFromEnum(kind)))) != 0;
}

fn createSet(comptime kinds: []const SyntaxKind) KindSet {
    var self: KindSet = 0;
    inline for (kinds) |kind| {
        self |= 1 << @intFromEnum(kind);
    }
    return self;
}

pub const SyntaxSet = fn (SyntaxKind) bool;

pub fn isEof(kind: SyntaxKind) bool {
    return kind == .eof;
}

pub fn isBlockEnd(kind: SyntaxKind) bool {
    const set = createSet(([_]SyntaxKind{ .eof, .end })[0..]);
    return hasKind(set, kind);
}

pub fn isConditionalEnd(kind: SyntaxKind) bool {
    const set = createSet(([_]SyntaxKind{ .eof, .end, .@"else" })[0..]);
    return hasKind(set, kind);
}
