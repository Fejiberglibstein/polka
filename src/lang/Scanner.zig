source: []const u8,
cursor: usize,

const Scanner = @This();

pub fn init(source: []const u8) Scanner {
    return Scanner{ .source = source, .cursor = 0 };
}

/// Consume a single character
///
/// Returns null if at the end of the string
pub fn eat(scanner: *Scanner) ?u8 {
    if (scanner.isDone()) {
        return null;
    }

    const old = scanner.cursor;
    scanner.cursor += 1;
    return scanner.source[old];
}

pub fn moveTo(scanner: *Scanner, m: usize) void {
    scanner.cursor = m;
}

pub fn isDone(scanner: Scanner) bool {
    return scanner.cursor >= scanner.source.len;
}

pub fn after(scanner: *Scanner) []const u8 {
    return scanner.source[scanner.cursor..];
}

pub fn before(scanner: *Scanner) []const u8 {
    return scanner.source[0..scanner.cursor];
}

pub fn from(scanner: *Scanner, cursor: usize) []const u8 {
    return scanner.source[cursor..scanner.cursor];
}

/// Check if the cursor is on top of the pattern
pub fn at(scanner: *Scanner, pat: Pattern) bool {
    if (scanner.isDone()) return false;
    return pat.matches(scanner.after());
}

pub fn eatIf(scanner: *Scanner, pat: Pattern) bool {
    if (scanner.isDone()) return false;
    if (pat.matches(scanner.after())) {
        scanner.cursor += pat.length();
        return true;
    }
    return false;
}

pub fn eatUntil(scanner: *Scanner, pat: Pattern) void {
    if (scanner.isDone()) return;
    while (!pat.matches(scanner.after())) {
        _ = scanner.eat();
        if (scanner.isDone()) return;
    }
}

pub fn eatWhile(scanner: *Scanner, pat: Pattern) void {
    if (scanner.isDone()) return;
    while (pat.matches(scanner.after())) {
        scanner.cursor += pat.length();
        if (scanner.isDone()) return;
    }
}

pub fn eatSpaces(scanner: *Scanner) void {
    scanner.eatWhile([_]u8{ ' ', '\t' });
}

pub fn eatWhitespace(scanner: *Scanner) void {
    scanner.eatWhile(.{ .any = &.{ ' ', '\t', 0x0B, 0x0C } });
}

pub fn eatNewline(scanner: *Scanner) bool {
    if (scanner.isDone()) return false;

    if (scanner.at(.{ .any = &.{ '\n', '\r' } })) {
        const char = scanner.eat();
        if (char == '\r') {
            _ = scanner.eatIf(.{ .char = '\n' });
        } else {
            assert(char == '\n');
        }
        return true;
    }

    return false;
}

pub fn peek(scanner: *Scanner) ?u8 {
    if (scanner.isDone()) return null;

    return scanner.source[scanner.cursor];
}

/// https://github.com/ziglang/zig/commit/d5e21a4f1a2920ef7bbe3c54feab1a3b5119bf77#diff-adfee52549c345d50c3acbd67802c959e2ba7d46f7c747844035b898c8510888L405
///
/// Used to be in stdlib but was removed
fn isZigString(comptime T: type) bool {
    return comptime blk: {
        // Only pointer types can be strings, no optionals
        const info = @typeInfo(T);
        if (info != .pointer) break :blk false;

        const ptr = &info.pointer;
        // Check for CV qualifiers that would prevent coerction to []const u8
        if (ptr.is_volatile or ptr.is_allowzero) break :blk false;
        // If it's already a slice, simple check.
        if (ptr.size == .slice) {
            break :blk ptr.child == u8;
        }

        // Otherwise check if it's an array type that coerces to slice.
        if (ptr.size == .one) {
            const child = @typeInfo(ptr.child);
            if (child == .array) {
                const arr = &child.array;
                break :blk arr.child == u8;
            }
        }

        break :blk false;
    };
}

pub const Pattern = union(enum) {
    char: u8,
    str: []const u8,
    any: []const u8,
    func: *const fn (u8) bool,

    pub fn matches(pat: Pattern, source: []const u8) bool {
        return switch (pat) {
            .str => |str| std.mem.eql(u8, str, source[0..str.len]),
            .char => |c| c == source[0],
            .func => |f| f(source[0]),

            .any => |chars| blk: {
                for (chars) |c| {
                    if (c == source[0]) break :blk true;
                }
                break :blk false;
            },
        };
    }

    pub fn length(pat: Pattern) usize {
        switch (pat) {
            .str => |str| return str.len,
            else => return 1,
        }
    }
};

fn dbo(h: u8) bool {
    return h == 'd' or h == 'b' or h == 'o';
}

test "scanner" {
    const expect = @import("std").testing.expect;
    const expectEql = @import("std").testing.expectEqual;

    var s = Scanner.init("hello worldboh");
    try expectEql(s.cursor, 0);

    try expect(s.eat() == 'h');
    try expectEql(s.cursor, 1);

    try expect(!s.eatIf(.{ .char = 'h' }));
    try expectEql(s.cursor, 1);

    try expect(s.eatIf(.{ .char = 'e' }));
    try expectEql(s.cursor, 2);

    s.eatWhile(.{ .char = 'l' });
    try expectEql(s.cursor, 4);

    s.eatUntil(.{ .str = "or" });
    try expectEql(s.cursor, 7);

    s.eatUntil(.{ .any = &.{ 'h', 'b', 'd' } });
    try expectEql(s.cursor, 10);

    try expect(s.at(.{ .any = &.{ 'h', 'b', 'd' } }));
    try expectEql(s.cursor, 10);

    s.eatWhile(.{ .func = dbo });
    try expectEql(s.cursor, 13);

    try expect(s.eatIf(.{ .char = 'h' }));
    try expectEql(s.eat(), null);
}

const std = @import("std");
const assert = std.debug.assert;
