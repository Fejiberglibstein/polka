const std = @import("std");

source: []const u8,
cursor: usize,

const Scanner = @This();

pub fn init(source: []const u8) Scanner {
    return Scanner{ .source = source, .cursor = 0 };
}

/// Consume a single character
///
/// Returns null if at the end of the string
pub fn eat(self: *Scanner) ?u8 {
    if (self.isDone()) {
        return null;
    }

    const old = self.cursor;
    self.cursor += 1;
    return self.source[old];
}

pub fn moveTo(self: *Scanner, m: usize) void {
    self.cursor = m;
}

pub fn isDone(self: Scanner) bool {
    return self.cursor >= self.source.len;
}

pub fn after(self: *Scanner) []const u8 {
    return self.source[self.cursor..];
}

pub fn before(self: *Scanner) []const u8 {
    return self.source[0..self.cursor];
}

pub fn from(self: *Scanner, cursor: usize) []const u8 {
    return self.source[cursor..self.cursor];
}

/// Check if the cursor is on top of the pattern
pub fn at(self: *Scanner, comptime pattern: anytype) bool {
    const pat = Pattern.get(pattern);

    if (self.isDone()) return false;
    return pat.matches(self.after());
}

pub fn eatIf(self: *Scanner, comptime pattern: anytype) bool {
    const pat = Pattern.get(pattern);

    if (self.isDone()) return false;
    if (pat.matches(self.after())) {
        self.cursor += pat.length();
        return true;
    }
    return false;
}

pub fn eatUntil(self: *Scanner, comptime pattern: anytype) void {
    const pat = Pattern.get(pattern);

    if (self.isDone()) return;
    while (!pat.matches(self.after())) {
        _ = self.eat();
        if (self.isDone()) return;
    }
}

pub fn eatWhile(self: *Scanner, comptime pattern: anytype) void {
    const pat = Pattern.get(pattern);

    if (self.isDone()) return;
    while (pat.matches(self.after())) {
        self.cursor += pat.length();
        if (self.isDone()) return;
    }
}

pub fn eatSpaces(self: *Scanner) void {
    self.eatWhile([_]u8{ ' ', '\t' });
}

pub fn eatWhitespace(self: *Scanner) void {
    self.eatWhile([_]u8{ ' ', '\t', '\n', '\r' });
}

pub fn eatNewline(self: *Scanner) bool {
    if (self.isDone()) return false;

    if (self.at([_]u8{ '\n', '\r' })) {
        if (self.eat() == '\r') {
            _ = self.eatIf('\n');
        }
        return true;
    }

    return false;
}

pub fn peek(self: *Scanner) ?u8 {
    if (self.isDone()) return null;

    return self.source[self.cursor];
}

inline fn isAlpha(c: u8) bool {
    return (c >= 'A' and c <= 'Z') or (c >= 'a' and c <= 'z');
}

pub fn eatAlpha(self: *Scanner) bool {
    const start = self.cursor;
    self.eatWhile(isAlpha);

    return start != self.cursor;
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

const Pattern = union(enum) {
    string: []const u8,
    any: []const u8,
    char: u8,
    @"fn": fn (u8) callconv(.@"inline") bool,

    pub fn get(comptime pat: anytype) Pattern {
        return sw: switch (@typeInfo(@TypeOf(pat))) {
            .comptime_int => .{ .char = pat },
            .int => |_| .{ .Char = pat },
            .pointer => |_| if (isZigString(@TypeOf(pat)))
                .{ .string = pat }
            else
                continue :sw .void,
            .array => |v| if (v.child == u8)
                .{ .any = pat[0..] }
            else
                continue :sw .void,
            .@"fn" => |_| if (@TypeOf(pat) == @FieldType(Pattern, "fn"))
                .{ .@"fn" = pat }
            else
                continue :sw .void,
            else => @compileError("Type " ++ @typeName(@TypeOf(pat)) ++ " is not a pattern"),
        };
    }

    pub fn matches(self: Pattern, source: []const u8) bool {
        switch (self) {
            .string => |str| {
                return std.mem.eql(u8, str, source[0..str.len]);
            },

            .any => |chars| {
                for (chars) |c| {
                    if (c == source[0]) {
                        return true;
                    }
                }
                return false;
            },

            .char => |c| {
                return c == source[0];
            },

            .@"fn" => |f| {
                return f(source[0]);
            },
        }
    }

    pub fn length(self: Pattern) usize {
        switch (self) {
            .string => |str| return str.len,
            else => return 1,
        }
    }
};

test "scanner" {
    const expect = @import("std").testing.expect;
    const expectEql = @import("std").testing.expectEqual;

    var s = Scanner.init("hello world");
    try expectEql(s.cursor, 0);

    try expect(s.eat() == 'h');
    try expectEql(s.cursor, 1);

    try expect(!s.eatIf('h'));
    try expectEql(s.cursor, 1);

    try expect(s.eatIf('e'));
    try expectEql(s.cursor, 2);

    s.eatWhile('l');
    try expectEql(s.cursor, 4);

    s.eatUntil("or");
    try expectEql(s.cursor, 7);

    s.eatUntil([_]u8{ 'h', 'b', 'd' });
    try expectEql(s.cursor, 10);

    try expect(s.at([_]u8{ 'h', 'b', 'd' }));

    try expect(s.eatIf('d'));

    try expectEql(s.eat(), null);
}
