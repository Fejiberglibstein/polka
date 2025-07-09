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
    if (self.cursor >= self.source.len) {
        return null;
    }

    const old = self.cursor;
    self.cursor += 1;
    return self.source[old];
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
pub fn at(self: *Scanner, pat: Pattern) bool {
    return pat.matches(self.after());
}

pub fn eatIf(self: *Scanner, pat: Pattern) bool {
    if (pat.matches(self.after())) {
        self.cursor += pat.length();
        return true;
    }
    return false;
}

pub fn eatUntil(self: *Scanner, pat: Pattern) void {
    while (!pat.matches(self.after())) {
        _ = self.eat();
    }
}

pub fn eatWhile(self: *Scanner, pat: Pattern) void {
    while (pat.matches(self.after())) {
        self.cursor += pat.length();
    }
}

pub fn eatWhitespace(self: *Scanner) void {
    self.eatWhile(.{ .Any = &[_]u8{ ' ', '\t' } });
}

pub fn eatNewline(self: *Scanner) bool {
    const newline = Pattern{ .Any = &[_]u8{ '\n', '\r' } };

    if (self.at(newline)) {
        if (self.eat() == '\r') {
            _ = self.eatIf(.{ .Char = '\n' });
        }
        return true;
    }

    return false;
}

pub fn peek(self: *Scanner) ?u8 {
    if (self.cursor >= self.source.len) {
        return null;
    }

    return self.source[self.cursor];
}

const Pattern = union(enum) {
    String: []const u8,
    Any: []const u8,
    Char: u8,

    pub fn matches(self: Pattern, source: []const u8) bool {
        switch (self) {
            .String => |str| {
                return std.mem.eql(u8, str, source[0..str.len]);
            },

            .Any => |chars| {
                for (chars) |c| {
                    if (c == source[0]) {
                        return true;
                    }
                }
                return false;
            },

            .Char => |c| {
                return c == source[0];
            },
        }
    }

    pub fn length(self: Pattern) usize {
        switch (self) {
            .String => |str| return str.len,
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

    try expect(!s.eatIf(.{ .Char = 'h' }));
    try expectEql(s.cursor, 1);

    try expect(s.eatIf(.{ .Char = 'e' }));
    try expectEql(s.cursor, 2);

    s.eatWhile(.{ .Char = 'l' });
    try expectEql(s.cursor, 4);

    s.eatUntil(.{ .String = "or" });
    try expectEql(s.cursor, 7);

    s.eatUntil(.{ .Any = &[_]u8{ 'h', 'b', 'd' } });
    try expectEql(s.cursor, 10);

    try expect(s.at(.{ .Any = &[_]u8{ 'h', 'b', 'd' } }));

    try expect(s.eatIf(.{ .Char = 'd' }));

    try expectEql(s.eat(), null);
}
