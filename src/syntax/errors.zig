pub const SyntaxErrorKind = union(enum) {
    unexpected_character,
};

pub const SyntaxError = struct {
    kind: SyntaxErrorKind,
    range: []const u8,
    line: u32,
    col: u32,
};
