pub const SyntaxErrorKind = union(enum) {
    unexpected_character: u8,
    unexpected_token: SyntaxKind,
    expected_token: struct { expected: SyntaxKind, actual: SyntaxKind },
};

pub const SyntaxError = struct {
    kind: SyntaxErrorKind,
    range: []const u8,
    pos: Position,
};

const SyntaxKind = @import("node.zig").SyntaxKind;
const Position = @import("Lexer.zig").Position;
