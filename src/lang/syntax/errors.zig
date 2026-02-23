pub const SyntaxErrorKind = union(enum) {
    invalid_token,
    unexpected_token: SyntaxKind,
    expected_token: struct { expected: SyntaxKind, actual: SyntaxKind },
};

pub const SyntaxError = struct {
    range: []const u8,
    position: Position,
    kind: SyntaxErrorKind,
};

const SyntaxKind = @import("node.zig").SyntaxKind;
const Position = @import("Lexer.zig").Position;
