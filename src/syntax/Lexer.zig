const Scanner = @import("Scanner.zig");
const ErrorNode = @import("node.zig").ErrorNode;
const SyntaxNode = @import("node.zig").SyntaxNode;
const SyntaxKind = @import("node.zig").SyntaxKind;

const Lexer = @This();

pub const Mode = enum {
    /// Lines beginning with `#*`. If any text comes before the `#*`, then the Mode would be
    /// [Mode::CodeExpr] instead
    ///
    /// ```text
    ///     #* if (sys.hostname == "foo")
    ///     #*   // ...
    ///     #* end
    /// ```
    CodeLine,
    /// While in text mode, expressions beginning with `#*`
    ///
    /// ```text
    ///     The hostname is #*sys.hostname
    /// ```
    CodeExpr,
    /// Code block beginning with `#**` and ending with `**#`
    ///
    /// Code expressions inside a block do not need to begin with the normal `#*`
    ///
    /// ```text
    ///     #**
    ///     local bar = function()
    ///         return "not"
    ///     end
    ///
    ///     local foo = {
    ///         bar = bar
    ///     }
    ///     **#
    /// ```
    CodeBlock,
    /// Normal file contents
    TopLevelText,
    /// Text inside ``
    Text,

    fn isCode(self: Mode) bool {
        return self == .Codeline or self == .CodeBlock or self == .CodeExpr;
    }
};
pub const Whitespace = enum { PrecedingWhitespace, None };

s: Scanner,
mode: Mode,
currentError: ?ErrorNode,

pub fn init(source: []const u8) Lexer {
    return Lexer{
        .mode = .TopLevelText,
        .s = Scanner.init(source),
        .currentError = null,
    };
}

pub fn next(self: *Lexer) struct { SyntaxNode, SyntaxKind, Whitespace } {
    const before_spaces = self.s.cursor;
    self.s.eatWhitespace();
    const start = self.s.cursor;
    const whitespace: Whitespace = if (start != before_spaces) .PrecedingWhitespace else .None;

    const kind: SyntaxKind = undefined;

    if (self.s.eatNewline()) {
        kind = .Newline;
    } else if (self.s.peek() == null) {
        kind = .EOF;
    } else switch (self.mode) {
        .CodeLine, .CodeExpr, .CodeBlock => self.code(),
        .TopLevelText, .Text => self.text(),
    }

    const range = self.s.from(start);
    const node: SyntaxNode = if (self.currentError) |err| .err(err, range) else .leaf(kind, range);

    return .{ node, kind, whitespace };
}
