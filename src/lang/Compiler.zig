const Variable = struct {
    /// The name of the variable
    name: []const u8,
    /// The index into the stack frame where this variable lives.
    frame_index: u32,
};

src: []const u8,
all_nodes: []const SyntaxNode,
gpa: std.mem.Allocator,

current_chunk: Chunk,
variables: std.ArrayList(Variable),
errors: std.ArrayList(CompilerErrorPayload),

const Compiler = @This();

pub fn init(all_nodes: []const SyntaxNode, src: []const u8, gpa: std.mem.Allocator) Compiler {
    return .{
        .all_nodes = all_nodes,
        .current_chunk = .init,
        .errors = .empty,
        .gpa = gpa,
        .src = src,
        .variables = .empty,
    };
}

pub fn getVariable(self: *Compiler, var_name: []const u8) ?Variable {
    for (self.variables.items) |variable| {
        if (variable.name == var_name) return variable;
    }
    return null;
}

pub fn setError(self: *Compiler, node: SyntaxNode, kind: CompilerErrorPayload.Kind) !noreturn {
    try self.errors.append(self.gpa, .{ .node = node, .kind = kind });

    return CompilerError.Error;
}

pub fn compile(self: *Compiler) !Chunk {
    if (self.all_nodes.len == 0) return self.current_chunk;
    const root = ast.toASTNode(ast.Text, self.all_nodes[self.all_nodes.len - 1]) orelse unreachable;
    try self.compileText(root);
    return self.current_chunk;
}

fn compileText(self: *Compiler, text: ast.Text) !void {
    var parts = text.parts(self.all_nodes);
    while (parts.next()) |part| {
        switch (part) {
            .text_line => |line| {
                try self.current_chunk.appendInstruction(self.gpa, .write);
                try self.current_chunk.appendBytes(self.gpa, std.mem.asBytes(&line.get(self.src)));
            },
            .newline => {
                try self.current_chunk.appendInstruction(self.gpa, .write);
                const newline: []const u8 = "\n";
                try self.current_chunk.appendBytes(self.gpa, std.mem.asBytes(&newline));
            },
            .code => |code| try self.compileCode(code),
        }
    }
}

fn compileCode(self: *Compiler, code: ast.Code) !void {
    var statements = code.statements(self.all_nodes);
    while (statements.next()) |statement| {
        switch (statement) {
            .for_loop => {},
            .while_loop => {},
            .expression => |expr| try self.compileExpression(expr),
            .let_statement => {},
            .break_statement => {},
            .return_statement => {},
            .export_statement => {},
            .continue_statement => {},
        }
    }
}

fn compileExpression(self: *Compiler, expr: ast.Expression) CompilerError!void {
    switch (expr) {
        .nil => try self.current_chunk.appendInstruction(self.gpa, .push_nil),
        .list => {},
        .dict => {},
        .true => try self.current_chunk.appendInstruction(self.gpa, .push_true),
        .unary => {},
        .ident => {},
        .false => try self.current_chunk.appendInstruction(self.gpa, .push_false),
        .binary => |node| try self.compileBinary(node),
        .number => {},
        .string => {},
        .integer => |node| {
            const int: u32 = node.get(self.src) catch {
                try self.setError(node.node, .number_too_large);
            };

            if (std.math.cast(u8, int)) |n| {
                try self.current_chunk.appendInstruction(self.gpa, .push_u8);
                try self.current_chunk.appendBytes(self.gpa, std.mem.asBytes(&n));
            } else @panic("TODO");
        },
        .grouping => |node| try self.compileExpression(node.inner(self.all_nodes)),
        .conditional => {},
        .function_def => {},
        .function_call => {},
    }
}

fn compileBinary(self: *Compiler, binary: ast.Binary) CompilerError!void {
    try self.compileExpression(binary.lhs(self.all_nodes));
    try self.compileExpression(binary.lhs(self.all_nodes));

    const instruction: InstructionKind = switch (binary.op(self.all_nodes)) {
        .in => .binary_in,
        .@"or" => .binary_or,
        .add => .binary_add,
        .assign => @panic("TODO"),
        .equal => .binary_equ,
        .@"and" => .binary_and,
        .less_than => .binary_lt,
        .divide => .binary_div,
        .multiply => .binary_mul,
        .subtract => .binary_sub,
        .modulo => .binary_mod,
        .greater_than => .binary_gt,
        .not_equal => .binary_neq,
        .less_than_equal => .binary_lte,
        .greater_than_equal => .binary_gte,
    };

    try self.current_chunk.appendInstruction(self.gpa, instruction);
}

const CompilerErrorPayload = struct {
    /// The node that caused the error
    node: SyntaxNode,
    kind: Kind,

    const Kind = union(enum) {
        /// Integer literal is too large
        number_too_large,
    };
};

const CompilerError = error{Error} || std.mem.Allocator.Error;

const std = @import("std");
const InstructionKind = @import("bytecode.zig").InstructionKind;
const Chunk = @import("bytecode.zig").Chunk;
const SyntaxNode = @import("syntax//node.zig").SyntaxNode;
const ast = @import("syntax/ast.zig");
