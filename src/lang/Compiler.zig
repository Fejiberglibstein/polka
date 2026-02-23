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
errors: std.ArrayList(CompilationError),

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
                try self.current_chunk.addInstruction(self.gpa, .write, .{line.get(self.src)});
            },
            .newline => {
                var newline: []const u8 = undefined;
                newline = "\n";
                try self.current_chunk.addInstruction(self.gpa, .write, .{newline});
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

fn compileExpression(self: *Compiler, expr: ast.Expression) !void {
    switch (expr) {
        .nil => {},
        .list => {},
        .dict => {},
        .true => {},
        .unary => {},
        .ident => {},
        .false => {},
        .binary => {},
        .number => {},
        .string => {},
        .grouping => {},
        .conditional => {},
        .function_def => {},
        .function_call => {},
    }
}

const CompilationError = struct {};

const std = @import("std");
const InstructionKind = @import("bytecode.zig").InstructionKind;
const Chunk = @import("bytecode.zig").Chunk;
const SyntaxNode = @import("syntax//node.zig").SyntaxNode;
const ast = @import("syntax/ast.zig");
