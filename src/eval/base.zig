const ast = @import("../syntax/ast.zig");
const SyntaxNode = @import("../syntax/node.zig");
const Value = @import("value.zig").Value;
const Vm = @import("Vm.zig");
const RuntimeErrorPayload = @import("error.zig").RuntimeErrorPayload;
const RuntimeError = @import("error.zig").RuntimeError;

const std = @import("std");

pub fn evalTextNode(node: ast.TextNode, vm: *Vm) !void {
    vm.locals.pushScope();
    defer vm.locals.popScope();

    // if the last node visited was a code node
    var last_was_code = false;

    var text_parts = node.text(vm.nodes);
    while (text_parts.next()) |part| {
        switch (part) {
            .code => |c| {
                const len = vm.output.items.len;
                try evalCode(c, vm);
                if (len != vm.output.items.len) {
                    try vm.outputPrint("\n", .{});
                }
                last_was_code = true;
            },
            .text => |t| {
                try vm.outputPrint("{s}", .{t.get()});
                last_was_code = false;
            },
            .newline => if (!last_was_code)
                try vm.outputPrint("\n", .{})
            else {
                last_was_code = false;
            },
        }
    }
}

pub fn evalCode(node: ast.Code, vm: *Vm) !void {

    var statements = node.statements(vm.nodes);

    while (statements.next()) |stmt| {
        switch (stmt) {
            .expr => |v| {},
            .for_loop => @panic("TODO"),
            .let_expr => @panic("TODO"),
            .while_loop => @panic("TODO"),
            .return_expr => @panic("TODO"),
            .conditional => @panic("TODO"),
            .export_expr => @panic("TODO"),
            .function_def => @panic("TODO"),
        }
    }
}
