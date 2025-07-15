const ast = @import("../syntax/ast.zig");
const SyntaxNode = @import("../syntax/node.zig");
const Value = @import("../runtime/value.zig").Value;
const Vm = @import("Vm.zig");

pub fn evalTextNode(node: ast.TextNode, vm: *Vm) !void {
    var text_parts = node.text(vm.nodes);

    // if the last node visited was a code node
    var last_was_code = false;
    while (text_parts.next()) |part| {
        switch (part) {
            .code => |c| {
                try evalCode(c, vm);
                last_was_code = true;
            },
            .text => |t| {
                try vm.content.print("{s}", .{t.get()});
                last_was_code = false;
            },
            .newline => if (!last_was_code)
                try vm.content.print("\n", .{})
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
            .conditional => |_| unreachable, // TODO
            .export_expr => unreachable, // TODO
            .for_loop => unreachable, // TODO
            .function_def => unreachable, // TODO
            .return_expr => unreachable, // TODO
            .expr => |v| try vm.writeValue(try evalExpr(v, vm)),
            .let_expr => |_| unreachable, // TODO
            .while_loop => |_| unreachable, // TODO
        }
    }
}

pub fn evalExpr(node: ast.Expr, vm: *Vm) !Value {
    _ = vm;
    return switch (node) {
        .bool => |v| Value{ .bool = v.get() },
        .nil => .nil,
        .number => |v| Value{ .number = v.get() },
        else => unreachable,
    };
}
