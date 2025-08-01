test "assignment" {
    try testEval(
        \\Hello
        \\#* let h = 4
        \\#* h
        \\
        \\#* let b = h
        \\#* b
        \\Bar
        \\
        \\#* let h = 2
        \\#* b
        \\#* h
        \\
        \\this
        \\#* let g = 4
        \\is great
    ,
        \\Hello
        \\4
        \\4
        \\Bar
        \\
        \\4 2
        \\this
        \\is great
    );
}

test "strings" {
    try testEval(
        \\#* "hello world"
        \\#* let h = "hello"
        \\#* "" + 10 + h + 10 + 2
        \\hi
    ,
        \\hello world 10hello102
        \\hi
    );
}

test "binary" {
    try testEval(
        \\#* 3 - 2
        \\#* 4 * 3 - 6
        \\#* 2.2 + 9
        \\#* -8
    ,
        \\1 6 11.2 -8
        \\
    );
}
//
// test "conditional" {
//     try testEval(
//         \\
//         \\hi
//         \\#* if true then
//         \\hello there
//         \\#*     10
//         \\#* end
//         \\#* 10
//         \\#*
//         \\#* if false then
//         \\goodbye
//         \\#* end
//         \\
//         \\#* let h = 10
//         \\#* if h > 4 then
//         \\whoo
//         \\#* end
//         \\#* if h < 4 then
//         \\**nothing**
//         \\#* end
//         \\
//         \\bye
//     ,
//         \\hi
//         \\hello there
//         \\10 10
//         \\whoo
//         \\
//         \\bye
//     );
// }
//
// test "while_loop" {
//     try testEval(
//         \\
//         \\Numbers!!:
//         \\
//         \\#* let h = 0
//         \\#* while h < 4
//         \\#* h = h + 1
//         \\#* h
//         \\#* end
//     ,
//         \\Numbers!!:
//         \\
//         \\1 2 3 4
//     );
// }
//
const Vm = @import("Vm.zig");
const parser = @import("../syntax/parser.zig");

const std = @import("std");

fn testEval(source: []const u8, expected: []const u8) !void {
    var allocator = std.heap.DebugAllocator(.{}).init;
    defer {
        _ = allocator.deinit();
    }

    const node, const nodes = try parser.parse(source, allocator.allocator());
    defer nodes.deinit();
    var vm = try Vm.init(allocator.allocator(), nodes.items);
    defer vm.deinit();

    const result = try vm.eval(node);

    try std.testing.expectEqualStrings(result, expected);
}
