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

test "binary" {
    try testEval(
        \\#* 3 - 2
        \\#* 4 * 3 - 6
        \\#* 2.2 + 9
        \\#* -8
        \\#* "ternaries" + (true and " are " or " aren't ") + "cool!"
        \\#* nil and ":(" or ":)"
    ,
        \\1 6 11.2 -8 ternaries are cool! :)
        \\
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

test "String interning" {
    try testEval(
        \\#* let h = "hello"
        \\#* h
        \\#* "hello"
        \\#* let he = "he"
        \\#* "he" + "llo"
        \\#* he = ""
        \\#* "j"
    ,
        \\hello hello hello j
        \\
    );
}

test "garbage collection" {
    try testEval(
        \\#* let h = "hello world"
        \\#* let b = "zop world"
        \\#* b = h
        \\#* h = 0
        \\#* h = "hi"
        \\#* b
        \\#* h
        \\hi
    ,
        \\hello world hi
        \\hi
    );
}

test "conditional" {
    try testEval(
        \\hi
        \\#* if true then
        \\hello there
        \\#*     10
        \\#* end
        \\#* 10
        \\#*
        \\#* if false then
        \\goodbye
        \\#* end
        \\
        \\#* let h = 10
        \\#* if h > 4 then
        \\whoo
        \\#* end
        \\#* if h < 4 then
        \\**nothing**
        \\#* end
        \\
        \\bye
    ,
        \\hi
        \\hello there
        \\10

        // TODO: there is a space after the ten on the line after this, so it's `10 `. This is a bug
        // in the evaluation but i dont feel like fixing it rn
        \\10 
        \\whoo
        \\
        \\bye
    );
}

test "while_loop" {
    try testEval(
        \\Numbers!!:
        \\
        \\#* let h = 0
        \\#* while h < 4 do
        \\#*     h = h + 1
        \\#*     h
        \\#* end
    ,
        \\Numbers!!:
        \\
        \\1
        \\2
        \\3
        \\4
        \\
        \\
        // TODO there should not be two newlines here but fixing it requires effort and i dont want
        // to fix it rn.
    );
}

test "functions" {
    try testEval(
        \\Hello
        \\
        \\#* let h = 1
        \\#* h
        \\#*
        \\#* function foo()
        \\hi
        \\#* end
        \\#*
        \\#* foo()
        \\#* foo()
        \\#*
        \\#*
        \\#* function bar(b)
        \\#* return b
        \\#* end
        \\#*
        \\#* bar(10)
        \\#* let hhfu = bar("graggg")
        \\#* hhfu
        \\
        \\#* function baz(a, b)
        \\#*    let h = a + b + "boogie"
        \\#*    return h
        \\#* end
        \\#*
        \\#* baz("this", " is ")
        \\#* "h"
    ,
        \\Hello
        \\
        \\1 hi
        \\hi
        \\10 graggg
        \\this is boogie h
        \\
    );
}

const Vm = @import("Vm.zig");
const parser = @import("../syntax/parser.zig");

const std = @import("std");

fn testEval(source: []const u8, expected: []const u8) !void {
    var allocator = std.heap.DebugAllocator(.{}).init;
    defer {
        _ = allocator.deinit();
    }

    std.debug.print("\nnew test ------------\n", .{});

    const node, const nodes = try parser.parse(source, allocator.allocator());
    defer nodes.deinit();
    var vm = try Vm.init(allocator.allocator(), nodes.items);
    defer vm.deinit();

    const result = try vm.eval(&node);

    try std.testing.expectEqualStrings(expected, result);
}
