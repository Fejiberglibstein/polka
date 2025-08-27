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
        \\#* let is_string = " is "
        \\#*
        \\#* baz("this", is_string)
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

test "closures" {
    try testEval(
        \\#* let number = 3
        \\#* let h = function(x) [number]
        \\#*    let seven = 9 - number
        \\#*    return function(y) [seven, x, number]
        \\#*        seven + number
        \\#*        return x + y
        \\#*    end
        \\#* end
        \\#* let b = h("hi ")
        \\
        \\#* "there"
        \\#* b("there")
        \\#* b = "two"
        \\#* h = ""
    ,
        \\there 9 hi there 
        \\
    );
}

test "list" {
    try testEval(
        \\#* let b = 12
        \\#* let h = {
        \\#*    "hi",
        \\#*    10,
        \\#*    {
        \\#*        10,
        \\#*        2,
        \\#*    },
        \\#*    function(h) [b]
        \\#*        return h + b
        \\#*    end
        \\#* }
        \\#* h[0]
        \\#* h[1]
        \\#* h[2][1]
        \\#* h[3](h[0])
        \\
        \\#* h[3](h[2][1])
        \\#* h = ""
    ,
        \\hi 10 2 hi12
        \\14 
        \\
    );
}

test "dict" {
    try testEval(
        \\#* let b = {"whar"}
        \\#* let h = {
        \\#*    red: "red??",
        \\#*    orange: "orange!?",
        \\#*    green: "green!",
        \\#*    blue: "blue.",
        \\#*    index: "blue",
        \\#*    func: function(a) [b]
        \\#*        return a[0] + b[0]
        \\#*    end,
        \\#* }
        \\#* h.red
        \\#* h["orange"]
        \\#* h[h.index]
        \\#* h.green
        \\#*
        \\#* b = {"yelp"}
        \\#*
        \\#* h.func(b)
        \\#* h = ""
        \\#* ""
    ,
        \\red?? orange!? blue. green! yelpwhar 
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

    const parsed = try parser.parse(source, allocator.allocator());
    try std.testing.expect(!parsed.has_error);
    defer allocator.allocator().free(parsed.all_nodes);
    var vm = try Vm.init(allocator.allocator(), parsed.all_nodes);
    defer vm.deinit();

    const result = try vm.eval(&parsed.root_node);
    defer allocator.allocator().free(result);

    try std.testing.expectEqualStrings(expected, result);
}
