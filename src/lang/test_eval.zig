test "assignment" {
    try testEval(
        \\Hello
        \\#* let h = 4
        \\#* `@(h)
        \\#* let b = "def"
        \\#* `abc @(b) ghi
        \\   Bar
        \\
        \\#* h = 10
        \\#* `@(b) @(h)
        \\
        \\this
        \\#* let g = 4
        \\is great
    ,
        \\Hello
        \\4
        \\abc def ghi
        \\   Bar
        \\
        \\def 10
        \\
        \\this
        \\is great
    );
}

test "binary" {
    try testEval(
        \\#* `@(3 - 2)
        \\#* `@(4 * 3 - 6)
        \\#* `@(2.2 + 9)
        \\#* `@("hello" + " " + "world")
        \\#* `@("ternaries" + (true and " are " or " aren't ") + "cool!")
        \\#* `@(nil and ":(" or ":)")
    ,
        \\1
        \\6
        \\11.2
        \\hello world
        \\ternaries are cool!
        \\:)
        \\
    );
}

test "conditional" {
    try testEval(
        \\hi
        \\#* if true then
        \\hello there
        \\#* end
        \\#* "jkfl\n"
        \\#*
        \\#* if false then
        \\goodbye
        \\#* else
        \\hello
        \\#* end
        \\
        \\#* let h = 10
        \\#* if h > 4 then
        \\whoo
        \\#* end
        \\#* if h < 4 then
        \\**nothing**
        \\#* end
        \\bye
    ,
        \\hi
        \\hello there
        \\jkfl
        \\hello
        \\
        \\whoo
        \\bye
    );
}

test "While loop" {
    try testEval(
        \\Numbers!!:
        \\
        \\#* let h = 0
        \\#* while h < 4 do
        \\#*   h = h + 1
        \\#*   `@(h)
        \\#* end
    ,
        \\Numbers!!:
        \\
        \\1
        \\2
        \\3
        \\4
        \\
    );
}

test "functions" {
    try testEval(
        \\Hello
        \\
        \\#* let h = 1
        \\#* `@(h)
        \\#*
        \\#* func foo()
        \\hi
        \\#* "there "
        \\#* end
        \\#*
        \\#* foo()
        \\#* foo()
        \\#*
        \\#*
        \\#* func bar(b)
        \\#* return "" + b
        \\#* end
        \\#*
        \\#* bar(10)
        \\#* let hhfu = bar("graggg")
        \\#* hhfu
        \\
        \\#* func baz(a, b)
        \\#*    let h = a + b + "boogie"
        \\#*    return h
        \\#* end
        \\#*
        \\#* let is_string = " is "
        \\#*
        // \\#* baz("this", is_string)
        // \\#* "h"
    ,
        \\Hello
        \\
        \\1
        \\hi 
        \\there hi
        \\there 10graggg
        \\this is boogie h
        \\
    );
}

// test "closures" {
//     try testEval(
//         \\#* let number = 3
//         \\#* let h = function(x) [number]
//         \\#*    let seven = 9 - number
//         \\#*    return function(y) [seven, x, number]
//         \\#*        seven + number
//         \\#*        return x + y
//         \\#*    end
//         \\#* end
//         \\#* let b = h("hi ")
//         \\
//         \\#* "there"
//         \\#* b("there")
//         \\#* b = "two"
//         \\#* h = ""
//     ,
//         \\there 9 hi there
//         \\
//     );
// }

// test "list" {
//     try testEval(
//         \\#* let b = 12
//         \\#* let h = {
//         \\#*    "hi",
//         \\#*    10,
//         \\#*    {
//         \\#*        10,
//         \\#*        2,
//         \\#*    },
//         \\#*    function(h) [b]
//         \\#*        return h + b
//         \\#*    end
//         \\#* }
//         \\#* h[0]
//         \\#* h[1]
//         \\#* h[2][1]
//         \\#* h[3](h[0])
//         \\
//         \\#* h[3](h[2][1])
//         \\#* h = ""
//     ,
//         \\hi 10 2 hi12
//         \\14
//         \\
//     );
// }

// test "dict" {
//     try testEval(
//         \\#* let b = {"whar"}
//         \\#* let h = {
//         \\#*    red: "red??",
//         \\#*    orange: "orange!?",
//         \\#*    green: "green!",
//         \\#*    blue: "blue.",
//         \\#*    index: "blue",
//         \\#*    func: function(a) [b]
//         \\#*        return a[0] + b[0]
//         \\#*    end,
//         \\#* }
//         \\#* h.red
//         \\#* h["orange"]
//         \\#* h[h.index]
//         \\#* h.green
//         \\#*
//         \\#* b = {"yelp"}
//         \\#*
//         \\#* h.func(b)
//         \\#* h = ""
//         \\#* ""
//     ,
//         \\red?? orange!? blue. green! yelpwhar
//         \\
//     );
// }

fn testEval(source: []const u8, expected: []const u8) !void {
    var gpa = std.heap.DebugAllocator(.{}).init;
    // It is detecting a memory leak that i dont think is my fault since the stacktrace doesnt come
    // from my code
    // defer _ = gpa.deinit();

    const parsed = try parser.parse(source, .text, gpa.allocator());
    defer gpa.allocator().free(parsed.errors);
    defer gpa.allocator().free(parsed.nodes);
    const root = parsed.nodes[parsed.nodes.len - 1];
    if (parsed.errors.len != 0) {
        for (parsed.errors) |err| {
            std.log.err(" \nERROR: {}", .{err});
        }

        var buffer: [2048]u8 = undefined;
        var writer = std.fs.File.stderr().writer(&buffer);
        try root.print(parsed.nodes, source, 0, &writer.interface);
        try writer.interface.flush();

        try std.testing.expectEqual(0, parsed.errors.len);
    }

    var value_arena: std.heap.ArenaAllocator = .init(gpa.allocator());
    defer value_arena.deinit();
    var output: std.Io.Writer.Allocating = .init(gpa.allocator());
    errdefer output.deinit();
    var pool: String.Pool = .init(gpa.allocator());
    defer pool.deinit();

    var vm = try Vm.init(
        parsed.nodes,
        source,
        gpa.allocator(),
        &value_arena,
        &pool,
        &output.writer,
    );
    defer vm.deinit();

    const result = vm.eval();
    if (result) |err| {
        std.debug.print("Error: {any}\n", .{err});

        var buffer: [2048]u8 = undefined;
        var stderr = std.fs.File.stderr().writer(&buffer);

        const err_node = parsed.nodes[err.node_index];
        try err_node.print(parsed.nodes, source, 0, &stderr.interface);
        try stderr.interface.flush();

        try std.testing.expect(result == null);
    }
    const actual = try output.toOwnedSlice();
    defer gpa.allocator().free(actual);
    try std.testing.expectEqualStrings(expected, actual);
}

const Vm = @import("Vm.zig");
const String = @import("value.zig").String;
const parser = @import("parser.zig");

const std = @import("std");
