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
        \\#* baz("this", is_string)
        \\#* " h"
    ,
        \\Hello
        \\
        \\1
        \\hi
        \\there hi
        \\there 10graggg
        \\this is boogie h
    );
}

test "list" {
    try testEval(
        \\#* let h = [
        \\#*    "hi",
        \\#*    10,
        \\#*    [
        \\#*        10,
        \\#*        2,
        \\#*    ],
        \\#*    func(h)
        \\#*        return h + 3
        \\#*    end
        \\#* ]
        \\#*
        \\#* `@(h[0])
        \\#* `@(h[1])
        \\#* `@(h[2][1])
        \\#* `@(h[3](h[0]))
        \\#* `@(h[3](h[2][0]))
        \\
        \\#* h[0] = "hello"
        \\#* h[2][0] = h[2][0] + h[2][1]
        \\#* `@(h[0])
        \\#* `@(h[2][0])
    ,
        \\hi
        \\10
        \\2
        \\hi3
        \\13
        \\
        \\hello
        \\12
        \\
    );
}

test "dict" {
    try testEval(
        \\#* let h = {
        \\#*    red = "red??",
        \\#*    orange = "orange!?",
        \\#*    green = "green!",
        \\#*    blue = "blue.",
        \\#*    index = "blue",
        \\#*    f = func(a)
        \\#*        return a[0]
        \\#*    end,
        \\#*    thing = {
        \\#*        hello = "a",
        \\#*        there = "b"
        \\#*    }
        \\#* }
        \\#*
        \\#* let b = ["yelp"]
        \\#*
        \\#* `@(h.red)
        \\#* `@(h["orange"])
        \\#* `@(h[h.index])
        \\#* `@(h.green)
        \\#* `@(h.f(b))
        \\#* `@(h.thing.hello + h.thing["there"] + h["thing"].there + h["thing"]["hello"])
        \\
        \\#* h.index     = "green"
        \\#* h.red       = "not " + h.red
        \\#* h["orange"] = "not " + h.orange
        \\#* h.blue      = "not " + h["blue"]
        \\#* h[h.index]  = "not " + h[h.index]
        \\#* h["thing"].hello = "b"
        \\#* h.thing["there"] = "a"
        \\#*
        \\#* `@(h.red)
        \\#* `@(h["orange"])
        \\#* `@(h.blue)
        \\#* `@(h[h.index])
        \\#* `@(h.thing.hello + h.thing["there"] + h["thing"].there + h["thing"]["hello"])
    ,
        \\red??
        \\orange!?
        \\blue.
        \\green!
        \\yelp
        \\abba
        \\
        \\not red??
        \\not orange!?
        \\not blue.
        \\not green!
        \\baab
        \\
    );
}

fn testEval(source: []const u8, expected: []const u8) !void {
    var gpa = std.heap.DebugAllocator(.{}).init;
    const io = std.testing.io;
    defer _ = gpa.deinit();

    const parsed = try parser.parse(source, .text, gpa.allocator());
    defer gpa.allocator().free(parsed.errors);
    defer gpa.allocator().free(parsed.nodes);
    const root = parsed.nodes[parsed.nodes.len - 1];
    if (parsed.errors.len != 0) {
        var buffer: [2048]u8 = undefined;
        var writer = std.Io.File.stderr().writer(io, &buffer);
        try root.print(parsed.nodes, source, 0, &writer.interface);
        try writer.interface.flush();

        for (parsed.errors) |err| {
            std.log.err(" \nERROR: {}", .{err});
        }

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

    const result = vm.run();
    if (result) |err| {
        std.debug.print("Error: {any}\n", .{err});

        var buffer: [2048]u8 = undefined;
        var stderr = std.Io.File.stderr().writer(io, &buffer);

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
const String = @import("value.zig").Value.String;
const parser = @import("parser.zig");

const std = @import("std");
