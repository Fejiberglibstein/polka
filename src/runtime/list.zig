const std = @import("std");
const Value = @import("value.zig").Value;

pub const List = struct {
    values: std.ArrayListUnmanaged(Value),
};
