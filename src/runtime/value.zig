const List = @import("list.zig").List;
const String = @import("string.zig").String;

pub const ValueType = enum(u8) {
    nil,
    bool,
    number,
    list,
    string,
};

pub const Value = union(ValueType) {
    nil: void,
    bool: bool,
    number: f64,
    list: List,
    string: String,

    pub fn isTruthy(self: Value) bool {
        // Lua-like truthy values.
        return switch (self) {
            .nil => false,
            .bool => |b| b,
            else => true,
        };
    }
};
