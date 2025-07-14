const List = @import("list.zig").List;
const String = @import("string.zig").String;

pub const Value = union(enum(u8)) {
    nil: void,
    bool: bool,
    number: f64,
    list: List,
    string: String,
};
