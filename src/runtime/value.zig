pub const ValueType = enum(u8) {
    nil,
    bool,
    number,
    object,
};

pub const Value = union(ValueType) {
    nil: void,
    bool: bool,
    number: f64,
    object: Object,

    pub fn isTruthy(self: Value) bool {
        // Lua-like truthy values.
        return switch (self) {
            .nil => false,
            .bool => |b| b,
            else => true,
        };
    }
};

pub const ObjectType = enum(u8) {
    string,
    list,
    dict,
};

/// Represents a dynamically allocated value on the heap.
///
/// Can be any one of 
/// - String
/// - List
/// - Dict
pub const Object = struct {
    // Any potential header information that may need to exist

};

pub const String = struct {
    length: usize,
    /// Pre-computed hash of the string
    hash: u32,
    // The characters of the string are stored after the length. This struct is the equivalent of
    // ```
    // struct foo {
    //     size_t length;
    //     char rest[];
    // };
    // ```
    // in c
};
