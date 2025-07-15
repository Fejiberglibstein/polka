const ValueType = @import("value.zig").ValueType;

pub const RuntimeErrorPayload = union(enum(u8)) {
    /// Invalid operands to binary data
    invalid_operands: struct { expected: ValueType, actual: ValueType },
};

pub const RuntimeError = error{Error};
