const ValueType = @import("value.zig").ValueType;

pub const RuntimeErrorPayload = union {
    InvalidOperands: struct { expected: ValueType, actual: ValueType },
};

pub const RuntimeError = error{Error};
