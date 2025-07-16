const ValueType = @import("../runtime/value.zig").ValueType;
const Value = @import("../runtime/value.zig").Value;

pub const RuntimeErrorPayload = union(enum(u8)) {
    /// Invalid operands to binary data
    invalid_operands: struct { expected: ValueType, actual: ValueType },
    /// The denominator of the modulus operator must be > 0. (got <rhs>)
    modulo_error: struct { rhs: Value },
};

pub const RuntimeError = error{Error};
