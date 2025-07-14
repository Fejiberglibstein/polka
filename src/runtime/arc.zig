//! Atomic reference counted, like in rust
const std = @import("std");


std.ArrayListUnmanaged(comptime T: type)

pub fn Arc(T: type) type {
    return struct {
        inner: T,
        references: usize,

        pub fn copy(self: Arc) ! {

        }
    };
}
