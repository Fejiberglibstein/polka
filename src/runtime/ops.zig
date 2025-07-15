const Value = @import("value.zig").Value;
const Vm = @import("../eval/Vm.zig");

pub fn add(lhs: Value, rhs: Value, vm: *Vm) !Value {
    switch (lhs) {
        .bool => |b| 
    }
}


