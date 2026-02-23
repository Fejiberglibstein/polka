/// Metadata about each InstructionKind
const InstructionData = struct {
    /// The name of the enum field
    name: [:0]const u8,
    /// The data associated with the instruction kind. this data will appear after the instruction
    /// in memory
    args: type = @TypeOf(.{}),
};

// zig fmt: off
/// Used to create InstructionKind enum
const instruction_kinds = [_]InstructionData{
    // Push the frame pointer onto the stack, and set the frame pointer to be stack.len - 1
    .{ .name = "pushfp" },
    // Pop the top value off the stack and set the frame pointer to it
    .{ .name = "popfp" },
    // Push a constant onto the stack. The u32 is a Heap.Ptr to a value in the heap
    .{ .name = "pushk", .args = struct { u32 } },
    // Push a variable onto the stack. The u32 is the offset from the frame pointer where the
    // variable lives
    .{ .name = "pushv", .args = struct { u32 } },
    // Pop a variable off the stack and discard it
    .{ .name = "pop" },
    // Write a string into the output file
    .{ .name = "write", .args = struct { []const u8 } },


    // rhs is the top of the stack, lhs is the value below it
    .{ .name = "add" },    // lhs + rhs
    .{ .name = "sub" },    // lhs - rhs
    .{ .name = "div" },    // lhs / rhs
    .{ .name = "mul" },    // lhs * rhs
    .{ .name = "mod" },    // lhs % rhs
    .{ .name = "equ" },    // lhs == rhs
    .{ .name = "neq" },    // lhs ~= rhs
    .{ .name = "lte" },    // lhs <= rhs
    .{ .name = "gte" },    // lhs >= rhs
    .{ .name = "lt" },     // lhs < rhs
    .{ .name = "gt" },     // lhs > rhs
    .{ .name = "in" },     // lhs in rhs
    .{ .name = "orr" },    // lhs orr rhs
    .{ .name = "and" },    // lhs and rhs
    .{ .name = "access" }, // lhs[rhs]


    // Before a function is called, the stack is set up in the following way:
    //
    // {
    //   frame_pointer: usize,
    //   captured_variables: [_]Value,
    //   arguments: [_]Value,
    //   function_to_be_called: Value
    // }
    //
    // The call instruction will pop off the top of the stack (function_to_be_called) & call it.
    .{ .name = "call" },
    .{ .name = "ret" },
};
// zig fmt: on

pub const InstructionKind: type = blk: {
    var fields: [instruction_kinds.len]std.builtin.Type.EnumField = undefined;
    for (instruction_kinds, 0..) |instr, i| {
        fields[i] = .{ .name = instr.name, .value = i };
    }

    break :blk @Type(std.builtin.Type{ .@"enum" = .{
        .tag_type = u8,
        .fields = &fields,
        .decls = &.{},
        .is_exhaustive = true,
    } });
};

pub const Chunk = struct {
    instruction_data: std.ArrayList(u8),

    pub const init: Chunk = .{ .instruction_data = .empty };

    fn addValue(self: *Chunk, gpa: std.mem.Allocator, value: anytype) !void {
        try self.instruction_data.appendSlice(gpa, std.mem.asBytes(&value));
    }

    pub fn addInstruction(
        self: *Chunk,
        gpa: std.mem.Allocator,
        comptime instruction: InstructionKind,
        args: anytype,
    ) !void {
        try self.addValue(gpa, instruction);

        const expected_type = instruction_kinds[@intFromEnum(instruction)].args;
        comptime if (@TypeOf(args) != expected_type)
            @compileError(
                "Expected " ++ @typeName(expected_type) ++ " but got " ++ @typeName(@TypeOf(args)),
            );

        switch (instruction) {
            .pushfp => {},
            .popfp => {},
            .pushk => try self.addValue(gpa, args.@"0"),
            .pushv => try self.addValue(gpa, args.@"0"),
            .write => try self.addValue(gpa, args.@"0"),

            .pop => {},
            .add => {},
            .sub => {},
            .div => {},
            .mul => {},
            .mod => {},
            .equ => {},
            .neq => {},
            .lte => {},
            .gte => {},
            .lt => {},
            .gt => {},
            .in => {},
            .orr => {},
            .@"and" => {},
            .access => {},

            .call => {},
            .ret => {},
        }
    }
};

const std = @import("std");
