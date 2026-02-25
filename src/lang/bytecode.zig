pub const InstructionKind = enum(u8) {
    /// Push `Value.true` onto the stack
    push_true,
    /// Push `Value.false` onto the stack
    push_false,
    /// Push `Value.nil` onto the stack
    push_nil,
    /// Push Value.number(N) onto the stack, where N is a u8 immediately following this instruction
    push_u8,

    /// Write some text to the output file. This instruction is followed by 2 words for the slice of
    /// characters that get written
    write,

    // TOS is the top of the stack
    binary_add, // TOS = TOS1 + TOS
    binary_sub, // TOS = TOS1 - TOS
    binary_div, // TOS = TOS1 / TOS
    binary_mul, // TOS = TOS1 * TOS
    binary_mod, // TOS = TOS1 % TOS
    binary_equ, // TOS = TOS1 == TOS
    binary_neq, // TOS = TOS1 ~= TOS
    binary_lte, // TOS = TOS1 <= TOS
    binary_gte, // TOS = TOS1 >= TOS
    binary_lt, // TOS = TOS1 < TOS
    binary_gt, // TOS = TOS1 > TOS
    binary_in, // TOS = TOS1 in TOS
    binary_or, // TOS = TOS1 or TOS
    binary_and, // TOS = TOS1 and TOS

    unary_negate, // TOS = -TOS
    unary_not, // TOS = not TOS

    /// Jump forward N bytes, where N is a u32 immediately following this instruction.
    jmp,
    /// Jump forward N bytes if TOS is truthy, where N is a u32 immediately following this
    /// instruction.
    jmp_if_truthy,

    /// Jump forward N bytes if TOS is falsey, where N is a u32 immediately following this
    /// instruction.
    jmp_if_falsey,
};

pub const Chunk = struct {
    instruction_data: std.ArrayList(u8),
    allocator: std.mem.Allocator,

    pub fn init(gpa: std.mem.Allocator) Chunk {
        return .{
            .instruction_data = .empty,
            .allocator = gpa,
        };
    }

    pub fn appendBytes(self: *Chunk, bytes: []const u8) !void {
        return self.instruction_data.appendSlice(self.allocator, bytes);
    }

    pub fn appendInstruction(self: *Chunk, instruction: InstructionKind) !void {
        return self.instruction_data.append(self.allocator, @intFromEnum(instruction));
    }
};

const std = @import("std");
