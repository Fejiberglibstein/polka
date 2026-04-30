/// Runtime configuration for polka. This may be changed per directory (by calling `polka({ ... })`
/// in a config.polka file), or per file (by calling `polka({})` inside a template file)
///
/// Each field in the config is a COW; if you want to modify a field, call config.cloneField(.field)
// TODO there is probably a more clever data structure to use for this that won't require
// copying it for every directory that gets walked
pub const Config = struct {
    modified_bitset: BitSet,
    allocator: Allocator,

    /// Marker to denote that every field after this is modifiable.
    @"--": void = undefined,

    /// Comment strings for each filetype
    /// {
    ///     "bash": "#",
    ///     "toml": "#",
    ///     "vim":  "\"",
    /// }
    comment_strings: String.HashMap(String),

    /// Ignored files/directories. Supports globbing, and files can be unignored by prefixing them
    /// with '!', like in .gitignore
    ignored_files: std.ArrayList(String),

    /// The path where this file/directory should be placed.
    destination_path: ?String,

    // const BitSet = @Int(.unsigned, @typeInfo(Config).@"struct".fields.len - owned_fields_index);
    comptime {
        assert(@typeInfo(Config).@"struct".fields.len - owned_fields_index == @typeInfo(BitSet).int.bits);
    }
    const BitSet = u3;
    const owned_fields_index = std.meta.fieldIndex(@This(), "--").? + 1;

    pub fn init(gpa: Allocator, pool: *String.Pool) Config {
        return .{
            .allocator = gpa,
            .ignored_files = .empty,
            .destination_path = null,
            .comment_strings = .init(pool),
            .modified_bitset = 0,
        };
    }

    const FieldEnum = std.meta.FieldEnum(Config);
    fn fieldBit(comptime field: FieldEnum) BitSet {
        const field_index = std.meta.fieldIndex(Config, @tagName(field)).?;
        if (comptime field_index < owned_fields_index)
            @compileError(@tagName(field) ++ " is not modifiable");

        const index = field_index - owned_fields_index;
        comptime assert(index <= @typeInfo(BitSet).int.bits);
        return 1 << index;
    }
    pub fn isModified(self: *Config, comptime field: FieldEnum) bool {
        return self.modified_bitset & fieldBit(field) != 0;
    }
    pub fn markModified(self: *Config, comptime field: FieldEnum) void {
        self.modified_bitset |= fieldBit(field);
    }

    pub fn deinit(self: *Config) void {
        // break @"continue" is used since you can't continue inside an inline for
        inline for (@typeInfo(Config).@"struct".fields[owned_fields_index..]) |f| @"continue": {
            if (!self.isModified(std.meta.stringToEnum(FieldEnum, f.name).?)) break :@"continue";

            const FieldType, const field = blk: {
                const FieldType = @FieldType(Config, f.name);
                switch (@typeInfo(FieldType)) {
                    .optional => {
                        const ChildType = @typeInfo(FieldType).optional.child;
                        // No need to deinit the field if it's null.
                        const field = &(@field(self, f.name) orelse break :@"continue");
                        break :blk .{ ChildType, field };
                    },
                    else => break :blk .{ FieldType, &@field(self, f.name) },
                }
            };

            comptime assert(@TypeOf(field) == *FieldType);
            switch (@typeInfo(FieldType)) {
                .@"enum", .@"struct", .@"union" => {
                    if (@hasDecl(FieldType, "deinit")) field.deinit(self.allocator);
                },
                else => {},
            }
        }
        self.* = undefined;
    }

    /// Marks the field as owned, and clone it if it was not already owned.
    pub fn cloneField(self: *Config, comptime field: FieldEnum) !void {
        const field_name = @tagName(field);
        inline for (@typeInfo(Config).@"struct".fields[owned_fields_index..]) |f| {
            if (comptime std.mem.eql(u8, f.name, field_name)) {
                if (!self.isModified(field)) return;
                self.markModified(field);

                @field(self, field_name) = try @field(self, field_name).clone(self.allocator);
                return;
            }
        }

        @compileError(field_name ++ " does not need to be cloned");
    }

    pub fn copy(self: *const Config) Config {
        var new = self.*;
        new.modified_bitset = 0; // Mark all flags as unmodified
        return new;
    }
};

const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const Value = @import("lang.zig").Value;
const String = Value.String;
