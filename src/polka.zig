/// Runtime configuration for polka. This may be changed per directory (by calling `polka({ ... })`
/// in a config.polka file), or per file (by calling `polka({})` inside a template file)
///
/// Each field in the config is a COW; if you want to modify a field, call config.cloneField(.field)
// TODO there is probably a more clever data structure to use for this that won't require
// copying it for every directory that gets walked
pub const Config = struct {
    owned_bitset: BitSet,
    allocator: Allocator,

    /// The path where this file/directory should be placed.
    destination_path: ?String,

    /// Marker to denote that every field after this is `clone()`able
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

    // const BitSet = @Int(.unsigned, @typeInfo(Config).@"struct".fields.len - owned_fields_index);
    const BitSet = u8;
    const owned_fields_index = std.meta.fieldIndex(@This(), "--").? + 1;

    pub fn init(gpa: Allocator, pool: *String.Pool) Config {
        return .{
            .allocator = gpa,
            .ignored_files = .empty,
            .destination_path = null,
            .comment_strings = .init(pool),
            .owned_bitset = std.math.maxInt(BitSet),
        };
    }

    pub fn deinit(self: *Config) void {
        inline for (@typeInfo(Config).@"struct".fields[owned_fields_index..], 0..) |f, i| {
            if (self.owned_bitset & (1 << i) != 0) {
                @field(self, f.name).deinit(self.allocator);
            }
        }
        self.* = undefined;
    }

    /// Marks the field as owned, and clone it if it was not already owned.
    pub fn cloneField(self: *Config, comptime field: std.meta.FieldEnum(Config)) !void {
        const field_name = @tagName(field);

        inline for (@typeInfo(Config).@"struct".fields[owned_fields_index..], 0..) |f, i| {
            if (comptime std.mem.eql(u8, f.name, field_name)) {
                // If it's already owned, do nothing
                if (self.owned_bitset & (1 << i) != 0) return;

                self.owned_bitset |= 1 << i;
                @field(self, field_name) = try @field(self, field_name).clone(self.allocator);
                return;
            }
        }

        @compileError(field_name ++ " does not need to be cloned");
    }

    pub fn copy(self: *const Config) Config {
        var new = self.*;
        new.owned_bitset = 0; // Mark all flags as unowned
        return new;
    }
};

const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const Value = @import("lang.zig").Value;
const String = Value.String;
