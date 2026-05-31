const length_payload_bits = 14;
const length_payload_mask: u16 = 0b00_11111111111111;
const length_payload_header: u16 = 0b10_00000000000000;
pub const String = enum(u32) {
    null = 0,
    empty = 1,
    _,

    pub fn get(string: String, pool: *const StringPool) []const u8 {
        assert(string != .null);
        if (string == .empty) return "";

        const max_bytes_to_check = 3;
        comptime assert(@bitSizeOf(String) <= max_bytes_to_check * length_payload_bits);
        const starting_index, const len = blk: {
            var ind = @intFromEnum(string);
            var len: u32 = 0;

            for (0..max_bytes_to_check) |i| {
                const bytes = pool.bytes.items[ind .. ind + 2];
                const vln_part: u16 = bytes[1] | (@as(u16, @intCast(bytes[0])) << 8);
                if (vln_part & ~length_payload_mask != length_payload_header)
                    break :blk .{ ind, len };

                const shamt: u5 = @intCast(i * length_payload_bits);
                len += @as(u32, @intCast(vln_part & length_payload_mask)) << shamt;
                ind += 2;
            } else @panic("Malformed string");
        };
        return pool.bytes.items[starting_index..][0..len];
    }

    pub fn slice(string: String, start: u32, end: ?u32) Slice {
        return .{
            .string = string,
            .start = start,
            .end = end,
        };
    }

    pub const Slice = struct {
        string: String,
        start: u32,
        end: ?u32,
        pub fn len(self: Slice, pool: *StringPool) usize {
            if (self.end) |end| {
                @branchHint(.likely);
                return end - self.start;
            } else {
                @branchHint(.cold);
                return self.get(pool).len;
            }
        }
        pub fn get(self: Slice, pool: *StringPool) []const u8 {
            const string = self.string.get(pool);
            const end = self.end orelse string.len;
            return string[self.start..end];
        }
    };

    pub const Pool = StringPool;
    pub const HashMap = StringHashMap;
};

/// Container for every allocated string throughout the execution of the program to go.
///
/// String values may be retrieved by calling String.get().
const StringPool = struct {
    gpa: Allocator,

    /// Each string is made up of a variable length number representing the length of the string,
    /// followed by the byte contents of the string.
    ///
    /// The variable length number made up of 1-3 u16s. The first two bytes of these u16s is always
    /// `0b10`, followed by 14 bits for the length of the string.
    bytes: std.ArrayListAligned(u8, .of(u16)),
    map: std.HashMapUnmanaged(String, void, StringIndexContext, max_load_percentage),

    pub fn init(gpa: Allocator) !String.Pool {
        var bytes: std.ArrayListAligned(u8, .of(u16)) = .empty;
        errdefer bytes.deinit(gpa);
        // add empty space for String.empty. 2 bytes so that a u16 can be padded
        _ = try bytes.appendSlice(gpa, &.{ undefined, undefined });

        return .{
            .bytes = bytes,
            .map = .empty,
            .gpa = gpa,
        };
    }

    pub fn deinit(pool: *@This()) void {
        pool.bytes.deinit(pool.gpa);
        pool.map.deinit(pool.gpa);
    }

    pub fn putSlice(pool: *@This(), slice: String.Slice) Allocator.Error!String {
        const slice_len = slice.len(pool);
        if (slice_len == 0) return .empty;
        const adp: StringIndexAdapter = .{ .pool = pool };
        const ctx: StringIndexContext = .{ .pool = pool };
        const gop = try pool.map.getOrPutContextAdapted(pool.gpa, slice.get(pool), adp, ctx);

        if (!gop.found_existing) {
            assert(pool.bytes.items.len % 2 == 0);
            defer assert(pool.bytes.items.len % 2 == 0);
            const start_index: String = @enumFromInt(pool.bytes.items.len);

            const old_len = pool.bytes.items.len;
            const num_bytes = try pool.growAndPutLen(slice_len);
            defer assert(pool.bytes.items.len == old_len + num_bytes);

            // slice.get(pool) MUST be called again here. If we try to cache it at the start of the
            // function call, it may point to invalid memory since we grew the pool's byte array.
            pool.bytes.appendSliceAssumeCapacity(slice.get(pool));
            pool.bytes.appendAssumeCapacity(0);

            // Add padding byte
            if (pool.bytes.items.len % 2 != 0)
                pool.bytes.appendAssumeCapacity(0);

            gop.key_ptr.* = start_index;
        }

        return gop.key_ptr.*;
    }

    pub fn put(pool: *@This(), str: []const u8) Allocator.Error!String {
        if (str.len == 0) return .empty;
        const adp: StringIndexAdapter = .{ .pool = pool };
        const ctx: StringIndexContext = .{ .pool = pool };
        const gop = try pool.map.getOrPutContextAdapted(pool.gpa, str, adp, ctx);

        if (!gop.found_existing) {
            assert(pool.bytes.items.len % 2 == 0);
            defer assert(pool.bytes.items.len % 2 == 0);
            const start_index: String = @enumFromInt(pool.bytes.items.len);

            const old_len = pool.bytes.items.len;
            const num_bytes = try pool.growAndPutLen(str.len);
            defer assert(pool.bytes.items.len == old_len + num_bytes);

            pool.bytes.appendSliceAssumeCapacity(str);
            pool.bytes.appendAssumeCapacity(0);

            // Add padding byte
            if (pool.bytes.items.len % 2 != 0)
                pool.bytes.appendAssumeCapacity(0);

            gop.key_ptr.* = start_index;
        }

        return gop.key_ptr.*;
    }

    fn growAndPutLen(pool: *StringPool, str_len: usize) !usize {
        const num_length_bytes = (std.math.log2(str_len) / length_payload_bits + 1) * @sizeOf(u16);
        const bytes_to_write = blk: {
            const num_bytes = num_length_bytes + str_len + 1;
            // Add an extra byte to get to multiple of 2 for padding.
            break :blk num_bytes + num_bytes % 2;
        };
        assert(pool.bytes.items.len + bytes_to_write < std.math.maxInt(u32));
        try pool.bytes.ensureUnusedCapacity(pool.gpa, bytes_to_write);

        // Write the length of the string; each byte of the length will be of the form `0b10xxxxxx`.
        // By beginning with `0b10`, we avoid collisions with utf-8 encoded strings.
        var len = str_len;
        assert(len < std.math.maxInt(u32));
        for (0..num_length_bytes / 2) |_| {
            assert(len != 0);
            const len_bits: u16 = @intCast(len & length_payload_mask);
            const bytes: u16 = length_payload_header | len_bits;
            // Store in big endian
            pool.bytes.appendAssumeCapacity(@intCast((bytes & 0xFF00) >> 8));
            pool.bytes.appendAssumeCapacity(@intCast(bytes & 0x00FF));
            len >>= length_payload_bits;
        }
        assert(len == 0);

        return bytes_to_write;
    }
};

test "StringPool bytes work" {
    const gpa = std.testing.allocator;
    var pool: StringPool = try .init(gpa);
    defer pool.deinit();

    const packNums = struct {
        pub inline fn it(comptime n: usize, comptime elems: [n]u16) [n * 2]u8 {
            comptime {
                var out: [elems.len * 2]u8 = undefined;
                for (elems, 0..) |elem, i| {
                    assert(elem & ~length_payload_mask == 0);
                    const bytes = elem | length_payload_header;
                    out[i * 2] = @intCast((bytes & 0xFF00) >> 8);
                    out[i * 2 + 1] = @intCast(bytes & 0x00FF);
                }
                return out;
            }
        }
    }.it;

    {
        const str = "help";
        const ind = try pool.put(str);
        try std.testing.expectEqualSlices(
            u8,
            (packNums(1, .{4}) ++ str ++ .{ 0, 0 }),
            pool.bytes.items[@intFromEnum(ind)..],
        );
        try std.testing.expectEqualSlices(u8, str, ind.get(&pool));
    }

    {
        const str = "hel";
        const ind = try pool.put(str);
        try std.testing.expectEqualSlices(
            u8,
            (packNums(1, .{3}) ++ str ++ .{0}),
            pool.bytes.items[@intFromEnum(ind)..],
        );
        try std.testing.expectEqualSlices(u8, str, ind.get(&pool));
    }

    {
        const str: [0b00_11111111111111]u8 = @splat('h');
        const ind = try pool.put(&str);
        try std.testing.expectEqualSlices(
            u8,
            &(packNums(1, .{0b00_11111111111111}) ++ str ++ .{0}),
            pool.bytes.items[@intFromEnum(ind)..],
        );
        try std.testing.expectEqualSlices(u8, &str, ind.get(&pool));
    }

    {
        const str: [0b01_00000000000000]u8 = @splat('h');
        const ind = try pool.put(&str);
        try std.testing.expectEqualSlices(
            u8,
            &(packNums(2, .{ 0, 1 }) ++ str ++ .{ 0, 0 }),
            pool.bytes.items[@intFromEnum(ind)..],
        );
        try std.testing.expectEqualSlices(u8, &str, ind.get(&pool));
    }

    {
        const str: [0b11_00000000000110]u8 = @splat('h');
        const ind = try pool.put(&str);
        try std.testing.expectEqualSlices(
            u8,
            &(packNums(2, .{ 6, 3 }) ++ str ++ .{ 0, 0 }),
            pool.bytes.items[@intFromEnum(ind)..],
        );
        try std.testing.expectEqualSlices(u8, &str, ind.get(&pool));
    }
}

const StringIndexContext = struct {
    pool: *StringPool,
    pub fn hash(ctx: @This(), key: String) u64 {
        return std.hash_map.hashString(key.get(ctx.pool));
    }
    pub fn eql(_: @This(), a: String, b: String) bool {
        return a == b;
    }
};

const StringIndexAdapter = struct {
    pool: *StringPool,
    pub fn hash(_: @This(), key: []const u8) u64 {
        assert(std.mem.indexOfScalar(u8, key, 0) == null);
        return std.hash_map.hashString(key);
    }
    pub fn eql(ctx: @This(), a: []const u8, b: String) bool {
        return std.mem.eql(u8, a, b.get(ctx.pool));
    }
};

/// Hashmap for strings in the StringPool as keys.
fn StringHashMap(V: type) type {
    return struct {
        unmanaged: Unmanaged,
        pool: *StringPool,

        /// The type of the unmanaged hash map underlying this wrapper
        pub const Unmanaged = std.HashMapUnmanaged(String, V, StringIndexContext, max_load_percentage);
        /// An entry, containing pointers to a key and value stored in the map
        pub const Entry = Unmanaged.Entry;
        /// A copy of a key and value which are no longer in the map
        pub const KV = Unmanaged.KV;
        /// The integer type that is the result of hashing
        pub const Hash = Unmanaged.Hash;
        /// The iterator type returned by iterator()
        pub const Iterator = Unmanaged.Iterator;

        pub const KeyIterator = Unmanaged.KeyIterator;
        pub const ValueIterator = Unmanaged.ValueIterator;

        /// The integer type used to store the size of the map
        pub const Size = Unmanaged.Size;
        /// The type returned from getOrPut and variants
        pub const GetOrPutResult = Unmanaged.GetOrPutResult;

        const Self = @This();

        pub fn init(pool: *StringPool) Self {
            return .{
                .unmanaged = .empty,
                .pool = pool,
            };
        }

        fn getContext(self: Self) StringIndexContext {
            return .{ .pool = self.pool };
        }
        fn getAdapter(self: Self) StringIndexAdapter {
            return .{ .pool = self.pool };
        }

        /// Puts the hash map into a state where any method call that would
        /// cause an existing key or value pointer to become invalidated will
        /// instead trigger an assertion.
        ///
        /// An additional call to `lockPointers` in such state also triggers an
        /// assertion.
        ///
        /// `unlockPointers` returns the hash map to the previous state.
        pub fn lockPointers(self: *Self) void {
            self.unmanaged.lockPointers();
        }

        /// Undoes a call to `lockPointers`.
        pub fn unlockPointers(self: *Self) void {
            self.unmanaged.unlockPointers();
        }

        /// Release the backing array and invalidate this map.
        /// This does *not* deinit keys, values, or the context!
        /// If your keys or values need to be released, ensure
        /// that that is done before calling this function.
        pub fn deinit(self: *Self, allocator: Allocator) void {
            self.unmanaged.deinit(allocator);
            self.* = undefined;
        }

        /// Empty the map, but keep the backing allocation for future use.
        /// This does *not* free keys or values! Be sure to
        /// release them if they need deinitialization before
        /// calling this function.
        pub fn clearRetainingCapacity(self: *Self) void {
            return self.unmanaged.clearRetainingCapacity();
        }

        /// Empty the map and release the backing allocation.
        /// This does *not* free keys or values! Be sure to
        /// release them if they need deinitialization before
        /// calling this function.
        pub fn clearAndFree(self: *Self, allocator: Allocator) void {
            return self.unmanaged.clearAndFree(allocator);
        }

        /// Return the number of items in the map.
        pub fn count(self: Self) Size {
            return self.unmanaged.count();
        }

        /// Create an iterator over the entries in the map.
        /// The iterator is invalidated if the map is modified.
        pub fn iterator(self: *const Self) Iterator {
            return self.unmanaged.iterator();
        }

        /// Create an iterator over the keys in the map.
        /// The iterator is invalidated if the map is modified.
        pub fn keyIterator(self: Self) KeyIterator {
            return self.unmanaged.keyIterator();
        }

        /// Create an iterator over the values in the map.
        /// The iterator is invalidated if the map is modified.
        pub fn valueIterator(self: Self) ValueIterator {
            return self.unmanaged.valueIterator();
        }

        /// If key exists this function cannot fail.
        /// If there is an existing item with `key`, then the result's
        /// `Entry` pointers point to it, and found_existing is true.
        /// Otherwise, puts a new item with undefined value, and
        /// the `Entry` pointers point to it. Caller should then initialize
        /// the value (but not the key).
        pub fn getOrPut(self: *Self, allocator: Allocator, key: String) Allocator.Error!GetOrPutResult {
            return self.unmanaged.getOrPutContext(allocator, key, self.getContext());
        }

        /// If key exists this function cannot fail.
        /// If there is an existing item with `key`, then the result's
        /// `Entry` pointers point to it, and found_existing is true.
        /// Otherwise, puts a new item with undefined value, and
        /// the `Entry` pointers point to it. Caller should then initialize
        /// the value (but not the key).
        pub fn getOrPutAdapted(self: *Self, allocator: Allocator, key: []const u8) Allocator.Error!GetOrPutResult {
            return self.unmanaged.getOrPutContext(allocator, try self.pool.put(key), self.getContext());
        }

        /// If there is an existing item with `key`, then the result's
        /// `Entry` pointers point to it, and found_existing is true.
        /// Otherwise, puts a new item with undefined value, and
        /// the `Entry` pointers point to it. Caller should then initialize
        /// the value (but not the key).
        /// If a new entry needs to be stored, this function asserts there
        /// is enough capacity to store it.
        pub fn getOrPutAssumeCapacity(self: *Self, key: String) GetOrPutResult {
            return self.unmanaged.getOrPutAssumeCapacityContext(key, self.getContext());
        }

        /// If there is an existing item with `key`, then the result's
        /// `Entry` pointers point to it, and found_existing is true.
        /// Otherwise, puts a new item with undefined value, and
        /// the `Entry` pointers point to it. Caller must then initialize
        /// the key and value.
        /// If a new entry needs to be stored, this function asserts there
        /// is enough capacity to store it.
        pub fn getOrPutAssumeCapacityAdapted(self: *Self, key: []const u8) GetOrPutResult {
            return self.unmanaged.getOrPutAssumeCapacityContext(try self.pool.put(key), self.getContext());
        }

        pub fn getOrPutValue(self: *Self, allocator: Allocator, key: String, value: V) Allocator.Error!Entry {
            return self.unmanaged.getOrPutValueContext(allocator, key, value, self.getContext());
        }

        /// Increases capacity, guaranteeing that insertions up until the
        /// `expected_count` will not cause an allocation, and therefore cannot fail.
        pub fn ensureTotalCapacity(self: *Self, allocator: Allocator, expected_count: Size) Allocator.Error!void {
            return self.unmanaged.ensureTotalCapacityContext(allocator, expected_count, self.getContext());
        }

        /// Increases capacity, guaranteeing that insertions up until
        /// `additional_count` **more** items will not cause an allocation, and
        /// therefore cannot fail.
        pub fn ensureUnusedCapacity(self: *Self, allocator: Allocator, additional_count: Size) Allocator.Error!void {
            return self.unmanaged.ensureUnusedCapacityContext(allocator, additional_count, self.getContext());
        }

        /// Returns the number of total elements which may be present before it is
        /// no longer guaranteed that no allocations will be performed.
        pub fn capacity(self: Self) Size {
            return self.unmanaged.capacity();
        }

        /// Clobbers any existing data. To detect if a put would clobber
        /// existing data, see `getOrPut`.
        pub fn put(self: *Self, allocator: Allocator, key: String, value: V) Allocator.Error!void {
            return self.unmanaged.putContext(allocator, key, value, self.getContext());
        }

        /// Clobbers any existing data. To detect if a put would clobber
        /// existing data, see `getOrPut`.
        pub fn putAdapted(self: *Self, allocator: Allocator, key: []const u8, value: V) Allocator.Error!void {
            return self.unmanaged.putContext(allocator, try self.pool.put(key), value, self.getContext());
        }

        /// Inserts a key-value pair into the hash map, asserting that no previous
        /// entry with the same key is already present
        pub fn putNoClobber(self: *Self, allocator: Allocator, key: String, value: V) Allocator.Error!void {
            return self.unmanaged.putNoClobberContext(allocator, key, value, self.getContext());
        }

        /// Asserts there is enough capacity to store the new key-value pair.
        /// Clobbers any existing data. To detect if a put would clobber
        /// existing data, see `getOrPutAssumeCapacity`.
        pub fn putAssumeCapacity(self: *Self, key: String, value: V) void {
            return self.unmanaged.putAssumeCapacityContext(key, value, self.getContext());
        }

        /// Asserts there is enough capacity to store the new key-value pair.
        /// Asserts that it does not clobber any existing data.
        /// To detect if a put would clobber existing data, see `getOrPutAssumeCapacity`.
        pub fn putAssumeCapacityNoClobber(self: *Self, key: String, value: V) void {
            return self.unmanaged.putAssumeCapacityNoClobberContext(key, value, self.getContext());
        }

        /// Inserts a new `Entry` into the hash map, returning the previous one, if any.
        pub fn fetchPut(self: *Self, allocator: Allocator, key: String, value: V) Allocator.Error!?KV {
            return self.unmanaged.fetchPutContext(allocator, key, value, self.getContext());
        }

        /// Inserts a new `Entry` into the hash map, returning the previous one, if any.
        /// If insertion happens, asserts there is enough capacity without allocating.
        pub fn fetchPutAssumeCapacity(self: *Self, key: String, value: V) ?KV {
            return self.unmanaged.fetchPutAssumeCapacityContext(key, value, self.getContext());
        }

        /// Removes a value from the map and returns the removed kv pair.
        pub fn fetchRemove(self: *Self, key: String) ?KV {
            return self.unmanaged.fetchRemoveContext(key, self.getContext());
        }

        pub fn fetchRemoveAdapted(self: *Self, key: anytype, ctx: anytype) ?KV {
            return self.unmanaged.fetchRemoveAdapted(key, ctx);
        }

        /// Finds the value associated with a key in the map
        pub fn get(self: Self, key: String) ?V {
            return self.unmanaged.getContext(key, self.getContext());
        }

        pub fn getPtr(self: Self, key: String) ?*V {
            return self.unmanaged.getPtrContext(key, self.getContext());
        }
        pub fn getPtrAdapted(self: Self, key: []const u8) ?*V {
            return self.unmanaged.getPtrAdapted(key, self.getAdapter());
        }

        pub fn getKeyAdapted(self: Self, key: []const u8) ?String {
            return self.unmanaged.getKeyAdapted(key, self.getAdapter());
        }

        pub fn getKeyPtr(self: Self, key: String) ?*String {
            return self.unmanaged.getKeyPtrContext(key, self.getContext());
        }
        pub fn getKeyPtrAdapted(self: Self, key: []const u8) ?*String {
            return self.unmanaged.getKeyPtrAdapted(key, self.getAdapter());
        }

        /// Finds the key and value associated with a key in the map
        pub fn getEntry(self: Self, key: String) ?Entry {
            return self.unmanaged.getEntryContext(key, self.getContext());
        }

        pub fn getEntryAdapted(self: Self, key: []const u8) ?Entry {
            return self.unmanaged.getEntryAdapted(key, self.getAdapter());
        }

        /// Check if the map contains a key
        pub fn contains(self: Self, key: String) bool {
            return self.unmanaged.containsContext(key, self.getContext());
        }

        pub fn containsAdapted(self: Self, key: []const u8) bool {
            return self.unmanaged.containsAdapted(key, self.getAdapter());
        }

        /// If there is an `Entry` with a matching key, it is deleted from
        /// the hash map, and this function returns true.  Otherwise this
        /// function returns false.
        ///
        /// TODO: answer the question in these doc comments, does this
        /// increase the unused capacity by one?
        pub fn remove(self: *Self, key: String) bool {
            return self.unmanaged.removeContext(key, self.getContext());
        }

        /// TODO: answer the question in these doc comments, does this
        /// increase the unused capacity by one?
        pub fn removeAdapted(self: *Self, key: []const u8) bool {
            return self.unmanaged.removeAdapted(key, self.getAdapter());
        }

        /// Delete the entry with key pointed to by key_ptr from the hash map.
        /// key_ptr is assumed to be a valid pointer to a key that is present
        /// in the hash map.
        ///
        /// TODO: answer the question in these doc comments, does this
        /// increase the unused capacity by one?
        pub fn removeByPtr(self: *Self, key_ptr: *String) void {
            self.unmanaged.removeByPtr(key_ptr);
        }

        /// Creates a copy of this map, using the same allocator
        pub fn clone(self: Self, allocator: Allocator) Allocator.Error!Self {
            const other = try self.unmanaged.cloneContext(allocator, self.getContext());
            return .{
                .unmanaged = other,
                .pool = self.pool,
            };
        }

        /// Set the map to an empty state, making deinitialization a no-op, and
        /// returning a copy of the original.
        pub fn move(self: *Self) Self {
            self.unmanaged.pointer_stability.assertUnlocked();
            const result = self.*;
            self.unmanaged = .empty;
            return result;
        }

        /// Rehash the map, in-place.
        ///
        /// Over time, due to the current tombstone-based implementation, a
        /// HashMap could become fragmented due to the buildup of tombstone
        /// entries that causes a performance degradation due to excessive
        /// probing. The kind of pattern that might cause this is a long-lived
        /// HashMap with repeated inserts and deletes.
        ///
        /// After this function is called, there will be no tombstones in
        /// the HashMap, each of the entries is rehashed and any existing
        /// key/value pointers into the HashMap are invalidated.
        pub fn rehash(self: *Self) void {
            self.unmanaged.rehash(self.getContext());
        }
    };
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const max_load_percentage = std.hash_map.default_max_load_percentage;
const assert = std.debug.assert;
