/// Filesystem to manage interconnections between source and destination files.
/// The vocabulary used throughout this source is defined here
///
/// - template file: file not ending in the .polka extension, containing any polka code that
///   produces text
/// - symlinked/normal: A file that does _not_ contain any polka code, or a directory who meets the
///   criteria for being symlinkable (see docs for `Dir` for greater detail)
/// - dotfiles directory: directory containing .polka, this is where you store all your templates
///   and symlinkable files.
/// - source files: files with paths relative to the dotfiles directory, these are where your
///   templates/normal files live
/// - destination files: file locations where your template/symlinked files end up. These are all
///   absolute paths
pub const VirtualFilesystem = @This();
fn HashMap(K: type, V: type) type {
    const max_load_percentage = std.hash_map.default_max_load_percentage;
    return std.HashMapUnmanaged(K, V, std.hash_map.AutoContext(K), max_load_percentage);
}

pool: *String.Pool,
gpa: Allocator,

/// Mapping string destination paths -> ID
dst_paths: String.HashMap(Entry.Index),

/// Each entry in entries has an equivalent entry in hierarchy.
///
/// A Entry.Index is used to index into this array
entries: std.ArrayList(Entry),

pub const Entry = struct {
    /// parent directory. The root's parent is .null
    parent: Entry.Index,
    /// Singly linked list of siblings. .null for the end of the list
    sibling: Entry.Index,
    /// The destination path where this file will eventually be placed in the real file system
    dst_path: String,

    item: Item,

    /// Pointer may be invalidated when a new string is created; Don't expect these to stick around
    /// very long
    fn basename(entry: Entry, pool: *const String.Pool) []const u8 {
        return path.basename(entry.dst_path.get(pool));
    }

    // TODO i hate the name of this
    const Item = union(enum) {
        dir: Dir,
        file: File,
    };

    pub const Index = enum(u16) {
        root = 0,
        null = std.math.maxInt(u16),
        _,

        fn get(idx: Index, fs: *VirtualFilesystem) *Entry {
            return &fs.entries.items[@intFromEnum(idx)];
        }
    };
    pub const root: Entry.Index = .root;
};

pub const Dir = struct {
    first_child: Entry.Index,
    status: union(enum) {
        /// Dir inside the .dotfiles directory that should be symlinked to
        symlinked: struct { src_path: String },
        /// A dir may be unlinked if
        /// - Any of its children are not symlinkable
        /// - There exists children from two separate sources inside it (by setting the file
        ///   destination in a config file)
        /// - There exists a child in the destination directory that is not present anywhere in the
        ///   .dotfiles directory
        unlinked: void,
        pub fn symlinkIf(src_path: ?String) @This() {
            return if (src_path) |link|
                .{ .symlinked = .{ .src_path = link } }
            else
                .unlinked;
        }
    },
    fn deinit(dir: Dir, io: Io) void {
        _ = dir;
        _ = io;
    }
};

pub const File = struct {
    /// The file source relative to the directory containing `.polka/`
    src_path: String,
    status: Status,
    pub const Status = union(enum) {
        /// The file path that should be symlinked is `file.src_path`
        symlinked: void,
        /// The file may be symlinked if the parent directory is to be symlinked. However, if the
        /// parent directory is not symlinked, then this file does not need to be copied.
        ///
        /// It is used for `.polka` files; it is perfectly fine for these to be symlinked, but they
        /// don't _need_ to be if it's not necessary.
        weak_link: void,
        /// File inside a TmpDir that contains the evaluated template
        templated: struct { content: Io.File },
    };
    fn deinit(file: File, io: Io) void {
        switch (file.status) {
            .templated => |template| template.content.close(io),
            .weak_link => {},
            .symlinked => {},
        }
    }
};

pub fn init(gpa: Allocator, pool: *String.Pool, root_path: String) !VirtualFilesystem {
    var paths: String.HashMap(Entry.Index) = .init(pool);
    errdefer paths.deinit(gpa);
    try paths.put(gpa, root_path, .root);

    var entries: std.ArrayList(Entry) = .empty;
    errdefer entries.deinit(gpa);
    try entries.append(gpa, .{
        .dst_path = root_path,
        .sibling = .null,
        .parent = .null,
        .item = .{ .dir = .{
            .first_child = .null,
            .status = .unlinked,
        } },
    });

    return .{
        .gpa = gpa,
        .pool = pool,
        .dst_paths = paths,
        .entries = entries,
    };
}

pub fn deinit(fs: *VirtualFilesystem, io: Io) void {
    for (fs.entries.items) |entry| {
        switch (entry.item) {
            .dir => |dir| dir.deinit(io),
            .file => |file| file.deinit(io),
        }
    }
    fs.entries.deinit(fs.gpa);
    fs.dst_paths.deinit(fs.gpa);
    fs.* = undefined;
}

pub fn print(fs: *VirtualFilesystem, w: *Io.Writer) !void {
    try fs.printIndex(w, .root);
    return fs.printImpl(w, 0, 0, .root);
}

fn printIndex(fs: *VirtualFilesystem, w: *Io.Writer, ind: Entry.Index) !void {
    try w.writeByte('[');
    try w.printInt(@intFromEnum(ind), 10, .lower, .{
        .alignment = .right,
        .fill = ' ',
        .width = std.math.log10(fs.entries.items.len - 1) + 1,
    });
    try w.writeAll("] ");
}

fn printImpl(fs: *VirtualFilesystem, w: *Io.Writer, depth: u8, bar_bits: u32, parent: Entry.Index) !void {
    const parent_entry = parent.get(fs);
    assert(parent_entry.item == .dir);
    switch (parent_entry.item.dir.status) {
        .unlinked => try w.print(" {s}\n", .{parent_entry.basename(fs.pool)}),
        .symlinked => try w.print(" {s} -> {s}\n", .{
            parent_entry.basename(fs.pool),
            parent_entry.item.dir.status.symlinked.src_path.get(fs.pool),
        }),
    }

    if (depth > 32) return;
    var child = parent_entry.item.dir.first_child;
    while (child != .null) : (child = child.get(fs).sibling) {
        try fs.printIndex(w, child);
        for (0..depth) |i| {
            if ((bar_bits & (@as(u32, 1) << @intCast(i))) == 0)
                try w.writeAll("   ")
            else
                try w.writeAll("│  ");
        }
        const child_entry = child.get(fs);
        if (child_entry.sibling != .null)
            try w.writeAll("├─ ")
        else
            try w.writeAll("╰─ ");

        const new_bits = if (child_entry.sibling != .null)
            bar_bits | (@as(u32, 1) << @intCast(depth))
        else
            bar_bits | (@as(u32, 0) << @intCast(depth));

        switch (child_entry.item) {
            .dir => try fs.printImpl(w, depth + 1, new_bits, child),
            .file => |file| switch (file.status) {
                .weak_link => try w.print(" {s} ~> {s}\n", .{
                    child_entry.basename(fs.pool),
                    file.src_path.get(fs.pool),
                }),
                .symlinked => try w.print(" {s} -> {s}\n", .{
                    child_entry.basename(fs.pool),
                    file.src_path.get(fs.pool),
                }),
                .templated => try w.print(" {s} (template @ {s})\n", .{
                    child_entry.basename(fs.pool),
                    file.src_path.get(fs.pool),
                }),
            },
        }
    }
}

fn unlinkDir(fs: *VirtualFilesystem, dst_path: String) void {
    const entry_index = fs.dst_paths.get(dst_path) orelse unreachable;
    const entry = entry_index.get(fs);
    assert(entry.item == .dir);

    var child = entry_index;
    while (child != .root) {
        const child_entry = child.get(fs);
        assert(child_entry.item == .dir);
        switch (child_entry.item.dir.status) {
            .unlinked => break,
            .symlinked => child_entry.*.item.dir.status = .unlinked,
        }
        child = child_entry.parent;
    }
}

fn parentDir(fs: *VirtualFilesystem, dir_path: String) !?String {
    const dir_path_bytes = dir_path.get(fs.pool);
    const end = std.mem.findLastLinear(u8, dir_path_bytes, path.sep_str) orelse return null;
    const slice = dir_path.slice(0, @intCast(end));
    return try fs.pool.putSlice(slice);
}

pub const AddFileArgs = struct {
    /// The destination path that this file should be placed in the physical filesystem
    dst_path: []const u8,
    /// The path that this file is located in the physical filesystem
    src_path: []const u8,
    status: File.Status,
};

/// Add a file named `dst_path` to the filesystem. Its parent directories will be created and if the
/// file may be symlinked, the parent directories will also be recursively symlinked to the matching
/// `src_path` parent directories.
pub fn addFile(fs: *VirtualFilesystem, args: AddFileArgs) !void {
    const dst_path = try fs.pool.put(args.dst_path);
    const src_path = try fs.pool.put(args.src_path);
    const status = args.status;

    const dst_parent = try fs.parentDir(dst_path) orelse unreachable;
    const src_parent = try fs.parentDir(src_path); // If it's null, it won't get symlinked.
    const parent = try fs.addDirectoryAndItsParents(dst_parent, src_parent);

    try fs.entries.ensureUnusedCapacity(fs.gpa, 1);
    const parent_entry = parent.get(fs);
    assert(parent_entry.item == .dir);

    const file_index: Entry.Index = @enumFromInt(fs.entries.items.len);
    try fs.dst_paths.put(fs.gpa, dst_path, file_index);
    const sibling = parent_entry.item.dir.first_child;
    parent_entry.item.dir.first_child = file_index;
    fs.entries.appendAssumeCapacity(.{
        .parent = parent,
        .sibling = sibling,
        .dst_path = dst_path,
        .item = .{ .file = .{
            .src_path = src_path,
            .status = status,
        } },
    });

    if (status == .templated) {
        fs.unlinkDir(parent_entry.dst_path);
    }
}

/// Adds `dest_path` and all of its parents to the filesystem, symlinking with `symlink_to_path` the
/// directories until a known parent is reached.
fn addDirectoryAndItsParents(
    fs: *VirtualFilesystem,
    dest_path: String,
    symlink_to_path: ?String,
) !Entry.Index {
    const youngest_child_gop = try fs.dst_paths.getOrPut(fs.gpa, dest_path);
    if (youngest_child_gop.found_existing) {
        const parent = youngest_child_gop.value_ptr.*;
        const parent_entry = parent.get(fs);
        switch (parent_entry.item.dir.status) {
            .symlinked => |symlinked| {
                assert(symlinked.src_path != .null);
                if (symlink_to_path orelse .null != symlinked.src_path) {
                    fs.unlinkDir(parent_entry.dst_path);
                }
            },
            else => {},
        }
        return parent;
    }

    const root_path = Entry.root.get(fs).dst_path;
    assert(std.mem.startsWith(u8, dest_path.get(fs.pool), root_path.get(fs.pool)));

    // The next entry that gets appended to entries will be the index of the child to return
    const youngest_child_index: Entry.Index = @enumFromInt(fs.entries.items.len);
    youngest_child_gop.value_ptr.* = youngest_child_index;

    var child_dest_path = dest_path;
    var child_index = youngest_child_index;
    var child_symlink_path = symlink_to_path;
    var child: Dir = .{
        .first_child = .null,
        .status = .symlinkIf(child_symlink_path),
    };
    loop: while (true) {
        const parent_symlink_path = if (child_symlink_path) |link|
            try fs.parentDir(link)
        else
            null;
        const parent_dest_path = try fs.parentDir(child_dest_path) orelse unreachable;
        const parent_gop = try fs.dst_paths.getOrPut(fs.gpa, parent_dest_path);
        if (parent_gop.found_existing) {
            try fs.entries.ensureUnusedCapacity(fs.gpa, 1);
            const parent = parent_gop.value_ptr.*;
            const parent_entry = parent.get(fs);
            assert(parent_entry.item == .dir);
            const sibling = parent_entry.item.dir.first_child;
            parent_entry.item.dir.first_child = child_index;
            fs.entries.appendAssumeCapacity(.{
                .parent = parent,
                .sibling = sibling,
                .item = .{ .dir = child },
                .dst_path = child_dest_path,
            });

            // Unlink parent if paths dont match
            switch (parent_entry.item.dir.status) {
                .symlinked => |symlinked| {
                    assert(symlinked.src_path != .null);
                    if (parent_symlink_path orelse .null != symlinked.src_path) {
                        fs.unlinkDir(parent_entry.dst_path);
                    }
                },
                else => {},
            }
            break :loop;
        }

        const child_ptr = try fs.entries.addOne(fs.gpa);
        assert(child_ptr == &fs.entries.items[@intFromEnum(child_index)]);
        // parent will be added next in the list
        const parent_index: Entry.Index = @enumFromInt(fs.entries.items.len);
        child_ptr.* = .{
            .parent = parent_index,
            .sibling = .null,
            .item = .{ .dir = child },
            .dst_path = child_dest_path,
        };
        const parent: Dir = .{
            .first_child = child_index,
            .status = .symlinkIf(parent_symlink_path),
        };
        parent_gop.value_ptr.* = parent_index;

        // Make the parent the new child
        child = parent;
        child_index = parent_index;
        child_dest_path = parent_dest_path;
        child_symlink_path = parent_symlink_path;
    }

    return youngest_child_index;
}

fn testEntries(fs: *VirtualFilesystem, start: Entry.Index, entries: []const Entry) !void {
    var last: Entry.Index = .null;
    var current = start;
    for (entries) |expected| {
        const actual = current.get(fs);
        try std.testing.expect(actual.item == .dir);
        try std.testing.expectEqual(expected.sibling, actual.sibling);
        // Make sure that last is one of the children
        blk: {
            var child = actual.item.dir.first_child;
            if (child == .null) break :blk;
            while (child != .null) : (child = child.get(fs).sibling) {
                if (child == last) break;
            } else return error.SiblingNotFound;
        }

        if (expected.item.dir.status == .symlinked) {
            try std.testing.expect(actual.item.dir.status == .symlinked);
            const exp_status = expected.item.dir.status.symlinked.src_path.get(fs.pool);
            const act_status = actual.item.dir.status.symlinked.src_path.get(fs.pool);
            try std.testing.expectEqualStrings(exp_status, act_status);
        } else try std.testing.expect(actual.item.dir.status == .unlinked);
        const exp_dest_path = expected.dst_path.get(fs.pool);
        const act_dest_path = actual.dst_path.get(fs.pool);
        try std.testing.expectEqualStrings(exp_dest_path, act_dest_path);

        last = current;
        current = actual.parent;
    }
}

// TODO rewrite this test because i think i could do a better job
test addDirectoryAndItsParents {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var pool: String.Pool = try .init(gpa);
    var vfs: VirtualFilesystem = try .init(gpa, &pool, try pool.put("/home"));
    defer vfs.deinit(io);
    defer pool.deinit();
    try std.testing.expectEqual(1, vfs.entries.items.len);

    const stderr_file = Io.File.stderr();
    var stderr_buf: [2048]u8 = undefined;
    var stderr = stderr_file.writer(io, &stderr_buf);
    defer stderr.interface.flush() catch {};

    const makeEntry = struct {
        fn it(
            fs: *VirtualFilesystem,
            dest_path: []const u8,
            link: ?[]const u8,
            sibling: Entry.Index,
        ) !Entry {
            return .{
                .parent = .null,
                .sibling = sibling,
                .dst_path = try fs.pool.put(dest_path),
                .item = .{ .dir = .{
                    .first_child = .null,
                    .status = if (link) |p|
                        .{ .symlinked = .{ .src_path = try fs.pool.put(p) } }
                    else
                        .unlinked,
                } },
            };
        }
    }.it;

    // Insert initial entries
    const help_me_index = blk: {
        const dest_path = try pool.put("/home/help/me");
        const symlink_to = try pool.put("a/help/d");
        const entry_index = try vfs.addDirectoryAndItsParents(dest_path, symlink_to);
        try std.testing.expectEqual(3, vfs.entries.items.len);
        try vfs.testEntries(entry_index, &.{
            try makeEntry(&vfs, "/home/help/me", "a/help/d", .null),
            try makeEntry(&vfs, "/home/help", "a/help", .null),
            try makeEntry(&vfs, "/home", null, .null),
        });
        break :blk entry_index;
    };

    // Insert directory inside help/
    _ = blk: {
        const dest_path = try pool.put("/home/help/a/hi");
        const symlink_to = try pool.put("a/help/new_thing/ok");
        const entry_index = try vfs.addDirectoryAndItsParents(dest_path, symlink_to);
        try std.testing.expectEqual(5, vfs.entries.items.len);
        try vfs.testEntries(entry_index, &.{
            try makeEntry(&vfs, "/home/help/a/hi", "a/help/new_thing/ok", .null),
            try makeEntry(&vfs, "/home/help/a", "a/help/new_thing", help_me_index),
            try makeEntry(&vfs, "/home/help", "a/help", .null),
            try makeEntry(&vfs, "/home", null, .null),
        });
        break :blk entry_index;
    };

    // Create new directory
    _ = blk: {
        const dest_path = try pool.put("/home/thing/b");
        const symlink_to = try pool.put("x/y");
        const entry_index = try vfs.addDirectoryAndItsParents(dest_path, symlink_to);
        try vfs.print(&stderr.interface);
        try std.testing.expectEqual(7, vfs.entries.items.len);
        try vfs.testEntries(entry_index, &.{
            try makeEntry(&vfs, "/home/thing/b", "x/y", .null),
            try makeEntry(&vfs, "/home/thing", "x", help_me_index.get(&vfs).parent),
            try makeEntry(&vfs, "/home", null, .null),
        });
        break :blk entry_index;
    };

    // Unlink help/me
    _ = blk: {
        const dest_path = try pool.put("/home/help/me/again");
        const symlink_to = try pool.put("a/help/different/foo");
        const entry_index = try vfs.addDirectoryAndItsParents(dest_path, symlink_to);
        try vfs.print(&stderr.interface);
        try std.testing.expectEqual(8, vfs.entries.items.len);
        try vfs.testEntries(entry_index, &.{
            try makeEntry(&vfs, "/home/help/me/again", "a/help/different/foo", .null),
            try makeEntry(&vfs, "/home/help/me", null, .null),
            try makeEntry(&vfs, "/home/help", null, .null),
            try makeEntry(&vfs, "/home", null, .null),
        });
        break :blk entry_index;
    };
}

/// Temporary directory to store computed results in
///
/// Copied from testing.tmpDir
pub const TmpDir = struct {
    dir: Io.Dir,

    const random_bytes_count = 12;
    const sub_path_len = std.base64.url_safe.Encoder.calcSize(random_bytes_count);

    const path_name = "tmp";

    pub fn init(io: Io, polka_dir: Io.Dir, opts: Io.Dir.OpenOptions) !TmpDir {
        const dir = try polka_dir.createDirPathOpen(io, path_name, .{ .open_options = opts });
        errdefer dir.close(io);
        return .{ .dir = dir };
    }

    pub fn createFile(tmp: *TmpDir, io: Io, opts: Io.Dir.CreateFileOptions) !Io.File {
        var random_bytes: [random_bytes_count]u8 = undefined;
        io.random(&random_bytes);
        var sub_path: [sub_path_len]u8 = undefined;
        _ = std.base64.url_safe.Encoder.encode(&sub_path, &random_bytes);

        return try tmp.dir.createFile(io, &sub_path, opts);
    }

    pub fn deinit(tmp: *TmpDir, io: Io, parent_dir: Io.Dir) void {
        tmp.dir.close(io);
        parent_dir.deleteTree(io, path_name) catch {};
        tmp.* = undefined;
    }
};

pub const PathBuf = struct {
    bytes: std.ArrayList(u8),
    file_start: ?Marker,
    stack_top: Marker,

    pub const Marker = enum(u32) { _ };

    pub const StackMarker = struct {
        top: Marker,
        file_start: ?Marker,
        pub const init: StackMarker = .{ .top = @enumFromInt(0), .file_start = null };
    };

    pub const empty: PathBuf = .{
        .bytes = .empty,
        .file_start = null,
        .stack_top = @enumFromInt(0),
    };

    pub fn deinit(self: *PathBuf, gpa: Allocator) void {
        self.bytes.deinit(gpa);
    }

    /// Append the file name to the buffer. It can be exited by calling .exit().
    ///
    /// This will panic if a file has already been entered without being exited.
    pub fn enterFile(self: *PathBuf, gpa: Allocator, file: []const u8) !Marker {
        assert(self.file_start == null);
        assert(file.len > 0);

        const marker: Marker = @enumFromInt(self.bytes.items.len);
        try self.bytes.appendSlice(gpa, file);

        self.file_start = marker;
        return marker;
    }

    /// Marks all directories above this id as unlinked.
    /// Append the directory name to the buffer. It can be exited by calling .exit()
    ///
    /// This will panic if a file has been entered without being exited.
    pub fn enterDir(self: *PathBuf, gpa: Allocator, dir: []const u8) !Marker {
        assert(dir.len > 0);
        assert(self.file_start == null);
        const marker: Marker = @enumFromInt(self.bytes.items.len);
        try self.bytes.appendSlice(gpa, dir);
        if (dir[dir.len - 1] != path_sep)
            try self.bytes.append(gpa, path_sep);

        return marker;
    }

    /// Exit a file/directory by removing it from the buffer.
    pub fn exit(self: *PathBuf, marker: Marker) void {
        const marker_num = @intFromEnum(marker);
        assert(marker_num <= self.bytes.items.len);

        if (self.file_start) |file_start| if (marker_num <= @intFromEnum(file_start)) {
            self.file_start = null;
        };

        self.bytes.items.len = marker_num;
    }

    pub fn path(self: *const PathBuf) []const u8 {
        if (@intFromEnum(self.stack_top) == self.bytes.items.len) return self.bytes.items[0..0];
        return self.bytes.items[@intFromEnum(self.stack_top)..];
    }

    /// Return the basename of the file.
    ///
    /// Memory will be invalidated when .exit() is called.
    pub fn basename(self: *const PathBuf) ?[]const u8 {
        return if (self.file_start) |file_start|
            self.bytes.items[@intFromEnum(file_start)..]
        else
            null;
    }

    /// Return the directory path.
    ///
    /// Memory will be invalidated when .exit() or .enter*() is called.
    pub fn dirname(self: *const PathBuf) []const u8 {
        if (self.bytes.items.len == 0) return self.bytes.items[0..0];

        const end = if (self.file_start) |file_start| blk: {
            if (@intFromEnum(file_start) == 0) break :blk 0;
            const dir_end = @intFromEnum(file_start) - 1;
            assert(self.bytes.items[dir_end] == '/');
            break :blk dir_end;
        } else blk: {
            assert(self.bytes.getLast() == '/');
            break :blk self.bytes.items.len - 1;
        };
        return self.bytes.items[@intFromEnum(self.stack_top)..end];
    }

    pub fn new(self: *PathBuf, gpa: Allocator, starting_path: []const u8) !StackMarker {
        const ret: StackMarker = .{
            .top = self.stack_top,
            .file_start = self.file_start,
        };
        try self.bytes.append(gpa, 0);
        self.stack_top = @enumFromInt(self.bytes.items.len);
        self.file_start = null;

        _ = try self.enterDir(gpa, starting_path);

        return ret;
    }

    pub fn delete(self: *PathBuf, stack_marker: StackMarker) void {
        assert(@intFromEnum(stack_marker.top) <= self.bytes.items.len);

        assert(self.bytes.items[@intFromEnum(self.stack_top) - 1] == 0);
        self.bytes.items.len = @intFromEnum(self.stack_top) - 1;
        self.stack_top = stack_marker.top;
        self.file_start = stack_marker.file_start;
    }

    test exit {
        const gpa = std.testing.allocator;
        var buf: PathBuf = .empty;
        try std.testing.expectEqualStrings(buf.path(), "");
        defer buf.deinit(gpa);

        const m1 = try buf.enterDir(gpa, "foo");
        try std.testing.expectEqualStrings(buf.path(), "foo/");
        {
            const m2 = try buf.enterDir(gpa, "bar");
            try std.testing.expectEqualStrings(buf.path(), "foo/bar/");
            {
                const m3 = try buf.enterFile(gpa, "baz.txt");
                try std.testing.expectEqualStrings(buf.path(), "foo/bar/baz.txt");
                buf.exit(m3);
                try std.testing.expectEqualStrings(buf.path(), "foo/bar/");
            }
            buf.exit(m2);
            try std.testing.expectEqualStrings(buf.path(), "foo/");
        }
        buf.exit(m1);
        try std.testing.expectEqualStrings(buf.path(), "");
    }

    test dirname {
        const gpa = std.testing.allocator;
        var buf: PathBuf = .empty;
        try std.testing.expect(buf.basename() == null);
        try std.testing.expectEqualStrings(buf.dirname(), "");
        try std.testing.expectEqualStrings(buf.path(), "");
        defer buf.deinit(gpa);
        _ = try buf.enterDir(gpa, "foo");
        try std.testing.expect(buf.basename() == null);
        try std.testing.expectEqualStrings(buf.dirname(), "foo");
        try std.testing.expectEqualStrings(buf.path(), "foo/");

        _ = try buf.enterDir(gpa, "bar");
        try std.testing.expect(buf.basename() == null);
        try std.testing.expectEqualStrings(buf.dirname(), "foo/bar");
        try std.testing.expectEqualStrings(buf.path(), "foo/bar/");

        _ = try buf.enterFile(gpa, "baz.txt");
        try std.testing.expectEqualStrings(buf.basename() orelse return error.Failed, "baz.txt");
        try std.testing.expectEqualStrings(buf.dirname(), "foo/bar");
        try std.testing.expectEqualStrings(buf.path(), "foo/bar/baz.txt");
    }

    test new {
        const gpa = std.testing.allocator;
        var buf: PathBuf = .empty;
        defer buf.deinit(gpa);

        _ = try buf.enterDir(gpa, "foo");
        _ = try buf.enterDir(gpa, "bar");
        _ = try buf.enterFile(gpa, "baz.txt");
        try std.testing.expectEqualStrings("foo/bar/baz.txt", buf.path());

        const sm = try buf.new(gpa, "help");
        try std.testing.expectEqualStrings("help/", buf.path());
        _ = try buf.enterDir(gpa, "one");
        _ = try buf.enterDir(gpa, "two");
        _ = try buf.enterFile(gpa, "three.txt");
        try std.testing.expectEqualStrings("help/one/two/three.txt", buf.path());
        buf.delete(sm);
        try std.testing.expectEqualStrings("foo/bar/baz.txt", buf.path());
    }
};

const String = @import("lang.zig").Value.String;

const std = @import("std");
const Allocator = std.mem.Allocator;
const Io = std.Io;
const path = std.Io.Dir.path;
const path_sep = path.sep;
const assert = std.debug.assert;
