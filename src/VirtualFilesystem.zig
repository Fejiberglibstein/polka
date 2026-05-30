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

/// Cwd in the virtual file system. Can be accessed by the outside world to open files, directories,
/// etc.
// dunno if i should write this in the doccomments but all files/dir use this path as their
// dest_path
cwd: PathBuf,

/// Mapping string paths -> ID
paths: String.HashMap(Entry.Index),

/// Each entry in entries has an equivalent entry in hierarchy.
///
/// A Entry.Index is used to index into this array
entries: std.ArrayList(Entry),

pub const Entry = struct {
    /// parent directory. The root's parent is .null
    parent: Entry.Index,
    /// Singly linked list of siblings. .null for the end of the list
    sibling: Entry.Index,
    dest_path: String,

    item: Item,

    /// Pointer may be invalidated when a new string is created; Don't expect these to stick around
    /// very long
    fn destBasename(entry: Entry, pool: *const String.Pool) []const u8 {
        return path.basename(pool.get(entry.dest_path));
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

pub fn init(gpa: Allocator, pool: *String.Pool, root_path: []const u8) !VirtualFilesystem {
    var cwd: PathBuf = .empty;
    errdefer cwd.deinit(gpa);
    _ = try cwd.enterDir(gpa, root_path);

    const root_path_string = try pool.put(cwd.dirname());

    var entries: std.ArrayList(Entry) = .empty;
    errdefer entries.deinit(gpa);
    try entries.append(gpa, .{
        .dest_path = root_path_string,
        .sibling = .null,
        .parent = .null,
        .item = .{ .dir = .{
            .first_child = .null,
            .status = .unlinked,
        } },
    });

    var paths: String.HashMap(Entry.Index) = .init(pool);
    errdefer paths.deinit(gpa);
    try paths.put(gpa, root_path_string, .root);

    return .{
        .gpa = gpa,
        .cwd = cwd,
        .pool = pool,
        .paths = paths,
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
    fs.paths.deinit(fs.gpa);
    fs.cwd.deinit(fs.gpa);
    fs.* = undefined;
}

pub fn print(fs: *VirtualFilesystem, w: *Io.Writer) !void {
    try w.writeByte('[');
    try w.printInt(0, 10, .lower, .{
        .alignment = .right,
        .fill = ' ',
        .width = std.math.log10(fs.entries.items.len) + 1,
    });
    try w.writeAll("] ");
    return fs.printImpl(w, 0, 0, .root);
}

fn printImpl(fs: *VirtualFilesystem, w: *Io.Writer, depth: u6, bar_bits: u32, parent: Entry.Index) !void {
    const parent_entry = parent.get(fs);
    assert(parent_entry.item == .dir);
    {
        try w.print(" {s}", .{parent_entry.destBasename(fs.pool)});
        switch (parent_entry.item.dir.status) {
            .symlinked => |link| try w.print(" -> {s}", .{fs.pool.get(link.src_path)}),
            .unlinked => {},
        }
        try w.writeByte('\n');
    }

    if (depth > 32) return;
    var child = parent_entry.item.dir.first_child;
    while (child != .null) : (child = child.get(fs).sibling) {
        {
            try w.writeByte('[');
            try w.printInt(@intFromEnum(child), 10, .lower, .{
                .alignment = .right,
                .fill = ' ',
                .width = std.math.log10(fs.entries.items.len) + 1,
            });
            try w.writeAll("] ");
        }
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
                    child_entry.destBasename(fs.pool),
                    fs.pool.get(file.src_path),
                }),
                .symlinked => try w.print(" {s} -> {s}\n", .{
                    child_entry.destBasename(fs.pool),
                    fs.pool.get(file.src_path),
                }),
                .templated => try w.print(" {s} (template @ {s})\n", .{
                    child_entry.destBasename(fs.pool),
                    fs.pool.get(file.src_path),
                }),
            },
        }
    }
}

fn unlinkDir(fs: *VirtualFilesystem, dir_path: String) void {
    const entry_index = fs.paths.get(dir_path) orelse unreachable;
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
    var iter: path.NativeComponentIterator = .init(fs.pool.get(dir_path));
    _ = iter.last();
    const parent = iter.previous() orelse return null;
    return try fs.pool.put(parent.path);
}

pub fn addCwdFile(fs: *VirtualFilesystem, source_path: String, status: File.Status) !Entry.Index {
    const parent_path = try fs.pool.put(fs.cwd.dirname());
    // Parent must already exist in the file system from calling addCwdDir
    const parent = fs.paths.get(parent_path) orelse unreachable;
    try fs.entries.ensureUnusedCapacity(fs.gpa, 1);
    const parent_entry = parent.get(fs);
    assert(parent_entry.item == .dir);

    const file_path = try fs.pool.put(fs.cwd.path());
    const file_index: Entry.Index = @enumFromInt(fs.entries.items.len);
    try fs.paths.put(fs.gpa, file_path, file_index);
    const sibling = parent_entry.item.dir.first_child;
    parent_entry.item.dir.first_child = file_index;
    fs.entries.appendAssumeCapacity(.{
        .parent = parent,
        .sibling = sibling,
        .dest_path = file_path,
        .item = .{ .file = .{
            .src_path = source_path,
            .status = status,
        } },
    });
    if (status == .templated) {
        fs.unlinkDir(parent_entry.dest_path);
    }

    return file_index;
}

/// Adds `fs.cwd` and all of its parents to the filesystem. Returns the index of
/// the cwd.
pub fn addCwdDir(fs: *VirtualFilesystem, symlink_to_path: ?String) !Entry.Index {
    const entry_path = fs.cwd.dirname();

    const entry_path_string, const path_value_ptr = blk: {
        const gop = try fs.paths.getOrPutAdapted(fs.gpa, entry_path);
        if (gop.found_existing) return gop.value_ptr.*;
        break :blk .{ gop.key_ptr.*, gop.value_ptr };
    };

    // The next entry appended will be the child we just made, which is what we want to return.
    const ret_value: Entry.Index = @enumFromInt(fs.entries.items.len);
    path_value_ptr.* = ret_value;

    var iter: path.NativeComponentIterator = .init(entry_path);
    const last = iter.last();
    assert(last != null);
    assert(iter.peekPrevious() != null);

    // We do NOT need to offset by 1 for this index. The child has not yet been added to the list;
    // this is the index the child will have by the time it's added to the list.
    var child_index: Entry.Index = @enumFromInt(fs.entries.items.len);
    // reopen the directory since the caller is expected to close it
    var child_symlink_path: ?String = symlink_to_path;
    var child_dest_path = entry_path_string;
    var child: Entry.Item = .{ .dir = .{
        .first_child = .null,
        .status = .symlinkIf(child_symlink_path),
    } };
    loop: while (iter.previous()) |component| {
        const parent_symlink_path = if (child_symlink_path) |link|
            try fs.parentDir(link)
        else
            null;
        const parent_dest_path = try fs.pool.put(component.path);
        const parent_gop = try fs.paths.getOrPut(fs.gpa, parent_dest_path);
        if (parent_gop.found_existing) {
            try fs.entries.ensureUnusedCapacity(fs.gpa, 1);
            const parent_index = parent_gop.value_ptr.*;
            const parent = parent_index.get(fs);
            assert(parent.item == .dir);
            const sibling = parent.item.dir.first_child;
            parent.item.dir.first_child = child_index;
            fs.entries.appendAssumeCapacity(.{
                .item = child,
                .sibling = sibling,
                .parent = parent_index,
                .dest_path = child_dest_path,
            });

            // Unlink parent if paths dont match
            if (parent_symlink_path) |parent_dir_path| {
                switch (parent.item.dir.status) {
                    .unlinked => {},
                    .symlinked => |symlinked| {
                        if (parent_dir_path != symlinked.src_path) {
                            fs.unlinkDir(parent.dest_path);
                        }
                    },
                }
            }
            break :loop;
        }

        const child_ptr = try fs.entries.addOne(fs.gpa);
        assert(child_ptr == &fs.entries.items[@intFromEnum(child_index)]);
        // parent will be added next in the list
        const parent_index: Entry.Index = @enumFromInt(fs.entries.items.len);
        child_ptr.* = .{
            .item = child,
            .sibling = .null,
            .parent = parent_index,
            .dest_path = child_dest_path,
        };
        const parent: Entry.Item = .{ .dir = .{
            .first_child = child_index,
            .status = .symlinkIf(parent_symlink_path),
        } };
        parent_gop.value_ptr.* = parent_index;

        // Make the parent the new child
        child = parent;
        child_index = parent_index;
        child_dest_path = parent_dest_path;
        child_symlink_path = parent_symlink_path;
    }

    return ret_value;
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
            const exp_status = fs.pool.get(expected.item.dir.status.symlinked.src_path);
            const act_status = fs.pool.get(actual.item.dir.status.symlinked.src_path);
            try std.testing.expectEqualStrings(exp_status, act_status);
        } else try std.testing.expect(actual.item.dir.status == .unlinked);
        const exp_dest_path = fs.pool.get(expected.dest_path);
        const act_dest_path = fs.pool.get(actual.dest_path);
        try std.testing.expectEqualStrings(exp_dest_path, act_dest_path);

        last = current;
        current = actual.parent;
    }
}

// TODO rewrite this test because i think i could do a better job
test addCwdDir {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var pool: String.Pool = .init(gpa);
    var vfs: VirtualFilesystem = try .init(gpa, &pool, "/home/");
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
                .dest_path = try fs.pool.put(dest_path),
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
        const m1 = try vfs.cwd.enterDir(gpa, "help");
        defer vfs.cwd.exit(m1);
        const m2 = try vfs.cwd.enterDir(gpa, "me");
        defer vfs.cwd.exit(m2);
        const entry_index = try vfs.addCwdDir(try pool.put("a/help/d"));
        try vfs.testEntries(entry_index, &.{
            try makeEntry(&vfs, "/home/help/me", "a/help/d", .null),
            try makeEntry(&vfs, "/home/help", "a/help", .null),
            try makeEntry(&vfs, "/home", null, .null),
        });
        break :blk entry_index;
    };

    // Insert directory inside help/
    _ = blk: {
        const m1 = try vfs.cwd.enterDir(gpa, "help");
        defer vfs.cwd.exit(m1);
        const m2 = try vfs.cwd.enterDir(gpa, "a");
        defer vfs.cwd.exit(m2);
        const m3 = try vfs.cwd.enterDir(gpa, "hi");
        defer vfs.cwd.exit(m3);
        const entry_index = try vfs.addCwdDir(try pool.put("a/help/new_thing/ok"));
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
        const m1 = try vfs.cwd.enterDir(gpa, "thing");
        defer vfs.cwd.exit(m1);
        const m2 = try vfs.cwd.enterDir(gpa, "b");
        defer vfs.cwd.exit(m2);
        const entry_index = try vfs.addCwdDir(try pool.put("x/y"));
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
        const m1 = try vfs.cwd.enterDir(gpa, "help");
        defer vfs.cwd.exit(m1);
        const m2 = try vfs.cwd.enterDir(gpa, "me");
        defer vfs.cwd.exit(m2);
        const m3 = try vfs.cwd.enterDir(gpa, "again");
        defer vfs.cwd.exit(m3);
        const entry_index = try vfs.addCwdDir(try pool.put("a/help/different/foo"));
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

        const sm = try buf.new(gpa);
        try std.testing.expectEqualStrings("", buf.path());
        _ = try buf.enterDir(gpa, "one");
        _ = try buf.enterDir(gpa, "two");
        _ = try buf.enterFile(gpa, "three.txt");
        try std.testing.expectEqualStrings("one/two/three.txt", buf.path());
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
