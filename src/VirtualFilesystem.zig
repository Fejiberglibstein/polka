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

tmp: *TmpDir,
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

    item: Item,

    fn destPath(entry: Entry, pool: String.Pool) []const u8 {
        const dest_path = switch (entry.item) {
            .dir => |dir| dir.dest_path,
            .file => |file| file.dest_path,
        };
        return pool.get(dest_path);
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

        fn get(idx: Index, fs: *const VirtualFilesystem) *Entry {
            return &fs.entries.items[@intFromEnum(idx)];
        }
    };
};

pub const Dir = struct {
    dest_path: String,
    first_child: Entry.Index,
    status: union(enum) {
        /// Dir inside the .dotfiles directory that should be symlinked to
        symlinked: struct { source: Io.Dir },
        /// A dir may be unlinked if
        /// - Any of its children are not symlinkable
        /// - There exists children from two separate sources inside it (by setting the file
        ///   destination in a config file)
        /// - There exists a child in the destination directory that is not present anywhere in the
        ///   .dotfiles directory
        unlinked: void,
    },
};

pub const File = struct {
    /// The absolute path to the file that should be written to.
    dest_path: String,
    /// The file source relative to the directory containing `.polka/`
    src_path: String,
    status: union(enum) {
        /// The file path that should be symlinked is `file.src_path`
        symlinked: void,
        /// File inside a TmpDir that contains the evaluated template
        templated: struct { content: Io.File },
    },
};

pub fn init(gpa: Allocator, pool: *String.Pool, root_path: String, tmp_dir: *TmpDir) !VirtualFilesystem {
    var entries: std.ArrayList(Entry) = .empty;
    errdefer entries.deinit(gpa);
    try entries.append(gpa, .{
        .sibling = .null,
        .parent = .null,
        .item = .{ .dir = .{
            .first_child = .null,
            .dest_path = root_path,
            .status = .unlinked,
        } },
    });

    var paths: String.HashMap(Entry.Index) = .init(pool);
    errdefer paths.deinit(gpa);
    try paths.put(gpa, root_path, .root);

    const cwd: PathBuf = .empty;
    errdefer cwd.deinit();

    return .{
        .gpa = gpa,
        .cwd = cwd,
        .pool = pool,
        .tmp = tmp_dir,
        .paths = paths,
        .entries = entries,
    };
}

pub fn deinit(fs: *VirtualFilesystem) void {
    fs.entries.deinit(fs.gpa);
    fs.paths.deinit(fs.gpa);
    fs.cwd.deinit(fs.gpa);
    fs.* = undefined;
}

fn putEntry(fs: *VirtualFilesystem, entry: Entry.Item, parent_index: Entry.Index) !Entry.Index {
    assert(parent_index != .null);
    assert(fs.entries.items[@intFromEnum(parent_index)].item == .dir);

    const index: Entry.Index = @intFromEnum(try fs.entries.addOne(fs.gpa));
    assert(index != .null);

    const parent: *Entry.Item = &fs.entries.items[@intFromEnum(parent_index)].item;
    const sibling = parent.dir.first_child;
    parent.dir.first_child = index;
    fs.entries.items[@intFromEnum(index)] = .{
        .sibling = sibling,
        .parent = parent_index,
        .item = entry,
    };

    return index;
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

        return try tmp.dir.createFile(io, sub_path, opts);
    }

    pub fn deinit(self: *TmpDir, io: Io, parent_dir: Io.Dir) void {
        self.dir.close(io);
        parent_dir.deleteTree(io, path_name) catch {};
        self.* = undefined;
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
    /// This will panic if a file has already been visited without being exited.
    pub fn visitFile(self: *PathBuf, gpa: Allocator, file: []const u8) !Marker {
        assert(self.file_start == null);
        assert(file.len > 0);

        const marker: Marker = @enumFromInt(self.bytes.items.len);
        try self.bytes.appendSlice(gpa, file);

        self.file_start = marker;
        return marker;
    }

    const path_sep = std.Io.Dir.path.sep;

    /// Marks all directories above this id as unlinked.
    /// Append the directory name to the buffer. It can be exited by calling .exit()
    ///
    /// This will panic if a file has been visited without being exited.
    pub fn visitDir(self: *PathBuf, gpa: Allocator, dir: []const u8) !Marker {
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
    /// Memory will be invalidated when .exit() or .visit*() is called.
    pub fn dirname(self: *const PathBuf) []const u8 {
        if (self.bytes.items.len == 0) return self.bytes.items[0..0];

        const end = if (self.file_start) |file_start| blk: {
            const dir_end = @intFromEnum(file_start) - 1;
            assert(self.bytes.items[dir_end] == '/');
            break :blk dir_end;
        } else blk: {
            assert(self.bytes.getLast() == '/');
            break :blk self.bytes.items.len - 1;
        };
        return self.bytes.items[@intFromEnum(self.stack_top)..end];
    }

    pub fn new(self: *PathBuf, gpa: Allocator) !StackMarker {
        const ret: StackMarker = .{
            .top = self.stack_top,
            .file_start = self.file_start,
        };
        try self.bytes.append(gpa, 0);
        self.stack_top = @enumFromInt(self.bytes.items.len);
        self.file_start = null;

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

        const m1 = try buf.visitDir(gpa, "foo");
        try std.testing.expectEqualStrings(buf.path(), "foo/");
        {
            const m2 = try buf.visitDir(gpa, "bar");
            try std.testing.expectEqualStrings(buf.path(), "foo/bar/");
            {
                const m3 = try buf.visitFile(gpa, "baz.txt");
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
        _ = try buf.visitDir(gpa, "foo");
        try std.testing.expect(buf.basename() == null);
        try std.testing.expectEqualStrings(buf.dirname(), "foo");
        try std.testing.expectEqualStrings(buf.path(), "foo/");

        _ = try buf.visitDir(gpa, "bar");
        try std.testing.expect(buf.basename() == null);
        try std.testing.expectEqualStrings(buf.dirname(), "foo/bar");
        try std.testing.expectEqualStrings(buf.path(), "foo/bar/");

        _ = try buf.visitFile(gpa, "baz.txt");
        try std.testing.expectEqualStrings(buf.basename() orelse return error.Failed, "baz.txt");
        try std.testing.expectEqualStrings(buf.dirname(), "foo/bar");
        try std.testing.expectEqualStrings(buf.path(), "foo/bar/baz.txt");
    }

    test new {
        const gpa = std.testing.allocator;
        var buf: PathBuf = .empty;
        defer buf.deinit(gpa);

        _ = try buf.visitDir(gpa, "foo");
        _ = try buf.visitDir(gpa, "bar");
        _ = try buf.visitFile(gpa, "baz.txt");
        try std.testing.expectEqualStrings("foo/bar/baz.txt", buf.path());

        const sm = try buf.new(gpa);
        try std.testing.expectEqualStrings("", buf.path());
        _ = try buf.visitDir(gpa, "one");
        _ = try buf.visitDir(gpa, "two");
        _ = try buf.visitFile(gpa, "three.txt");
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
const assert = std.debug.assert;
