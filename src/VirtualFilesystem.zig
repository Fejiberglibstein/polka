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
    return std.HashMapUnmanaged(K, V, void, std.hash_map.default_max_load_percentage);
}

gpa: std.mem.Allocator,
pool: *String.Pool,
files: HashMap(FileID, File),
directories: HashMap(DirID, Dir),
/// child -> parent mapping. Each parent can have multiple children but a child may only have one
/// parent.
hierarchy: HashMap(ID, DirID),
/// Returns the ID of a file or directory based on its `dest_path`
dest_path_lookup: String.HashMap(ID),
/// Returns the ID of a file based on its `src_path`
src_path_lookup: String.HashMap(ID),
next_id: ID,

pub const Dir = struct {
    dest_path: String,
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

pub const Kind = enum {
    dir,
    file,
};

const DirID = enum(u16) { root = std.math.maxInt(u16), _ };
const FileID = enum(u16) { _ };
pub const ID = enum(u16) {
    root = @intFromEnum(DirID.root),
    _,
    pub fn from(id: anytype) ID {
        const T = @TypeOf(id);
        return switch (T) {
            FileID, DirID, ID => @enumFromInt(@intFromEnum(id)),
            else => @compileError("Cannot convert from " ++ @typeName(T) ++ " to ID"),
        };
    }

    fn into(id: ID, fs: *const VirtualFilesystem, comptime kind: Kind) switch (kind) {
        .dir => Dir,
        .file => File,
    } {
        switch (kind) {
            .dir => {
                const dir_id: DirID = @enumFromInt(@intFromEnum(id));
                assert(fs.directories.contains(dir_id));
                return dir_id;
            },
            .file => {
                const file_id: FileID = @enumFromInt(@intFromEnum(id));
                assert(fs.files.contains(file_id));
                return file_id;
            },
        }
    }
};

const Entry = union(Kind) {
    dir: *Dir,
    file: *File,
    pub fn from(fs: *VirtualFilesystem, id: anytype) Entry {
        const T = @TypeOf(id);
        return switch (T) {
            FileID => .{ .file = fs.files.getPtr(id) orelse unreachable },
            DirID => .{ .dir = fs.directories.getPtr(id) orelse unreachable },
            ID => blk: {
                const file_id: FileID = @enumFromInt(@intFromEnum(id));
                const dir_id: DirID = @enumFromInt(@intFromEnum(id));
                break :blk if (fs.files.getPtr(file_id)) |file|
                    .{ .file = file }
                else if (fs.directories.getPtr(dir_id)) |dir|
                    .{ .dir = dir }
                else
                    unreachable;
            },
            else => @compileError("Cannot convert from " ++ @typeName(T) ++ " to ID"),
        };
    }
};

pub fn init(gpa: std.mem.Allocator, pool: *String.Pool, home_path: String) !VirtualFilesystem {
    const root_dir: Dir = .{
        .dest_path = home_path,
        .status = .unlinked,
    };

    var directories: HashMap(DirID, Dir) = .empty;
    errdefer directories.deinit(gpa);
    try directories.put(gpa, .root, root_dir);

    const files: HashMap(FileID, File) = .empty;
    const hierarchy: HashMap(ID, DirID) = .empty;

    return .{
        .gpa = gpa,
        .pool = pool,
        .files = files,
        .hierarchy = hierarchy,
        .directories = directories,
        .next_id = @enumFromInt(0),
        .src_path_lookup = .init(pool),
        .dest_path_lookup = .init(pool),
    };
}

pub fn deinit(fs: *VirtualFilesystem) void {
    fs.files.deinit(fs.gpa);
    fs.hierarchy.deinit(fs.gpa);
    fs.directories.deinit(fs.gpa);
    fs.src_path_lookup.deinit(fs.gpa);
    fs.dest_path_lookup.deinit(fs.gpa);
}

pub fn newID(fs: *VirtualFilesystem) ID {
    const old_id = fs.next_id;
    fs.next_id = @enumFromInt(@intFromEnum(fs.next_id) + 1);
    assert(fs.next_id != .root);
    return old_id;
}

fn getDestPath(fs: *const VirtualFilesystem, id: anytype) []const u8 {
    const entry: Entry = .from(fs, id);
    const dest_path = switch (entry) {
        .dir => |dir| dir.dest_path,
        .file => |file| file.dest_path,
    };

    return fs.pool.get(dest_path);
}

/// Get the parent directory of a file. The parent of the root directory is null.
pub fn parentDir(fs: *const VirtualFilesystem, id: anytype) !?DirID {
    if (ID.from(id) == .root) return null;

    const dest_path = fs.getDestPath(id);
    var iter: path.NativeComponentIterator = .init(dest_path);
    _ = iter.last();

    const component = iter.previous() orelse unreachable;

    const new_id: ID = if (fs.dest_path_lookup.getEntryAdapted(component.path)) |entry|
        entry.value_ptr.*
    else
        // TODO maybe add the parent directory in case it doesn't already exist
        unreachable;

    return new_id.into(fs, .dir);
}

/// Marks all directories above this id as unlinked.
pub fn unlink(fs: *VirtualFilesystem, id: anytype) !void {
    const dest_path = fs.getDestPath(id);

    var iter: path.NativeComponentIterator = .init(dest_path);
    _ = iter.last();
    while (iter.previous()) |component| {
        const new_id: ID = if (fs.dest_path_lookup.getEntryAdapted(component.path)) |entry|
            entry.value_ptr.*
        else
            // TODO handle this case
            continue;

        const entry: Entry = .from(fs, new_id);
        const is_already_unlinked = switch (entry) {
            .dir => |dir| switch (dir.status) {
                .symlinked => blk: {
                    dir.status = .unlinked;
                    break :blk false;
                },
                .unlinked => true,
            },
            .file => unreachable, // Component iterator should not give us a file.
        };

        if (is_already_unlinked) break;
    }
}

/// Temporary directory to store computed results in
///
/// Copied from testing.tmpDir
pub const TmpDir = struct {
    dir: Io.Dir,
    sub_path: [sub_path_len]u8,

    const random_bytes_count = 12;
    const sub_path_len = std.base64.url_safe.Encoder.calcSize(random_bytes_count);

    pub fn init(io: Io, parent_dir: Io.Dir, opts: Io.Dir.OpenOptions) !TmpDir {
        var random_bytes: [random_bytes_count]u8 = undefined;
        io.random(&random_bytes);
        var sub_path: [sub_path_len]u8 = undefined;
        _ = std.base64.url_safe.Encoder.encode(&sub_path, &random_bytes);

        const dir = try parent_dir.createDirPathOpen(io, &sub_path, .{ .open_options = opts });
        return .{
            .dir = dir,
            .sub_path = sub_path,
        };
    }

    pub fn cleanup(self: *TmpDir, io: Io, parent_dir: Io.Dir) void {
        self.dir.close(io);
        parent_dir.deleteTree(io, &self.sub_path) catch {};
        self.* = undefined;
    }
};

pub const PathBuf = struct {
    bytes: std.ArrayList(u8),
    gpa: std.mem.Allocator,
    file_start: ?Marker,
    stack_top: Marker,

    pub const Marker = enum(u32) { _ };

    pub const StackMarker = struct {
        top: Marker,
        file_start: ?Marker,
        pub const init: StackMarker = .{ .top = @enumFromInt(0), .file_start = null };
    };

    pub fn init(gpa: std.mem.Allocator) PathBuf {
        return .{
            .gpa = gpa,
            .bytes = .empty,
            .file_start = null,
            .stack_top = @enumFromInt(0),
        };
    }

    pub fn deinit(self: *PathBuf) void {
        self.bytes.deinit(self.gpa);
    }

    /// Append the file name to the buffer. It can be exited by calling .exit().
    ///
    /// This will panic if a file has already been visited without being exited.
    pub fn visitFile(self: *PathBuf, file: []const u8) !Marker {
        assert(self.file_start == null);
        assert(file.len > 0);

        const marker: Marker = @enumFromInt(self.bytes.items.len);
        try self.bytes.appendSlice(self.gpa, file);

        self.file_start = marker;
        return marker;
    }

    const path_sep = std.Io.Dir.path.sep;

    /// Append the directory name to the buffer. It can be exited by calling .exit()
    ///
    /// This will panic if a file has been visited without being exited.
    pub fn visitDir(self: *PathBuf, dir: []const u8) !Marker {
        assert(dir.len > 0);
        assert(self.file_start == null);
        const marker: Marker = @enumFromInt(self.bytes.items.len);
        try self.bytes.appendSlice(self.gpa, dir);
        if (dir[dir.len - 1] != path_sep)
            try self.bytes.append(self.gpa, path_sep);

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

    pub fn new(self: *PathBuf) !StackMarker {
        const ret: StackMarker = .{
            .top = self.stack_top,
            .file_start = self.file_start,
        };
        try self.bytes.append(self.gpa, 0);
        self.stack_top = @enumFromInt(self.bytes.items.len);
        self.file_start = null;

        return ret;
    }

    pub fn delete(self: *PathBuf, stack_marker: StackMarker) void {
        assert(@intFromEnum(stack_marker.top) <= self.bytes.items.len);

        assert(self.bytes.items[@intFromEnum(self.stack_top) - 1] == 0);
        self.bytes.resize(self.gpa, @intFromEnum(self.stack_top) - 1) catch unreachable;
        self.stack_top = stack_marker.top;
        self.file_start = stack_marker.file_start;
    }

    test exit {
        const gpa = std.testing.allocator;
        var buf: PathBuf = .init(gpa);
        try std.testing.expectEqualStrings(buf.path(), "");
        defer buf.deinit();

        const m1 = try buf.visitDir("foo");
        try std.testing.expectEqualStrings(buf.path(), "foo/");
        {
            const m2 = try buf.visitDir("bar");
            try std.testing.expectEqualStrings(buf.path(), "foo/bar/");
            {
                const m3 = try buf.visitFile("baz.txt");
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
        var buf: PathBuf = .init(gpa);
        try std.testing.expect(buf.basename() == null);
        try std.testing.expectEqualStrings(buf.dirname(), "");
        try std.testing.expectEqualStrings(buf.path(), "");
        defer buf.deinit();
        _ = try buf.visitDir("foo");
        try std.testing.expect(buf.basename() == null);
        try std.testing.expectEqualStrings(buf.dirname(), "foo");
        try std.testing.expectEqualStrings(buf.path(), "foo/");

        _ = try buf.visitDir("bar");
        try std.testing.expect(buf.basename() == null);
        try std.testing.expectEqualStrings(buf.dirname(), "foo/bar");
        try std.testing.expectEqualStrings(buf.path(), "foo/bar/");

        _ = try buf.visitFile("baz.txt");
        try std.testing.expectEqualStrings(buf.basename() orelse return error.Failed, "baz.txt");
        try std.testing.expectEqualStrings(buf.dirname(), "foo/bar");
        try std.testing.expectEqualStrings(buf.path(), "foo/bar/baz.txt");
    }

    test new {
        const gpa = std.testing.allocator;
        var buf: PathBuf = .init(gpa);
        defer buf.deinit();

        _ = try buf.visitDir("foo");
        _ = try buf.visitDir("bar");
        _ = try buf.visitFile("baz.txt");
        try std.testing.expectEqualStrings("foo/bar/baz.txt", buf.path());

        const sm = try buf.new();
        try std.testing.expectEqualStrings("", buf.path());
        _ = try buf.visitDir("one");
        _ = try buf.visitDir("two");
        _ = try buf.visitFile("three.txt");
        try std.testing.expectEqualStrings("one/two/three.txt", buf.path());
        buf.delete(sm);
        try std.testing.expectEqualStrings("foo/bar/baz.txt", buf.path());
    }
};

const String = @import("lang.zig").Value.String;

const std = @import("std");
const Io = std.Io;
const path = std.Io.Dir.path;
const assert = std.debug.assert;
