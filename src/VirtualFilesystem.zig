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
files: HashMap(ID, File),
directories: HashMap(ID, Dir),
/// child -> parent mapping. Each parent can have multiple children but a child may only have one
/// parent.
hierarchy: HashMap(ID, ID),
/// Returns the ID of a file or directory based on its `dest_path`
dest_path_lookup: String.HashMap(ID),
/// Returns the ID of a file based on its `src_path`
src_path_lookup: String.HashMap(ID),
next_id: ID,

const ID = enum(u16) {
    root = std.math.maxInt(u16),
    _,
};

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
        /// File inside a TmpDir that contains the evaluated template
        templated: struct { content: Io.File },
        /// The file path that should be symlinked is `file.src_path`
        symlinked: void,
    },
};

pub fn init(gpa: std.mem.Allocator, pool: *String.Pool) VirtualFilesystem {
    return .{
        .gpa = gpa,
        .pool = pool,
        .files = .empty,
        .hierarchy = .empty,
        .directories = .empty,
        .next_id = @enumFromInt(0),
        .src_path_lookup = .init(pool),
        .dest_path_lookup = .init(pool),
    };
}

/// Temporary directory to store computed results in
///
/// Copied from testing.tmpDir
const TmpDir = struct {
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
    stack_top: StackMarker,

    pub fn init(gpa: std.mem.Allocator) PathBuf {
        return .{
            .gpa = gpa,
            .bytes = .empty,
            .file_start = null,
            .stack_top = .init,
        };
    }

    pub const Marker = enum(u32) { _ };

    pub const StackMarker = struct {
        top: enum(u32) { _ },
        file_start: ?Marker,
        pub const init: StackMarker = .{ .top = @enumFromInt(0), .file_start = null };
    };

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
        return self.bytes.items[@intFromEnum(self.stack_top.top)..];
    }

    /// Return the basename of the file.
    ///
    /// Memory will be invalidated when .exit() is called.
    pub fn basename(self: *const PathBuf) []const u8 {
        assert(self.file_start != null);
        return self.bytes.items[@intFromEnum(self.file_start.?)..];
    }

    /// Return the directory path.
    ///
    /// Memory will be invalidated when .exit() or .visit*() is called.
    pub fn dirname(self: *const PathBuf) []const u8 {
        const end = @intFromEnum(self.file_start) orelse self.bytes.items.len - 1;
        return self.bytes.items[@intFromEnum(self.stack_top.top)..end];
    }

    pub fn push(self: *PathBuf) !StackMarker {
        defer self.file_start = null;
        self.bytes.append(self.gpa, 0);
        return .{
            .top = @intFromEnum(self.bytes.items.len),
            .file_start = self.file_start,
        };
    }

    pub fn pop(self: *PathBuf, stack_top: StackMarker) void {
        assert(stack_top.top <= self.bytes.items.len);
        self.bytes.resize(self.gpa, stack_top.top) catch unreachable;
        self.file_start = stack_top.file_start;
    }
};

const String = @import("lang.zig").Value.String;

const std = @import("std");
const Io = std.Io;
const path = std.Io.Dir.path;
const assert = std.debug.assert;
