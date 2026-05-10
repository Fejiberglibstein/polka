bytes: std.ArrayList(u8),
gpa: std.mem.Allocator,
file_start: ?Marker,

const PathBuf = @This();

pub fn init(gpa: std.mem.Allocator) PathBuf {
    return .{
        .gpa = gpa,
        .bytes = .empty,
        .file_start = null,
    };
}

pub const Marker = enum(u32) { _ };

pub fn deinit(self: *PathBuf) void {
    self.bytes.deinit(self.gpa);
}

/// Returns an owned copy of the path that is currently in the buffer.
pub fn dupePath(self: *PathBuf, alloc: std.mem.Allocator) ![]const u8 {
    var copy = try self.bytes.clone(alloc);
    return copy.toOwnedSlice(alloc);
}

/// Append the file name to the buffer. It can be exited by calling .exit().
///
/// This will panic if a file has already been visited without being exited.
pub fn visitFile(self: *PathBuf, path: []const u8) !Marker {
    assert(self.file_start == null);
    assert(path.len > 0);

    const marker: Marker = @enumFromInt(self.bytes.items.len);
    try self.bytes.appendSlice(self.gpa, path);

    self.file_start = marker;
    return marker;
}

/// Append the directory name to the buffer. It can be exited by calling .exit()
///
/// This will panic if a file has been visited without being exited.
pub fn visitDir(self: *PathBuf, path: []const u8) !Marker {
    assert(path.len > 0);
    assert(self.file_start == null);
    const marker: Marker = @enumFromInt(self.bytes.items.len);
    try self.bytes.appendSlice(self.gpa, path);
    if (path[path.len - 1] != '/')
        try self.bytes.append(self.gpa, '/');

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
    return self.bytes.items[0..end];
}

const std = @import("std");
const assert = std.debug.assert;
