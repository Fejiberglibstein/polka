bytes: std.ArrayList(u8),
gpa: std.mem.Allocator,
file_start: ?Marker,
stack_top: StackMarker,

const PathBuf = @This();

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

const std = @import("std");
const assert = std.debug.assert;
