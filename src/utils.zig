const std = @import("std");

const Allocator = std.mem.Allocator;

pub fn read_entire_file(allocator: Allocator, filepath: []const u8) ![:0]u8 {
    const fd = try std.posix.open(filepath, .{}, 0);
    const size: usize = size: {
        const stats = try std.posix.fstat(fd);
        break :size @intCast(stats.size);
    };
    const text = allocator.alloc(u8, size + 1) catch {
        std.posix.exit(1);
    };
    std.debug.assert(try std.posix.read(fd, text[0..size]) == size);
    text[size] = 0;

    return text[0..size :0];
}

pub fn is_prefix(prefix: []const u8, rest: []const u8) bool {
    if (prefix.len > rest.len) {
        return false;
    } else {
        return std.mem.eql(u8, prefix, rest[0..prefix.len]);
    }
}
