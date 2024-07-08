const std = @import("std");
const Allocator = std.mem.Allocator;

pub var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
pub var gpa = general_purpose_allocator.allocator();

const stderr = std.io.getStdErr().writer();

pub fn eprint(comptime format: []const u8, args: anytype) void {
    stderr.print(format, args) catch {
        std.posix.exit(1);
    };
}

pub fn read_entire_file(allocator: Allocator, filepath: [:0]const u8) ![:0]u8 {
    const fd = try std.posix.open(filepath, .{}, 0);
    defer std.posix.close(fd);
    const stats = try std.posix.fstat(fd);
    const size: usize = @intCast(stats.size);

    const data = try allocator.alloc(u8, size + 1);
    const read = try std.posix.read(fd, data);
    std.debug.assert(read == size);
    data[size] = 0;

    return data[0..size :0];
}
