const std = @import("std");

const Allocator = std.mem.Allocator;

pub fn count_bits(value: u64) u8 {
    const state = struct {
        pub const b = [_]u64{ 0x2, 0xC, 0xF0, 0xFF00, 0xFFFF0000, 0xFFFFFFFF00000000 };
        pub const s = [_]u6{ 1, 2, 4, 8, 16, 32 };
    };

    var v = value;
    var i: u8 = 5;
    var r: u8 = 0;
    while (i > 0) {
        i -= 1;
        if ((v & state.b[i]) != 0) {
            v >>= state.s[i];
            r |= state.s[i];
        }
    }

    return r + @intFromBool(v != 0);
}

pub fn round_to_next_pow2(value: u64) u64 {
    var v = value;
    v -%= 1;
    v |= v >> 1;
    v |= v >> 2;
    v |= v >> 4;
    v |= v >> 8;
    v |= v >> 16;
    v |= v >> 32;
    v +%= 1;
    return v;
}

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

    std.posix.close(fd);

    return text[0..size :0];
}

pub fn is_prefix(prefix: []const u8, rest: []const u8) bool {
    return prefix.len <= rest.len and std.mem.eql(u8, prefix, rest[0..prefix.len]);
}
