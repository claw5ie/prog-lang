const std = @import("std");
const Allocator = std.mem.Allocator;

pub var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
pub var gpa = general_purpose_allocator.allocator();

const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();

pub fn oprint(comptime format: []const u8, args: anytype) void {
    stdout.print(format, args) catch {
        std.posix.exit(1);
    };
}

pub fn eprint(comptime format: []const u8, args: anytype) void {
    stderr.print(format, args) catch {
        std.posix.exit(1);
    };
}

pub fn is_prefix(prefix: []const u8, string: []const u8) bool {
    return prefix.len <= string.len and std.mem.eql(u8, prefix, string[0..prefix.len]);
}

pub fn sign_extend(value: u64, sbits: u6) u64 {
    if (value >= (@as(u64, 1) << (sbits - 1))) {
        var ones: u64 = 0xFFFF_FFFF_FFFF_FFFF;
        ones >>= sbits;
        ones <<= sbits;
        return value | ones;
    } else {
        return value;
    }
}

pub fn count_bits(value: u64) u6 {
    const state = struct {
        const b = [_]u64{ 0x2, 0xC, 0xF0, 0xFF00, 0xFFFF0000, 0xFFFFFFFF00000000 };
        const s = [_]u6{ 1, 2, 4, 8, 16, 32 };
    };
    var v: u64 = value;
    var r: u6 = 0;

    var i: u8 = 5;
    while (i > 0) {
        i -= 1;
        if ((v & state.b[i]) != 0) {
            v >>= state.s[i];
            r |= state.s[i];
        }
    }

    r += @intFromBool(value != 0);

    return r;
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
