const std = @import("std");

const Allocator = std.mem.Allocator;

pub const Alignment = enum(u2) {
    BYTE = 0,
    WORD = 1,
    DWORD = 2,
    QWORD = 3,

    pub inline fn to_byte_size(alignment: Alignment) u4 {
        return @as(u4, 1) << @intFromEnum(alignment);
    }
};

pub const Order = enum(u2) {
    Less = 0,
    Greater = 1,
    Equal = 2,
};

pub fn compare(lhs: anytype, rhs: @TypeOf(lhs)) Order {
    return if (lhs < rhs) .Less else if (lhs > rhs) .Greater else .Equal;
}

pub fn left_shift(value: u64, offset: u64) u64 {
    return if (offset < 64)
        value << @as(u6, @intCast(offset))
    else
        0;
}

pub fn right_shift(value: u64, offset: u64) u64 {
    return if (offset < 64)
        value >> @as(u6, @intCast(offset))
    else
        0;
}

pub fn sign_extend(value: u64, bits: u8) u64 {
    if (bits == 0 or bits == 64) {
        return value;
    }

    const b: u6 = @intCast(bits);

    if ((value >> (b - 1)) & 0x1 == 1) {
        const ones = @as(u64, 0xFFFF_FFFF_FFFF_FFFF) << b;
        return value | ones;
    }

    return value;
}

pub fn align_u64(value: u64, alignment: Alignment) u64 {
    const pow2_minus_one: u64 = alignment.to_byte_size() - 1;
    var v = value;
    v += pow2_minus_one;
    v &= ~pow2_minus_one;
    return v;
}

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
