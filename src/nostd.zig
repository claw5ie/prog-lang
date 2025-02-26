pub var stdout_buffer = std.io.bufferedWriter(std.io.getStdOut().writer());
pub var stderr_buffer = std.io.bufferedWriter(std.io.getStdErr().writer());

pub var stdout = stdout_buffer.writer();
pub var stderr = stderr_buffer.writer();

pub fn todo(comptime text: []const u8) noreturn {
    @compileError(text);
}

pub fn oprint(comptime format: []const u8, args: anytype) void {
    stdout.print(format, args) catch {
        exit(1);
    };
}

pub fn eprint(comptime format: []const u8, args: anytype) void {
    stderr.print(format, args) catch {
        exit(1);
    };
}

pub fn exit(code: u8) noreturn {
    stdout_buffer.flush() catch {
        std.posix.exit(1);
    };
    stderr_buffer.flush() catch {
        std.posix.exit(1);
    };
    std.posix.exit(code);
}

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

pub fn max_enum_value(comptime EnumType: type) @typeInfo(EnumType).Enum.tag_type {
    const info = @typeInfo(EnumType).Enum;

    var max: info.tag_type = std.math.minInt(info.tag_type);
    for (info.fields) |field| {
        max = @max(max, field.value);
    }

    return max;
}

pub fn align_u64(value: u64, alignment: Alignment) u64 {
    return align_by_pow2(value, alignment.to_byte_size());
}

pub fn align_by_pow2(value: u64, alignment: u64) u64 {
    std.debug.assert(round_to_next_pow2(alignment) == alignment);
    const pow2_minus_one: u64 = alignment - 1;
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

pub fn read_from_file_v(fd: std.posix.fd_t, buffer: []u8) void {
    const count = std.posix.read(fd, buffer) catch {
        eprint("error: failed to read {} bytes\n", .{buffer.len});
        exit(1);
    };
    std.debug.assert(count == buffer.len);
}

pub fn read_from_file_u64(fd: std.posix.fd_t) u64 {
    var value: u64 = 0;
    var buffer: []u8 = undefined;
    buffer.ptr = @ptrCast(&value);
    buffer.len = 8;
    read_from_file_v(fd, buffer);
    return value;
}

pub fn write_to_file_v(fd: std.posix.fd_t, bytes: []const u8) void {
    const count = std.posix.write(fd, bytes) catch {
        eprint("error: failed to write {} bytes\n", .{bytes.len});
        exit(1);
    };
    std.debug.assert(count == bytes.len);
}

pub fn write_to_file_u64(fd: std.posix.fd_t, value: u64) void {
    var view: []const u8 = undefined;
    view.ptr = @ptrCast(&value);
    view.len = 8;
    write_to_file_v(fd, view);
}

pub fn is_prefix(prefix: []const u8, rest: []const u8) bool {
    return prefix.len <= rest.len and std.mem.eql(u8, prefix, rest[0..prefix.len]);
}

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
