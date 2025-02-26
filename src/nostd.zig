pub var general_purpose_allocator_context = std.heap.GeneralPurposeAllocator(.{}){};
pub var general_allocator = general_purpose_allocator_context.allocator();

pub const Allocator = std.mem.Allocator;

pub fn todo(comptime text: []const u8) noreturn {
    @compileError(text);
}

var stdout_buffer = std.io.bufferedWriter(std.io.getStdOut().writer());
var stdout = stdout_buffer.writer();

pub fn oprint(comptime format: []const u8, args: anytype) void {
    stdout.print(format, args) catch {
        exit(1);
    };
}

var stderr_buffer = std.io.bufferedWriter(std.io.getStdErr().writer());
var stderr = stderr_buffer.writer();

pub fn eprint(comptime format: []const u8, args: anytype) void {
    stderr.print(format, args) catch {
        exit(1);
    };
}

pub fn report_error(comptime format: []const u8, args: anytype) void {
    eprint("error: " ++ format ++ "\n", args);
}

pub fn report_fatal_error(comptime format: []const u8, args: anytype) noreturn {
    report_error(format, args);
    exit(1);
}

pub fn exit(code: u8) noreturn {
    stdout_buffer.flush() catch {};
    stderr_buffer.flush() catch {};
    if (code == 0) {
        std.debug.assert(general_purpose_allocator_context.deinit() == .ok);
    }
    std.posix.exit(code);
}

pub const File = std.fs.File;

pub fn read_entire_file(allocator: Allocator, filepath: []const u8) File.OpenError![:0]u8 {
    const file = try std.fs.cwd().openFile(filepath, .{});
    defer file.close();
    const stats = file.stat() catch {
        report_fatal_error("{s}: failed to stat the file", .{filepath});
    };
    const text = allocator.allocSentinel(u8, stats.size, 0) catch {
        report_fatal_error("{s}: failed to allocate {} bytes", .{ filepath, stats.size + 1 });
    };
    const bytes_read = file.read(text) catch {
        report_fatal_error("{s}: failed to read {} bytes", .{ filepath, stats.size });
    };
    std.debug.assert(bytes_read == text.len);

    return text;
}

pub fn read_from_file(file: File, buffer: []u8) void {
    const bytes_read = file.read(buffer) catch {
        report_fatal_error("failed to read {} bytes", .{buffer.len});
    };
    std.debug.assert(bytes_read == buffer.len);
}

pub fn read_u64_from_file(file: File) u64 {
    var value: u64 = 0;
    var buffer: []u8 = undefined;
    buffer.ptr = @ptrCast(&value);
    buffer.len = 8;
    read_from_file(file, buffer);
    return value;
}

pub fn write_to_file(file: File, bytes: []const u8) void {
    const bytes_written = file.write(bytes) catch {
        report_fatal_error("failed to write {} bytes", .{bytes.len});
    };
    std.debug.assert(bytes_written == bytes.len);
}

pub fn write_u64_to_file(file: File, value: u64) void {
    var bytes: []const u8 = undefined;
    bytes.ptr = @ptrCast(&value);
    bytes.len = 8;
    write_to_file(file, bytes);
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

pub fn max_enum_value(comptime EnumType: type) @typeInfo(EnumType).Enum.tag_type {
    const info = @typeInfo(EnumType).Enum;

    var max: info.tag_type = std.math.minInt(info.tag_type);
    for (info.fields) |field| {
        max = @max(max, field.value);
    }

    return max;
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

pub fn align_up(value: u64, alignment: Alignment) u64 {
    return align_up_by_pow2(value, alignment.to_byte_size());
}

pub fn align_up_by_pow2(value: u64, alignment: u64) u64 {
    std.debug.assert(alignment > 0 and round_to_next_pow2(alignment) == alignment);
    const alignment_minus_one = alignment - 1;
    var _value = value;
    _value += alignment_minus_one;
    _value &= ~alignment_minus_one;
    return _value;
}

// Source: https://graphics.stanford.edu/~seander/bithacks.html#IntegerLog
// Author: Sean Eron Anderson
pub fn integer_log2(value: u64) u6 {
    const state = struct {
        pub const b = [_]u64{ 0x2, 0xC, 0xF0, 0xFF00, 0xFFFF0000, 0xFFFFFFFF00000000 };
        pub const s = [_]u6{ 1, 2, 4, 8, 16, 32 };
    };

    var _value = value;
    var result: u6 = 0;

    var i: u8 = 6;
    while (i > 0) {
        i -= 1;
        if ((_value & state.b[i]) != 0) {
            _value >>= state.s[i];
            result |= state.s[i];
        }
    }

    return result;
}

pub fn highest_bit_count(value: u64) u8 {
    return if (value != 0) @as(u8, integer_log2(value)) + 1 else 0;
}

// Source: https://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
// Author: Sean Eron Anderson
pub fn round_to_next_pow2(value: u64) u64 {
    var _value = value + @intFromBool(value == 0);
    _value -%= 1;
    _value |= _value >> 1;
    _value |= _value >> 2;
    _value |= _value >> 4;
    _value |= _value >> 8;
    _value |= _value >> 16;
    _value |= _value >> 32;
    _value +%= 1;
    return _value;
}

pub fn is_prefix(prefix: []const u8, rest: []const u8) bool {
    return prefix.len <= rest.len and std.mem.eql(u8, prefix, rest[0..prefix.len]);
}

pub const StringPool = struct {
    map: HashMap,

    pub fn init(allocator: Allocator) StringPool {
        return .{ .map = HashMap.init(allocator) };
    }

    pub fn deinit(pool: *StringPool) void {
        var it = pool.map.valueIterator();
        while (it.next()) |value_ptr| {
            pool.map.allocator.free(value_ptr.*);
        }
        pool.map.deinit();
    }

    pub fn insert(pool: *StringPool, string: []const u8) []const u8 {
        const insert_result = pool.map.getOrPut(string) catch {
            exit(1);
        };

        if (!insert_result.found_existing) {
            const value = pool.map.allocator.alloc(u8, string.len) catch {
                exit(1);
            };
            @memcpy(value, string);
            insert_result.key_ptr.* = value;
            insert_result.value_ptr.* = value;
        }

        return insert_result.value_ptr.*;
    }

    pub const Key = []const u8;
    pub const Value = []u8;

    pub const Context = struct {
        pub fn hash(_: Context, key: Key) u64 {
            const MurMur = std.hash.Murmur2_64;
            return MurMur.hash(key);
        }

        pub fn eql(_: Context, k0: Key, k1: Key) bool {
            return std.mem.eql(u8, k0, k1);
        }
    };

    pub const HashMap = std.HashMap(Key, Value, Context, 80);
};

pub const Alignment = enum(u2) {
    Byte = 0,
    Word = 1,
    Dword = 2,
    Qword = 3,

    pub inline fn to_byte_size(alignment: Alignment) u8 {
        return @as(u8, 1) << @intFromEnum(alignment);
    }
};

pub const FilePosition = struct {
    line: u32,
    column: u32,
    offset: usize,
};

const std = @import("std");
