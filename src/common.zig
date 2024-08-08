pub var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
pub var gpa = general_purpose_allocator.allocator();

pub const stderr = std.io.getStdErr().writer();

pub fn eprint(comptime format: []const u8, args: anytype) void {
    stderr.print(format, args) catch {
        exit(1);
    };
}

pub fn exit(code: u8) noreturn {
    std.posix.exit(code);
}

pub fn print_error(filepath: []const u8, line_info: LineInfo, comptime format: []const u8, args: anytype) void {
    eprint("{s}:{}:{}: error: " ++ format ++ "\n", .{ filepath, line_info.line, line_info.column } ++ args);
}

pub fn print_note(filepath: []const u8, line_info: LineInfo, comptime format: []const u8, args: anytype) void {
    eprint("{s}:{}:{}: note: " ++ format ++ "\n", .{ filepath, line_info.line, line_info.column } ++ args);
}

const std = @import("std");

pub const Allocator = std.mem.Allocator;
pub const ArenaAllocator = std.heap.ArenaAllocator;

pub const Alignment = enum(u2) {
    BYTE = 0,
    WORD = 1,
    DWORD = 2,
    QWORD = 3,
};

pub const LineInfo = struct {
    line: u32,
    column: u32,
    offset: usize,
};

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
