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

pub const LineInfo = struct {
    line: u32,
    column: u32,
    offset: usize,
};
