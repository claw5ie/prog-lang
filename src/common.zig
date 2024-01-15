const std = @import("std");

pub var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
pub var gpa = general_purpose_allocator.allocator();

pub const stderr = std.io.getStdErr().writer();

pub const LineInfo = struct {
    line: usize = 1,
    column: usize = 1,
    offset: usize = 0,
};

pub fn print_error(filepath: []const u8, line_info: LineInfo, comptime format: []const u8, args: anytype) void {
    stderr.print("{s}:{}:{}: error: " ++ format ++ "\n", .{ filepath, line_info.line, line_info.column } ++ args) catch {
        std.os.exit(1);
    };
}

pub fn print_note(filepath: []const u8, line_info: LineInfo, comptime format: []const u8, args: anytype) void {
    stderr.print("{s}:{}:{}: note: " ++ format ++ "\n", .{ filepath, line_info.line, line_info.column } ++ args) catch {
        std.os.exit(1);
    };
}
