const std = @import("std");
const Lexer = @import("lexer.zig");

pub fn main() void {
    var lexer = Lexer{ .source_code = "6;9;6;\n2;1;4//$" };

    while (true) {
        const token = lexer.take();
        std.debug.print("{}:{}:{}: '{}'\n", .{
            token.line_info.line,
            token.line_info.column,
            token.line_info.offset,
            token.as,
        });
        if (token.as == .End_Of_File) {
            break;
        }
    }
}
