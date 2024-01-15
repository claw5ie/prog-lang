const std = @import("std");
const Lexer = @import("lexer.zig");

pub fn main() void {
    var lexer = Lexer.init("examples/debug");

    while (true) {
        var token = lexer.grab();
        lexer.advance();

        std.debug.print("{}\n", .{token});

        if (token.tag == .End_Of_File) break;
    }
}
