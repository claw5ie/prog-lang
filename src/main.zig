const std = @import("std");
const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");
const Ast = @import("ast.zig");

pub fn main() void {
    var ast = Parser.parse("examples/debug");
    _ = ast;
}
