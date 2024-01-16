const std = @import("std");
const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");
const Ast = @import("ast.zig");
const resolve_identifiers = @import("resolve_identifiers.zig");
const typechecker = @import("typechecker.zig");

pub fn main() void {
    var ast = Parser.parse("examples/debug");
    resolve_identifiers.resolve(&ast);
    typechecker.typecheck(&ast);
}
