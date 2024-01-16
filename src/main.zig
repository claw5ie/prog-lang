const std = @import("std");
const common = @import("common.zig");
const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");
const Ast = @import("ast.zig");
const resolve_identifiers = @import("resolve_identifiers.zig");
const typechecker = @import("typechecker.zig");
const IrCode = @import("ircode.zig");

pub fn main() void {
    var args = std.process.args();
    var filepath: [:0]const u8 = "examples/debug";

    _ = args.next().?;
    if (args.next()) |arg| {
        filepath = arg;
    }

    var ast = Parser.parse(filepath); // leaking parser.
    resolve_identifiers.resolve(&ast);
    typechecker.typecheck(&ast);
    var ircode = IrCode.generate_ir(&ast); // leaking ast arena and symbols.
    IrCode.debug_print_ir(&ircode);

    // leaking ir_instrs

    // std.debug.assert(common.general_purpose_allocator.deinit() == .ok);
}
