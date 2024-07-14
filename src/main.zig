const std = @import("std");
const utils = @import("utils.zig");
const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");
const Typechecker = @import("typechecker.zig");
const IRC = @import("ircode.zig");
const Interp = @import("interpreter.zig");

pub fn main() void {
    var arg_it = std.process.args();

    _ = arg_it.next();

    var filepath: [:0]const u8 = "./tests/debug";

    if (arg_it.next()) |arg| {
        filepath = arg;
    }

    var ast = Parser.parse(filepath);
    Typechecker.typecheck(&ast);
    var irc = IRC.generate_ir(&ast);
    Interp.interpret(&irc);

    {
        irc.instrs.deinit();
        std.debug.assert(utils.general_purpose_allocator.deinit() == .ok);
    }
}
