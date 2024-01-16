const std = @import("std");
const common = @import("common.zig");
const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");
const Ast = @import("ast.zig");
const resolve_identifiers = @import("resolve_identifiers.zig");
const typechecker = @import("typechecker.zig");
const Ir = @import("ircode.zig");
const Interpreter = @import("interpreter.zig");

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
    var ir = Ir.generate(&ast); // leaking ast arena and symbols.
    ir.debug_print();
    Interpreter.interpret(&ir);

    // leaking ir_instrs

    // gpa.free(source_code);
    // compiler.symbols.deinit();
    // ast_arena.deinit();
    // compiler.ir_instrs.deinit();
    // std.debug.assert(common.general_purpose_allocator.deinit() == .ok);
}
