pub fn main() void {
    {
        const state = struct {
            pub var void_pointer_type_data = Ast.Type.SharedData{
                .as = .{ .Pointer = &void_type },
                .byte_size = Ast.pointer_byte_size,
                .alignment = Ast.pointer_alignment,
                .stages = Ast.default_stages_done,
            };
            pub var bool_type_data = Ast.Type.SharedData{
                .as = .Bool,
                .byte_size = 1,
                .alignment = .BYTE,
                .stages = Ast.default_stages_done,
            };
            pub var void_type_data = Ast.Type.SharedData{
                .as = .Void,
                .byte_size = 0,
                .alignment = .BYTE,
                .stages = Ast.default_stages_done,
            };

            pub var void_pointer_type = Ast.Type{
                .line_info = .{ .line = 1, .column = 1, .offset = 0 },
                .data = &void_pointer_type_data,
                .symbol = null,
            };
            pub var bool_type = Ast.Type{
                .line_info = .{ .line = 1, .column = 1, .offset = 0 },
                .data = &bool_type_data,
                .symbol = null,
            };
            pub var void_type = Ast.Type{
                .line_info = .{ .line = 1, .column = 1, .offset = 0 },
                .data = &void_type_data,
                .symbol = null,
            };
        };

        Ast.void_pointer_type = &state.void_pointer_type;
        Ast.bool_type = &state.bool_type;
        Ast.void_type = &state.void_type;
    }

    var args = std.process.args();
    var filepath: [:0]const u8 = "examples/debug";

    _ = args.next().?;
    if (args.next()) |arg| {
        filepath = arg;
    }

    var ast = Parser.parse(filepath);
    Typechecker.typecheck(&ast);
    var irc = GenIr.generate_ir(&ast);
    irc.print();
    Interp.interpret(&irc);

    ast.arena.deinit();
    ast.symbol_table.deinit();
    ast.string_pool.deinit();
    for (Ast.integer_types) |has_type| {
        if (has_type) |typ| {
            common.gpa.destroy(typ.data);
            common.gpa.destroy(typ);
        }
    }
    irc.instrs.deinit();
    std.debug.assert(common.general_purpose_allocator.deinit() == .ok);
}

const std = @import("std");
const common = @import("common.zig");
const Ast = @import("ast.zig");
const Parser = @import("parser.zig");
const Typechecker = @import("typechecker.zig");
const IRC = @import("irc.zig");
const GenIr = @import("generate-ir.zig");
const Interp = @import("interpreter.zig");
