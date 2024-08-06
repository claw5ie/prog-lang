pub fn main() void {
    var args = std.process.args();
    var filepath: [:0]const u8 = "examples/debug";

    _ = args.next().?;
    if (args.next()) |arg| {
        filepath = arg;
    }

    var ast = Parser.parse(filepath);

    ast.ast_arena.deinit();
    ast.symbols.deinit();
    std.debug.assert(common.general_purpose_allocator.deinit() == .ok);
}

const std = @import("std");
const common = @import("common.zig");
const Parser = @import("parser.zig");
