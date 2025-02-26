const std = @import("std");
const nostd = @import("nostd.zig");
const Compiler = @import("Compiler.zig");

pub fn main() void {
    Compiler.compile();

    std.debug.assert(Compiler.general_purpose_allocator.deinit() == .ok);

    Compiler.exit(0);
}
