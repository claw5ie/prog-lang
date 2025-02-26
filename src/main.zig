const std = @import("std");
const nostd = @import("nostd.zig");
const Compiler = @import("Compiler.zig");

pub fn main() void {
    Compiler.compile();

    std.debug.assert(nostd.general_purpose_allocator_context.deinit() == .ok);

    nostd.exit(0);
}
