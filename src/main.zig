const std = @import("std");
const utils = @import("utils.zig");
const Compiler = @import("compiler.zig");

pub fn main() void {
    Compiler.compile();
    std.debug.assert(Compiler.general_purpose_allocator.deinit() == .ok);

    Compiler.exit(0);
}
