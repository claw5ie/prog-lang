const std = @import("std");
const Compiler = @import("compiler.zig");

pub fn main() void {
    Compiler.compile();
    std.debug.assert(Compiler.general_purpose_allocator.deinit() == .ok);
}
