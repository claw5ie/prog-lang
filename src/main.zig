const std = @import("std");
const Compiler = @import("compiler.zig");

pub fn main() void {
    var compiler = Compiler.init();
    compiler.compile();
    std.debug.assert(Compiler.general_purpose_allocator.deinit() == .ok);
}
