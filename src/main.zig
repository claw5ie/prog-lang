const std = @import("std");
const Compiler = @import("compiler.zig");

pub fn main() void {
    var compiler = Compiler.init();
    compiler.compile();
}
