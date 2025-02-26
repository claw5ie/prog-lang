pub fn main() void {
    const nostd = @import("nostd.zig");
    const Compiler = @import("Compiler.zig");

    Compiler.compile();
    nostd.exit(0);
}
