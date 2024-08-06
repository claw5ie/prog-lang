const std = @import("std");
const common = @import("common.zig");
const Ir = @import("ircode.zig");

stack: []u8,
rsp: u64 = 0,
rbp: u64 = 0,
next_global: Ir.Offset,

const This = @This();

inline fn cast_ptr(comptime dst: type, src: anytype) dst {
    return @alignCast(@ptrCast(src));
}

pub fn interpret(ir: *Ir) void {
    const stack = common.gpa.alloc(u8, 2 * 1024 * 1024) catch {
        std.posix.exit(1);
    };
    defer common.gpa.free(stack);

    var interpreter: This = .{
        .stack = stack,
        .next_global = ir.next_global,
    };
    var interp = &interpreter;

    const labels = common.gpa.alloc(u64, ir.next_label) catch {
        std.posix.exit(1);
    };
    defer common.gpa.free(labels);

    for (ir.instrs.items, 0..) |instr, i| {
        switch (instr) {
            .Meta => |meta| {
                switch (meta) {
                    .GFB => |gfb| {
                        labels[gfb.label] = i;
                    },
                    .GFE => |gfe| {
                        labels[gfe.label] = i;
                    },
                    .Label => |label| {
                        labels[label] = i;
                    },
                }
            },
            else => {},
        }
    }

    interp.rsp = @intCast(interp.next_global);
    interp.rbp = interp.rsp;

    var ip: u64 = 0;
    while (ip < ir.instrs.items.len) {
        switch (ir.instrs.items[ip]) {
            .Binary_Op => |op| {
                const dst = grab_pointer_from_lvalue(interp, op.dst);
                var src0 = grab_value_from_rvalue(interp, op.src0);
                const src1 = grab_value_from_rvalue(interp, op.src1);

                switch (op.tag) {
                    .Or => src0 = @intFromBool((src0 != 0) or (src1 != 0)),
                    .And => src0 = @intFromBool((src0 != 0) and (src1 != 0)),
                    .Eq => src0 = @intFromBool(src0 == src1),
                    .Neq => src0 = @intFromBool(src0 != src1),
                    .Lt => src0 = @intFromBool(@as(i64, @bitCast(src0)) < @as(i64, @bitCast(src1))),
                    .Leq => src0 = @intFromBool(@as(i64, @bitCast(src0)) <= @as(i64, @bitCast(src1))),
                    .Gt => src0 = @intFromBool(@as(i64, @bitCast(src0)) > @as(i64, @bitCast(src1))),
                    .Geq => src0 = @intFromBool(@as(i64, @bitCast(src0)) >= @as(i64, @bitCast(src1))),
                    .Add => src0 = @bitCast(@as(i64, @bitCast(src0)) + @as(i64, @bitCast(src1))),
                    .Sub => src0 = @bitCast(@as(i64, @bitCast(src0)) - @as(i64, @bitCast(src1))),
                    .Mul => src0 = @bitCast(@as(i64, @bitCast(src0)) * @as(i64, @bitCast(src1))),
                    .Div => src0 = @bitCast(@divTrunc(@as(i64, @bitCast(src0)), @as(i64, @bitCast(src1)))),
                    .Mod => src0 = @bitCast(@rem(@as(i64, @bitCast(src0)), @as(i64, @bitCast(src1)))),
                }

                stack_write_u64(interp, dst, src0);
            },
            .Unary_Op => |op| {
                const dst = grab_pointer_from_lvalue(interp, op.dst);
                var src = grab_value_from_rvalue(interp, op.src);

                switch (op.tag) {
                    .Not => src = @intFromBool(src == 0),
                    .Neg => src = @bitCast(-@as(i64, @bitCast(src))),
                }

                stack_write_u64(interp, dst, src);
            },
            .Jmpc => |jmpc| {
                var src0 = grab_value_from_rvalue(interp, jmpc.src0);
                const src1 = grab_value_from_rvalue(interp, jmpc.src1);

                switch (jmpc.tag) {
                    .Eq => src0 = @intFromBool(src0 == src1),
                    .Neq => src0 = @intFromBool(src0 != src1),
                    .Lt => src0 = @intFromBool(@as(i64, @bitCast(src0)) < @as(i64, @bitCast(src1))),
                    .Leq => src0 = @intFromBool(@as(i64, @bitCast(src0)) <= @as(i64, @bitCast(src1))),
                    .Gt => src0 = @intFromBool(@as(i64, @bitCast(src0)) > @as(i64, @bitCast(src1))),
                    .Geq => src0 = @intFromBool(@as(i64, @bitCast(src0)) >= @as(i64, @bitCast(src1))),
                }

                if (src0 != 0) {
                    ip = labels[jmpc.label];
                    continue;
                }
            },
            .Instr => |instr| {
                switch (instr) {
                    .Print => |tmp| {
                        const src: i64 = @bitCast(grab_value_from_rvalue(interp, tmp));
                        std.debug.print("{}\n", .{src});
                    },
                    .Mov => |mov| {
                        const dst = grab_pointer_from_lvalue(interp, mov.dst);
                        const src = grab_value_from_rvalue(interp, mov.src);

                        stack_write_u64(interp, dst, src);
                    },
                    .Jmp => |label| {
                        ip = labels[label];
                        continue;
                    },
                    .Push_Frame_Pointer => |depth| {
                        var i = depth;
                        var rbp = interp.rbp;
                        while (i >= 0) : (i -= 1) {
                            rbp = stack_read_u64(interp, rbp - -Ir.LF_RBP_OFFSET);
                        }

                        stack_push(interp, rbp);
                    },
                    .Push => |src| {
                        const value = grab_value_from_rvalue(interp, src);
                        stack_push(interp, value);
                    },
                    .Pop => |count| {
                        interp.rsp -= count;
                    },
                    .Call => |src| {
                        stack_push(interp, ip);
                        stack_push(interp, interp.rbp);

                        const label = grab_value_from_rvalue(interp, src);
                        ip = labels[label];
                        continue;
                    },
                    .Ret => |label| {
                        ip = labels[label];
                        continue;
                    },
                    .Exit => {
                        break;
                    },
                }
            },
            .Meta => |meta| {
                switch (meta) {
                    .GFB => |gfb| {
                        interp.rbp = interp.rsp;
                        interp.rsp += gfb.stack_space_used;
                    },
                    .GFE => |gfe| {
                        interp.rsp -= gfe.stack_space_used;
                        interp.rbp = stack_pop(interp);
                        ip = stack_pop(interp);
                    },
                    .Label => {},
                }
            },
        }

        ip += 1;
    }
}

fn grab_pointer_from_tmp(interp: *This, tmp: Ir.Tmp) u64 {
    switch (tmp.tag) {
        .Global => return @intCast(tmp.offset),
        .Local => {
            var i = tmp.height;
            var rbp = interp.rbp;
            while (i > 0) : (i -= 1) {
                rbp = stack_read_u64(interp, rbp - -Ir.LF_RBP_OFFSET);
            }

            const rbp_i64: i64 = @bitCast(rbp);
            return @bitCast(rbp_i64 + tmp.offset);
        },
    }
}

fn grab_pointer_from_address(interp: *This, address: Ir.Mem) u64 {
    var offset: i64 = 0;

    if (address.choose & 0x1 == 0x1) {
        const base: i64 = @bitCast(grab_value_from_tmp(interp, address.base));
        offset += base;
    }

    if (address.choose & 0x2 == 0x2) {
        offset += address.disp;
    }

    return @bitCast(offset);
}

fn grab_pointer_from_lvalue(interp: *This, lvalue: Ir.Lvalue) u64 {
    switch (lvalue) {
        .Tmp => |tmp| return grab_pointer_from_tmp(interp, tmp),
        .Mem => |address| return grab_pointer_from_address(interp, address),
    }
}

fn grab_value_from_tmp(interp: *This, tmp: Ir.Tmp) u64 {
    const pointer = grab_pointer_from_tmp(interp, tmp);
    return stack_read_u64(interp, pointer);
}

fn grab_value_from_rvalue(interp: *This, rvalue: Ir.Rvalue) u64 {
    switch (rvalue) {
        .Lvalue => |lvalue| {
            const pointer = grab_pointer_from_lvalue(interp, lvalue);
            return stack_read_u64(interp, pointer);
        },
        .Addr => |lvalue| return grab_pointer_from_lvalue(interp, lvalue),
        .Label => |label| return label,
        .Imm => |imm| return @bitCast(imm),
    }
}

fn stack_read_u64(interp: *This, pointer: u64) u64 {
    if (pointer < 8) {
        std.debug.print("error: null pointer access (0x{x}).\n", .{pointer});
        std.posix.exit(1);
    } else if (pointer + 8 > interp.rsp) {
        std.debug.print("error: address 0x{x} is outside of current stack pointer (0x{x}).\n", .{ pointer, interp.rsp });
        std.posix.exit(1);
    }
    return stack_read_u64_no_check(interp, pointer);
}

inline fn stack_read_u64_no_check(interp: *This, pointer: u64) u64 {
    return cast_ptr(*u64, &interp.stack[pointer]).*;
}

fn stack_write_u64(interp: *This, pointer: u64, value: u64) void {
    if (pointer < 8) {
        std.debug.print("error: null pointer access (0x{x}).\n", .{pointer});
        std.posix.exit(1);
    } else if (pointer + 8 > interp.rsp) {
        std.debug.print("error: address 0x{x} is outside of current stack pointer (0x{x}).\n", .{ pointer, interp.rsp });
        std.posix.exit(1);
    }
    stack_write_u64_no_check(interp, pointer, value);
}

inline fn stack_write_u64_no_check(interp: *This, pointer: u64, value: u64) void {
    cast_ptr(*u64, &interp.stack[pointer]).* = value;
}

fn stack_push(interp: *This, value: u64) void {
    stack_write_u64_no_check(interp, interp.rsp, value);
    interp.rsp += 8;
}

fn stack_pop(interp: *This) u64 {
    interp.rsp -= 8;
    return stack_read_u64_no_check(interp, interp.rsp);
}
