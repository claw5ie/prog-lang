const std = @import("std");
const utils = @import("utils.zig");
const Compiler = @import("compiler.zig");

const IRC = Compiler.IRC;

pub fn interpret(c: *Compiler) void {
    c.interp.stack = Compiler.gpa.alloc(u8, 2 * 1024 * 1024) catch {
        std.posix.exit(1);
    };
    c.interp.labels = Compiler.gpa.alloc(u64, c.irc.label_count) catch {
        Compiler.exit(1);
    };

    interpret_top_level(c);
}

fn interpret_top_level(c: *Compiler) void {
    for (c.irc.instrs.items, 0..) |instr, i| {
        switch (instr) {
            .GFB => |gfb| {
                c.interp.labels[gfb.label] = i;
            },
            .GFE => |gfe| {
                c.interp.labels[gfe.label] = i;
            },
            .Label => |label| {
                c.interp.labels[label] = i;
            },
            else => {},
        }
    }

    c.interp.rsp = @intCast(c.irc.globals_count + 128);
    c.interp.rbp = c.interp.rsp;

    var ip: u64 = 0;
    while (ip < c.irc.instrs.items.len) {
        switch (c.irc.instrs.items[ip]) {
            .Print => |Print| {
                switch (Print) {
                    .Pointer => |src| {
                        const value = grab_u64_from_rvalue(c, src);
                        Compiler.oprint("0x{x}\n", .{value});
                    },
                    .Integer => |Integer| {
                        if (Integer.is_signed) {
                            const value = grab_i64_from_rvalue(c, Integer.src);
                            Compiler.oprint("{}\n", .{value});
                        } else {
                            const value = grab_u64_from_rvalue(c, Integer.src);
                            Compiler.oprint("{}\n", .{value});
                        }
                    },
                    .Boolean => |src| {
                        const value = grab_u64_from_rvalue(c, src);
                        Compiler.oprint("{s}\n", .{if (value != 0) "true" else "false"});
                    },
                }
            },
            .Binary_Op => |op| {
                if (op.is_signed) {
                    const dst = grab_pointer_from_lvalue(c, op.dst);
                    var src0 = grab_i64_from_rvalue(c, op.src0);
                    const src1 = grab_i64_from_rvalue(c, op.src1);

                    switch (op.tag) {
                        .Eq => src0 = @intFromBool(src0 == src1),
                        .Neq => src0 = @intFromBool(src0 != src1),
                        .Lt => src0 = @intFromBool(src0 < src1),
                        .Leq => src0 = @intFromBool(src0 <= src1),
                        .Gt => src0 = @intFromBool(src0 > src1),
                        .Geq => src0 = @intFromBool(src0 >= src1),
                        .Add => src0 += src1,
                        .Sub => src0 -= src1,
                        .Mul => src0 *= src1,
                        .Div => src0 = @divTrunc(src0, src1),
                        .Mod => src0 = @rem(src0, src1),
                    }

                    stack_write(c, dst, @bitCast(src0), op.dst.grab_size());
                } else {
                    const dst = grab_pointer_from_lvalue(c, op.dst);
                    var src0 = grab_u64_from_rvalue(c, op.src0);
                    const src1 = grab_u64_from_rvalue(c, op.src1);

                    switch (op.tag) {
                        .Eq => src0 = @intFromBool(src0 == src1),
                        .Neq => src0 = @intFromBool(src0 != src1),
                        .Lt => src0 = @intFromBool(src0 < src1),
                        .Leq => src0 = @intFromBool(src0 <= src1),
                        .Gt => src0 = @intFromBool(src0 > src1),
                        .Geq => src0 = @intFromBool(src0 >= src1),
                        .Add => src0 += src1,
                        .Sub => src0 -= src1,
                        .Mul => src0 *= src1,
                        .Div => src0 = @divTrunc(src0, src1),
                        .Mod => src0 = @rem(src0, src1),
                    }

                    stack_write(c, dst, src0, op.dst.grab_size());
                }
            },
            .Unary_Op => |op| {
                if (op.is_signed) {
                    const dst = grab_pointer_from_lvalue(c, op.dst);
                    var src = grab_i64_from_rvalue(c, op.src);

                    switch (op.tag) {
                        .Not => src = @intFromBool(src == 0),
                        .Neg => src = -src,
                    }

                    stack_write(c, dst, @bitCast(src), op.dst.grab_size());
                } else {
                    const dst = grab_pointer_from_lvalue(c, op.dst);
                    var src = grab_u64_from_rvalue(c, op.src);

                    switch (op.tag) {
                        .Not => src = @intFromBool(src == 0),
                        .Neg => unreachable,
                    }

                    stack_write(c, dst, src, op.dst.grab_size());
                }
            },
            .Jmp => |label| {
                ip = c.interp.labels[label];
                continue;
            },
            .Jmpc => |jmpc| {
                var condition = false;
                if (jmpc.is_signed) {
                    var src0 = grab_i64_from_rvalue(c, jmpc.src0);
                    const src1 = grab_i64_from_rvalue(c, jmpc.src1);

                    switch (jmpc.tag) {
                        .Eq => src0 = @intFromBool(src0 == src1),
                        .Neq => src0 = @intFromBool(src0 != src1),
                        .Lt => src0 = @intFromBool(src0 < src1),
                        .Leq => src0 = @intFromBool(src0 <= src1),
                        .Gt => src0 = @intFromBool(src0 > src1),
                        .Geq => src0 = @intFromBool(src0 >= src1),
                    }

                    condition = src0 != 0;
                } else {
                    var src0 = grab_u64_from_rvalue(c, jmpc.src0);
                    const src1 = grab_u64_from_rvalue(c, jmpc.src1);

                    switch (jmpc.tag) {
                        .Eq => src0 = @intFromBool(src0 == src1),
                        .Neq => src0 = @intFromBool(src0 != src1),
                        .Lt => src0 = @intFromBool(src0 < src1),
                        .Leq => src0 = @intFromBool(src0 <= src1),
                        .Gt => src0 = @intFromBool(src0 > src1),
                        .Geq => src0 = @intFromBool(src0 >= src1),
                    }

                    condition = src0 != 0;
                }

                if (condition) {
                    ip = c.interp.labels[jmpc.label];
                    continue;
                }
            },
            .Setnz => |Setnz| {
                const dst = grab_pointer_from_lvalue(c, Setnz.dst);
                const src = grab_u64_from_rvalue(c, Setnz.src);

                stack_write(c, dst, @intFromBool(src != 0), Setnz.dst.grab_size());
            },
            .Mov => |Mov| {
                move_rvalue_to_lvalue(c, Mov.dst, Mov.src);
            },
            .Movsx => |Movsx| {
                const dst = grab_pointer_from_lvalue(c, Movsx.dst);
                var src = grab_u64_from_rvalue(c, Movsx.src);
                src = utils.sign_extend(src, Movsx.src_bits);
                stack_write(c, dst, src, Movsx.dst.grab_size());
            },
            .Call => |Call| {
                const dst = grab_pointer_from_lvalue(c, Call.dst);
                stack_push(c, dst);
                stack_push(c, ip);
                stack_push(c, c.interp.rbp);

                const label = grab_u64_from_rvalue(c, Call.src);
                ip = c.interp.labels[label];
                continue;
            },
            .Push => |src| {
                stack_push_rvalue(c, src);
            },
            .Pop => |bytes| {
                c.interp.rsp -= bytes;
            },
            .Ret0 => |label| {
                ip = c.interp.labels[label];
                continue;
            },
            .Ret1 => |Ret1| {
                move_rvalue_to_lvalue(c, Ret1.dst, Ret1.src);
                ip = c.interp.labels[Ret1.label];
                continue;
            },
            .GFB => |gfb| {
                c.interp.rbp = c.interp.rsp;
                c.interp.rsp += gfb.stack_space_used;
            },
            .GFE => |gfe| {
                c.interp.rsp -= gfe.stack_space_used;
                c.interp.rbp = stack_pop(c);
                ip = stack_pop(c);
                _ = stack_pop(c);
            },
            .Label => {},
            .Exit => break,
        }

        ip += 1;
    }
}

fn move_rvalue_to_lvalue(c: *Compiler, dst: IRC.Lvalue, src: IRC.Rvalue) void {
    const pointer = grab_pointer_from_lvalue(c, dst);
    move_rvalue_to_pointer(c, pointer, src, dst.grab_size());
}

fn move_rvalue_to_pointer(c: *Compiler, dst: u64, src: IRC.Rvalue, size: u64) void {
    if (size <= 8) {
        const value = grab_u64_from_rvalue(c, src);
        stack_write(c, dst, value, size);
    } else {
        const src_ptr = grab_pointer_from_lvalue(c, src.Lvalue);

        const dst_view: []u8 = c.interp.stack[dst .. dst + size];
        const src_view: []u8 = c.interp.stack[src_ptr .. src_ptr + size];
        @memcpy(dst_view, src_view);
    }
}

fn grab_pointer_from_tmp(c: *Compiler, tmp: IRC.Tmp) u64 {
    return switch (tmp.offset) {
        .Global => |offset| offset,
        .Local => |offset| if (offset >= 0)
            c.interp.rbp + @as(u64, @intCast(offset))
        else
            @bitCast(c.interp.rbp - @as(u64, @intCast((-offset)))),
    };
}

fn grab_pointer_from_mem(c: *Compiler, mem: IRC.Mem) u64 {
    var offset: u64 = 0;

    if (mem.base) |base| {
        offset += grab_value_from_tmp(c, base, false);
    }

    offset += mem.offset;

    return offset;
}

fn grab_pointer_from_lvalue(c: *Compiler, lvalue: IRC.Lvalue) u64 {
    return switch (lvalue) {
        .Tmp => |tmp| grab_pointer_from_tmp(c, tmp),
        .Mem => |mem| grab_pointer_from_mem(c, mem),
    };
}

fn grab_value_from_tmp(c: *Compiler, tmp: IRC.Tmp, is_signed: bool) u64 {
    const pointer = grab_pointer_from_tmp(c, tmp);
    return stack_read(c, pointer, tmp.size, is_signed);
}

fn grab_value_from_rvalue(c: *Compiler, rvalue: IRC.Rvalue, is_signed: bool) u64 {
    switch (rvalue) {
        .Lvalue => |lvalue| {
            const pointer = grab_pointer_from_lvalue(c, lvalue);
            return stack_read(c, pointer, lvalue.grab_size(), is_signed);
        },
        .Addr => |lvalue| return grab_pointer_from_lvalue(c, lvalue),
        .Label => |label| return label,
        .Imm => |value| return value,
    }
}

inline fn grab_u64_from_rvalue(c: *Compiler, rvalue: IRC.Rvalue) u64 {
    return grab_value_from_rvalue(c, rvalue, false);
}

inline fn grab_i64_from_rvalue(c: *Compiler, rvalue: IRC.Rvalue) i64 {
    return @bitCast(grab_value_from_rvalue(c, rvalue, true));
}

fn stack_read(c: *Compiler, pointer: u64, size: u64, is_signed: bool) u64 {
    std.debug.assert(size <= 8);

    check_bounds(c, pointer, size);

    const offset: u6 = @intCast((pointer % 8) * 8);
    const bits: u8 = @intCast(size * 8);
    const ones: u64 = utils.left_shift(0xFFFF_FFFF_FFFF_FFFF, bits);

    var value = @as(*u64, @alignCast(@ptrCast(&c.interp.stack[pointer - (pointer % 8)]))).*;
    value >>= offset;
    value &= ~ones;

    if (is_signed) {
        value = utils.sign_extend(value, bits);
    }

    return value;
}

fn stack_write(c: *Compiler, pointer: u64, value: u64, size: u64) void {
    std.debug.assert(size <= 8);

    check_bounds(c, pointer, size);

    if (size == 0) {
        return;
    }

    const start_offset: u8 = @intCast((pointer % 8) * 8);
    const end_offset: u8 = 64 - start_offset - @as(u8, @intCast(size * 8));
    const ones: u64 = utils.left_shift(0xFFFF_FFFF_FFFF_FFFF, start_offset) &
        utils.right_shift((0xFFFF_FFFF_FFFF_FFFF), end_offset);
    const ptr = @as(*u64, @alignCast(@ptrCast(&c.interp.stack[pointer - (pointer % 8)])));
    ptr.* &= ~ones;
    ptr.* |= utils.left_shift(value, start_offset) & ones;
}

fn stack_push(c: *Compiler, value: u64) void {
    c.interp.rsp = utils.align_u64(c.interp.rsp, .QWORD);
    stack_write(c, c.interp.rsp, value, 8);
    c.interp.rsp += 8;
}

fn check_bounds(c: *Compiler, pointer: u64, size: u64) void {
    if (pointer + size > c.interp.stack.len) {
        std.debug.print("error: stack overflow: reading {} bytes at {}\n", .{ size, pointer });
        Compiler.exit(1);
    }
}

fn stack_push_rvalue(c: *Compiler, rvalue: IRC.Rvalue) void {
    const size = utils.align_u64(rvalue.grab_size(), .QWORD);
    move_rvalue_to_pointer(c, c.interp.rsp, rvalue, size);
    c.interp.rsp += size;
}

fn stack_pop(c: *Compiler) u64 {
    c.interp.rsp -= 8;
    return stack_read(c, c.interp.rsp, 8, false);
}
