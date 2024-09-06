irc: *IRC,
stack: []u8,
rsp: u64,
rbp: u64,

const Interp = @This();

pub fn interpret(irc: *IRC) void {
    const stack = common.gpa.alloc(u8, 2 * 1024 * 1024) catch {
        std.posix.exit(1);
    };
    defer common.gpa.free(stack);

    var interpreter = Interp{
        .irc = irc,
        .stack = stack,
        .rsp = 0,
        .rbp = 0,
    };

    interpret_top_level(&interpreter);
}

fn interpret_top_level(interp: *Interp) void {
    const labels = common.gpa.alloc(u64, interp.irc.label_count) catch {
        std.posix.exit(1);
    };
    defer common.gpa.free(labels);

    for (interp.irc.instrs.items, 0..) |instr, i| {
        switch (instr) {
            .GFB => |gfb| {
                labels[gfb.label] = i;
            },
            .GFE => |gfe| {
                labels[gfe.label] = i;
            },
            .Label => |label| {
                labels[label] = i;
            },
            else => {},
        }
    }

    interp.rsp = @intCast(interp.irc.globals_count + 128);
    interp.rbp = interp.rsp;

    var ip: u64 = 0;
    while (ip < interp.irc.instrs.items.len) {
        switch (interp.irc.instrs.items[ip]) {
            .Print => |Print| {
                switch (Print) {
                    .Pointer => |src| {
                        const value = grab_u64_from_rvalue(interp, src);
                        common.oprint("0x{x}\n", .{value});
                    },
                    .Integer => |Integer| {
                        if (Integer.is_signed) {
                            const value = grab_i64_from_rvalue(interp, Integer.src);
                            common.oprint("{}\n", .{value});
                        } else {
                            const value = grab_u64_from_rvalue(interp, Integer.src);
                            common.oprint("{}\n", .{value});
                        }
                    },
                    .Boolean => |src| {
                        const value = grab_u64_from_rvalue(interp, src);
                        common.oprint("{s}\n", .{if (value != 0) "true" else "false"});
                    },
                }
            },
            .Binary_Op => |op| {
                if (op.is_signed) {
                    const dst = grab_pointer_from_lvalue(interp, op.dst);
                    var src0 = grab_i64_from_rvalue(interp, op.src0);
                    const src1 = grab_i64_from_rvalue(interp, op.src1);

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

                    stack_write(interp, dst, @bitCast(src0), op.dst.grab_size());
                } else {
                    const dst = grab_pointer_from_lvalue(interp, op.dst);
                    var src0 = grab_u64_from_rvalue(interp, op.src0);
                    const src1 = grab_u64_from_rvalue(interp, op.src1);

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

                    stack_write(interp, dst, src0, op.dst.grab_size());
                }
            },
            .Unary_Op => |op| {
                if (op.is_signed) {
                    const dst = grab_pointer_from_lvalue(interp, op.dst);
                    var src = grab_i64_from_rvalue(interp, op.src);

                    switch (op.tag) {
                        .Not => src = @intFromBool(src == 0),
                        .Neg => src = -src,
                    }

                    stack_write(interp, dst, @bitCast(src), op.dst.grab_size());
                } else {
                    const dst = grab_pointer_from_lvalue(interp, op.dst);
                    var src = grab_u64_from_rvalue(interp, op.src);

                    switch (op.tag) {
                        .Not => src = @intFromBool(src == 0),
                        .Neg => unreachable,
                    }

                    stack_write(interp, dst, src, op.dst.grab_size());
                }
            },
            .Jmp => |label| {
                ip = labels[label];
                continue;
            },
            .Jmpc => |jmpc| {
                var condition = false;
                if (jmpc.is_signed) {
                    var src0 = grab_i64_from_rvalue(interp, jmpc.src0);
                    const src1 = grab_i64_from_rvalue(interp, jmpc.src1);

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
                    var src0 = grab_u64_from_rvalue(interp, jmpc.src0);
                    const src1 = grab_u64_from_rvalue(interp, jmpc.src1);

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
                    ip = labels[jmpc.label];
                    continue;
                }
            },
            .Setnz => |Setnz| {
                const dst = grab_pointer_from_lvalue(interp, Setnz.dst);
                const src = grab_u64_from_rvalue(interp, Setnz.src);

                stack_write(interp, dst, @intFromBool(src != 0), Setnz.dst.grab_size());
            },
            .Mov => |Mov| {
                move_rvalue_to_lvalue(interp, Mov.dst, Mov.src);
            },
            .Movsx => |Movsx| {
                const dst = grab_pointer_from_lvalue(interp, Movsx.dst);
                var src = grab_u64_from_rvalue(interp, Movsx.src);
                src = utils.sign_extend(src, Movsx.src_bits);
                stack_write(interp, dst, src, Movsx.dst.grab_size());
            },
            .Call => |Call| {
                const dst = grab_pointer_from_lvalue(interp, Call.dst);
                stack_push(interp, dst);
                stack_push(interp, ip);
                stack_push(interp, interp.rbp);

                const label = grab_u64_from_rvalue(interp, Call.src);
                ip = labels[label];
                continue;
            },
            .Push => |src| {
                stack_push_rvalue(interp, src);
            },
            .Pop => |bytes| {
                interp.rsp -= bytes;
            },
            .Ret0 => |label| {
                ip = labels[label];
                continue;
            },
            .Ret1 => |Ret1| {
                move_rvalue_to_lvalue(interp, Ret1.dst, Ret1.src);
                ip = labels[Ret1.label];
                continue;
            },
            .GFB => |gfb| {
                interp.rbp = interp.rsp;
                interp.rsp += gfb.stack_space_used;
            },
            .GFE => |gfe| {
                interp.rsp -= gfe.stack_space_used;
                interp.rbp = stack_pop(interp);
                ip = stack_pop(interp);
                _ = stack_pop(interp);
            },
            .Label => {},
            .Exit => break,
        }

        ip += 1;
    }
}

fn move_rvalue_to_lvalue(interp: *Interp, dst: IRC.Lvalue, src: IRC.Rvalue) void {
    const pointer = grab_pointer_from_lvalue(interp, dst);
    move_rvalue_to_pointer(interp, pointer, src, dst.grab_size());
}

fn move_rvalue_to_pointer(interp: *Interp, dst: u64, src: IRC.Rvalue, size: u64) void {
    if (size <= 8) {
        const value = grab_u64_from_rvalue(interp, src);
        stack_write(interp, dst, value, size);
    } else {
        const src_ptr = grab_pointer_from_lvalue(interp, src.Lvalue);

        const dst_view: []u8 = interp.stack[dst .. dst + size];
        const src_view: []u8 = interp.stack[src_ptr .. src_ptr + size];
        @memcpy(dst_view, src_view);
    }
}

fn grab_pointer_from_tmp(interp: *Interp, tmp: IRC.Tmp) u64 {
    return switch (tmp.offset) {
        .Global => |offset| offset,
        .Local => |offset| if (offset >= 0)
            interp.rbp + @as(u64, @intCast(offset))
        else
            @bitCast(interp.rbp - @as(u64, @intCast((-offset)))),
    };
}

fn grab_pointer_from_mem(interp: *Interp, mem: IRC.Mem) u64 {
    var offset: u64 = 0;

    if (mem.base) |base| {
        offset += grab_value_from_tmp(interp, base, false);
    }

    offset += mem.offset;

    return offset;
}

fn grab_pointer_from_lvalue(interp: *Interp, lvalue: IRC.Lvalue) u64 {
    return switch (lvalue) {
        .Tmp => |tmp| grab_pointer_from_tmp(interp, tmp),
        .Mem => |mem| grab_pointer_from_mem(interp, mem),
    };
}

fn grab_value_from_tmp(interp: *Interp, tmp: IRC.Tmp, is_signed: bool) u64 {
    const pointer = grab_pointer_from_tmp(interp, tmp);
    return stack_read(interp, pointer, tmp.size, is_signed);
}

fn grab_value_from_rvalue(interp: *Interp, rvalue: IRC.Rvalue, is_signed: bool) u64 {
    switch (rvalue) {
        .Lvalue => |lvalue| {
            const pointer = grab_pointer_from_lvalue(interp, lvalue);
            return stack_read(interp, pointer, lvalue.grab_size(), is_signed);
        },
        .Addr => |lvalue| return grab_pointer_from_lvalue(interp, lvalue),
        .Label => |label| return label,
        .Imm => |value| return value,
    }
}

inline fn grab_u64_from_rvalue(interp: *Interp, rvalue: IRC.Rvalue) u64 {
    return grab_value_from_rvalue(interp, rvalue, false);
}

inline fn grab_i64_from_rvalue(interp: *Interp, rvalue: IRC.Rvalue) i64 {
    return @bitCast(grab_value_from_rvalue(interp, rvalue, true));
}

fn stack_read(interp: *Interp, pointer: u64, size: u64, is_signed: bool) u64 {
    std.debug.assert(size <= 8);

    check_bounds(interp, pointer, size);

    const offset: u6 = @intCast((pointer % 8) * 8);
    const bits: u8 = @intCast(size * 8);
    const ones: u64 = utils.left_shift(0xFFFF_FFFF_FFFF_FFFF, bits);

    var value = @as(*u64, @alignCast(@ptrCast(&interp.stack[pointer - (pointer % 8)]))).*;
    value >>= offset;
    value &= ~ones;

    if (is_signed) {
        value = utils.sign_extend(value, bits);
    }

    return value;
}

fn stack_write(interp: *Interp, pointer: u64, value: u64, size: u64) void {
    std.debug.assert(size <= 8);

    check_bounds(interp, pointer, size);

    if (size == 0) {
        return;
    }

    const start_offset: u8 = @intCast((pointer % 8) * 8);
    const end_offset: u8 = 64 - start_offset - @as(u8, @intCast(size * 8));
    const ones: u64 = utils.left_shift(0xFFFF_FFFF_FFFF_FFFF, start_offset) &
        utils.right_shift((0xFFFF_FFFF_FFFF_FFFF), end_offset);
    const ptr = @as(*u64, @alignCast(@ptrCast(&interp.stack[pointer - (pointer % 8)])));
    ptr.* &= ~ones;
    ptr.* |= utils.left_shift(value, start_offset) & ones;
}

fn stack_push(interp: *Interp, value: u64) void {
    interp.rsp = utils.align_u64(interp.rsp, .QWORD);
    stack_write(interp, interp.rsp, value, 8);
    interp.rsp += 8;
}

fn check_bounds(interp: *Interp, pointer: u64, size: u64) void {
    if (pointer + size > interp.stack.len) {
        std.debug.print("error: stack overflow: reading {} bytes at {}\n", .{ size, pointer });
        common.exit(1);
    }
}

fn stack_push_rvalue(interp: *Interp, rvalue: IRC.Rvalue) void {
    const size = utils.align_u64(rvalue.grab_size(), .QWORD);
    move_rvalue_to_pointer(interp, interp.rsp, rvalue, size);
    interp.rsp += size;
}

fn stack_pop(interp: *Interp) u64 {
    interp.rsp -= 8;
    return stack_read(interp, interp.rsp, 8, false);
}

const std = @import("std");
const utils = @import("utils.zig");
const common = @import("common.zig");
const IRC = @import("irc.zig");
