const std = @import("std");
const utils = @import("utils.zig");
const notstd = @import("notstd.zig");

const Compiler = struct {
    stack: []u8,
    rsp: u64 = 0,
    rbp: u64 = 0,
};

inline fn cast_ptr(comptime dst: type, src: anytype) dst {
    return @alignCast(@ptrCast(src));
}

fn ir_grab_pointer_from_tmp(c: *Compiler, tmp: IrTmp) u64 {
    switch (tmp.tag) {
        .Global => return @intCast(tmp.offset),
        .Local => {
            var i = tmp.height;
            var rbp = c.rbp;
            while (i > 0) : (i -= 1) {
                rbp = ir_stack_read_u64(c, rbp - -LF_RBP_OFFSET);
            }

            var rbp_i64: i64 = @bitCast(rbp);
            return @bitCast(rbp_i64 + tmp.offset);
        },
    }
}

fn ir_grab_pointer_from_address(c: *Compiler, address: IrMem) u64 {
    var offset: i64 = 0;

    if (address.choose & 0x1 == 0x1) {
        var base: i64 = @bitCast(ir_grab_value_from_tmp(c, address.base));
        offset += base;
    }

    if (address.choose & 0x2 == 0x2) {
        offset += address.disp;
    }

    return @bitCast(offset);
}

fn ir_grab_pointer_from_lvalue(c: *Compiler, lvalue: IrLvalue) u64 {
    switch (lvalue) {
        .Tmp => |tmp| return ir_grab_pointer_from_tmp(c, tmp),
        .Mem => |address| return ir_grab_pointer_from_address(c, address),
    }
}

fn ir_grab_value_from_tmp(c: *Compiler, tmp: IrTmp) u64 {
    var pointer = ir_grab_pointer_from_tmp(c, tmp);
    return ir_stack_read_u64(c, pointer);
}

fn ir_grab_value_from_rvalue(c: *Compiler, rvalue: IrRvalue) u64 {
    switch (rvalue) {
        .Lvalue => |lvalue| {
            var pointer = ir_grab_pointer_from_lvalue(c, lvalue);
            return ir_stack_read_u64(c, pointer);
        },
        .Addr => |lvalue| return ir_grab_pointer_from_lvalue(c, lvalue),
        .Label => |label| return label,
        .Imm => |imm| return @bitCast(imm),
    }
}

fn ir_stack_read_u64(c: *Compiler, pointer: u64) u64 {
    if (pointer < 8) {
        std.debug.print("error: null pointer access (0x{x}).\n", .{pointer});
        std.os.exit(1);
    } else if (pointer + 8 > c.rsp) {
        std.debug.print("error: address 0x{x} is outside of current stack pointer (0x{x}).\n", .{ pointer, c.rsp });
        std.os.exit(1);
    }
    return ir_stack_read_u64_no_check(c, pointer);
}

inline fn ir_stack_read_u64_no_check(c: *Compiler, pointer: u64) u64 {
    return cast_ptr(*u64, &c.stack[pointer]).*;
}

fn ir_stack_write_u64(c: *Compiler, pointer: u64, value: u64) void {
    if (pointer < 8) {
        std.debug.print("error: null pointer access (0x{x}).\n", .{pointer});
        std.os.exit(1);
    } else if (pointer + 8 > c.rsp) {
        std.debug.print("error: address 0x{x} is outside of current stack pointer (0x{x}).\n", .{ pointer, c.rsp });
        std.os.exit(1);
    }
    ir_stack_write_u64_no_check(c, pointer, value);
}

inline fn ir_stack_write_u64_no_check(c: *Compiler, pointer: u64, value: u64) void {
    cast_ptr(*u64, &c.stack[pointer]).* = value;
}

fn ir_stack_push(c: *Compiler, value: u64) void {
    ir_stack_write_u64_no_check(c, c.rsp, value);
    c.rsp += 8;
}

fn ir_stack_pop(c: *Compiler) u64 {
    c.rsp -= 8;
    return ir_stack_read_u64_no_check(c, c.rsp);
}

fn interpret(c: *Compiler) void {
    c.stack = gpa.alloc(u8, 2 * 1024 * 1024) catch {
        std.os.exit(1);
    };
    defer gpa.free(c.stack);

    var labels = gpa.alloc(u64, c.next_label) catch {
        std.os.exit(1);
    };
    defer gpa.free(labels);

    for (c.ir_instrs.items, 0..) |ir_instr, i| {
        switch (ir_instr) {
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

    c.rsp = @intCast(c.next_global);
    c.rbp = c.rsp;

    var ip: u64 = 0;
    while (ip < c.ir_instrs.items.len) {
        switch (c.ir_instrs.items[ip]) {
            .Binary_Op => |op| {
                var dst = ir_grab_pointer_from_lvalue(c, op.dst);
                var src0 = ir_grab_value_from_rvalue(c, op.src0);
                var src1 = ir_grab_value_from_rvalue(c, op.src1);

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

                ir_stack_write_u64(c, dst, src0);
            },
            .Unary_Op => |op| {
                var dst = ir_grab_pointer_from_lvalue(c, op.dst);
                var src = ir_grab_value_from_rvalue(c, op.src);

                switch (op.tag) {
                    .Not => src = @intFromBool(src == 0),
                    .Neg => src = @bitCast(-@as(i64, @bitCast(src))),
                }

                ir_stack_write_u64(c, dst, src);
            },
            .Jmpc => |jmpc| {
                var src0 = ir_grab_value_from_rvalue(c, jmpc.src0);
                var src1 = ir_grab_value_from_rvalue(c, jmpc.src1);

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
                        var src: i64 = @bitCast(ir_grab_value_from_rvalue(c, tmp));
                        std.debug.print("{}\n", .{src});
                    },
                    .Mov => |mov| {
                        var dst = ir_grab_pointer_from_lvalue(c, mov.dst);
                        var src = ir_grab_value_from_rvalue(c, mov.src);

                        ir_stack_write_u64(c, dst, src);
                    },
                    .Jmp => |label| {
                        ip = labels[label];
                        continue;
                    },
                    .Push_Frame_Pointer => |depth| {
                        var i = depth;
                        var rbp = c.rbp;
                        while (i >= 0) : (i -= 1) {
                            rbp = ir_stack_read_u64(c, rbp - -LF_RBP_OFFSET);
                        }

                        ir_stack_push(c, rbp);
                    },
                    .Push => |src| {
                        var value = ir_grab_value_from_rvalue(c, src);
                        ir_stack_push(c, value);
                    },
                    .Pop => |count| {
                        c.rsp -= count;
                    },
                    .Call => |src| {
                        ir_stack_push(c, ip);
                        ir_stack_push(c, c.rbp);

                        var label = ir_grab_value_from_rvalue(c, src);
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
                        c.rbp = c.rsp;
                        c.rsp += gfb.stack_space_used;
                    },
                    .GFE => |gfe| {
                        c.rsp -= gfe.stack_space_used;
                        c.rbp = ir_stack_pop(c);
                        ip = ir_stack_pop(c);
                    },
                    .Label => {},
                }
            },
        }

        ip += 1;
    }
}

pub fn compile() void {
    var args = std.process.args();
    var filepath: [:0]const u8 = "examples/debug";

    _ = args.next().?;
    if (args.next()) |arg| {
        filepath = arg;
    }

    var compiler = Compiler{
        .stack = &[0]u8{},
    };

    parse_top_level(&compiler);
    resolve_identifiers(&compiler);
    typecheck(&compiler);
    generate_ir(&compiler);
    debug_print_ir(&compiler);
    interpret(&compiler);

    gpa.free(source_code);
    compiler.symbols.deinit();
    ast_arena.deinit();
    compiler.ir_instrs.deinit();
    std.debug.assert(general_purpose_allocator.deinit() == .ok);
}
