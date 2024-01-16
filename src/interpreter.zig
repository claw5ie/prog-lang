const Compiler = @import("compiler.zig");

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
