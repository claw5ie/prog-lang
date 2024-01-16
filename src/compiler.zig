const std = @import("std");
const utils = @import("utils.zig");
const notstd = @import("notstd.zig");

const Compiler = struct {
    main_function: *Symbol,

    stack: []u8,
    rsp: u64 = 0,
    rbp: u64 = 0,

    pub fn grab_global(c: *Compiler) IrTmp {
        var offset = c.next_global;
        c.next_global += 8;
        return .{
            .offset = offset,
            .tag = .Global,
            .height = 0,
        };
    }

    pub fn grab_local(c: *Compiler) IrTmp {
        var offset = c.next_local;
        c.next_local += 8;

        if (c.biggest_local_so_far < c.next_local) {
            c.biggest_local_so_far = c.next_local;
        }

        return .{
            .offset = offset,
            .tag = .Local,
            .height = 0,
        };
    }

    pub fn maybe_grab_local(c: *Compiler, has_lvalue: ?*IrLvalue) IrLvalue {
        return if (has_lvalue) |lvalue|
            lvalue.*
        else
            c.grab_local().as_lvalue();
    }

    pub fn return_local(c: *Compiler, tmp: IrTmp) void {
        std.debug.assert(tmp.tag == .Local);
        c.next_local -= 8;
        std.debug.assert(c.next_local == tmp.offset);
    }

    pub fn grab_label(c: *Compiler) IrLabel {
        var result = c.next_label;
        c.next_label += 1;
        return result;
    }

    pub fn grab_buncha_labels(c: *Compiler, count: u32) IrLabel {
        var result = c.next_label;
        c.next_label += count;
        return result;
    }
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

fn generate_ir_instr(c: *Compiler, instr: IrInstr) void {
    c.ir_instrs.append(instr) catch {
        std.os.exit(1);
    };
}

const GF_FIRST_ARG_OFFSET = GF_RETURN_ADDRESS_OFFSET - 8;
const GF_RETURN_ADDRESS_OFFSET = -16 - 8;
const LF_FIRST_ARG_OFFSET = LF_RETURN_ADDRESS_OFFSET - 8;
const LF_RETURN_ADDRESS_OFFSET = -16 - 16;
const LF_RBP_OFFSET = LF_RETURN_ADDRESS_OFFSET + 8;

fn generate_ir(c: *Compiler) void {
    generate_ir_instr(c, .{ .Instr = .{
        .Call = .{
            .Label = c.main_function.payload.Function.label,
        },
    } });
    generate_ir_instr(c, .{ .Instr = .Exit });

    var it = c.globals.iterator();
    while (it.next()) |symbol| {
        generate_ir_global_symbol(c, symbol.*);
    }

    it = c.local_functions.iterator();
    while (it.next()) |symbol| {
        generate_ir_function(c, &symbol.*.payload.Function, LF_FIRST_ARG_OFFSET);
    }
}

fn generate_ir_global_symbol(c: *Compiler, symbol: *Symbol) void {
    switch (symbol.payload) {
        .Variable => |*variable| {
            // Impelement globals as lvalue [imm]?
            variable.tmp = c.grab_global();

            if (variable.expr) |expr| {
                var src_tmp = generate_ir_short_lived_rvalue(c, expr);
                generate_ir_instr(c, .{ .Instr = .{
                    .Mov = .{
                        .dst = variable.tmp.as_lvalue(),
                        .src = src_tmp,
                    },
                } });
            }
        },
        .Function => |function| {
            generate_ir_function(c, &function, GF_FIRST_ARG_OFFSET);
        },
        .Type => {},
        .Parameter,
        .Definition,
        .Struct_Field,
        .Union_Field,
        .Enum_Field,
        => unreachable,
    }
}

fn generate_ir_local_symbol(c: *Compiler, symbol: *Symbol) void {
    switch (symbol.payload) {
        .Variable => |*variable| {
            if (variable.expr) |expr| {
                variable.tmp = generate_ir_expr(c, expr);
            } else {
                variable.tmp = c.grab_local();
            }
        },
        .Function => {
            // Generate local functions separately.
        },
        .Type => {},
        .Parameter,
        .Definition,
        .Struct_Field,
        .Union_Field,
        .Enum_Field,
        => unreachable,
    }
}

fn generate_ir_function(c: *Compiler, function: *const SymbolFunction, first_arg_offset: IrOffset) void {
    {
        var param_tmp = IrTmp{
            .offset = first_arg_offset,
            .tag = .Local,
            .height = 0,
        };
        var it = function._type.payload.Function.params.iterator();
        while (it.next()) |param_ptr| {
            var param = &param_ptr.*.payload.Parameter;
            param.tmp = param_tmp;
            param_tmp.offset -= 8;
        }
    }

    {
        var old_depth = c.function_depth;
        c.function_depth = function.depth;

        var function_end_label = c.grab_label();
        var index = c.ir_instrs.items.len;
        generate_ir_instr(c, undefined);

        {
            var ctx = IrStmtContext{
                .loop_cond_label = undefined,
                .loop_end_label = undefined,
                .function_end_label = function_end_label,
                .return_address_offset = first_arg_offset + 8, // NOTE[0]: Assume that return address is right after first argument.
            };
            var it = function.block.iterator();
            while (it.next()) |stmt| {
                generate_ir_stmt(c, &ctx, stmt);
            }
        }

        {
            var stack_space_used: u32 = @intCast(c.biggest_local_so_far);
            c.ir_instrs.items[index] = .{ .Meta = .{
                .GFB = .{
                    .stack_space_used = stack_space_used,
                    .label = function.label,
                },
            } };

            generate_ir_instr(c, .{ .Meta = .{
                .GFE = .{
                    .stack_space_used = stack_space_used,
                    .label = function_end_label,
                },
            } });
        }

        c.next_local = 0;
        c.biggest_local_so_far = 0;
        c.function_depth = old_depth;
    }
}

fn generate_ir_block(c: *Compiler, ctx: *const IrStmtContext, block: StmtBlock) void {
    var old_next_local = c.next_local;

    var it = block.stmts.iterator();
    while (it.next()) |stmt| {
        generate_ir_stmt(c, ctx, stmt);
    }

    if (block.scope != null) {
        c.next_local = old_next_local;
    }
}

fn generate_ir_stmt(c: *Compiler, ctx: *const IrStmtContext, stmt: *Stmt) void {
    var old_next_local = c.next_local;
    var should_clean_locals = true;

    switch (stmt.payload) {
        .Print => |expr| {
            var src_tmp = generate_ir_rvalue(c, expr, null);
            generate_ir_instr(c, .{ .Instr = .{
                .Print = src_tmp,
            } });
        },
        .Block => |block| {
            generate_ir_block(c, ctx, block);
        },
        .If => |_if| {
            var is_if_true_there: u2 = @intFromBool(_if.if_true.stmts.count != 0);
            var is_if_false_there: u2 = @intFromBool(_if.if_false.stmts.count != 0);
            switch ((is_if_true_there << 1) | is_if_false_there) {
                0b00 => {
                    _ = generate_ir_rvalue(c, _if.cond, null);
                },
                0b01 => {
                    var end_label = c.grab_label();

                    generate_ir_jump(c, _if.cond, true, end_label);
                    generate_ir_block(c, ctx, _if.if_false);
                    generate_ir_instr(c, .{ .Meta = .{
                        .Label = end_label,
                    } });
                },
                0b10 => {
                    var end_label = c.grab_label();

                    generate_ir_jump(c, _if.cond, false, end_label);
                    generate_ir_block(c, ctx, _if.if_true);
                    generate_ir_instr(c, .{ .Meta = .{
                        .Label = end_label,
                    } });
                },
                0b11 => {
                    var false_label = c.grab_label();
                    var end_label = c.grab_label();

                    generate_ir_jump(c, _if.cond, false, false_label);
                    generate_ir_block(c, ctx, _if.if_true);
                    generate_ir_instr(c, .{ .Instr = .{
                        .Jmp = end_label,
                    } });
                    generate_ir_instr(c, .{ .Meta = .{
                        .Label = false_label,
                    } });
                    generate_ir_block(c, ctx, _if.if_false);
                    generate_ir_instr(c, .{ .Meta = .{
                        .Label = end_label,
                    } });
                },
            }
        },
        .While => |_while| {
            var block_label = c.grab_label();
            var cond_label = c.grab_label();
            var end_label = c.grab_label();

            if (!_while.is_do_while) {
                generate_ir_instr(c, .{ .Instr = .{
                    .Jmp = cond_label,
                } });
            }

            generate_ir_instr(c, .{ .Meta = .{
                .Label = block_label,
            } });

            var new_ctx = ctx.*;
            new_ctx.loop_cond_label = cond_label;
            new_ctx.loop_end_label = end_label;

            generate_ir_block(c, &new_ctx, _while.block);
            generate_ir_instr(c, .{ .Meta = .{
                .Label = cond_label,
            } });
            generate_ir_jump(c, _while.cond, true, block_label);
        },
        .Break => {
            generate_ir_instr(c, .{ .Instr = .{
                .Jmp = ctx.loop_end_label,
            } });
        },
        .Continue => {
            generate_ir_instr(c, .{ .Instr = .{
                .Jmp = ctx.loop_cond_label,
            } });
        },
        .Switch => |_switch| {
            var cond_tmp = generate_ir_expr(c, _switch.cond);

            var first_case_label = c.grab_buncha_labels(@intCast(_switch.cases.count));
            var end_label = c.grab_label();

            {
                var label = first_case_label;
                var it = _switch.cases.iterator();
                while (it.next()) |case| {
                    var case_tmp = generate_ir_short_lived_rvalue(c, case.value);
                    generate_ir_instr(c, .{ .Jmpc = .{
                        .tag = .Eq,
                        .src0 = cond_tmp.as_rvalue(),
                        .src1 = case_tmp,
                        .label = label,
                    } });
                    label += 1;
                }
            }

            generate_ir_instr(c, .{ .Instr = .{
                .Jmp = end_label,
            } });

            {
                var label = first_case_label;
                var it = _switch.cases.iterator();
                while (it.next()) |case| {
                    generate_ir_instr(c, .{ .Meta = .{
                        .Label = label,
                    } });
                    generate_ir_block(c, ctx, case.block);
                    label += 1;

                    if (!case.should_fallthrough) {
                        generate_ir_instr(c, .{ .Instr = .{
                            .Jmp = end_label,
                        } });
                    }
                }
            }

            generate_ir_instr(c, .{ .Meta = .{
                .Label = end_label,
            } });
        },
        .Return => {
            generate_ir_instr(c, .{ .Instr = .{
                .Ret = ctx.function_end_label,
            } });
        },
        .Return_Expr => |expr| {
            var dst_tmp = .{ .Mem = .{
                .choose = CHOOSE_BASE,
                .base = .{
                    .offset = ctx.return_address_offset,
                    .tag = .Local,
                    .height = 0,
                },
            } };
            _ = generate_ir_rvalue(c, expr, &dst_tmp);
            generate_ir_instr(c, .{ .Instr = .{
                .Ret = ctx.function_end_label,
            } });
        },
        .Symbol => |symbol| {
            should_clean_locals = false;
            generate_ir_local_symbol(c, symbol);
        },
        .Assign => |assign| {
            var dst_tmp = generate_ir_lvalue(c, assign.lhs);
            _ = generate_ir_rvalue(c, assign.rhs, &dst_tmp);
        },
        .Expr => |expr| {
            _ = generate_ir_rvalue(c, expr, null);
        },
    }

    if (should_clean_locals) {
        c.next_local = old_next_local;
    }
}

fn generate_ir_jump(c: *Compiler, expr: *Expr, jump_if_true: bool, label: IrLabel) void {
    var old_next_local = c.next_local;

    var tag: IrInstrJmpcTag = .Neq;
    var src0_tmp: IrRvalue = undefined;
    var src1_tmp: IrRvalue = .{ .Imm = 0 };

    _ = fill_operands: {
        if (expr.payload == .Binary_Op) {
            var op = &expr.payload.Binary_Op;

            switch (op.tag) {
                .Eq => tag = .Eq,
                .Neq => tag = .Neq,
                .Lt => tag = .Lt,
                .Leq => tag = .Leq,
                .Gt => tag = .Gt,
                .Geq => tag = .Geq,
                else => {
                    src0_tmp = generate_ir_rvalue(c, expr, null);
                    break :fill_operands;
                },
            }

            src0_tmp = generate_ir_rvalue(c, op.lhs, null);
            src1_tmp = generate_ir_rvalue(c, op.rhs, null);
        } else {
            src0_tmp = generate_ir_rvalue(c, expr, null);
        }
    };

    if (!jump_if_true) {
        tag = switch (tag) {
            .Eq => .Neq,
            .Neq => .Eq,
            .Lt => .Geq,
            .Leq => .Gt,
            .Gt => .Leq,
            .Geq => .Lt,
        };
    }

    generate_ir_instr(c, .{ .Jmpc = .{
        .tag = tag,
        .src0 = src0_tmp,
        .src1 = src1_tmp,
        .label = label,
    } });

    c.next_local = old_next_local;
}

fn generate_ir_expr(c: *Compiler, expr: *Expr) IrTmp {
    var dst_tmp = c.grab_local();
    var dst_lvalue = dst_tmp.as_lvalue();
    _ = generate_ir_rvalue(c, expr, &dst_lvalue);
    return dst_tmp;
}

// Can't allocate any locals after this call to avoid trashing other locals.
fn generate_ir_short_lived_rvalue(c: *Compiler, expr: *Expr) IrRvalue {
    var old_next_local = c.next_local;
    var result = generate_ir_rvalue(c, expr, null);
    c.next_local = old_next_local;

    return result;
}

fn generate_ir_rvalue(c: *Compiler, expr: *Expr, has_dst_tmp: ?*IrLvalue) IrRvalue {
    var was_destination_used = false;
    var result: IrRvalue = result: {
        switch (expr.payload) {
            .Binary_Op => |op| {
                var lhs_tmp = generate_ir_rvalue(c, op.lhs, null);
                var rhs_tmp = generate_ir_rvalue(c, op.rhs, null);

                var tag: IrInstrBinaryOpTag = switch (op.tag) {
                    .Or => .Or,
                    .And => .And,
                    .Eq => .Eq,
                    .Neq => .Neq,
                    .Lt => .Lt,
                    .Leq => .Leq,
                    .Gt => .Gt,
                    .Geq => .Geq,
                    .Add => .Add,
                    .Sub => .Sub,
                    .Mul => .Mul,
                    .Div => .Div,
                    .Mod => .Mod,
                };

                was_destination_used = true;
                var dst_tmp = c.maybe_grab_local(has_dst_tmp);
                generate_ir_instr(c, .{ .Binary_Op = .{
                    .tag = tag,
                    .dst = dst_tmp,
                    .src0 = lhs_tmp,
                    .src1 = rhs_tmp,
                } });

                break :result dst_tmp.as_rvalue();
            },
            .Unary_Op => |op| {
                switch (op.tag) {
                    .Not => {
                        was_destination_used = true;
                        var dst_tmp = c.maybe_grab_local(has_dst_tmp);
                        var src_tmp = generate_ir_rvalue(c, op.subexpr, null);
                        generate_ir_instr(c, .{ .Unary_Op = .{
                            .tag = .Not,
                            .dst = dst_tmp,
                            .src = src_tmp,
                        } });
                        break :result dst_tmp.as_rvalue();
                    },
                    .Neg => {
                        was_destination_used = true;
                        var dst_tmp = c.maybe_grab_local(has_dst_tmp);
                        var src_tmp = generate_ir_rvalue(c, op.subexpr, null);
                        generate_ir_instr(c, .{ .Unary_Op = .{
                            .tag = .Neg,
                            .dst = dst_tmp,
                            .src = src_tmp,
                        } });
                        break :result dst_tmp.as_rvalue();
                    },
                    .Ref => {
                        break :result .{ .Addr = generate_ir_lvalue(c, op.subexpr) };
                    },
                    .Deref => {
                        var src_tmp = generate_ir_rvalue(c, op.subexpr, null);
                        break :result src_tmp.deref(c);
                    },
                }
            },
            .If => |_if| {
                var false_label = c.grab_label();
                var end_label = c.grab_label();

                generate_ir_jump(c, _if.cond, false, false_label);

                was_destination_used = true;
                var dst_tmp = c.maybe_grab_local(has_dst_tmp);
                _ = generate_ir_rvalue(c, _if.if_true, &dst_tmp);
                generate_ir_instr(c, .{ .Instr = .{
                    .Jmp = end_label,
                } });
                generate_ir_instr(c, .{ .Meta = .{
                    .Label = false_label,
                } });

                _ = generate_ir_rvalue(c, _if.if_false, &dst_tmp);
                generate_ir_instr(c, .{ .Meta = .{
                    .Label = end_label,
                } });

                break :result dst_tmp.as_rvalue();
            },
            .Call => |call| {
                var it = call.args.reverse_iterator();
                while (it.prev()) |arg| {
                    var arg_tmp = generate_ir_short_lived_rvalue(c, arg);
                    generate_ir_instr(c, .{ .Instr = .{
                        .Push = arg_tmp,
                    } });
                }

                var bytes_to_pop: u64 = call.args.count * 8;

                // Return address must the first thing pushed to the stack after arguments, because NOTE[0] relies on that assumption.
                was_destination_used = true;
                var dst_tmp = c.maybe_grab_local(has_dst_tmp);
                generate_ir_instr(c, .{ .Instr = .{
                    .Push = .{ .Addr = dst_tmp },
                } });

                bytes_to_pop += 8;

                if (call.lhs.payload == .Symbol and call.lhs.payload.Symbol.payload == .Function) {
                    var symbol = call.lhs.payload.Symbol;
                    var function = &symbol.payload.Function;

                    if (symbol.key.scope != c.global_scope) {
                        generate_ir_instr(c, .{ .Instr = .{
                            .Push_Frame_Pointer = c.function_depth - function.depth,
                        } });
                        bytes_to_pop += 8;
                    }
                }

                var src_tmp = generate_ir_rvalue(c, call.lhs, null);
                generate_ir_instr(c, .{ .Instr = .{
                    .Call = src_tmp,
                } });
                if (bytes_to_pop > 0) {
                    generate_ir_instr(c, .{ .Instr = .{
                        .Pop = bytes_to_pop,
                    } });
                }

                break :result dst_tmp.as_rvalue();
            },
            .Index,
            .Field,
            .Initializer,
            .Expr_List,
            .Designator,
            .Cast1,
            .Cast2,
            => unreachable,
            .Bool => |value| {
                break :result .{ .Imm = @intFromBool(value) };
            },
            .Int64 => |value| {
                break :result .{ .Imm = value };
            },
            .Null => {
                break :result .{ .Imm = 0 };
            },
            .Symbol => |symbol| {
                switch (symbol.payload) {
                    .Variable => |variable| {
                        var tmp = variable.tmp;
                        tmp.height = c.function_depth - symbol.depth;

                        break :result tmp.as_rvalue();
                    },
                    .Parameter => |parameter| {
                        break :result parameter.tmp.as_rvalue();
                    },
                    .Function => |function| {
                        break :result .{ .Label = function.label };
                    },
                    .Enum_Field => |field| {
                        break :result .{ .Imm = @intCast(field.value) };
                    },
                    .Struct_Field,
                    .Union_Field,
                    .Type,
                    .Definition,
                    => unreachable,
                }
            },
            .Enum_Field_From_Type,
            .Enum_Field,
            .Type,
            .Identifier,
            => unreachable,
        }
    };

    if (!was_destination_used) {
        if (has_dst_tmp) |dst_tmp| {
            generate_ir_instr(c, .{ .Instr = .{
                .Mov = .{
                    .dst = dst_tmp.*,
                    .src = result,
                },
            } });
            return dst_tmp.as_rvalue();
        }
    }

    return result;
}

fn generate_ir_lvalue(c: *Compiler, expr: *Expr) IrLvalue {
    switch (expr.payload) {
        .Unary_Op => |op| {
            std.debug.assert(op.tag == .Deref);
            var src_tmp = generate_ir_rvalue(c, op.subexpr, null);
            return src_tmp.ref(c);
        },
        .Symbol => |symbol| {
            switch (symbol.payload) {
                .Variable => |variable| {
                    var tmp = variable.tmp;
                    tmp.height = c.function_depth - symbol.depth;

                    return tmp.as_lvalue();
                },
                .Parameter => |parameter| {
                    return parameter.tmp.as_lvalue();
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

fn debug_print_ir_tmp(tmp: IrTmp) void {
    switch (tmp.tag) {
        .Global => {
            std.debug.assert(tmp.height == 0);
            std.debug.print("${}", .{tmp.offset});
        },
        .Local => {
            if (tmp.height == 0) {
                std.debug.print("%{:<1}", .{tmp.offset});
            } else {
                std.debug.print("%{:<1}({}^)", .{ tmp.offset, tmp.height });
            }
        },
    }
}

fn debug_print_ir_address(address: IrMem) void {
    std.debug.print("[", .{});

    var should_print_delim = false;

    if (address.choose & CHOOSE_BASE == CHOOSE_BASE) {
        debug_print_ir_tmp(address.base);
        should_print_delim = true;
    }

    if (address.choose & CHOOSE_DISP == CHOOSE_DISP) {
        if (should_print_delim) {
            std.debug.print(" + ", .{});
        }
        std.debug.print("{}", .{address.disp});
        should_print_delim = true;
    }

    std.debug.print("]", .{});
}

fn debug_print_ir_lvalue(lvalue: IrLvalue) void {
    switch (lvalue) {
        .Tmp => |tmp| debug_print_ir_tmp(tmp),
        .Mem => |address| debug_print_ir_address(address),
    }
}

fn debug_print_ir_rvalue(rvalue: IrRvalue) void {
    switch (rvalue) {
        .Lvalue => |lvalue| debug_print_ir_lvalue(lvalue),
        .Addr => |lvalue| {
            std.debug.print("addr ", .{});
            debug_print_ir_lvalue(lvalue);
        },
        .Label => |label| std.debug.print("L{}", .{label}),
        .Imm => |imm| std.debug.print("{}", .{imm}),
    }
}

fn debug_print_ir_instr(ir_instr: IrInstr) void {
    switch (ir_instr) {
        .Binary_Op => |op| {
            var text: []const u8 = switch (op.tag) {
                .Or => "or",
                .And => "and",
                .Eq => "eq ",
                .Neq => "neq",
                .Lt => "lt ",
                .Leq => "leq",
                .Gt => "gt ",
                .Geq => "geq",
                .Add => "add",
                .Sub => "sub",
                .Mul => "mul",
                .Div => "div",
                .Mod => "mod",
            };

            std.debug.print("    {s}    ", .{text});
            debug_print_ir_lvalue(op.dst);
            std.debug.print(", ", .{});
            debug_print_ir_rvalue(op.src0);
            std.debug.print(", ", .{});
            debug_print_ir_rvalue(op.src1);
        },
        .Unary_Op => |op| {
            var text: []const u8 = switch (op.tag) {
                .Not => "not",
                .Neg => "neg",
            };

            std.debug.print("    {s}    ", .{text});
            debug_print_ir_lvalue(op.dst);
            std.debug.print(", ", .{});
            debug_print_ir_rvalue(op.src);
        },
        .Jmpc => |jmpc| {
            var text: []const u8 = switch (jmpc.tag) {
                .Eq => "je ",
                .Neq => "jne",
                .Lt => "jl ",
                .Leq => "jle",
                .Gt => "jg ",
                .Geq => "jge",
            };

            std.debug.print("    {s}    ", .{text});
            debug_print_ir_rvalue(jmpc.src0);
            std.debug.print(", ", .{});
            debug_print_ir_rvalue(jmpc.src1);
            std.debug.print(", L{}", .{jmpc.label});
        },
        .Instr => |instr| {
            switch (instr) {
                .Print => |src| {
                    std.debug.print("    print  ", .{});
                    debug_print_ir_rvalue(src);
                },
                .Mov => |mov| {
                    std.debug.print("    mov    ", .{});
                    debug_print_ir_lvalue(mov.dst);
                    std.debug.print(", ", .{});
                    debug_print_ir_rvalue(mov.src);
                },
                .Jmp => |label| {
                    std.debug.print("    jmp    L{}", .{label});
                },
                .Push_Frame_Pointer => |depth| {
                    std.debug.print("    push   ({}^)", .{depth});
                },
                .Push => |src| {
                    std.debug.print("    push   ", .{});
                    debug_print_ir_rvalue(src);
                },
                .Pop => |count| {
                    std.debug.print("    pop    {}", .{count});
                },
                .Call => |src| {
                    std.debug.print("    call   ", .{});
                    debug_print_ir_rvalue(src);
                },
                .Ret => |label| {
                    std.debug.print("    ret    L{}", .{label});
                },
                .Exit => {
                    std.debug.print("    exit", .{});
                },
            }
        },
        .Meta => |meta| {
            switch (meta) {
                .GFB => |gfb| {
                    std.debug.print("GFB L{}: {}b", .{ gfb.label, gfb.stack_space_used });
                },
                .GFE => |gfe| {
                    std.debug.print("GFE L{}: {}b", .{ gfe.label, gfe.stack_space_used });
                    std.debug.print("\n", .{});
                },
                .Label => |label| {
                    std.debug.print("L{}:", .{label});
                },
            }
        },
    }

    std.debug.print("\n", .{});
}

fn debug_print_ir(c: *Compiler) void {
    for (c.ir_instrs.items) |ir_instr| {
        debug_print_ir_instr(ir_instr);
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
        .globals = .{},
        .local_functions = .{},
        .ir_instrs = IrInstrList.init(gpa),
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
