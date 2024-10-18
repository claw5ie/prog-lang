const std = @import("std");
const utils = @import("utils.zig");
const Compiler = @import("compiler.zig");
const IRGen = @import("irc-generator.zig");

const Ast = Compiler.Ast;
const IR = Compiler.IR;
const IRE = IR.Encoded;
const IRD = IR.Decoded;
const SegmentTag = Compiler.Interpreter.Vtable.SegmentTag;

pub fn interpret(c: *Compiler) void {
    interpret_top_level(c);
}

// Expression is not struct/union/array.
pub fn compute_simple_expr(c: *Compiler, expr: *Ast.Expr) u64 {
    const op = compute_expr(c, expr);
    return grab_value_from_operand(c, op, expr.typ.is_signed());
}

pub fn compute_expr_to_operand(c: *Compiler, dst: IRE.Operand, expr: *Ast.Expr) void {
    std.debug.assert(dst.is_lvalue());
    const op = compute_expr(c, expr);
    const dst_address = grab_value_from_operand(c, dst.addr_of().decode(), false);
    const src_address = grab_address_from_operand(c, op);
    interp_write_big(c, dst_address, src_address, expr.typ.data.byte_size);
}

fn compute_expr(c: *Compiler, expr: *Ast.Expr) IRD.Operand {
    if (!expr.flags.is_const) {
        c.report_error(expr.line_info, "expression is not constant", .{});
        Compiler.exit(1);
    }

    var old_irgen = c.irgen;

    const dst = IRGen.grab_local_from_type(c, expr.typ.data);
    std.debug.assert(dst.is_lvalue());
    _ = IRGen.generate_ir_expr(c, dst, expr);
    IRGen.generate_ir_instr0(c, .exit);
    IRGen.remove_labels_and_set_vtable(c);
    interpret(c);

    c.ir.instrs.clearRetainingCapacity();

    old_irgen.labels = c.irgen.labels;
    c.irgen = old_irgen;

    return dst.decode();
}

fn interpret_top_level(c: *Compiler) void {
    c.interp.rsp = c.interp.vtable.start_stack;
    c.interp.rbp = c.interp.rsp;

    var ip: u64 = c.interp.vtable.start_instr;
    while (true) {
        const segment, const tag = segment_from_vtable(c, ip);
        if (tag != .Instructions) {
            Compiler.eprint("error: can't read instructions from {s} section\n", .{tag.to_string()});
            Compiler.exit(1);
        }
        const instr, const count = IRD.decode_instr(segment);
        ip += count;

        switch (instr.opcode) {
            .exit => break,
            .printp => {
                const value = grab_u64_from_operand(c, instr.ops[0]);
                Compiler.oprint("0x{x}\n", .{value});
            },
            .printi => {
                const value = grab_i64_from_operand(c, instr.ops[0]);
                Compiler.oprint("{}\n", .{value});
            },
            .printu => {
                const value = grab_u64_from_operand(c, instr.ops[0]);
                Compiler.oprint("{}\n", .{value});
            },
            .printb => {
                const value = grab_u64_from_operand(c, instr.ops[0]);
                Compiler.oprint("{s}\n", .{if (value != 0) "true" else "false"});
            },
            .ue,
            .une,
            .ul,
            .ule,
            .ug,
            .uge,
            .uadd,
            .usub,
            .umul,
            .udiv,
            .umod,
            => {
                const dst = grab_address_from_operand(c, instr.ops[0]);
                var src0 = grab_u64_from_operand(c, instr.ops[1]);
                const src1 = grab_u64_from_operand(c, instr.ops[2]);

                switch (instr.opcode) {
                    .ue => src0 = @intFromBool(src0 == src1),
                    .une => src0 = @intFromBool(src0 != src1),
                    .ul => src0 = @intFromBool(src0 < src1),
                    .ule => src0 = @intFromBool(src0 <= src1),
                    .ug => src0 = @intFromBool(src0 > src1),
                    .uge => src0 = @intFromBool(src0 >= src1),
                    .uadd => src0 += src1,
                    .usub => src0 -= src1,
                    .umul => src0 *= src1,
                    .udiv => src0 = @divTrunc(src0, src1),
                    .umod => src0 = @rem(src0, src1),
                    else => unreachable,
                }

                interp_write(c, dst, src0, instr.ops[0].grab_size());
            },
            .ie,
            .ine,
            .il,
            .ile,
            .ig,
            .ige,
            .iadd,
            .isub,
            .imul,
            .idiv,
            .imod,
            => {
                const dst = grab_address_from_operand(c, instr.ops[0]);
                var src0 = grab_i64_from_operand(c, instr.ops[1]);
                const src1 = grab_i64_from_operand(c, instr.ops[2]);

                switch (instr.opcode) {
                    .ie => src0 = @intFromBool(src0 == src1),
                    .ine => src0 = @intFromBool(src0 != src1),
                    .il => src0 = @intFromBool(src0 < src1),
                    .ile => src0 = @intFromBool(src0 <= src1),
                    .ig => src0 = @intFromBool(src0 > src1),
                    .ige => src0 = @intFromBool(src0 >= src1),
                    .iadd => src0 += src1,
                    .isub => src0 -= src1,
                    .imul => src0 *= src1,
                    .idiv => src0 = @divTrunc(src0, src1),
                    .imod => src0 = @rem(src0, src1),
                    else => unreachable,
                }

                interp_write(c, dst, @bitCast(src0), instr.ops[0].grab_size());
            },
            .neg => {
                const dst = grab_address_from_operand(c, instr.ops[0]);
                const src = grab_i64_from_operand(c, instr.ops[1]);

                interp_write(c, dst, @bitCast(-src), instr.ops[0].grab_size());
            },
            .not => {
                const dst = grab_address_from_operand(c, instr.ops[0]);
                const src = grab_u64_from_operand(c, instr.ops[1]);

                interp_write(c, dst, @intFromBool(src == 0), instr.ops[0].grab_size());
            },
            .jmp => {
                const offset = grab_u64_from_operand(c, instr.ops[0]);
                ip = offset;
            },
            .uje,
            .ujne,
            .ujl,
            .ujle,
            .ujg,
            .ujge,
            => {
                const src0 = grab_u64_from_operand(c, instr.ops[0]);
                const src1 = grab_u64_from_operand(c, instr.ops[1]);

                const should_jump: bool = switch (instr.opcode) {
                    .uje => src0 == src1,
                    .ujne => src0 != src1,
                    .ujl => src0 < src1,
                    .ujle => src0 <= src1,
                    .ujg => src0 > src1,
                    .ujge => src0 >= src1,
                    else => unreachable,
                };

                if (should_jump) {
                    const offset = grab_u64_from_operand(c, instr.ops[2]);
                    ip = offset;
                }
            },
            .ije,
            .ijne,
            .ijl,
            .ijle,
            .ijg,
            .ijge,
            => {
                const src0 = grab_i64_from_operand(c, instr.ops[0]);
                const src1 = grab_i64_from_operand(c, instr.ops[1]);

                const should_jump: bool = switch (instr.opcode) {
                    .ije => src0 == src1,
                    .ijne => src0 != src1,
                    .ijl => src0 < src1,
                    .ijle => src0 <= src1,
                    .ijg => src0 > src1,
                    .ijge => src0 >= src1,
                    else => unreachable,
                };

                if (should_jump) {
                    const offset = grab_u64_from_operand(c, instr.ops[2]);
                    ip = offset;
                }
            },
            .setnz => {
                const dst = grab_address_from_operand(c, instr.ops[0]);
                const src = grab_u64_from_operand(c, instr.ops[1]);

                interp_write(c, dst, @intFromBool(src != 0), instr.ops[0].grab_size());
            },
            .mov => {
                const dst = grab_address_from_operand(c, instr.ops[0]);
                const src = grab_u64_from_operand(c, instr.ops[1]);

                interp_write(c, dst, src, instr.ops[0].grab_size());
            },
            .mov_big => {
                const dst = grab_u64_from_operand(c, instr.ops[0]);
                const src = grab_u64_from_operand(c, instr.ops[1]);
                const size = grab_u64_from_operand(c, instr.ops[2]);

                interp_write_big(c, dst, src, size);
            },
            .movsx => {
                const dst = grab_address_from_operand(c, instr.ops[0]);
                var src = grab_u64_from_operand(c, instr.ops[1]);
                const bits = grab_u64_from_operand(c, instr.ops[2]);

                if (bits > 64) {
                    Compiler.eprint("error: can't extend more than 64 bits\n", .{});
                    Compiler.exit(1);
                }

                src = utils.sign_extend(src, @intCast(bits));
                interp_write(c, dst, src, instr.ops[0].grab_size());
            },
            .call => {
                stack_push(c, ip);

                const offset = grab_u64_from_operand(c, instr.ops[0]);
                ip = offset;
            },
            .push => {
                const src = grab_u64_from_operand(c, instr.ops[0]);
                stack_push(c, src);
            },
            .push_big => {
                const src = grab_u64_from_operand(c, instr.ops[0]);
                const size = grab_u64_from_operand(c, instr.ops[1]);

                stack_push_big(c, src, size);
            },
            .pop => {
                const bytes = grab_u64_from_operand(c, instr.ops[0]);
                c.interp.rsp -= bytes;
            },
            .start_proc => {
                const stack_space_used = grab_u64_from_operand(c, instr.ops[0]);

                stack_push(c, c.interp.rbp);
                c.interp.rbp = c.interp.rsp;
                c.interp.rsp += stack_space_used;
            },
            .end_proc => {
                const stack_space_used = grab_u64_from_operand(c, instr.ops[0]);

                c.interp.rsp -= stack_space_used;
                c.interp.rbp = stack_pop(c);
                ip = stack_pop(c);
            },
        }
    }
}

fn grab_address_from_operand(c: *Compiler, op: IRD.Operand) u64 {
    return switch (op) {
        .Tmp => |tmp| grab_address_from_tmp(c, tmp),
        .Mem_B => |mem| grab_address_from_mem(c, mem.base, 0),
        .Mem_BO => |mem| grab_address_from_mem(c, mem.base, mem.offset),
        .Addr_T,
        .Addr_BO,
        .Imm,
        .Label,
        => unreachable,
    };
}

inline fn grab_u64_from_operand(c: *Compiler, op: IRD.Operand) u64 {
    return grab_value_from_operand(c, op, false);
}

inline fn grab_i64_from_operand(c: *Compiler, op: IRD.Operand) i64 {
    return @bitCast(grab_value_from_operand(c, op, true));
}

fn grab_value_from_operand(c: *Compiler, op: IRD.Operand, is_signed: bool) u64 {
    switch (op) {
        .Tmp,
        .Mem_B,
        .Mem_BO,
        => {
            const address = grab_address_from_operand(c, op);
            return interp_read(c, address, op.grab_size(), is_signed);
        },
        .Addr_T => |tmp| return grab_address_from_tmp(c, .{ .offset = tmp.offset, .size_minus_one = 0, .tag = tmp.tag }),
        .Addr_BO => |mem| return grab_address_from_mem(c, mem.base, mem.offset),
        .Imm => |imm| return imm,
        .Label => unreachable,
    }
}

fn grab_address_from_tmp(c: *Compiler, tmp: IRD.Tmp) u64 {
    return switch (tmp.tag) {
        .Relative => inc_u_by_i(c.interp.rbp, tmp.offset),
        .Global => inc_u_by_i(c.interp.vtable.start_global, tmp.offset),
        .Absolute => @intCast(tmp.offset),
    };
}

fn grab_address_from_mem(c: *Compiler, base: IRD.Tmp, offset: u64) u64 {
    var r: u64 = 0;

    r += grab_value_from_tmp(c, base, false);
    r += offset;

    return r;
}

fn grab_value_from_tmp(c: *Compiler, tmp: IRD.Tmp, is_signed: bool) u64 {
    const address = grab_address_from_tmp(c, tmp);
    return interp_read(c, address, tmp.size_minus_one, is_signed);
}

fn segment_from_vtable(c: *Compiler, address: u64) struct { []u8, Compiler.Interpreter.Vtable.SegmentTag } {
    const vtable = &c.interp.vtable;
    if (address < vtable.start_instr) {
        Compiler.eprint("error: can't read/write from/to unused space\n", .{});
        Compiler.exit(1);
    } else if (address < vtable.start_global) {
        const off = address - vtable.start_instr;
        return .{ c.ir.instrs.items[off..], .Instructions };
    } else if (address < vtable.start_stack) {
        const off = address - vtable.start_global;
        return .{ c.ir.globals.items[off..], .Globals };
    } else if (address < vtable.end) {
        const off = address - vtable.start_stack;
        return .{ c.interp.stack[off..], .Stack };
    } else {
        Compiler.eprint("error: address '{}' outside of address space ({})\n", .{ address, vtable.end });
        Compiler.exit(1);
    }
}

fn interp_read(c: *Compiler, address: u64, size: u8, is_signed: bool) u64 {
    std.debug.assert(size <= 8 and size > 0);

    const offset: u6 = @intCast((address % 8) * 8);
    const bits: u8 = @intCast(size * 8);
    const ones: u64 = utils.left_shift(0xFFFF_FFFF_FFFF_FFFF, bits);

    const segment, const tag = segment_from_vtable(c, address - (address % 8));
    check_bounds(segment, tag, size, false);

    var value = @as(*u64, @alignCast(@ptrCast(&segment[0]))).*; // Read 8 bytes (aligned by 8).
    value >>= offset; // Shift to begging.
    value &= ~ones; // Zero upper bits.

    if (is_signed) {
        value = utils.sign_extend(value, bits);
    }

    return value;
}

fn interp_write(c: *Compiler, address: u64, value: u64, size: u8) void {
    std.debug.assert(size <= 8 and size > 0);

    const start_offset: u8 = @intCast((address % 8) * 8);
    const end_offset: u8 = 64 - start_offset - (size * 8);
    const ones: u64 = utils.left_shift(0xFFFF_FFFF_FFFF_FFFF, start_offset) &
        utils.right_shift((0xFFFF_FFFF_FFFF_FFFF), end_offset);

    const segment, const tag = segment_from_vtable(c, address - (address % 8));
    check_bounds(segment, tag, size, true);

    const ptr = @as(*u64, @alignCast(@ptrCast(&segment[0])));
    ptr.* &= ~ones;
    ptr.* |= utils.left_shift(value, start_offset) & ones;
}

fn interp_write_big(c: *Compiler, dst_address: u64, src_address: u64, size: u64) void {
    const dst_segment, const dst_tag = segment_from_vtable(c, dst_address);
    const src_segment, const src_tag = segment_from_vtable(c, src_address);
    check_bounds(dst_segment, dst_tag, size, true);
    check_bounds(src_segment, src_tag, size, false);

    var dst_slice: []u8 = undefined;
    dst_slice.ptr = @ptrCast(&dst_segment[0]);
    dst_slice.len = size;
    var src_slice: []const u8 = undefined;
    src_slice.ptr = @ptrCast(&src_segment[0]);
    src_slice.len = size;
    @memcpy(dst_slice, src_slice);
}

fn stack_push(c: *Compiler, value: u64) void {
    std.debug.assert(c.interp.rsp % 8 == 0);
    interp_write(c, c.interp.rsp, value, 8);
    c.interp.rsp += 8;
}

fn stack_push_big(c: *Compiler, address: u64, size: u64) void {
    std.debug.assert(c.interp.rsp % 8 == 0);
    interp_write_big(c, c.interp.rsp, address, size);
    c.interp.rsp += utils.align_u64(size, .QWORD);
}

fn stack_pop(c: *Compiler) u64 {
    c.interp.rsp -= 8;
    return interp_read(c, c.interp.rsp, 8, false);
}

fn check_bounds(segment: []u8, tag: SegmentTag, size: u64, for_writing: bool) void {
    if (for_writing) {
        switch (tag) {
            .Instructions => {
                Compiler.eprint("error: can't write to instruction segment\n", .{});
                Compiler.exit(1);
            },
            .Globals,
            .Stack,
            => {},
        }
    }

    if (segment.len < size) {
        Compiler.eprint("error: memory out of bounds: can't read {} bytes at {s} segment\n", .{ size, tag.to_string() });
        Compiler.exit(1);
    }
}

inline fn inc_u_by_i(value: u64, offset: i59) u64 {
    return if (offset >= 0)
        value + @as(u64, @intCast(offset))
    else
        value - @as(u64, @intCast(-offset));
}
