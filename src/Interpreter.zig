stack: []u8,
rsp: u64,
rbp: u64,
vtable: Vtable,
ir: *IR,

const Interpreter = @This();

pub const STACK_SIZE = 4 * 1024 * 1024;

const UNUSED_SPACE_SIZE = 1024;

pub fn init(ir: *IR) Interpreter {
    const stack = Compiler.gpa.alloc(u8, STACK_SIZE) catch {
        Compiler.exit(1);
    };

    const vtable = init_vtable(ir);

    return .{
        .stack = stack,
        .rsp = vtable.start_stack,
        .rbp = vtable.start_stack,
        .vtable = vtable,
        .ir = ir,
    };
}

pub fn deinit(interp: *Interpreter) void {
    Compiler.gpa.free(interp.stack);
}

pub fn init_vtable(ir: *IR) Vtable {
    const start_instr = UNUSED_SPACE_SIZE;
    const start_global = start_instr + nostd.align_by_pow2(ir.instrs.items.len, 1024);
    const start_stack = start_global + nostd.align_by_pow2(ir.globals.items.len, 1024);
    const end = start_stack + STACK_SIZE;

    return .{
        .unused_space = 0,
        .start_instr = start_instr,
        .start_global = start_global,
        .start_stack = start_stack,
        .end = end,
    };
}

pub fn reset(interp: *Interpreter) void {
    interp.vtable = init_vtable(interp.ir);
    interp.rsp = interp.vtable.start_stack;
    interp.rbp = interp.vtable.start_stack;
}

pub fn interpret(interp: *Interpreter) void {
    interpret_top_level(interp);
}

fn interpret_top_level(interp: *Interpreter) void {
    var ip: u64 = interp.vtable.start_instr;
    while (true) {
        const segment, const tag = segment_from_vtable(interp, ip);
        if (tag != .Instructions) {
            Compiler.eprint("error: can't read instructions from {s} section\n", .{tag.to_string()});
            Compiler.exit(1);
        }
        const instr, const count = IRD.decode_instr(segment);
        ip += count;

        switch (instr.opcode) {
            .exit => break,
            .printp => {
                const value = grab_u64_from_operand(interp, instr.ops[0]);
                Compiler.oprint("0x{x}\n", .{value});
            },
            .printi => {
                const value = grab_i64_from_operand(interp, instr.ops[0]);
                Compiler.oprint("{}\n", .{value});
            },
            .printu => {
                const value = grab_u64_from_operand(interp, instr.ops[0]);
                Compiler.oprint("{}\n", .{value});
            },
            .printb => {
                const value = grab_u64_from_operand(interp, instr.ops[0]);
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
                const dst = grab_address_from_operand(interp, instr.ops[0]);
                var src0 = grab_u64_from_operand(interp, instr.ops[1]);
                const src1 = grab_u64_from_operand(interp, instr.ops[2]);

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

                write(interp, dst, src0, instr.ops[0].grab_size());
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
                const dst = grab_address_from_operand(interp, instr.ops[0]);
                var src0 = grab_i64_from_operand(interp, instr.ops[1]);
                const src1 = grab_i64_from_operand(interp, instr.ops[2]);

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

                write(interp, dst, @bitCast(src0), instr.ops[0].grab_size());
            },
            .neg => {
                const dst = grab_address_from_operand(interp, instr.ops[0]);
                const src = grab_i64_from_operand(interp, instr.ops[1]);

                write(interp, dst, @bitCast(-src), instr.ops[0].grab_size());
            },
            .not => {
                const dst = grab_address_from_operand(interp, instr.ops[0]);
                const src = grab_u64_from_operand(interp, instr.ops[1]);

                write(interp, dst, @intFromBool(src == 0), instr.ops[0].grab_size());
            },
            .jmp => {
                const offset = grab_u64_from_operand(interp, instr.ops[0]);
                ip = offset;
            },
            .uje,
            .ujne,
            .ujl,
            .ujle,
            .ujg,
            .ujge,
            => {
                const src0 = grab_u64_from_operand(interp, instr.ops[0]);
                const src1 = grab_u64_from_operand(interp, instr.ops[1]);

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
                    const offset = grab_u64_from_operand(interp, instr.ops[2]);
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
                const src0 = grab_i64_from_operand(interp, instr.ops[0]);
                const src1 = grab_i64_from_operand(interp, instr.ops[1]);

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
                    const offset = grab_u64_from_operand(interp, instr.ops[2]);
                    ip = offset;
                }
            },
            .setnz => {
                const dst = grab_address_from_operand(interp, instr.ops[0]);
                const src = grab_u64_from_operand(interp, instr.ops[1]);

                write(interp, dst, @intFromBool(src != 0), instr.ops[0].grab_size());
            },
            .mov => {
                const dst = grab_address_from_operand(interp, instr.ops[0]);
                const src = grab_u64_from_operand(interp, instr.ops[1]);

                write(interp, dst, src, instr.ops[0].grab_size());
            },
            .mov_big => {
                const dst = grab_u64_from_operand(interp, instr.ops[0]);
                const src = grab_u64_from_operand(interp, instr.ops[1]);
                const size = grab_u64_from_operand(interp, instr.ops[2]);

                write_big(interp, dst, src, size);
            },
            .movsx => {
                const dst = grab_address_from_operand(interp, instr.ops[0]);
                var src = grab_u64_from_operand(interp, instr.ops[1]);
                const bits = grab_u64_from_operand(interp, instr.ops[2]);

                if (bits > 64) {
                    Compiler.eprint("error: can't extend more than 64 bits\n", .{});
                    Compiler.exit(1);
                }

                src = nostd.sign_extend(src, @intCast(bits));
                write(interp, dst, src, instr.ops[0].grab_size());
            },
            .call => {
                stack_push(interp, ip);

                const offset = grab_u64_from_operand(interp, instr.ops[0]);
                ip = offset;
            },
            .push => {
                const src = grab_u64_from_operand(interp, instr.ops[0]);
                stack_push(interp, src);
            },
            .push_big => {
                const src = grab_u64_from_operand(interp, instr.ops[0]);
                const size = grab_u64_from_operand(interp, instr.ops[1]);

                stack_push_big(interp, src, size);
            },
            .pop => {
                const bytes = grab_u64_from_operand(interp, instr.ops[0]);
                interp.rsp -= bytes;
            },
            .start_proc => {
                const stack_space_used = grab_u64_from_operand(interp, instr.ops[0]);

                stack_push(interp, interp.rbp);
                interp.rbp = interp.rsp;
                interp.rsp += stack_space_used;
            },
            .end_proc => {
                const stack_space_used = grab_u64_from_operand(interp, instr.ops[0]);

                interp.rsp -= stack_space_used;
                interp.rbp = stack_pop(interp);
                ip = stack_pop(interp);
            },
        }
    }
}

pub fn grab_address_from_operand(interp: *Interpreter, op: IRD.Operand) u64 {
    return switch (op) {
        .Tmp => |tmp| grab_address_from_tmp(interp, tmp),
        .Mem_B => |mem| grab_address_from_mem(interp, mem.base, 0),
        .Mem_BO => |mem| grab_address_from_mem(interp, mem.base, mem.offset),
        .Addr_T,
        .Addr_BO,
        .Imm,
        .Label,
        => unreachable,
    };
}

inline fn grab_u64_from_operand(interp: *Interpreter, op: IRD.Operand) u64 {
    return grab_value_from_operand(interp, op, false);
}

inline fn grab_i64_from_operand(interp: *Interpreter, op: IRD.Operand) i64 {
    return @bitCast(grab_value_from_operand(interp, op, true));
}

pub fn grab_value_from_operand(interp: *Interpreter, op: IRD.Operand, is_signed: bool) u64 {
    switch (op) {
        .Tmp,
        .Mem_B,
        .Mem_BO,
        => {
            const address = grab_address_from_operand(interp, op);
            return read(interp, address, op.grab_size(), is_signed);
        },
        .Addr_T => |tmp| return grab_address_from_tmp(interp, .{ .offset = tmp.offset, .size_minus_one = 0, .tag = tmp.tag }),
        .Addr_BO => |mem| return grab_address_from_mem(interp, mem.base, mem.offset),
        .Imm => |imm| return imm,
        .Label => unreachable,
    }
}

fn grab_address_from_tmp(interp: *Interpreter, tmp: IRD.Tmp) u64 {
    return switch (tmp.tag) {
        .Relative => inc_u_by_i(interp.rbp, tmp.offset),
        .Global => inc_u_by_i(interp.vtable.start_global, tmp.offset),
        .Absolute => @intCast(tmp.offset),
    };
}

fn grab_address_from_mem(interp: *Interpreter, base: IRD.Tmp, offset: u64) u64 {
    var r: u64 = 0;

    r += grab_value_from_tmp(interp, base, false);
    r += offset;

    return r;
}

fn grab_value_from_tmp(interp: *Interpreter, tmp: IRD.Tmp, is_signed: bool) u64 {
    const address = grab_address_from_tmp(interp, tmp);
    return read(interp, address, tmp.size_minus_one, is_signed);
}

fn segment_from_vtable(interp: *Interpreter, address: u64) struct { []u8, Vtable.SegmentTag } {
    const vtable = &interp.vtable;
    if (address < vtable.start_instr) {
        Compiler.eprint("error: can't read/write from/to unused space\n", .{});
        Compiler.exit(1);
    } else if (address < vtable.start_global) {
        const off = address - vtable.start_instr;
        return .{ interp.ir.instrs.items[off..], .Instructions };
    } else if (address < vtable.start_stack) {
        const off = address - vtable.start_global;
        return .{ interp.ir.globals.items[off..], .Globals };
    } else if (address < vtable.end) {
        const off = address - vtable.start_stack;
        return .{ interp.stack[off..], .Stack };
    } else {
        Compiler.eprint("error: address '{}' outside of address space ({})\n", .{ address, vtable.end });
        Compiler.exit(1);
    }
}

pub fn read(interp: *Interpreter, address: u64, size: u8, is_signed: bool) u64 {
    std.debug.assert(size <= 8 and size > 0);

    const offset: u6 = @intCast((address % 8) * 8);
    const bits: u8 = @intCast(size * 8);
    const ones: u64 = nostd.left_shift(0xFFFF_FFFF_FFFF_FFFF, bits);

    const segment, const tag = segment_from_vtable(interp, address - (address % 8));
    check_bounds(segment, tag, size, false);

    var value = @as(*u64, @alignCast(@ptrCast(&segment[0]))).*; // Read 8 bytes (aligned by 8).
    value >>= offset; // Shift to begging.
    value &= ~ones; // Zero upper bits.

    if (is_signed) {
        value = nostd.sign_extend(value, bits);
    }

    return value;
}

fn write(interp: *Interpreter, address: u64, value: u64, size: u8) void {
    std.debug.assert(size <= 8 and size > 0);

    const start_offset: u8 = @intCast((address % 8) * 8);
    const end_offset: u8 = 64 - start_offset - (size * 8);
    const ones: u64 = nostd.left_shift(0xFFFF_FFFF_FFFF_FFFF, start_offset) &
        nostd.right_shift((0xFFFF_FFFF_FFFF_FFFF), end_offset);

    const segment, const tag = segment_from_vtable(interp, address - (address % 8));
    check_bounds(segment, tag, size, true);

    const ptr = @as(*u64, @alignCast(@ptrCast(&segment[0])));
    ptr.* &= ~ones;
    ptr.* |= nostd.left_shift(value, start_offset) & ones;
}

pub fn write_big(interp: *Interpreter, dst_address: u64, src_address: u64, size: u64) void {
    const dst_segment, const dst_tag = segment_from_vtable(interp, dst_address);
    const src_segment, const src_tag = segment_from_vtable(interp, src_address);
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

fn stack_push(interp: *Interpreter, value: u64) void {
    std.debug.assert(interp.rsp % 8 == 0);
    write(interp, interp.rsp, value, 8);
    interp.rsp += 8;
}

fn stack_push_big(interp: *Interpreter, address: u64, size: u64) void {
    std.debug.assert(interp.rsp % 8 == 0);
    write_big(interp, interp.rsp, address, size);
    interp.rsp += nostd.align_u64(size, .QWORD);
}

fn stack_pop(interp: *Interpreter) u64 {
    interp.rsp -= 8;
    return read(interp, interp.rsp, 8, false);
}

fn check_bounds(segment: []u8, tag: Vtable.SegmentTag, size: u64, for_writing: bool) void {
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

const std = @import("std");
const nostd = @import("nostd.zig");
const Compiler = @import("Compiler.zig");
const Ast = @import("Ast.zig");
const IR = @import("IR.zig");
const IRGenerator = @import("IRGenerator.zig");

const IRE = IR.Encoded;
const IRD = IR.Decoded;

pub const Vtable = struct {
    unused_space: u64,
    start_instr: u64,
    start_global: u64,
    start_stack: u64,
    end: u64,

    pub const SegmentTag = enum {
        Instructions,
        Globals,
        Stack,

        pub fn to_string(tag: SegmentTag) []const u8 {
            return switch (tag) {
                .Instructions => "instruction",
                .Globals => "globals",
                .Stack => "stack",
            };
        }
    };
};
