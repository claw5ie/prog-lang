instrs: ByteList,
globals: ByteList,

const IR = @This();

pub fn init() IR {
    return .{
        .instrs = ByteList.initCapacity(Compiler.gpa, 4 * 1024) catch {
            Compiler.exit(1);
        },
        .globals = ByteList.initCapacity(Compiler.gpa, 4 * 1024) catch {
            Compiler.exit(1);
        },
    };
}

pub fn deinit(ir: *IR) void {
    ir.instrs.deinit();
    ir.globals.deinit();
}

pub fn print(ir: *IR) void {
    Compiler.oprint("globals byte count: {}\n" ++
        "instruction byte count: {}\n" ++
        "\n", .{
        ir.globals.items.len,
        ir.instrs.items.len,
    });

    var instrs = ir.instrs.items;
    var ip: usize = 0;
    while (ip < instrs.len) {
        const instr, const count = Decoded.decode_instr(instrs[ip..]);
        Compiler.oprint("{:>4}  {}\n", .{ ip, instr });
        ip += count;
    }
}

pub fn write_to_file(ir: *IR, filepath: [:0]const u8) void {
    const fd = std.posix.open(
        filepath,
        .{
            .ACCMODE = .WRONLY,
            .CREAT = true,
            .TRUNC = true,
        },
        0o644,
    ) catch {
        Compiler.eprint("error: couldn't open a file '{s}'\n", .{filepath});
        Compiler.exit(1);
    };
    defer std.posix.close(fd);

    nostd.write_to_file_v(fd, Compiler.magic_number_string);
    nostd.write_to_file_u64(fd, ir.globals.items.len);
    nostd.write_to_file_u64(fd, ir.instrs.items.len);
    nostd.write_to_file_v(fd, ir.globals.items);
    nostd.write_to_file_v(fd, ir.instrs.items);
}

pub fn read_from_file(filepath: [:0]const u8) IR {
    const fd = std.posix.open(filepath, .{}, 0) catch {
        Compiler.eprint("error: couldn't open a file '{s}'\n", .{filepath});
        Compiler.exit(1);
    };
    defer std.posix.close(fd);

    const magic_number = nostd.read_from_file_u64(fd);
    std.debug.assert(magic_number == Compiler.magic_number_value);
    const globals_byte_count = nostd.read_from_file_u64(fd);
    const instrs_byte_count = nostd.read_from_file_u64(fd);
    var globals = ByteList.init(Compiler.gpa);
    var instrs = ByteList.init(Compiler.gpa);

    globals.resize(globals_byte_count) catch {
        Compiler.exit(1);
    };
    instrs.resize(instrs_byte_count) catch {
        Compiler.exit(1);
    };

    nostd.read_from_file_v(fd, globals.items);
    nostd.read_from_file_v(fd, instrs.items);

    return .{
        .instrs = instrs,
        .globals = globals,
    };
}

const std = @import("std");
const nostd = @import("nostd.zig");
const Compiler = @import("Compiler.zig");

pub const ByteList = std.ArrayList(u8);

// Stuff needed to generate IR instructions.
pub const Encoded = struct {
    // Size of return pointer + IP + rbp
    pub const FIRST_PARAM_OFFSET = 8 + 8 + 8;

    pub const Label = u64;

    pub const Tmp = Decoded.Tmp;

    pub const Operand = union(enum) {
        Tmp: packed struct {
            offset: Tmp.OffsetType,
            pad: SizeType = 0,
            tag: Tmp.Tag,
            size: u64,
        },
        Mem: packed struct {
            base: Tmp,
            offset: u64,
            size: u64,
        },
        Addr_T: packed struct {
            offset: Tmp.OffsetType,
            pad: SizeType = 0,
            tag: Tmp.Tag,
        },
        Addr_M: packed struct {
            base: Tmp,
            offset: u64,
        },
        Imm: u64,
        Label: Label,

        pub const SizeType = Decoded.Operand.SizeType;

        pub fn grab_size(op: Operand) u64 {
            return switch (op) {
                .Tmp => |tmp| tmp.size,
                .Mem => |mem| mem.size,
                .Addr_T,
                .Addr_M,
                .Imm,
                .Label,
                => unreachable,
            };
        }

        pub fn grab_size_no_fail(op: Operand) u64 {
            return switch (op) {
                .Tmp => |tmp| tmp.size,
                .Mem => |mem| mem.size,
                .Addr_T,
                .Addr_M,
                .Imm,
                .Label,
                => 8,
            };
        }

        pub fn is_lvalue(op: Operand) bool {
            return switch (op) {
                .Tmp,
                .Mem,
                => true,
                .Addr_T,
                .Addr_M,
                .Imm,
                .Label,
                => false,
            };
        }

        pub fn bump(op: Operand, offset: u64, size: u64) Operand {
            var r = op;

            switch (r) {
                .Tmp => |*tmp| {
                    tmp.offset += @intCast(offset);
                    tmp.size = size;
                },
                .Mem => |*mem| {
                    mem.offset += offset;
                    mem.size = size;
                },
                .Addr_T,
                .Addr_M,
                .Imm,
                .Label,
                => unreachable,
            }

            return r;
        }

        pub fn addr_of(op: Operand) Operand {
            switch (op) {
                .Tmp => |tmp| {
                    return .{ .Addr_T = .{
                        .offset = tmp.offset,
                        .tag = tmp.tag,
                    } };
                },
                .Mem => |mem| {
                    return .{ .Addr_M = .{
                        .base = mem.base,
                        .offset = mem.offset,
                    } };
                },
                .Addr_T,
                .Addr_M,
                .Imm,
                .Label,
                => unreachable,
            }
        }

        pub fn mem_from_tmp(op: Operand, offset: u64, size: u64) Operand {
            const tmp = &op.Tmp;
            return .{ .Mem = .{
                .base = .{
                    .offset = tmp.offset,
                    .size_minus_one = @intCast(tmp.size - 1),
                    .tag = tmp.tag,
                },
                .offset = offset,
                .size = size,
            } };
        }

        pub fn decode(op: Operand) Decoded.Operand {
            return switch (op) {
                .Tmp => |tmp| .{ .Tmp = .{
                    .offset = tmp.offset,
                    .size_minus_one = @intCast(tmp.size - 1),
                    .tag = tmp.tag,
                } },
                .Mem => |mem| if (mem.offset != 0)
                    .{ .Mem_BO = .{
                        .base = mem.base,
                        .offset = mem.offset,
                        .size_minus_one = @intCast(mem.size - 1),
                    } }
                else
                    .{ .Mem_B = .{
                        .base = mem.base,
                        .size_minus_one = @intCast(mem.size - 1),
                    } },
                .Addr_T => |tmp| .{ .Addr_T = .{
                    .offset = tmp.offset,
                    .tag = tmp.tag,
                } }, // Could memcpy/reinterpet as u64?
                .Addr_M => |mem| if (mem.offset != 0)
                    .{ .Addr_BO = .{
                        .base = mem.base,
                        .offset = mem.offset,
                    } }
                else
                    .{ .Tmp = mem.base },
                .Imm => |imm| .{ .Imm = imm },
                .Label => |label| .{ .Label = label },
            };
        }
    };

    pub const Opcode = Decoded.Opcode;

    pub const Instr = struct {
        opcode: Opcode,
        ops: [3]Operand,
        ops_count: u8,
    };
};

// Stuff needed to decode IR instructions.
pub const Decoded = struct {
    pub const Tmp = packed struct {
        offset: OffsetType,
        size_minus_one: Operand.SizeType,
        tag: Tag,

        pub const Tag = enum(u2) {
            Relative = 0,
            Global = 1,
            Absolute = 2,
        };

        pub const OffsetType = i59;

        pub fn format(tmp: Tmp, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            try writer.print("{c}{}/{}", .{
                @as(u8, switch (tmp.tag) {
                    .Relative => 'r',
                    .Global => 'g',
                    .Absolute => 'a',
                }),
                tmp.offset,
                @as(u64, tmp.size_minus_one) + 1,
            });
        }
    };

    pub const Operand = union(Operand.Tag) {
        Tmp: Tmp,
        Mem_B: packed struct {
            base: Tmp,
            size_minus_one: SizeType,
        },
        Mem_BO: packed struct {
            base: Tmp,
            offset: u64,
            size_minus_one: SizeType,
        },
        Addr_T: packed struct {
            offset: Tmp.OffsetType,
            pad: SizeType = 0,
            tag: Tmp.Tag,
        },
        Addr_BO: packed struct {
            base: Tmp,
            offset: u64,
        },
        Imm: u64,
        Label: u64,

        pub const SizeType = u3;

        pub const Tag = enum(u3) {
            Tmp = 0,
            Mem_B = 1,
            Mem_BO = 2,
            Addr_T = 3,
            Addr_BO = 4,
            Imm = 5,
            Label = 6,
        };

        pub fn format(op: Operand, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            switch (op) {
                .Tmp => |tmp| {
                    try writer.print("{}", .{tmp});
                },
                .Mem_B => |mem| {
                    try writer.print("[{}]/{}", .{ mem.base, @as(u64, mem.size_minus_one) + 1 });
                },
                .Mem_BO => |mem| {
                    try writer.print("[{} + {}]/{}", .{ mem.base, mem.offset, @as(u64, mem.size_minus_one) + 1 });
                },
                .Addr_T => |tmp| {
                    try writer.print(
                        "addr {c}{}",
                        .{
                            @as(u8, switch (tmp.tag) {
                                .Relative => 'r',
                                .Global => 'g',
                                .Absolute => 'a',
                            }),
                            tmp.offset,
                        },
                    );
                },
                .Addr_BO => |mem| {
                    try writer.print("addr [{} + {}]", .{ mem.base, mem.offset });
                },
                .Imm => |imm| {
                    try writer.print("{}", .{imm});
                },
                .Label => |label| {
                    try writer.print("l{}", .{label});
                },
            }
        }

        pub fn grab_size(op: Operand) u8 {
            const size: u8 = switch (op) {
                .Tmp => |tmp| tmp.size_minus_one,
                .Mem_B => |mem| mem.size_minus_one,
                .Mem_BO => |mem| mem.size_minus_one,
                .Addr_T,
                .Addr_BO,
                .Imm,
                .Label,
                => unreachable,
            };
            return size + 1;
        }
    };

    pub const Opcode = enum(u8) {
        exit = 0,

        printp,
        printi,
        printu,
        printb,

        ue,
        une,
        ul,
        ule,
        ug,
        uge,
        uadd,
        usub,
        umul,
        udiv,
        umod,

        ie,
        ine,
        il,
        ile,
        ig,
        ige,
        iadd,
        isub,
        imul,
        idiv,
        imod,

        neg,
        not,

        jmp,

        uje,
        ujne,
        ujl,
        ujle,
        ujg,
        ujge,

        ije,
        ijne,
        ijl,
        ijle,
        ijg,
        ijge,

        setnz,

        mov,
        mov_big,
        movsx,

        call,
        push,
        push_big,
        pop,

        start_proc,
        end_proc,

        pub fn to_signed(opcode: Opcode) Opcode {
            return switch (opcode) {
                .ue => .ie,
                .une => .ine,
                .ul => .il,
                .ule => .ile,
                .ug => .ig,
                .uge => .ige,
                .uadd => .iadd,
                .usub => .isub,
                .umul => .imul,
                .udiv => .idiv,
                .umod => .imod,
                .uje => .ije,
                .ujne => .ijne,
                .ujl => .ijl,
                .ujle => .ijle,
                .ujg => .ijg,
                .ujge => .ijge,
                else => unreachable,
            };
        }

        pub fn operand_count(opcode: Opcode) u8 {
            return switch (opcode) {
                .exit => 0,
                .printp,
                .printi,
                .printu,
                .printb,
                .jmp,
                .call,
                .push,
                .pop,
                .start_proc,
                .end_proc,
                => 1,
                .neg,
                .not,
                .setnz,
                .mov,
                .push_big,
                => 2,
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
                .uje,
                .ujne,
                .ujl,
                .ujle,
                .ujg,
                .ujge,
                .ije,
                .ijne,
                .ijl,
                .ijle,
                .ijg,
                .ijge,
                .mov_big,
                .movsx,
                => 3,
            };
        }

        pub fn to_string(opcode: Opcode) []const u8 {
            return switch (opcode) {
                .exit => "exit",
                .printp => "printp",
                .printi => "printi",
                .printu => "printu",
                .printb => "printb",
                .ue => "ue",
                .une => "une",
                .ul => "ul",
                .ule => "ule",
                .ug => "ug",
                .uge => "uge",
                .uadd => "uadd",
                .usub => "usub",
                .umul => "umul",
                .udiv => "udiv",
                .umod => "umod",
                .ie => "ie",
                .ine => "ine",
                .il => "il",
                .ile => "ile",
                .ig => "ig",
                .ige => "ige",
                .iadd => "iadd",
                .isub => "isub",
                .imul => "imul",
                .idiv => "idiv",
                .imod => "imod",
                .neg => "neg",
                .not => "not",
                .jmp => "jmp",
                .uje => "uje",
                .ujne => "ujne",
                .ujl => "ujl",
                .ujle => "ujle",
                .ujg => "ujg",
                .ujge => "ujge",
                .ije => "ije",
                .ijne => "ijne",
                .ijl => "ijl",
                .ijle => "ijle",
                .ijg => "ijg",
                .ijge => "ijge",
                .setnz => "setnz",
                .mov => "mov",
                .mov_big => "mov_big",
                .movsx => "movsx",
                .call => "call",
                .push => "push",
                .push_big => "push_big",
                .pop => "pop",
                .start_proc => "start_proc",
                .end_proc => "end_proc",
            };
        }
    };

    pub const Instr = struct {
        opcode: Opcode,
        ops: [3]Operand,
        ops_count: u8,

        pub fn format(instr: Instr, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            const pad_size = 12;
            const op_name = instr.opcode.to_string();

            try writer.print("{s} ", .{op_name});
            {
                var i = pad_size - op_name.len;
                while (i > 0) {
                    i -= 1;
                    try writer.print(" ", .{});
                }
            }
            for (instr.ops[0..instr.ops_count], 0..) |op, i| {
                try writer.print("{}", .{op});

                if (i + 1 < instr.ops_count) {
                    try writer.print(", ", .{});
                }
            }
        }
    };

    pub fn decode_instr(data: []const u8) struct { Instr, usize } {
        const State = struct {
            data: []const u32,
            opsinfo: u24,
            at: u8,
        };

        const fns = struct {
            pub fn init_state(d: []const u8) State {
                var new_data: []const u32 = undefined;
                new_data.ptr = @alignCast(@ptrCast(d.ptr));
                new_data.len = @divExact(d.len, 4);
                return .{
                    .data = new_data,
                    .opsinfo = 0,
                    .at = 0,
                };
            }

            pub fn decode_header(s: *State) Opcode {
                const header = s.data[s.at];
                s.at += 1;
                s.opsinfo = @intCast(header >> 8);
                return @enumFromInt(header & 0xFF);
            }

            pub fn decode_tmp(s: *State) Tmp {
                const tmp_as_u64 = decode_imm(s);
                return @as(*const Tmp, @ptrCast(&tmp_as_u64)).*;
            }

            pub fn decode_imm(s: *State) u64 {
                const imm = @as(u64, s.data[s.at]) | (@as(u64, s.data[s.at + 1]) << 32);
                s.at += 2;
                return imm;
            }

            pub fn decode_operand(s: *State) Operand {
                const optag: Operand.Tag = @enumFromInt(s.opsinfo & 0b111);
                s.opsinfo >>= 3;

                switch (optag) {
                    .Tmp => {
                        const tmp = decode_tmp(s);
                        return .{ .Tmp = tmp };
                    },
                    .Mem_B => {
                        const size_minus_one = decode_operand_size(s);
                        const tmp = decode_tmp(s);
                        return .{ .Mem_B = .{
                            .base = tmp,
                            .size_minus_one = size_minus_one,
                        } };
                    },
                    .Mem_BO => {
                        const size_minus_one = decode_operand_size(s);
                        const base = decode_tmp(s);
                        const offset = decode_imm(s);
                        return .{ .Mem_BO = .{
                            .base = base,
                            .offset = offset,
                            .size_minus_one = size_minus_one,
                        } };
                    },
                    .Addr_T => {
                        const tmp = decode_tmp(s);
                        return .{ .Addr_T = .{
                            .offset = tmp.offset,
                            .tag = tmp.tag,
                        } };
                    },
                    .Addr_BO => {
                        const base = decode_tmp(s);
                        const offset = decode_imm(s);
                        return .{ .Addr_BO = .{
                            .base = base,
                            .offset = offset,
                        } };
                    },
                    .Imm => {
                        const imm = decode_imm(s);
                        return .{ .Imm = imm };
                    },
                    .Label => {
                        const label = decode_imm(s);
                        return .{ .Label = label };
                    },
                }
            }

            fn decode_operand_size(s: *State) u3 {
                const size: u3 = @intCast(s.opsinfo & 0b111);
                s.opsinfo >>= 3;
                return size;
            }
        };

        std.debug.assert(data.len >= 4);

        var state = fns.init_state(data);
        var instr: Instr = instr: {
            const opcode = fns.decode_header(&state);
            break :instr .{
                .opcode = opcode,
                .ops = undefined,
                .ops_count = opcode.operand_count(),
            };
        };

        for (0..instr.ops_count) |i| {
            instr.ops[i] = fns.decode_operand(&state);
        }

        return .{ instr, state.at * 4 };
    }
};
