instructions: ByteList, // TODO: should be aligned by u32?
globals: ByteList, // TODO: should be aligned by u64?

const IR = @This();

pub fn init() IR {
    return .{
        .instructions = ByteList.initCapacity(nostd.general_allocator, 4 * 1024) catch {
            exit(1);
        },
        .globals = ByteList.initCapacity(nostd.general_allocator, 4 * 1024) catch {
            exit(1);
        },
    };
}

pub fn deinit(ir: *IR) void {
    ir.instructions.deinit();
    ir.globals.deinit();
}

pub fn print(ir: *IR) void {
    oprint("globals byte count: {}\n" ++
        "instruction byte count: {}\n" ++
        "\n", .{
        ir.globals.items.len,
        ir.instructions.items.len,
    });

    var instructions = ir.instructions.items;
    var ip: usize = 0;
    while (ip < instructions.len) {
        const instr, const count = parse_instruction(instructions[ip..]);
        oprint("{:>4}  {}\n", .{ ip, instr });
        ip += count;
    }
}

pub fn write_to_file(ir: *IR, filepath: [:0]const u8) void {
    const file = std.fs.cwd().createFile(filepath, .{}) catch {
        report_fatal_error("{s}: couldn't open the file", .{filepath});
    };
    defer file.close();

    nostd.write_to_file(file, Compiler.magic_number_string);
    nostd.write_u64_to_file(file, ir.globals.items.len);
    nostd.write_u64_to_file(file, ir.instructions.items.len);
    nostd.write_to_file(file, ir.globals.items);
    nostd.write_to_file(file, ir.instructions.items);
}

pub fn read_from_file(filepath: [:0]const u8) IR {
    const file = std.fs.cwd().openFile(filepath, .{}) catch {
        report_fatal_error("{s}: couldn't open the file", .{filepath});
    };
    defer file.close();

    const magic_number = nostd.read_u64_from_file(file);
    std.debug.assert(magic_number == Compiler.magic_number_value);
    const globals_byte_count = nostd.read_u64_from_file(file);
    const instructions_byte_count = nostd.read_u64_from_file(file);
    var globals = ByteList.init(nostd.general_allocator);
    var instructions = ByteList.init(nostd.general_allocator);

    globals.resize(globals_byte_count) catch {
        nostd.exit(1);
    };
    instructions.resize(instructions_byte_count) catch {
        nostd.exit(1);
    };

    nostd.read_from_file(file, globals.items);
    nostd.read_from_file(file, instructions.items);

    return .{
        .instructions = instructions,
        .globals = globals,
    };
}

const oprint = nostd.oprint;
const eprint = nostd.eprint;
const exit = nostd.exit;
const report_fatal_error = nostd.report_fatal_error;

const std = @import("std");
const nostd = @import("nostd.zig");
const Compiler = @import("Compiler.zig");

pub const ByteList = std.ArrayList(u8);

// Size of return pointer + IP + rbp
pub const first_param_offset = 8 + 8 + 8;

pub const BigOperand = union(enum) {
    Tmp: packed struct {
        offset: Tmp.OffsetType,
        pad: Tmp.SizeMinusOneType = 0,
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
        pad: Tmp.SizeMinusOneType = 0,
        tag: Tmp.Tag,
    },
    Addr_M: packed struct {
        base: Tmp,
        offset: u64,
    },
    Imm: u64,
    Label: Label,

    pub fn grab_size(operand: BigOperand) u64 {
        return switch (operand) {
            .Tmp => |tmp| tmp.size,
            .Mem => |mem| mem.size,
            .Addr_T,
            .Addr_M,
            .Imm,
            .Label,
            => unreachable,
        };
    }

    pub fn grab_size_no_fail(operand: BigOperand) u64 {
        return switch (operand) {
            .Tmp => |tmp| tmp.size,
            .Mem => |mem| mem.size,
            .Addr_T,
            .Addr_M,
            .Imm,
            .Label,
            => 8,
        };
    }

    pub fn is_lvalue(operand: BigOperand) bool {
        return switch (operand) {
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

    pub fn bump(operand: BigOperand, offset: u64, size: u64) BigOperand {
        var r = operand;

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

    pub fn addr_of(operand: BigOperand) BigOperand {
        switch (operand) {
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

    pub fn mem_from_tmp(operand: BigOperand, offset: u64, size: u64) BigOperand {
        const tmp = &operand.Tmp;
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

    pub fn to_operand(operand: BigOperand) Operand {
        return switch (operand) {
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

pub const Tmp = packed struct {
    offset: OffsetType,
    size_minus_one: SizeMinusOneType,
    tag: Tag,

    pub const Tag = enum(u2) {
        Relative = 0,
        Global = 1,
        Absolute = 2,
    };

    pub const OffsetType = i59;
    pub const SizeMinusOneType = u3;

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

pub const Label = u64;

pub const Operand = union(Operand.Tag) {
    Tmp: Tmp,
    Mem_B: packed struct {
        base: Tmp,
        size_minus_one: Tmp.SizeMinusOneType,
    },
    Mem_BO: packed struct {
        base: Tmp,
        offset: u64,
        size_minus_one: Tmp.SizeMinusOneType,
    },
    Addr_T: packed struct {
        offset: Tmp.OffsetType,
        pad: Tmp.SizeMinusOneType = 0,
        tag: Tmp.Tag,
    },
    Addr_BO: packed struct {
        base: Tmp,
        offset: u64,
    },
    Imm: u64,
    Label: Label,

    pub const Tag = enum(u3) {
        Tmp = 0,
        Mem_B = 1,
        Mem_BO = 2,
        Addr_T = 3,
        Addr_BO = 4,
        Imm = 5,
        Label = 6,
    };

    pub fn format(operand: Operand, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (operand) {
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

    // TODO: should increment size by one?
    pub fn grab_size(operand: Operand) u8 {
        const size: u8 = switch (operand) {
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

pub const Instruction = struct {
    opcode: Opcode,
    operands: [maximum_operands_count]Operand,
    operands_count: u8,

    pub const maximum_operands_count = 3;

    pub fn format(instruction: Instruction, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        const pad_size = 12;
        const operand_name = instruction.opcode.to_string();

        try writer.print("{s} ", .{operand_name});
        {
            var i = pad_size - operand_name.len;
            while (i > 0) {
                i -= 1;
                try writer.print(" ", .{});
            }
        }
        for (instruction.operands[0..instruction.operands_count], 0..) |operand, i| {
            try writer.print("{}", .{operand});

            if (i + 1 < instruction.operands_count) {
                try writer.print(", ", .{});
            }
        }
    }
};

pub fn parse_instruction(data: []const u8) struct { Instruction, usize } {
    const stuff = struct {
        const State = struct {
            data: []const u32,
            opsinfo: u24,
            at: u8,

            pub fn init(d: []const u8) State {
                var new_data: []const u32 = undefined;
                new_data.ptr = @alignCast(@ptrCast(d.ptr));
                new_data.len = @divExact(d.len, 4);
                return .{
                    .data = new_data,
                    .opsinfo = 0,
                    .at = 0,
                };
            }

            pub fn parse_header(s: *State) Opcode {
                const header = s.data[s.at];
                s.at += 1;
                s.opsinfo = @intCast(header >> 8);
                return @enumFromInt(header & 0xFF);
            }

            pub fn parse_tmp(s: *State) Tmp {
                const tmp_as_u64 = parse_imm(s);
                return @as(*const Tmp, @ptrCast(&tmp_as_u64)).*;
            }

            pub fn parse_imm(s: *State) u64 {
                const imm = @as(u64, s.data[s.at]) | (@as(u64, s.data[s.at + 1]) << 32);
                s.at += 2;
                return imm;
            }

            pub fn parse_operand(s: *State) Operand {
                const optag: Operand.Tag = @enumFromInt(s.opsinfo & 0b111);
                s.opsinfo >>= 3;

                switch (optag) {
                    .Tmp => {
                        const tmp = parse_tmp(s);
                        return .{ .Tmp = tmp };
                    },
                    .Mem_B => {
                        const size_minus_one = parse_operand_size(s);
                        const tmp = parse_tmp(s);
                        return .{ .Mem_B = .{
                            .base = tmp,
                            .size_minus_one = size_minus_one,
                        } };
                    },
                    .Mem_BO => {
                        const size_minus_one = parse_operand_size(s);
                        const base = parse_tmp(s);
                        const offset = parse_imm(s);
                        return .{ .Mem_BO = .{
                            .base = base,
                            .offset = offset,
                            .size_minus_one = size_minus_one,
                        } };
                    },
                    .Addr_T => {
                        const tmp = parse_tmp(s);
                        return .{ .Addr_T = .{
                            .offset = tmp.offset,
                            .tag = tmp.tag,
                        } };
                    },
                    .Addr_BO => {
                        const base = parse_tmp(s);
                        const offset = parse_imm(s);
                        return .{ .Addr_BO = .{
                            .base = base,
                            .offset = offset,
                        } };
                    },
                    .Imm => {
                        const imm = parse_imm(s);
                        return .{ .Imm = imm };
                    },
                    .Label => {
                        const label = parse_imm(s);
                        return .{ .Label = label };
                    },
                }
            }

            fn parse_operand_size(s: *State) u3 {
                const size: u3 = @intCast(s.opsinfo & 0b111);
                s.opsinfo >>= 3;
                return size;
            }
        };
    };

    std.debug.assert(data.len >= 4);

    var state = stuff.State.init(data);
    var instruction: Instruction = instruction: {
        const opcode = state.parse_header();
        break :instruction .{
            .opcode = opcode,
            .operands = undefined,
            .operands_count = opcode.operand_count(),
        };
    };

    for (0..instruction.operands_count) |i| {
        instruction.operands[i] = state.parse_operand();
    }

    return .{ instruction, state.at * 4 };
}
