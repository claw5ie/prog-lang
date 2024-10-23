typechecker: Typechecker,
irgen: IRGenerator,
ir: IR,
interp: Interpreter,

symbol_table: SymbolTable,
string_pool: StringPool,
filepath: [:0]const u8,
source_code: [:0]u8,
had_error: bool,

const Compiler = @This();

const std = @import("std");
const utils = @import("utils.zig");
const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");
const Ast = @import("ast.zig");

const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const Alignment = utils.Alignment;
pub const ByteList = std.ArrayList(u8);
pub const oprint = utils.oprint;
pub const eprint = utils.eprint;
pub const exit = utils.exit;

const magic_number_string: []const u8 = "PROGLANG";
const magic_number_value: u64 = magic_number_value: {
    const ptr: *const u64 = @alignCast(@ptrCast(magic_number_string.ptr));
    break :magic_number_value ptr.*;
};

pub var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
pub var gpa = general_purpose_allocator.allocator();

const Options = struct {
    has_input_filepath: ?[:0]const u8 = null,
    has_output_filepath: ?[:0]const u8 = null,
    mode: Mode = .Build,

    const Mode = enum {
        Build,
        Run,
        Print,
    };
};

pub const LineInfo = struct {
    line: u32,
    column: u32,
    offset: usize,
};

pub const Attributes = packed struct {
    is_const: bool = false,
    is_static: bool = false,
    is_global: bool = false,

    pub fn is_empty(attr: Attributes) bool {
        const ptr: *const u8 = @ptrCast(&attr);
        return ptr.* == 0;
    }

    pub fn combine(self: *Attributes, other: Attributes) void {
        const d: *u8 = @ptrCast(self);
        const s: *const u8 = @ptrCast(&other);
        d.* |= s.*;
    }
};

pub const Typechecker = struct {
    enum_type: ?*Ast.Type,
    return_type: ?*Ast.Type,
    is_in_loop: bool,
};

pub const IR = struct {
    instrs: ByteList,
    globals: ByteList,

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
                try writer.print("{c}({}, {})", .{
                    @as(u8, switch (tmp.tag) {
                        .Relative => 'r',
                        .Global => 'g',
                        .Absolute => 'a',
                    }),
                    @as(u64, tmp.size_minus_one) + 1,
                    tmp.offset,
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
                        try writer.print("{} [{}]", .{ @as(u64, mem.size_minus_one) + 1, mem.base });
                    },
                    .Mem_BO => |mem| {
                        try writer.print("{} [{} + {}]", .{ @as(u64, mem.size_minus_one) + 1, mem.base, mem.offset });
                    },
                    .Addr_T => |tmp| {
                        try writer.print(
                            "addr [{c}{}]",
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
};

pub const IRGenerator = struct {
    labels: LabelMap,
    next_local: u64,
    biggest_next_local: u64,
    next_global: u64,
    next_label: IR.Encoded.Label,
    loop_condition_label: ?IR.Encoded.Label,
    loop_end_label: ?IR.Encoded.Label,
    return_label: ?IR.Encoded.Label,

    pub const LabelMap = std.ArrayList(u64);
};

pub const Interpreter = struct {
    stack: []u8,
    rsp: u64,
    rbp: u64,
    vtable: Vtable,

    pub const STACK_SIZE = 4 * 1024 * 1024;

    pub const UNUSED_SPACE_SIZE = 1024;

    pub const Vtable = struct {
        unused_space: u64 = 0,
        start_instr: u64 = UNUSED_SPACE_SIZE,
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

    pub const LabelList = std.ArrayList(u64);
};

pub const SymbolTable = struct {
    map: HashMap,

    pub inline fn init(allocator: Allocator) SymbolTable {
        return .{ .map = HashMap.init(allocator) };
    }

    pub fn deinit(table: *SymbolTable) void {
        table.map.deinit();
    }

    pub fn insert(table: *SymbolTable, key: Key) InsertResult {
        return table.map.getOrPut(key) catch {
            exit(1);
        };
    }

    pub inline fn find(table: *SymbolTable, key: Key) ?Value {
        return table.map.get(key);
    }

    pub const Key = Ast.Symbol.Key;
    pub const Value = *Ast.Symbol;

    pub const InsertResult = HashMap.GetOrPutResult;

    pub const Context = struct {
        pub fn hash(_: Context, key: Key) u64 {
            const MurMur = std.hash.Murmur2_64;

            const h0 = MurMur.hash(key.name);
            const h1 = MurMur.hashUint64(@intFromPtr(key.scope));

            return h0 +% 33 *% h1;
        }

        pub fn eql(_: Context, k0: Key, k1: Key) bool {
            return k0.scope == k1.scope and std.mem.eql(u8, k0.name, k1.name);
        }
    };

    pub const HashMap = std.HashMap(Key, Value, Context, 80);
};

pub const StringPool = struct {
    map: HashMap,

    pub fn init(allocator: Allocator) StringPool {
        return .{ .map = HashMap.init(allocator) };
    }

    pub fn deinit(pool: *StringPool) void {
        var it = pool.map.valueIterator();
        while (it.next()) |value_ptr| {
            pool.map.allocator.free(value_ptr.*);
        }
        pool.map.deinit();
    }

    pub fn insert(pool: *StringPool, string: []const u8) []const u8 {
        const insert_result = pool.map.getOrPut(string) catch {
            exit(1);
        };

        if (!insert_result.found_existing) {
            const value = pool.map.allocator.alloc(u8, string.len) catch {
                exit(1);
            };
            @memcpy(value, string);
            insert_result.key_ptr.* = value;
            insert_result.value_ptr.* = value;
        }

        return insert_result.value_ptr.*;
    }

    pub const Key = []const u8;
    pub const Value = []u8;

    pub const Context = struct {
        pub fn hash(_: Context, key: Key) u64 {
            const MurMur = std.hash.Murmur2_64;
            return MurMur.hash(key);
        }

        pub fn eql(_: Context, k0: Key, k1: Key) bool {
            return std.mem.eql(u8, k0, k1);
        }
    };

    pub const HashMap = std.HashMap(Key, Value, Context, 80);
};

pub const Stage = enum {
    None,
    Going,
    Done,
};

pub fn init_vtable(c: *Compiler) void {
    const start_global = c.interp.vtable.start_instr + utils.align_by_pow2(c.ir.instrs.items.len, 1024);
    const start_stack = start_global + utils.align_by_pow2(c.ir.globals.items.len, 1024);
    const end = start_stack + Compiler.Interpreter.STACK_SIZE;

    c.interp.vtable = .{
        .start_global = start_global,
        .start_stack = start_stack,
        .end = end,
    };
}

fn init() Compiler {
    const stack = gpa.alloc(u8, 2 * 1024 * 1024) catch {
        exit(1);
    };

    var labels = IRGenerator.LabelMap.init(gpa);
    labels.resize(256) catch {
        Compiler.exit(1);
    };

    return .{
        .typechecker = .{
            .enum_type = null,
            .return_type = null,
            .is_in_loop = false,
        },
        .ir = .{
            .instrs = ByteList.initCapacity(gpa, 4 * 1024) catch {
                Compiler.exit(1);
            },
            .globals = ByteList.initCapacity(gpa, 4 * 1024) catch {
                Compiler.exit(1);
            },
        },
        .irgen = .{
            .labels = labels,
            .next_local = 0,
            .biggest_next_local = 0,
            .next_global = 0,
            .next_label = 0,
            .loop_condition_label = null,
            .loop_end_label = null,
            .return_label = null,
        },
        .interp = .{
            .stack = stack,
            .rsp = 0,
            .rbp = 0,
            .vtable = .{
                .start_global = Interpreter.UNUSED_SPACE_SIZE,
                .start_stack = Interpreter.UNUSED_SPACE_SIZE,
                .end = Interpreter.UNUSED_SPACE_SIZE,
            },
        },
        .symbol_table = SymbolTable.init(gpa),
        .string_pool = StringPool.init(gpa),
        .filepath = undefined,
        .source_code = undefined,
        .had_error = false,
    };
}

fn deinit(c: *Compiler) void {
    c.ir.instrs.deinit();
    c.ir.globals.deinit();
    gpa.free(c.interp.stack);
    c.irgen.labels.deinit();
    c.symbol_table.deinit();
    c.string_pool.deinit();
    gpa.free(c.source_code);
}

pub fn compile() void {
    var c = Compiler.init();

    const options = parse_cmd_options();
    switch (options.mode) {
        .Build => {
            var buffer = [1]u8{0} ** std.posix.PATH_MAX;

            const input_filepath = options.has_input_filepath.?;
            const output_filepath: [:0]const u8 = output_filepath: {
                if (options.has_output_filepath) |path| {
                    @memcpy(buffer[0..path.len], path);
                    break :output_filepath buffer[0..path.len :0];
                } else {
                    const extension: []const u8 = ".ir";
                    @memcpy(buffer[0..input_filepath.len], input_filepath);
                    @memcpy(buffer[input_filepath.len .. input_filepath.len + extension.len], extension);
                    break :output_filepath buffer[0 .. input_filepath.len + extension.len :0];
                }
            };

            c.filepath = input_filepath;
            c.source_code = utils.read_entire_file(Compiler.gpa, input_filepath) catch {
                Compiler.eprint("error: failed to read from a file '{s}'\n", .{input_filepath});
                Compiler.exit(1);
            };

            var ast = Ast.init(&c);
            var parser = Parser.init(&c, &ast);

            _ = output_filepath;

            parser.parse();
            // typecheck(&c);
            // generate_ir(&c);
            // write_ir(&c, output_filepath);

            parser.deinit();
            ast.deinit();
        },
        .Run => {
            // TODO: ABOLISH THIS! BURN IT WITH FIRE!
            // c.ir.instrs.deinit();
            // c.ir.globals.deinit();

            // read_ir(&c, options.has_input_filepath.?);
            // interpret(&c);
        },
        .Print => {
            // TODO: ABOLISH THIS AS WELL!!
            // c.ir.instrs.deinit();
            // c.ir.globals.deinit();

            // read_ir(&c, options.has_input_filepath.?);
            // c.ir.print();
        },
    }

    c.deinit();
}

fn write_ir(c: *Compiler, filepath: [:0]const u8) void {
    const fd = std.posix.open(
        filepath,
        .{
            .ACCMODE = .WRONLY,
            .CREAT = true,
            .TRUNC = true,
        },
        0o644,
    ) catch {
        eprint("error: couldn't open a file '{s}'\n", .{filepath});
        exit(1);
    };
    defer std.posix.close(fd);

    utils.write_to_file_v(fd, magic_number_string);
    utils.write_to_file_u64(fd, c.ir.globals.items.len);
    utils.write_to_file_u64(fd, c.ir.instrs.items.len);
    utils.write_to_file_v(fd, c.ir.globals.items);
    utils.write_to_file_v(fd, c.ir.instrs.items);
}

fn read_ir(c: *Compiler, filepath: [:0]const u8) void {
    const fd = std.posix.open(filepath, .{}, 0) catch {
        eprint("error: couldn't open a file '{s}'\n", .{filepath});
        exit(1);
    };
    defer std.posix.close(fd);

    const magic_number = utils.read_from_file_u64(fd);
    std.debug.assert(magic_number == magic_number_value);
    const globals_byte_count = utils.read_from_file_u64(fd);
    const instrs_byte_count = utils.read_from_file_u64(fd);
    var globals = ByteList.init(gpa);
    var instrs = ByteList.init(gpa);

    globals.resize(globals_byte_count) catch {
        Compiler.exit(1);
    };
    instrs.resize(instrs_byte_count) catch {
        Compiler.exit(1);
    };

    utils.read_from_file_v(fd, globals.items);
    utils.read_from_file_v(fd, instrs.items);

    c.ir = .{
        .instrs = instrs,
        .globals = globals,
    };

    init_vtable(c);
}

fn parse_cmd_options() Options {
    var options = Options{};
    var activated_modes_count: u32 = 0;
    var had_error = false;
    var args = std.process.args();

    _ = args.next().?;
    while (args.next()) |arg| {
        if (arg[0] == '-') {
            if (arg.len > 2) {
                had_error = true;
                eprint("error: unrecognized option '{s}'\n", .{arg});
            } else {
                switch (arg[1]) {
                    'r' => {
                        activated_modes_count += 1;
                        options.mode = .Run;
                    },
                    'p' => {
                        activated_modes_count += 1;
                        options.mode = .Print;
                    },
                    'o' => {
                        if (options.has_output_filepath) |path| {
                            had_error = true;
                            eprint("error: output filepath was already provided ('{s}')\n", .{path});
                        } else if (args.next()) |path| {
                            options.has_output_filepath = path;
                        } else {
                            had_error = true;
                            eprint("error: expected argument for '-o' option\n", .{});
                        }
                    },
                    else => {
                        had_error = true;
                        eprint("error: unrecognized option '{s}'\n", .{arg});
                    },
                }
            }
        } else {
            if (options.has_input_filepath) |filepath| {
                had_error = true;
                eprint("error: filepath was already given ('{s}')\n", .{filepath});
            } else {
                options.has_input_filepath = arg;
            }
        }
    }

    if (activated_modes_count > 1) {
        had_error = true;
        eprint("error: only one options needs to be enabled\n", .{});
    }

    if (options.has_input_filepath == null) {
        had_error = true;
        eprint("error: no file supplied\n", .{});
    }

    if (had_error) {
        exit(1);
    }

    return options;
}

pub fn find_symbol_in_scope(c: *Compiler, key: Ast.Symbol.Key, offset: usize) ?*Ast.Symbol {
    const has_symbol = c.symbol_table.find(key);

    if (has_symbol) |symbol| {
        switch (symbol.as) {
            .Variable, .Parameter => {
                if (symbol.attributes.is_const or symbol.attributes.is_static or symbol.line_info.offset < offset) {
                    return symbol;
                }
            },
            .Procedure,
            .Struct_Field,
            .Union_Field,
            .Enum_Field,
            .Type,
            => return symbol,
        }
    }

    return null;
}

pub fn find_symbol(c: *Compiler, key: Ast.Symbol.Key, offset: usize) ?*Ast.Symbol {
    var k = key;
    while (true) {
        const has_symbol = find_symbol_in_scope(c, k, offset);
        if (has_symbol) |symbol| {
            return symbol;
        } else if (k.scope.parent) |parent| {
            k.scope = parent;
        } else {
            break;
        }
    }
    return null;
}

pub fn report_error(c: *Compiler, line_info: LineInfo, comptime format: []const u8, args: anytype) void {
    c.had_error = true;
    eprint("{s}:{}:{}: error: " ++ format ++ "\n", .{ c.filepath, line_info.line, line_info.column } ++ args);
}

pub fn report_note(c: *Compiler, line_info: LineInfo, comptime format: []const u8, args: anytype) void {
    eprint("{s}:{}:{}: note: " ++ format ++ "\n", .{ c.filepath, line_info.line, line_info.column } ++ args);
}
