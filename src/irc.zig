instrs: InstrList,
label_count: u64,
globals_count: u64,

pub const IRC = @This();

// Size of return pointer + IP + rbp
pub const FIRST_PARAM_OFFSET = 8 + 8 + 8;

pub fn print(irc: *IRC) void {
    const IDENT = "    ";

    for (irc.instrs.items, 0..) |instr, ip| {
        common.oprint("{:>4}: ", .{ip});

        switch (instr) {
            .Print => |Print| {
                switch (Print) {
                    .Pointer => |src| {
                        common.oprint(IDENT ++ "printp   {}", .{src});
                    },
                    .Integer => |Integer| {
                        common.oprint(IDENT ++ "print{c}   {}", .{ @as(u8, if (Integer.is_signed) 'i' else 'u'), Integer.src });
                    },
                    .Boolean => |src| {
                        common.oprint(IDENT ++ "printb   {}", .{src});
                    },
                }
            },
            .Binary_Op => |Binary_Op| {
                const op: []const u8 = switch (Binary_Op.tag) {
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
                common.oprint(IDENT ++ "{c}{s}     {}, {}, {}", .{ @as(u8, if (Binary_Op.is_signed) 'i' else 'u'), op, Binary_Op.dst, Binary_Op.src0, Binary_Op.src1 });
            },
            .Unary_Op => |Unary_Op| {
                const op: []const u8 = switch (Unary_Op.tag) {
                    .Neg => "neg",
                    .Not => "not",
                };
                common.oprint(IDENT ++ "{c}{s}     {}, {}", .{ @as(u8, if (Unary_Op.is_signed) 'i' else 'u'), op, Unary_Op.dst, Unary_Op.src });
            },
            .Jmp => |label| {
                common.oprint(IDENT ++ "jmp      l{}", .{label});
            },
            .Jmpc => |Jmpc| {
                const op = switch (Jmpc.tag) {
                    .Eq => "je ",
                    .Neq => "jne",
                    .Lt => "jl ",
                    .Leq => "jle",
                    .Gt => "jg ",
                    .Geq => "jge",
                };
                common.oprint(IDENT ++ "{c}{s}     {}, {}, l{}", .{ @as(u8, if (Jmpc.is_signed) 'i' else 'u'), op, Jmpc.src0, Jmpc.src1, Jmpc.label });
            },
            .Setnz => |Setnz| {
                common.oprint(IDENT ++ "setnz    {}, {}", .{ Setnz.dst, Setnz.src });
            },
            .Mov => |Mov| {
                common.oprint(IDENT ++ "mov      {}, {}", .{ Mov.dst, Mov.src });
            },
            .Movsx => |Mov| {
                common.oprint(IDENT ++ "movsx    {}, {}, {}", .{ Mov.dst, Mov.src, Mov.src_bits });
            },
            .Call => |Call| {
                common.oprint(IDENT ++ "call     {}, {}", .{ Call.dst, Call.src });
            },
            .Push => |src| {
                common.oprint(IDENT ++ "push     {}", .{src});
            },
            .Pop => |bytes| {
                common.oprint(IDENT ++ "pop      {}", .{bytes});
            },
            .Ret0 => |label| {
                common.oprint(IDENT ++ "ret l{}", .{label});
            },
            .Ret1 => |Ret1| {
                common.oprint(IDENT ++ "ret      {}, {}, l{}", .{ Ret1.dst, Ret1.src, Ret1.label });
            },
            .GFB => |GFB| {
                common.oprint("GFB {}B, l{}:", .{ GFB.stack_space_used, GFB.label });
            },
            .GFE => |GFE| {
                common.oprint("GFE {}B, l{}:", .{ GFE.stack_space_used, GFE.label });
            },
            .Label => |label| {
                common.oprint("l{}:", .{label});
            },
            .Exit => {
                common.oprint(IDENT ++ "exit", .{});
            },
        }

        common.oprint("\n", .{});
    }
}

pub const Instr = union(enum) {
    Print: Instr.Print,
    Binary_Op: Instr.BinaryOp,
    Unary_Op: Instr.UnaryOp,
    Jmp: Label,
    Jmpc: Instr.Jmpc,
    Setnz: Instr.Setnz,
    Mov: Instr.Mov,
    Movsx: Instr.Movsx,
    Call: Instr.Call,
    Push: Rvalue,
    Pop: u64,
    Ret0: Label,
    Ret1: Instr.Ret1,
    GFB: Instr.GF,
    GFE: Instr.GF,
    Label: Label,
    Exit: void,

    pub const Print = union(enum) {
        Pointer: Rvalue,
        Integer: struct {
            src: Rvalue,
            is_signed: bool,
        },
        Boolean: Rvalue,
    };

    pub const BinaryOp = struct {
        dst: Lvalue,
        src0: Rvalue,
        src1: Rvalue,
        tag: Tag,
        is_signed: bool,

        pub const Tag = enum {
            Eq,
            Neq,
            Lt,
            Leq,
            Gt,
            Geq,
            Add,
            Sub,
            Mul,
            Div,
            Mod,
        };
    };

    pub const UnaryOp = struct {
        dst: Lvalue,
        src: Rvalue,
        tag: Tag,
        is_signed: bool,

        pub const Tag = enum {
            Neg,
            Not,
        };
    };

    pub const Jmpc = struct {
        src0: Rvalue,
        src1: Rvalue,
        label: Label,
        tag: Tag,
        is_signed: bool,

        pub const Tag = enum {
            Eq,
            Neq,
            Lt,
            Leq,
            Gt,
            Geq,
        };
    };

    pub const Setnz = struct {
        dst: Lvalue,
        src: Rvalue,
    };

    pub const Mov = struct {
        dst: Lvalue,
        src: Rvalue,
    };

    pub const Movsx = struct {
        dst: Lvalue,
        src: Rvalue,
        src_bits: u8,
    };

    pub const Call = struct {
        dst: Lvalue,
        src: Rvalue,
    };

    pub const Ret1 = struct {
        dst: Lvalue,
        src: Rvalue,
        label: Label,
    };

    pub const GF = struct {
        label: Label,
        stack_space_used: u64,
    };
};

pub const Label = u64;

pub const Offset = union(enum) {
    Local: i64,
    Global: u64,
};

pub const Tmp = struct {
    offset: Offset,
    size: u64,

    pub fn format(tmp: Tmp, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (tmp.offset) {
            .Local => |offset| {
                try writer.print("%({}B,{})", .{ tmp.size, offset });
            },
            .Global => |offset| {
                try writer.print("$({}B,{})", .{ tmp.size, offset });
            },
        }
    }

    pub fn as_lvalue(tmp: Tmp) Lvalue {
        return .{ .Tmp = tmp };
    }

    pub fn as_rvalue(tmp: Tmp) Rvalue {
        return .{ .Lvalue = .{ .Tmp = tmp } };
    }
};

pub const Mem = struct {
    base: ?Tmp,
    offset: u64,
    size: u64,
};

pub const Lvalue = union(enum) {
    Tmp: Tmp,
    Mem: Mem,

    pub fn format(lvalue: Lvalue, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (lvalue) {
            .Tmp => |tmp| {
                try writer.print("{}", .{tmp});
            },
            .Mem => |mem| {
                try writer.print("{}B [", .{mem.size});
                if (mem.base) |base| {
                    try writer.print("{}", .{base});
                }
                if (mem.offset != 0) {
                    try writer.print("+{}", .{mem.offset});
                }
                try writer.print("]", .{});
            },
        }
    }

    pub fn as_rvalue(lvalue: Lvalue) Rvalue {
        return .{ .Lvalue = lvalue };
    }

    pub fn grab_size(lvalue: Lvalue) u64 {
        return switch (lvalue) {
            .Tmp => |tmp| tmp.size,
            .Mem => |mem| mem.size,
        };
    }
};

pub const Rvalue = union(enum) {
    Lvalue: Lvalue,
    Addr: Lvalue,
    Label: Label,
    Imm: u64,

    pub fn format(rvalue: Rvalue, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (rvalue) {
            .Lvalue => |lvalue| {
                try writer.print("{}", .{lvalue});
            },
            .Addr => |lvalue| {
                try writer.print("addr {}", .{lvalue});
            },
            .Label => |label| {
                try writer.print("l{}", .{label});
            },
            .Imm => |imm| {
                try writer.print("{}", .{imm});
            },
        }
    }

    pub fn grab_size(rvalue: Rvalue) u64 {
        return switch (rvalue) {
            .Lvalue => |lvalue| lvalue.grab_size(),
            .Addr, .Label, .Imm => 8,
        };
    }
};

pub const InstrList = std.ArrayList(Instr);

const std = @import("std");
const common = @import("common.zig");
