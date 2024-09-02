instrs: InstrList,

pub const IRC = @This();

pub const FIRST_PARAM_OFFSET = 16;

pub fn print(irc: *IRC) void {
    const IDENT = "    ";

    for (irc.instrs.items) |instr| {
        switch (instr) {
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
                    .Eqi => "eqi ",
                    .Neqi => "neqi",
                    .Lti => "lti ",
                    .Leqi => "leqi",
                    .Gti => "gti ",
                    .Geqi => "geqi",
                    .Addi => "addi",
                    .Subi => "subi",
                    .Muli => "muli",
                    .Divi => "divi",
                    .Modi => "modi",
                };
                common.oprint(IDENT ++ "{s}      {}, {}, {}", .{ op, Binary_Op.dst, Binary_Op.src0, Binary_Op.src1 });
            },
            .Unary_Op => |Unary_Op| {
                const op: []const u8 = switch (Unary_Op.tag) {
                    .Neg => "neg",
                    .Not => "not",
                };
                common.oprint(IDENT ++ "{s}      {}, {}", .{ op, Unary_Op.dst, Unary_Op.src });
            },
            .Label => |label| {
                common.oprint("l{}:", .{label});
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
                common.oprint(IDENT ++ "{s}      {}, {}, l{}", .{ op, Jmpc.src0, Jmpc.src1, Jmpc.label });
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
            .Call => |src| {
                common.oprint(IDENT ++ "call     {}", .{src});
            },
            .Ret0 => {
                common.oprint(IDENT ++ "ret", .{});
            },
            .Ret2 => |Ret| {
                common.oprint(IDENT ++ "ret      {}, {}", .{ Ret.dst, Ret.src });
            },
            .GFB => |GFB| {
                common.oprint("GFB l{}, {}B:", .{ GFB.label, GFB.stack_space_used });
            },
            .GFE => |GFE| {
                common.oprint("GFE l{}, {}B:", .{ GFE.label, GFE.stack_space_used });
            },
        }

        common.oprint("\n", .{});
    }
}

pub const Instr = union(enum) {
    Binary_Op: Instr.BinaryOp,
    Unary_Op: Instr.UnaryOp,
    Label: Label,
    Jmp: Label,
    Jmpc: Instr.Jmpc,
    Setnz: Instr.Setnz,
    Mov: Instr.Mov,
    Movsx: Instr.Movsx,
    Call: Rvalue,
    Ret0: void,
    Ret2: Instr.Ret2,
    GFB: Instr.GF,
    GFE: Instr.GF,

    pub const BinaryOp = struct {
        dst: Lvalue,
        src0: Rvalue,
        src1: Rvalue,
        tag: Tag,

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

            Eqi,
            Neqi,
            Lti,
            Leqi,
            Gti,
            Geqi,
            Addi,
            Subi,
            Muli,
            Divi,
            Modi,
        };
    };

    pub const UnaryOp = struct {
        dst: Lvalue,
        src: Rvalue,
        tag: Tag,

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

    pub const Ret2 = struct {
        dst: Lvalue,
        src: Rvalue,
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
                try writer.print("{}B %{}", .{ tmp.size, offset });
            },
            .Global => |offset| {
                try writer.print("{}B ${}", .{ tmp.size, offset });
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
};

pub const InstrList = std.ArrayList(Instr);

const std = @import("std");
const common = @import("common.zig");
