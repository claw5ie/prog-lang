const std = @import("std");
const Ast = @import("ast.zig");

ir_instrs: InstrList,
next_global: Offset = 0,
next_local: Offset = 0,
next_label: Label = 0,
biggest_local_so_far: Offset = 0,

pub const This = @This();

pub const InstrList = std.ArrayList(Instr);

pub const CHOOSE_BASE: u3 = 0x1;
pub const CHOOSE_DISP: u3 = 0x2;

pub const Offset = i31;

pub const TmpTag = enum {
    Global,
    Local,
};

pub const Tmp = packed struct {
    offset: Offset,
    tag: TmpTag,
    height: Ast.FunctionDepth,

    pub inline fn as_lvalue(tmp: Tmp) Lvalue {
        return .{ .Tmp = tmp };
    }

    pub inline fn as_rvalue(tmp: Tmp) Rvalue {
        return .{ .Lvalue = .{ .Tmp = tmp } };
    }
};

pub const LvalueTag = enum {
    Tmp,
    Mem,
};

pub const Lvalue = union(LvalueTag) {
    Tmp: Tmp,
    Mem: Mem,

    pub inline fn as_rvalue(lvalue: Lvalue) Rvalue {
        return .{ .Lvalue = lvalue };
    }
};

pub const RvalueTag = enum {
    Lvalue,
    Addr,
    Label,
    Imm,
};

pub const Rvalue = union(RvalueTag) {
    Lvalue: Lvalue,
    Addr: Lvalue,
    Label: Label,
    Imm: i64,

    // pub fn ref(src_tmp: Rvalue, ir: *This) Lvalue {
    //     switch (src_tmp) {
    //         .Lvalue => |lvalue| {
    //             switch (lvalue) {
    //                 .Tmp => |tmp| {
    //                     return .{ .Mem = .{
    //                         .choose = CHOOSE_BASE,
    //                         .base = tmp,
    //                     } };
    //                 },
    //                 .Mem => {
    //                     var dst_tmp = ir.grab_local();

    //                     generate_ir_instr(ir, .{ .Instr = .{
    //                         .Mov = .{
    //                             .dst = dst_tmp.as_lvalue(),
    //                             .src = src_tmp,
    //                         },
    //                     } });

    //                     return .{ .Mem = .{
    //                         .choose = CHOOSE_BASE,
    //                         .base = dst_tmp,
    //                     } };
    //                 },
    //             }
    //         },
    //         .Addr => |dst_tmp| {
    //             return dst_tmp;
    //         },
    //         .Label => |label| {
    //             return .{ .Mem = .{
    //                 .choose = CHOOSE_DISP,
    //                 .disp = label,
    //             } };
    //         },
    //         .Imm => |imm| {
    //             return .{ .Mem = .{
    //                 .choose = CHOOSE_DISP,
    //                 .disp = imm,
    //             } };
    //         },
    //     }
    // }

    // pub fn deref(src_tmp: Rvalue, c: *Compiler) Rvalue {
    //     return .{ .Lvalue = src_tmp.ref(c) };
    // }
};

pub const Mem = struct {
    choose: u2 = 0,
    base: Tmp = .{
        .offset = 0,
        .tag = .Global,
        .height = 0,
    },
    disp: i64 = 0,
};

pub const Label = u32;

pub const InstrJmpcTag = enum {
    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,
};

pub const InstrBinaryOpTag = enum {
    Or,
    And,
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

pub const InstrUnaryOpTag = enum {
    Not,
    Neg,
};

pub const InstrOpcode = enum {
    Print,
    Mov,
    Jmp,
    Push_Frame_Pointer,
    Push,
    Pop,
    Call,
    Ret,
    Exit,
};

pub const InstrMetaTag = enum {
    GFB,
    GFE,
    Label,
};

pub const InstrType = enum {
    Binary_Op,
    Unary_Op,
    Jmpc,
    Instr,
    Meta,
};

pub const Instr = union(InstrType) {
    Binary_Op: struct {
        tag: InstrBinaryOpTag,
        dst: Lvalue,
        src0: Rvalue,
        src1: Rvalue,
    },
    Unary_Op: struct {
        tag: InstrUnaryOpTag,
        dst: Lvalue,
        src: Rvalue,
    },
    Jmpc: struct {
        tag: InstrJmpcTag,
        src0: Rvalue,
        src1: Rvalue,
        label: Label,
    },
    Instr: union(InstrOpcode) {
        Print: Rvalue,
        Mov: struct {
            dst: Lvalue,
            src: Rvalue,
        },
        Jmp: Label,
        Push_Frame_Pointer: i32,
        Push: Rvalue,
        Pop: u64,
        Call: Rvalue,
        Ret: Label,
        Exit: void,
    },
    Meta: union(InstrMetaTag) {
        GFB: struct {
            stack_space_used: u32,
            label: Label,
        },
        GFE: struct {
            stack_space_used: u32,
            label: Label,
        },
        Label: Label,
    },
};

pub const StmtContext = struct {
    loop_cond_label: Label,
    loop_end_label: Label,
    function_end_label: Label,
    return_address_offset: Offset,
};
