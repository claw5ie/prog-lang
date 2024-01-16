const std = @import("std");
const common = @import("common.zig");
const Ast = @import("ast.zig");

instrs: InstrList,
next_global: Offset = 0,
next_local: Offset = 0,
next_label: Label = 0,
biggest_local_so_far: Offset = 0,
function_depth: Ast.FunctionDepth = 0,
global_scope: *Ast.Scope,

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

    pub fn ref(src_tmp: Rvalue, ir: *This) Lvalue {
        switch (src_tmp) {
            .Lvalue => |lvalue| {
                switch (lvalue) {
                    .Tmp => |tmp| {
                        return .{ .Mem = .{
                            .choose = CHOOSE_BASE,
                            .base = tmp,
                        } };
                    },
                    .Mem => {
                        var dst_tmp = ir.grab_local();

                        generate_instr(ir, .{ .Instr = .{
                            .Mov = .{
                                .dst = dst_tmp.as_lvalue(),
                                .src = src_tmp,
                            },
                        } });

                        return .{ .Mem = .{
                            .choose = CHOOSE_BASE,
                            .base = dst_tmp,
                        } };
                    },
                }
            },
            .Addr => |dst_tmp| {
                return dst_tmp;
            },
            .Label => |label| {
                return .{ .Mem = .{
                    .choose = CHOOSE_DISP,
                    .disp = label,
                } };
            },
            .Imm => |imm| {
                return .{ .Mem = .{
                    .choose = CHOOSE_DISP,
                    .disp = imm,
                } };
            },
        }
    }

    pub fn deref(src_tmp: Rvalue, ir: *This) Rvalue {
        return .{ .Lvalue = src_tmp.ref(ir) };
    }
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

pub fn grab_global(ir: *This) Tmp {
    var offset = ir.next_global;
    ir.next_global += 8;
    return .{
        .offset = offset,
        .tag = .Global,
        .height = 0,
    };
}

pub fn grab_local(ir: *This) Tmp {
    var offset = ir.next_local;
    ir.next_local += 8;

    if (ir.biggest_local_so_far < ir.next_local) {
        ir.biggest_local_so_far = ir.next_local;
    }

    return .{
        .offset = offset,
        .tag = .Local,
        .height = 0,
    };
}

pub fn grab_label(ir: *This) Label {
    var result = ir.next_label;
    ir.next_label += 1;
    return result;
}

fn generate_instr(ir: *This, instr: Instr) void {
    ir.instrs.append(instr) catch {
        std.os.exit(1);
    };
}

pub const GF_FIRST_ARG_OFFSET = GF_RETURN_ADDRESS_OFFSET - 8;
pub const GF_RETURN_ADDRESS_OFFSET = -16 - 8;
pub const LF_FIRST_ARG_OFFSET = LF_RETURN_ADDRESS_OFFSET - 8;
pub const LF_RETURN_ADDRESS_OFFSET = -16 - 16;
pub const LF_RBP_OFFSET = LF_RETURN_ADDRESS_OFFSET + 8;

pub fn generate(ast: *Ast) This {
    var ircode = This{
        .instrs = InstrList.init(common.gpa),
        .next_label = ast.next_label,
        .global_scope = ast.global_scope,
    };

    generate_instr(&ircode, .{ .Instr = .{
        .Call = .{
            .Label = ast.main.payload.Function.label,
        },
    } });
    generate_instr(&ircode, .{ .Instr = .Exit });

    var it = ast.globals.iterator();
    while (it.next()) |symbol| {
        generate_global_symbol(&ircode, symbol.*);
    }

    it = ast.locals.iterator();
    while (it.next()) |symbol| {
        generate_function(&ircode, &symbol.*.payload.Function, LF_FIRST_ARG_OFFSET);
    }

    return ircode;
}

fn generate_global_symbol(ir: *This, symbol: *Ast.Symbol) void {
    switch (symbol.payload) {
        .Variable => |*variable| {
            // Impelement globals as lvalue [imm]?
            variable.tmp = ir.grab_global();

            if (variable.expr) |expr| {
                generate_rvalue(ir, expr, variable.tmp.as_lvalue());
            }
        },
        .Function => |function| {
            generate_function(ir, &function, GF_FIRST_ARG_OFFSET);
        },
        .Type => {},
        .Parameter,
        .Definition,
        .Struct_Field,
        .Enum_Field,
        => unreachable,
    }
}

fn generate_local_symbol(ir: *This, symbol: *Ast.Symbol) void {
    switch (symbol.payload) {
        .Variable => |*variable| {
            variable.tmp = ir.grab_local();

            if (variable.expr) |expr| {
                generate_rvalue(ir, expr, variable.tmp.as_lvalue());
            }
        },
        .Function => {
            // Generate local functions separately.
        },
        .Type => {},
        .Parameter,
        .Definition,
        .Struct_Field,
        .Enum_Field,
        => unreachable,
    }
}

fn generate_function(ir: *This, function: *const Ast.SymbolFunction, first_arg_offset: Offset) void {
    {
        var param_tmp = Tmp{
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
        var old_depth = ir.function_depth;
        ir.function_depth = function.depth;

        var function_end_label = ir.grab_label();
        var index = ir.instrs.items.len;
        generate_instr(ir, undefined);

        {
            var ctx = StmtContext{
                .loop_cond_label = undefined,
                .loop_end_label = undefined,
                .function_end_label = function_end_label,
                .return_address_offset = first_arg_offset + 8, // NOTE[0]: Assume that return address is right after first argument.
            };
            var it = function.block.iterator();
            while (it.next()) |stmt| {
                generate_stmt(ir, &ctx, stmt);
            }
        }

        {
            var stack_space_used: u32 = @intCast(ir.biggest_local_so_far);
            ir.instrs.items[index] = .{ .Meta = .{
                .GFB = .{
                    .stack_space_used = stack_space_used,
                    .label = function.label,
                },
            } };

            generate_instr(ir, .{ .Meta = .{
                .GFE = .{
                    .stack_space_used = stack_space_used,
                    .label = function_end_label,
                },
            } });
        }

        ir.next_local = 0;
        ir.biggest_local_so_far = 0;
        ir.function_depth = old_depth;
    }
}

fn generate_stmt(ir: *This, ctx: *const StmtContext, stmt: *Ast.Stmt) void {
    var old_next_local = ir.next_local;
    var should_clean_locals = true;

    switch (stmt.payload) {
        .Print => |expr| {
            var src_tmp = ir.grab_local();
            generate_rvalue(ir, expr, src_tmp.as_lvalue());
            generate_instr(ir, .{ .Instr = .{
                .Print = src_tmp.as_rvalue(),
            } });
        },
        .Block => |block| {
            var it = block.iterator();
            while (it.next()) |substmt| {
                generate_stmt(ir, ctx, substmt);
            }
        },
        .If => |_if| {
            var false_label = ir.grab_label();
            var end_label = ir.grab_label();

            generate_jump(ir, _if.cond, false, false_label);
            generate_stmt(ir, ctx, _if.if_true);
            generate_instr(ir, .{ .Instr = .{
                .Jmp = end_label,
            } });
            generate_instr(ir, .{ .Meta = .{
                .Label = false_label,
            } });
            if (_if.if_false) |if_false| generate_stmt(ir, ctx, if_false);
            generate_instr(ir, .{ .Meta = .{
                .Label = end_label,
            } });
        },
        .While => |_while| {
            var block_label = ir.grab_label();
            var cond_label = ir.grab_label();
            var end_label = ir.grab_label();

            if (!_while.is_do_while) {
                generate_instr(ir, .{ .Instr = .{
                    .Jmp = cond_label,
                } });
            }

            generate_instr(ir, .{ .Meta = .{
                .Label = block_label,
            } });

            var new_ctx = ctx.*;
            new_ctx.loop_cond_label = cond_label;
            new_ctx.loop_end_label = end_label;

            generate_stmt(ir, &new_ctx, _while.block);
            generate_instr(ir, .{ .Meta = .{
                .Label = cond_label,
            } });
            generate_jump(ir, _while.cond, true, block_label);
        },
        .Break => {
            generate_instr(ir, .{ .Instr = .{
                .Jmp = ctx.loop_end_label,
            } });
        },
        .Continue => {
            generate_instr(ir, .{ .Instr = .{
                .Jmp = ctx.loop_cond_label,
            } });
        },
        .Switch => |_switch| {
            var cond_tmp = ir.grab_local();
            generate_rvalue(ir, _switch.cond, cond_tmp.as_lvalue());
            var end_label = ir.grab_label();

            var it = _switch.cases.iterator();
            while (it.next()) |top_level_case| {
                var case_beg_label = ir.grab_label();
                var case_end_label = ir.grab_label();

                var substmt = top_level_case;
                while (true) {
                    var case = &substmt.payload.Case;
                    var case_tmp = ir.grab_local();
                    generate_rvalue(ir, case.expr, case_tmp.as_lvalue());

                    generate_instr(ir, .{ .Jmpc = .{
                        .tag = .Eq,
                        .src0 = cond_tmp.as_rvalue(),
                        .src1 = case_tmp.as_rvalue(),
                        .label = case_beg_label,
                    } });

                    substmt = case.stmt;
                    if (substmt.payload != .Case) break;
                }
                generate_instr(ir, .{ .Instr = .{
                    .Jmp = case_end_label,
                } });
                generate_instr(ir, .{ .Meta = .{
                    .Label = case_beg_label,
                } });
                generate_stmt(ir, ctx, substmt);
                generate_instr(ir, .{ .Instr = .{
                    .Jmp = end_label,
                } });
                generate_instr(ir, .{ .Meta = .{
                    .Label = case_end_label,
                } });
            }

            generate_instr(ir, .{ .Meta = .{
                .Label = end_label,
            } });
        },
        .Case => unreachable,
        .Return => {
            generate_instr(ir, .{ .Instr = .{
                .Ret = ctx.function_end_label,
            } });
        },
        .Return_Expr => |expr| {
            var dst_tmp: Lvalue = .{ .Mem = .{
                .choose = CHOOSE_BASE,
                .base = .{
                    .offset = ctx.return_address_offset,
                    .tag = .Local,
                    .height = 0,
                },
            } };
            generate_rvalue(ir, expr, dst_tmp);
            generate_instr(ir, .{ .Instr = .{
                .Ret = ctx.function_end_label,
            } });
        },
        .Symbol => |symbol| {
            should_clean_locals = false;
            generate_local_symbol(ir, symbol);
        },
        .Assign => |assign| {
            var dst_tmp = generate_lvalue(ir, assign.lhs);
            generate_rvalue(ir, assign.rhs, dst_tmp);
        },
        .Expr => |expr| {
            var tmp = ir.grab_local();
            generate_rvalue(ir, expr, tmp.as_lvalue());
        },
    }

    if (should_clean_locals) {
        ir.next_local = old_next_local;
    }
}

fn generate_jump(ir: *This, expr: *Ast.Expr, jump_if_true: bool, label: Label) void {
    var tag: InstrJmpcTag = .Neq;
    var src0_tmp: Tmp = ir.grab_local();
    var src1_tmp: Tmp = ir.grab_local();

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
                    generate_rvalue(ir, expr, src0_tmp.as_lvalue());
                    generate_instr(ir, .{ .Instr = .{
                        .Mov = .{
                            .dst = src1_tmp.as_lvalue(),
                            .src = .{ .Imm = 0 },
                        },
                    } });
                    break :fill_operands;
                },
            }

            generate_rvalue(ir, op.lhs, src0_tmp.as_lvalue());
            generate_rvalue(ir, op.rhs, src1_tmp.as_lvalue());
        } else {
            generate_rvalue(ir, expr, src0_tmp.as_lvalue());
            generate_instr(ir, .{ .Instr = .{
                .Mov = .{
                    .dst = src1_tmp.as_lvalue(),
                    .src = .{ .Imm = 0 },
                },
            } });
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

    generate_instr(ir, .{ .Jmpc = .{
        .tag = tag,
        .src0 = src0_tmp.as_rvalue(),
        .src1 = src1_tmp.as_rvalue(),
        .label = label,
    } });
}

fn generate_rvalue(ir: *This, expr: *Ast.Expr, dst_tmp: Lvalue) void {
    switch (expr.payload) {
        .Binary_Op => |op| {
            var lhs_tmp = ir.grab_local();
            var rhs_tmp = ir.grab_local();
            generate_rvalue(ir, op.lhs, lhs_tmp.as_lvalue());
            generate_rvalue(ir, op.rhs, rhs_tmp.as_lvalue());

            var tag: InstrBinaryOpTag = switch (op.tag) {
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

            generate_instr(ir, .{ .Binary_Op = .{
                .tag = tag,
                .dst = dst_tmp,
                .src0 = lhs_tmp.as_rvalue(),
                .src1 = rhs_tmp.as_rvalue(),
            } });
        },
        .Unary_Op => |op| {
            switch (op.tag) {
                .Not => {
                    var src_tmp = ir.grab_local();
                    generate_rvalue(ir, op.subexpr, src_tmp.as_lvalue());
                    generate_instr(ir, .{ .Unary_Op = .{
                        .tag = .Not,
                        .dst = dst_tmp,
                        .src = src_tmp.as_rvalue(),
                    } });
                },
                .Neg => {
                    var src_tmp = ir.grab_local();
                    generate_rvalue(ir, op.subexpr, src_tmp.as_lvalue());
                    generate_instr(ir, .{ .Unary_Op = .{
                        .tag = .Neg,
                        .dst = dst_tmp,
                        .src = src_tmp.as_rvalue(),
                    } });
                },
                .Ref => {
                    var src_tmp: Rvalue = .{ .Addr = generate_lvalue(ir, op.subexpr) };
                    generate_instr(ir, .{ .Instr = .{
                        .Mov = .{
                            .dst = dst_tmp,
                            .src = src_tmp,
                        },
                    } });
                },
                .Deref => {
                    var src_tmp = ir.grab_local();
                    generate_rvalue(ir, op.subexpr, src_tmp.as_lvalue());
                    var new_src_tmp = src_tmp.as_rvalue().deref(ir);
                    generate_instr(ir, .{ .Instr = .{
                        .Mov = .{
                            .dst = dst_tmp,
                            .src = new_src_tmp,
                        },
                    } });
                },
            }
        },
        .If => |_if| {
            var false_label = ir.grab_label();
            var end_label = ir.grab_label();

            generate_jump(ir, _if.cond, false, false_label);

            generate_rvalue(ir, _if.if_true, dst_tmp);
            generate_instr(ir, .{ .Instr = .{
                .Jmp = end_label,
            } });
            generate_instr(ir, .{ .Meta = .{
                .Label = false_label,
            } });
            generate_rvalue(ir, _if.if_false, dst_tmp);
            generate_instr(ir, .{ .Meta = .{
                .Label = end_label,
            } });
        },
        .Call => |call| {
            var it = call.args.reverse_iterator();
            while (it.prev()) |arg| {
                var arg_tmp = ir.grab_local();
                generate_rvalue(ir, arg, arg_tmp.as_lvalue());
                generate_instr(ir, .{ .Instr = .{
                    .Push = arg_tmp.as_rvalue(),
                } });
            }

            var bytes_to_pop: u64 = call.args.count * 8;

            // Return address must the first thing pushed to the stack after arguments, because NOTE[0] relies on that assumption.
            generate_instr(ir, .{ .Instr = .{
                .Push = .{ .Addr = dst_tmp },
            } });

            bytes_to_pop += 8;

            if (call.lhs.payload == .Symbol and call.lhs.payload.Symbol.payload == .Function) {
                var symbol = call.lhs.payload.Symbol;
                var function = &symbol.payload.Function;

                if (symbol.key.scope != ir.global_scope) {
                    generate_instr(ir, .{ .Instr = .{
                        .Push_Frame_Pointer = ir.function_depth - function.depth,
                    } });
                    bytes_to_pop += 8;
                }
            }

            var src_tmp = ir.grab_local();
            generate_rvalue(ir, call.lhs, src_tmp.as_lvalue());
            generate_instr(ir, .{ .Instr = .{
                .Call = src_tmp.as_rvalue(),
            } });
            if (bytes_to_pop > 0) {
                generate_instr(ir, .{ .Instr = .{
                    .Pop = bytes_to_pop,
                } });
            }
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
            var src_tmp: Rvalue = .{ .Imm = @intFromBool(value) };
            generate_instr(ir, .{ .Instr = .{
                .Mov = .{
                    .dst = dst_tmp,
                    .src = src_tmp,
                },
            } });
        },
        .Int64 => |value| {
            var src_tmp: Rvalue = .{ .Imm = value };
            generate_instr(ir, .{ .Instr = .{
                .Mov = .{
                    .dst = dst_tmp,
                    .src = src_tmp,
                },
            } });
        },
        .Null => {
            var src_tmp: Rvalue = .{ .Imm = 0 };
            generate_instr(ir, .{ .Instr = .{
                .Mov = .{
                    .dst = dst_tmp,
                    .src = src_tmp,
                },
            } });
        },
        .Symbol => |symbol| {
            switch (symbol.payload) {
                .Variable => |variable| {
                    var tmp = variable.tmp;
                    tmp.height = ir.function_depth - symbol.depth;

                    generate_instr(ir, .{ .Instr = .{
                        .Mov = .{
                            .dst = dst_tmp,
                            .src = tmp.as_rvalue(),
                        },
                    } });
                },
                .Parameter => |parameter| {
                    generate_instr(ir, .{ .Instr = .{
                        .Mov = .{
                            .dst = dst_tmp,
                            .src = parameter.tmp.as_rvalue(),
                        },
                    } });
                },
                .Function => |function| {
                    generate_instr(ir, .{ .Instr = .{
                        .Mov = .{
                            .dst = dst_tmp,
                            .src = .{ .Label = function.label },
                        },
                    } });
                },
                .Enum_Field => |field| {
                    generate_instr(ir, .{ .Instr = .{
                        .Mov = .{
                            .dst = dst_tmp,
                            .src = .{ .Imm = @intCast(field.value) },
                        },
                    } });
                },
                .Struct_Field,
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
}

fn generate_lvalue(ir: *This, expr: *Ast.Expr) Lvalue {
    switch (expr.payload) {
        .Unary_Op => |op| {
            std.debug.assert(op.tag == .Deref);
            var src_tmp = ir.grab_local();
            generate_rvalue(ir, op.subexpr, src_tmp.as_lvalue());
            return src_tmp.as_rvalue().ref(ir);
        },
        .Symbol => |symbol| {
            switch (symbol.payload) {
                .Variable => |variable| {
                    var tmp = variable.tmp;
                    tmp.height = ir.function_depth - symbol.depth;

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

pub fn debug_print(ir: *This) void {
    for (ir.instrs.items) |instr| {
        debug_print_instr(instr);
    }
}

fn debug_print_tmp(tmp: Tmp) void {
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

fn debug_print_address(address: Mem) void {
    std.debug.print("[", .{});

    var should_print_delim = false;

    if (address.choose & CHOOSE_BASE == CHOOSE_BASE) {
        debug_print_tmp(address.base);
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

fn debug_print_lvalue(lvalue: Lvalue) void {
    switch (lvalue) {
        .Tmp => |tmp| debug_print_tmp(tmp),
        .Mem => |address| debug_print_address(address),
    }
}

fn debug_print_rvalue(rvalue: Rvalue) void {
    switch (rvalue) {
        .Lvalue => |lvalue| debug_print_lvalue(lvalue),
        .Addr => |lvalue| {
            std.debug.print("addr ", .{});
            debug_print_lvalue(lvalue);
        },
        .Label => |label| std.debug.print("L{}", .{label}),
        .Imm => |imm| std.debug.print("{}", .{imm}),
    }
}

fn debug_print_instr(instr: Instr) void {
    switch (instr) {
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
            debug_print_lvalue(op.dst);
            std.debug.print(", ", .{});
            debug_print_rvalue(op.src0);
            std.debug.print(", ", .{});
            debug_print_rvalue(op.src1);
        },
        .Unary_Op => |op| {
            var text: []const u8 = switch (op.tag) {
                .Not => "not",
                .Neg => "neg",
            };

            std.debug.print("    {s}    ", .{text});
            debug_print_lvalue(op.dst);
            std.debug.print(", ", .{});
            debug_print_rvalue(op.src);
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
            debug_print_rvalue(jmpc.src0);
            std.debug.print(", ", .{});
            debug_print_rvalue(jmpc.src1);
            std.debug.print(", L{}", .{jmpc.label});
        },
        .Instr => |subinstr| {
            switch (subinstr) {
                .Print => |src| {
                    std.debug.print("    print  ", .{});
                    debug_print_rvalue(src);
                },
                .Mov => |mov| {
                    std.debug.print("    mov    ", .{});
                    debug_print_lvalue(mov.dst);
                    std.debug.print(", ", .{});
                    debug_print_rvalue(mov.src);
                },
                .Jmp => |label| {
                    std.debug.print("    jmp    L{}", .{label});
                },
                .Push_Frame_Pointer => |depth| {
                    std.debug.print("    push   ({}^)", .{depth});
                },
                .Push => |src| {
                    std.debug.print("    push   ", .{});
                    debug_print_rvalue(src);
                },
                .Pop => |count| {
                    std.debug.print("    pop    {}", .{count});
                },
                .Call => |src| {
                    std.debug.print("    call   ", .{});
                    debug_print_rvalue(src);
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
