ast: *Ast,
instrs: InstrList,
allocator: Allocator,

const std = @import("std");
const utils = @import("utils.zig");
const Ast = @import("ast.zig");

const Allocator = std.mem.Allocator;
const IRC = @This();

pub const InstrList = std.ArrayList(Instr);

pub const Instr = union(Instr.Tag) {
    Printb: Rvalue,
    Printi: Rvalue,
    Printu: Rvalue,
    Printbt: void,
    Printit: u32,

    pub const Tag = enum {
        Printb,
        Printi,
        Printu,
        Printbt,
        Printit,
    };
};

pub const Rvalue = union(Rvalue.Tag) {
    Imm: u64,

    pub const Tag = enum {
        Imm,
    };
};

pub fn generate_ir(ast: *Ast) IRC {
    var irc = IRC{
        .ast = ast,
        .instrs = InstrList.init(utils.gpa),
        .allocator = utils.gpa,
    };

    generate_ir_top_level(&irc);

    return irc;
}

fn generate_ir_top_level(irc: *IRC) void {
    var it = irc.ast.stmt_list.first;
    while (it) |node| {
        generate_ir_stmt(irc, node.data);
        it = node.next;
    }
}

fn generate_ir_stmt(irc: *IRC, stmt: *Ast.Stmt) void {
    switch (stmt.*) {
        .Print => |expr| {
            const rvalue = generate_ir_expr(irc, expr);
            switch (expr.typ.?.as) {
                .Bool => {
                    generate_ir_instr(irc, .{ .Printb = rvalue });
                },
                .Integer => |Integer| {
                    if (Integer.is_signed) {
                        generate_ir_instr(irc, .{ .Printi = rvalue });
                    } else {
                        generate_ir_instr(irc, .{ .Printu = rvalue });
                    }
                },
            }
        },
        .Print_Type => |typ| {
            switch (typ.as) {
                .Bool => {
                    generate_ir_instr(irc, .Printbt);
                },
                .Integer => |Integer| {
                    var text: u32 = if (Integer.is_signed) 'i' else 'u';

                    text |= Integer.bits << 8;

                    generate_ir_instr(irc, .{ .Printit = text });
                },
            }
        },
        .Expr => |expr| {
            _ = generate_ir_expr(irc, expr);
        },
    }
}

fn generate_ir_expr(irc: *IRC, expr: *Ast.Expr) Rvalue {
    const BinaryOpType = enum {
        Unsigned,
        Signed,
    };

    switch (expr.as) {
        .Binary_Op => |Binary_Op| {
            const lhs_rvalue = generate_ir_expr(irc, Binary_Op.lhs);
            const rhs_rvalue = generate_ir_expr(irc, Binary_Op.rhs);

            const op_type: BinaryOpType = switch (Binary_Op.lhs.typ.?.as) {
                .Bool => .Unsigned,
                .Integer => |Integer| if (Integer.is_signed) .Signed else .Unsigned,
            };

            switch (op_type) {
                .Unsigned => {
                    const l: u64 = @bitCast(lhs_rvalue.Imm);
                    const r: u64 = @bitCast(rhs_rvalue.Imm);
                    const res: u64 = switch (Binary_Op.tag) {
                        .Or => @intFromBool((l != 0) or (r != 0)),
                        .And => @intFromBool((l != 0) and (r != 0)),
                        .Eq => @intFromBool(l == r),
                        .Neq => @intFromBool(l != r),
                        .Lt => @intFromBool(l < r),
                        .Leq => @intFromBool(l <= r),
                        .Gt => @intFromBool(l > r),
                        .Geq => @intFromBool(l >= r),
                        .Add => l + r,
                        .Sub => l - r,
                        .Mul => l * r,
                        .Div => l / r,
                        .Mod => l % r,
                    };
                    return .{ .Imm = res };
                },
                .Signed => {
                    const l: i64 = @bitCast(lhs_rvalue.Imm);
                    const r: i64 = @bitCast(rhs_rvalue.Imm);
                    const res: i64 = switch (Binary_Op.tag) {
                        .Or => @intFromBool((l != 0) or (r != 0)),
                        .And => @intFromBool((l != 0) and (r != 0)),
                        .Eq => @intFromBool(l == r),
                        .Neq => @intFromBool(l != r),
                        .Lt => @intFromBool(l < r),
                        .Leq => @intFromBool(l <= r),
                        .Gt => @intFromBool(l > r),
                        .Geq => @intFromBool(l >= r),
                        .Add => l + r,
                        .Sub => l - r,
                        .Mul => l * r,
                        .Div => @divTrunc(l, r),
                        .Mod => @rem(l, r),
                    };
                    return .{ .Imm = @bitCast(res) };
                },
            }
        },
        .Unary_Op => |Unary_Op| {
            const subexpr_rvalue = generate_ir_expr(irc, Unary_Op.subexpr);

            switch (Unary_Op.tag) {
                .Plus => {
                    return subexpr_rvalue;
                },
                .Minus => {
                    return .{ .Imm = @bitCast(-@as(i64, @bitCast(subexpr_rvalue.Imm))) };
                },
                .Not => {
                    return .{ .Imm = @intFromBool(subexpr_rvalue.Imm == 0) };
                },
            }
        },
        .Call => unreachable,
        .Constructor => |Constructor| {
            switch (Constructor.typ.as) {
                .Bool, .Integer => {
                    return generate_ir_expr(irc, Constructor.args.first.?.data);
                },
            }
        },
        .Bit_Size_Of,
        .Byte_Size_Of,
        .Type_Of,
        => unreachable,
        .As, .Cast => |Cast| {
            const rvalue = generate_ir_expr(irc, Cast.expr);

            switch (Cast.typ.as) {
                .Bool => {
                    return .{ .Imm = @intFromBool(rvalue.Imm != 0) };
                },
                .Integer => |dInteger| {
                    switch (Cast.expr.typ.?.as) {
                        .Bool => {
                            return .{ .Imm = @intFromBool(rvalue.Imm != 0) };
                        },
                        .Integer => |sInteger| {
                            if (dInteger.is_signed and sInteger.is_signed and dInteger.bits > sInteger.bits) {
                                const imm = utils.sign_extend(rvalue.Imm, @intCast(sInteger.bits));
                                return .{ .Imm = imm };
                            } else {
                                return rvalue;
                            }
                        },
                    }
                },
            }
        },
        .Type => unreachable,
        .Bool => |value| {
            return .{ .Imm = @intFromBool(value) };
        },
        .Integer => |value| {
            return .{ .Imm = value };
        },
    }
}

fn generate_ir_instr(irc: *IRC, instr: Instr) void {
    irc.instrs.append(instr) catch {
        std.posix.exit(1);
    };
}
