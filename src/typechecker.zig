ast: *Ast,
had_error: bool,

const std = @import("std");
const utils = @import("utils.zig");
const Ast = @import("ast.zig");

const LineInfo = Ast.LineInfo;
const Typechecker = @This();

const Sign = enum {
    Unsigned,
    Signed,
    Both,
};

const TypecheckExprResult = struct {
    typ: *Ast.Type,
    is_type: bool,
};

pub fn typecheck(ast: *Ast) void {
    var typechecker = Typechecker{
        .ast = ast,
        .had_error = false,
    };
    typecheck_top_level(&typechecker);
}

fn typecheck_top_level(t: *Typechecker) void {
    var it = t.ast.stmt_list.first;
    while (it) |node| {
        typecheck_stmt(t, node.data);
        it = node.next;
    }

    if (t.had_error) {
        std.posix.exit(1);
    }
}

fn implicit_cast_to_integer(t: *Typechecker, expr: *Ast.Expr, expr_type: *Ast.Type, what_sign: Sign) ?*Ast.Type {
    var casted: ?*Ast.Type = null;
    var should_cast = false;

    switch (expr_type.as) {
        .Bool => {},
        .Integer => |Integer| {
            switch (what_sign) {
                .Unsigned => {
                    if (!Integer.is_signed) {
                        casted = expr_type;
                    }
                },
                .Signed => {
                    if (Integer.is_signed) {
                        casted = expr_type;
                    } else if (Integer.bits < Ast.MAX_BITS_IN_INTEGER) {
                        const typ = t.ast.create(Ast.Type);
                        const bits = Integer.bits + 1;
                        typ.* = .{
                            .line_info = expr.line_info,
                            .as = .{ .Integer = .{
                                .bits = bits,
                                .is_signed = true,
                            } },
                            .size = bits,
                        };
                        casted = typ;
                        should_cast = true;
                    }
                },
                .Both => {
                    casted = expr_type;
                },
            }
        },
    }

    if (should_cast) {
        const typ = casted.?;
        const inner = t.ast.create(Ast.Expr);
        inner.* = expr.*;
        expr.* = .{
            .line_info = inner.line_info,
            .as = .{ .Cast = .{
                .typ = typ,
                .expr = inner,
            } },
            .typ = typ,
        };
    }

    return casted;
}

fn implicit_cast_two_integers(t: *Typechecker, lhs: *Ast.Expr, lhs_type: *Ast.Type, rhs: *Ast.Expr, rhs_type: *Ast.Type) ?*Ast.Type {
    const WhichSideToCast = enum {
        None,
        Left,
        Right,
        Both,
    };

    var casted: ?*Ast.Type = null;
    var which_side_to_cast: WhichSideToCast = .None;

    switch (lhs_type.as) {
        .Bool => {},
        .Integer => |lInteger| {
            switch (rhs_type.as) {
                .Bool => {},
                .Integer => |rInteger| {
                    if (lInteger.is_signed == rInteger.is_signed) {
                        if (lInteger.bits > rInteger.bits) {
                            casted = lhs_type;
                            which_side_to_cast = .Left;
                        } else if (rInteger.bits > lInteger.bits) {
                            casted = rhs_type;
                            which_side_to_cast = .Right;
                        } else {
                            casted = rhs_type;
                            which_side_to_cast = .None;
                        }
                    } else {
                        if (lInteger.bits == rInteger.bits or
                            (lInteger.is_signed and lInteger.bits < rInteger.bits) or
                            (rInteger.is_signed and rInteger.bits < lInteger.bits))
                        {
                            const bits = @max(lInteger.bits, rInteger.bits) + 1;
                            if (bits < Ast.MAX_BITS_IN_INTEGER) {
                                const typ = t.ast.create(Ast.Type);
                                typ.* = .{
                                    .line_info = undefined, // TODO: set line info.
                                    .as = .{ .Integer = .{
                                        .bits = bits,
                                        .is_signed = true,
                                    } },
                                    .size = bits,
                                };
                                casted = typ;
                                which_side_to_cast = .Both;
                            }
                        } else if (lInteger.is_signed and lInteger.bits > rInteger.bits) {
                            casted = lhs_type;
                            which_side_to_cast = .Right;
                        } else if (rInteger.is_signed and rInteger.bits > lInteger.bits) {
                            casted = rhs_type;
                            which_side_to_cast = .Left;
                        }
                    }
                },
            }
        },
    }

    if (casted) |typ| {
        if (which_side_to_cast == .Left or which_side_to_cast == .Both) {
            const inner = t.ast.create(Ast.Expr);
            inner.* = lhs.*;
            lhs.* = .{
                .line_info = inner.line_info,
                .as = .{ .Cast = .{
                    .typ = typ,
                    .expr = inner,
                } },
                .typ = typ,
            };
        }

        if (which_side_to_cast == .Right or which_side_to_cast == .Both) {
            const inner = t.ast.create(Ast.Expr);
            inner.* = rhs.*;
            rhs.* = .{
                .line_info = inner.line_info,
                .as = .{ .Cast = .{
                    .typ = typ,
                    .expr = inner,
                } },
                .typ = typ,
            };
        }
    }

    return casted;
}

fn implicit_cast(t: *Typechecker, expr: *Ast.Expr, expr_type: *Ast.Type, cast_to_type: *Ast.Type) bool {
    if (expr_type.equal(cast_to_type)) {
        return true;
    }

    var casted: ?*Ast.Type = null;

    switch (cast_to_type.as) {
        .Bool => {
            switch (expr_type.as) {
                .Bool => {
                    casted = cast_to_type;
                },
                .Integer => {},
            }
        },
        .Integer => |dInteger| {
            switch (expr_type.as) {
                .Bool => {},
                .Integer => |sInteger| {
                    if (dInteger.is_signed == sInteger.is_signed) {
                        if (dInteger.bits >= sInteger.bits) {
                            casted = cast_to_type;
                        }
                    } else if (dInteger.is_signed) {
                        if (dInteger.bits > sInteger.bits) {
                            casted = cast_to_type;
                        }
                    }
                },
            }
        },
    }

    if (casted) |typ| {
        const inner = t.ast.create(Ast.Expr);
        inner.* = expr.*;
        expr.* = .{
            .line_info = inner.line_info,
            .as = .{ .Cast = .{
                .typ = typ,
                .expr = inner,
            } },
            .typ = typ,
        };
    }

    return casted != null;
}

fn cast(t: *Typechecker, expr: *Ast.Expr, expr_type: *Ast.Type, cast_to_type: *Ast.Type) bool {
    if (expr_type.equal(cast_to_type)) {
        return true;
    }

    var casted: ?*Ast.Type = null;

    switch (cast_to_type.as) {
        .Bool, .Integer => {
            switch (expr_type.as) {
                .Bool, .Integer => {
                    casted = cast_to_type;
                },
            }
        },
    }

    if (casted) |typ| {
        const inner = t.ast.create(Ast.Expr);
        inner.* = expr.*;
        expr.* = .{
            .line_info = inner.line_info,
            .as = .{ .Cast = .{
                .typ = typ,
                .expr = inner,
            } },
            .typ = typ,
        };
    }

    return casted != null;
}

fn typecheck_stmt(t: *Typechecker, stmt: *Ast.Stmt) void {
    switch (stmt.*) {
        .Print => |expr| {
            const expr_result = typecheck_expr(t, expr);

            if (expr_result.is_type) {
                stmt.* = .{ .Print_Type = expr_result.typ };
            }
        },
        .Print_Type => unreachable,
        .Expr => |expr| {
            _ = typecheck_expr_only(t, expr);
        },
    }
}

fn typecheck_expr_only(t: *Typechecker, expr: *Ast.Expr) *Ast.Type {
    const result = typecheck_expr(t, expr);
    if (result.is_type) {
        report_error(t, expr.line_info, "unexpected type", .{});
        std.posix.exit(1);
    }
    return result.typ;
}

fn typecheck_expr(t: *Typechecker, expr: *Ast.Expr) TypecheckExprResult {
    var is_type = false;
    const typ: *Ast.Type = typ: {
        switch (expr.as) {
            .Binary_Op => |Binary_Op| {
                const lhs_type = typecheck_expr_only(t, Binary_Op.lhs);
                const rhs_type = typecheck_expr_only(t, Binary_Op.rhs);

                switch (Binary_Op.tag) {
                    .Or, .And => {
                        if (!implicit_cast(t, Binary_Op.lhs, lhs_type, &Ast.bool_type) or
                            !implicit_cast(t, Binary_Op.rhs, rhs_type, &Ast.bool_type))
                        {
                            report_error(t, Binary_Op.line_info, "expected 'bool'/'bool', but got '{}'/'{}'", .{ lhs_type, rhs_type });
                        }

                        break :typ &Ast.bool_type;
                    },
                    .Eq, .Neq => {
                        switch (lhs_type.as) {
                            .Bool => {
                                if (!implicit_cast(t, Binary_Op.rhs, rhs_type, lhs_type)) {
                                    report_error(t, Binary_Op.line_info, "can't compare values of type '{}'/'{}'", .{ lhs_type, rhs_type });
                                }
                            },
                            .Integer => {
                                if (implicit_cast_two_integers(t, Binary_Op.lhs, lhs_type, Binary_Op.rhs, rhs_type) == null) {
                                    report_error(t, Binary_Op.line_info, "can't compare values of type '{}'/'{}'", .{ lhs_type, rhs_type });
                                }
                            },
                        }

                        break :typ &Ast.bool_type;
                    },
                    .Lt, .Leq, .Gt, .Geq => {
                        if (implicit_cast_two_integers(t, Binary_Op.lhs, lhs_type, Binary_Op.rhs, rhs_type) == null) {
                            report_error(t, Binary_Op.line_info, "can't compare values of type '{}'/'{}'", .{ lhs_type, rhs_type });
                        }

                        break :typ &Ast.bool_type;
                    },
                    .Add, .Sub, .Mul, .Div, .Mod => {
                        const casted = implicit_cast_two_integers(t, Binary_Op.lhs, lhs_type, Binary_Op.rhs, rhs_type);

                        if (casted == null) {
                            report_error(t, Binary_Op.line_info, "can't perform arithmetic operation on values of type '{}'/'{}'", .{ lhs_type, rhs_type });
                            std.posix.exit(1);
                        }

                        break :typ casted.?;
                    },
                }
            },
            .Unary_Op => |Unary_Op| {
                const subexpr_type = typecheck_expr_only(t, Unary_Op.subexpr);

                switch (Unary_Op.tag) {
                    .Plus => {
                        const casted = implicit_cast_to_integer(t, Unary_Op.subexpr, subexpr_type, .Both);

                        if (casted == null) {
                            report_error(t, expr.line_info, "expected integer type, but got '{}'", .{subexpr_type});
                            std.posix.exit(1);
                        }

                        break :typ casted.?;
                    },
                    .Minus => {
                        const casted = implicit_cast_to_integer(t, Unary_Op.subexpr, subexpr_type, .Signed);

                        if (casted == null) {
                            report_error(t, expr.line_info, "expected signed integer, but got '{}'", .{subexpr_type});
                            std.posix.exit(1);
                        }

                        break :typ casted.?;
                    },
                    .Not => {
                        if (!implicit_cast(t, Unary_Op.subexpr, subexpr_type, &Ast.bool_type)) {
                            report_error(t, expr.line_info, "expected 'bool', but got '{}'", .{subexpr_type});
                        }

                        break :typ &Ast.bool_type;
                    },
                }
            },
            .Call => |Call| {
                const subexpr_result = typecheck_expr(t, Call.subexpr);

                if (subexpr_result.is_type) {
                    typecheck_type(t, subexpr_result.typ);

                    switch (subexpr_result.typ.as) {
                        .Bool, .Integer => {
                            if (Call.args.len != 1) {
                                report_error(t, Call.subexpr.line_info, "expected 1 argument, but got {}", .{Call.args.len});
                                std.posix.exit(1);
                            }

                            const arg = Call.args.first.?.data;
                            const arg_type = typecheck_expr_only(t, arg);

                            if (!implicit_cast(t, arg, arg_type, subexpr_result.typ)) {
                                report_error(t, arg.line_info, "expected '{}', but got '{}'", .{ subexpr_result.typ, arg_type });
                                std.posix.exit(1);
                            }
                        },
                    }

                    const args = Call.args;
                    expr.as = .{ .Constructor = .{
                        .typ = subexpr_result.typ,
                        .args = args,
                    } };

                    break :typ subexpr_result.typ;
                } else {
                    report_error(t, Call.subexpr.line_info, "function calls are not supported yet", .{});
                    std.posix.exit(1);
                }
            },
            .Constructor => unreachable,
            .Bit_Size_Of => |subexpr| {
                const subexpr_type = typecheck_expr_only(t, subexpr);
                const bits = utils.count_bits(subexpr_type.size);

                const typ = t.ast.create(Ast.Type);
                typ.* = .{
                    .line_info = expr.line_info,
                    .as = .{ .Integer = .{
                        .bits = bits,
                        .is_signed = false,
                    } },
                    .size = bits,
                };

                expr.as = .{ .Integer = subexpr_type.size };

                break :typ typ;
            },
            .Byte_Size_Of => |subexpr| {
                const subexpr_type = typecheck_expr_only(t, subexpr);
                const byte_size = utils.round_to_next_pow2(subexpr_type.size);
                const bits = utils.count_bits(byte_size);

                const typ = t.ast.create(Ast.Type);
                typ.* = .{
                    .line_info = expr.line_info,
                    .as = .{ .Integer = .{
                        .bits = bits,
                        .is_signed = false,
                    } },
                    .size = bits,
                };

                expr.as = .{ .Integer = byte_size };

                break :typ typ;
            },
            .Type_Of => |subexpr| {
                const subexpr_type = typecheck_expr_only(t, subexpr);

                is_type = true;
                expr.as = .{ .Type = subexpr_type.* };

                break :typ subexpr_type;
            },
            .As => |As| {
                typecheck_type(t, As.typ);
                const expr_type = typecheck_expr_only(t, As.expr);
                if (!implicit_cast(t, As.expr, expr_type, As.typ)) {
                    report_error(t, As.expr.line_info, "can't safe cast '{}' to '{}'", .{ expr_type, As.typ });
                    std.posix.exit(1);
                }
                break :typ As.typ;
            },
            .Cast => |Cast| {
                typecheck_type(t, Cast.typ);
                const expr_type = typecheck_expr_only(t, Cast.expr);
                if (!cast(t, Cast.expr, expr_type, Cast.typ)) {
                    report_error(t, Cast.expr.line_info, "can't cast '{}' to '{}'", .{ expr_type, Cast.typ });
                    std.posix.exit(1);
                }
                break :typ Cast.typ;
            },
            .Type => |*typ| {
                is_type = true;
                break :typ typ;
            },
            .Bool => {
                break :typ &Ast.bool_type;
            },
            .Integer => |value| {
                const typ = t.ast.create(Ast.Type);
                const bits = utils.count_bits(value);
                typ.* = .{
                    .line_info = expr.line_info,
                    .as = .{ .Integer = .{
                        .bits = bits,
                        .is_signed = false,
                    } },
                    .size = bits,
                };
                break :typ typ;
            },
        }
    };

    expr.typ = typ;

    return .{ .typ = typ, .is_type = is_type };
}

fn typecheck_type(_: *Typechecker, typ: *Ast.Type) void {
    switch (typ.as) {
        .Bool => {
            std.debug.assert(typ.size == 1);
        },
        .Integer => |Integer| {
            std.debug.assert(typ.size == Integer.bits);
        },
    }
}

fn report_error(t: *Typechecker, line_info: LineInfo, comptime format: []const u8, args: anytype) void {
    t.had_error = true;
    utils.eprint("{s}:{}:{}: error: " ++ format ++ "\n", .{ t.ast.filepath, line_info.line, line_info.column } ++ args);
}
