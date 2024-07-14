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

    switch (expr_type.*) {
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
                        typ.* = .{
                            .Integer = .{
                                .line_info = undefined, // TODO: set line info.
                                .bits = Integer.bits + 1,
                                .is_signed = true,
                            },
                        };
                        casted = typ;
                    }
                },
                .Both => {
                    casted = expr_type;
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

    return casted;
}

fn implicit_cast(t: *Typechecker, expr: *Ast.Expr, expr_type: *Ast.Type, cast_to_type: *Ast.Type) ?*Ast.Type {
    var casted: ?*Ast.Type = null;

    switch (cast_to_type.*) {
        .Bool => {
            switch (expr_type.*) {
                .Bool => {
                    casted = cast_to_type;
                },
                .Integer => {},
            }
        },
        .Integer => |dInteger| {
            switch (expr_type.*) {
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

    switch (lhs_type.*) {
        .Bool => {},
        .Integer => |lInteger| {
            switch (rhs_type.*) {
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
                            const bits = @max(lInteger.bits, rInteger.bits);
                            if (bits < Ast.MAX_BITS_IN_INTEGER) {
                                const typ = t.ast.create(Ast.Type);
                                typ.* = .{
                                    .Integer = .{
                                        .line_info = undefined, // TODO: set line info.
                                        .bits = bits + 1,
                                        .is_signed = true,
                                    },
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

fn typecheck_stmt(t: *Typechecker, stmt: *Ast.Stmt) void {
    switch (stmt.*) {
        .Print => |expr| {
            _ = typecheck_expr(t, expr);
        },
        .Expr => |expr| {
            _ = typecheck_expr(t, expr);
        },
    }
}

fn typecheck_expr(t: *Typechecker, expr: *Ast.Expr) *Ast.Type {
    const typ: *Ast.Type = typ: {
        switch (expr.as) {
            .Binary_Op => |Binary_Op| {
                const lhs_type = typecheck_expr(t, Binary_Op.lhs);
                const rhs_type = typecheck_expr(t, Binary_Op.rhs);

                switch (Binary_Op.tag) {
                    .Or, .And => {
                        if (implicit_cast(t, Binary_Op.lhs, lhs_type, &Ast.bool_type) == null or
                            implicit_cast(t, Binary_Op.rhs, rhs_type, &Ast.bool_type) == null)
                        {
                            report_error(t, Binary_Op.line_info, "expected 'bool'/'bool', but got '{}'/'{}'", .{ lhs_type, rhs_type });
                        }

                        break :typ &Ast.bool_type;
                    },
                    .Eq, .Neq => {
                        switch (lhs_type.*) {
                            .Bool => {
                                if (implicit_cast(t, Binary_Op.rhs, rhs_type, lhs_type) == null) {
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
                const subexpr_type = typecheck_expr(t, Unary_Op.subexpr);

                switch (Unary_Op.tag) {
                    .Plus => {
                        const casted = implicit_cast_to_integer(t, Unary_Op.subexpr, subexpr_type, .Signed);

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
                        if (implicit_cast(t, Unary_Op.subexpr, subexpr_type, &Ast.bool_type) == null) {
                            report_error(t, expr.line_info, "expected 'bool', but got '{}'", .{subexpr_type});
                        }

                        break :typ &Ast.bool_type;
                    },
                }
            },
            .Cast => unreachable,
            .Bool => {
                break :typ &Ast.bool_type;
            },
            .Integer => |value| {
                const typ = t.ast.create(Ast.Type);
                typ.* = .{ .Integer = .{
                    .line_info = expr.line_info,
                    .bits = utils.count_bits(value),
                    .is_signed = false,
                } };
                break :typ typ;
            },
        }
    };

    expr.typ = typ;

    return typ;
}

fn report_error(t: *Typechecker, line_info: LineInfo, comptime format: []const u8, args: anytype) void {
    t.had_error = true;
    utils.eprint("{s}:{}:{}: error: " ++ format ++ "\n", .{ t.ast.filepath, line_info.line, line_info.column } ++ args);
}
