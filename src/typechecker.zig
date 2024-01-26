const std = @import("std");
const common = @import("common.zig");
const Ast = @import("ast.zig");

const TypecheckerStmtContext = struct {
    return_type: *Ast.Type,
    is_in_loop: bool,
};

const TypecheckTypeFlags = packed struct {
    do_shallow_typecheck: bool = false,
    reject_void_type: bool = false,
};

var VOID_TYPE_HINT = Ast.Type{
    .payload = .Void,
    .size = 0,
    .typechecking_stage = .Fully_Typechecked,
    .line_info = .{},
};
var VOID_PTR_TYPE_HINT = Ast.Type{
    .payload = .{ .Pointer = &VOID_TYPE_HINT },
    .size = 8,
    .typechecking_stage = .Fully_Typechecked,
    .line_info = .{},
};
var BOOL_TYPE_HINT = Ast.Type{
    .payload = .Bool,
    .size = 1,
    .typechecking_stage = .Fully_Typechecked,
    .line_info = .{},
};
var INT64_TYPE_HINT = Ast.Type{
    .payload = .Int64,
    .size = 8,
    .typechecking_stage = .Fully_Typechecked,
    .line_info = .{},
};

pub fn reduce_expr(ast: *Ast, expr: *Ast.Expr) void {
    if (expr.payload != .Int64) {
        common.print_error(ast.filepath, expr.line_info, "TODO: evaluate expression in compile-time.", .{});
        std.os.exit(1);
    }
}

pub fn typecheck(ast: *Ast) void {
    var it = ast.globals.iterator();
    while (it.next()) |symbol| {
        typecheck_symbol(ast, symbol.*);
    }

    switch (ast.main.payload) {
        .Function => |function| {
            var _type = &function._type.payload.Function;

            if (_type.params.count != 0) {
                common.print_error(ast.filepath, function._type.line_info, "expected 0 arguments, but got {}.", .{_type.params.count});
                std.os.exit(1);
            }

            var return_type = _type.return_type;

            if (return_type.payload != .Void) {
                common.print_error(ast.filepath, _type.return_type.line_info, "expected 'void', but got '{}'.", .{_type.return_type});
                std.os.exit(1);
            }
        },
        else => {
            common.print_error(ast.filepath, ast.main.line_info, "'main' should be a function.", .{});
            std.os.exit(1);
        },
    }
}

fn typecheck_symbol(ast: *Ast, symbol: *Ast.Symbol) void {
    switch (symbol.payload) {
        .Variable => |*variable| {
            if (variable._type) |_type| {
                typecheck_type(ast, _type);

                if (variable.expr) |expr| {
                    var expected_type = _type;
                    var actual_type = typecheck_expr(ast, expected_type, expr);
                    if (!actual_type.eql(expected_type)) {
                        common.print_error(ast.filepath, expr.line_info, "expected '{}', but got '{}'.", .{ expected_type, actual_type });
                        common.print_note(ast.filepath, _type.line_info, "expected type is here", .{});
                        common.print_note(ast.filepath, expr.line_info, "expression is here", .{});
                        std.os.exit(1);
                    }
                }
            } else {
                if (variable.expr) |expr| {
                    var _type = typecheck_expr(ast, null, expr);
                    variable._type = _type;

                    if (_type.payload == .Function) {
                        if (_type.payload.Function.scope != ast.global_scope) {
                            common.print_error(ast.filepath, symbol.line_info, "can't use function pointers to local functions.", .{});
                            std.os.exit(1);
                        }
                    }
                } else {
                    unreachable;
                }
            }

            variable.was_visited = true;
        },
        .Parameter => |parameter| {
            typecheck_type(ast, parameter._type);
        },
        .Function => |function| {
            typecheck_type(ast, function._type);

            var ctx: TypecheckerStmtContext = .{
                .return_type = function._type.payload.Function.return_type,
                .is_in_loop = false,
            };
            var it = function.block.iterator();
            while (it.next()) |stmt| {
                typecheck_stmt(ast, &ctx, stmt);
            }
        },
        .Type => |_type| typecheck_type_fully(ast, _type, .{ .reject_void_type = false }),
        .Struct_Field, .Enum_Field, .Definition => unreachable,
    }
}

inline fn typecheck_type(ast: *Ast, _type: *Ast.Type) void {
    typecheck_type_fully(ast, _type, .{ .reject_void_type = true });
}

fn typecheck_type_fully(ast: *Ast, _type: *Ast.Type, flags: TypecheckTypeFlags) void {
    typecheck_type_rec(ast, _type, .{ .do_shallow_typecheck = true, .reject_void_type = flags.reject_void_type });
    typecheck_type_rec(ast, _type, .{ .do_shallow_typecheck = false, .reject_void_type = flags.reject_void_type });
}

fn typecheck_type_rec(ast: *Ast, _type: *Ast.Type, flags: TypecheckTypeFlags) void {
    switch (_type.typechecking_stage) {
        .Not_Typechecked => {
            _type.typechecking_stage = .Being_Shallow_Typechecked;
        },
        .Being_Shallow_Typechecked => {
            common.print_error(ast.filepath, _type.line_info, "cyclic reference detected.", .{});
            std.os.exit(1);
        },
        .Shallow_Typechecked => {
            if (flags.do_shallow_typecheck) {
                return;
            }

            _type.typechecking_stage = .Being_Fully_Typechecked;
        },
        .Being_Fully_Typechecked, .Fully_Typechecked => return,
    }

    switch (_type.payload) {
        .Struct => |_struct| {
            var size: Ast.TypeSize = 0;

            var new_flags = flags;
            new_flags.reject_void_type = true;

            var it = _struct.fields.iterator();
            while (it.next()) |field_ptr| {
                var field = &field_ptr.*.payload.Struct_Field;
                typecheck_type_rec(ast, field._type, new_flags);
                size += field._type.size;
            }

            _type.size = size;
        },
        .Union => |_union| {
            var size: Ast.TypeSize = 0;

            var new_flags = flags;
            new_flags.reject_void_type = true;

            var it = _union.fields.iterator();
            while (it.next()) |field_ptr| {
                var field = &field_ptr.*.payload.Struct_Field;
                typecheck_type_rec(ast, field._type, new_flags);
                size = @max(size, field._type.size);
            }

            _type.size = size;
        },
        .Enum => |_enum| {
            var enum_value: u64 = 0;

            var it = _enum.fields.iterator();
            while (it.next()) |field_ptr| {
                var field = &field_ptr.*.payload.Enum_Field;
                field.value = enum_value;
                enum_value += 1;
            }

            _type.size = 8;
        },
        .Function => |function| {
            _type.size = 8;
            if (!flags.do_shallow_typecheck) {
                var new_flags = flags;
                new_flags.reject_void_type = true;

                var it = function.params.iterator();
                while (it.next()) |param| {
                    typecheck_type_rec(ast, param.*.payload.Parameter._type, new_flags);
                }

                new_flags.reject_void_type = false;
                typecheck_type_rec(ast, function.return_type, new_flags);
            }
        },
        .Array => |*array| {
            var size_type = typecheck_expr(ast, &INT64_TYPE_HINT, array.expr);
            var size_flags = size_type.compare();
            reduce_expr(ast, array.expr);

            if (!size_flags.is_integer) {
                common.print_error(ast.filepath, array.expr.line_info, "expected integer, but got '{}'.", .{size_type});
                std.os.exit(1);
            }

            var new_flags = flags;
            new_flags.reject_void_type = true;

            typecheck_type_rec(ast, array.subtype, new_flags);

            var size = array.expr.payload.Int64;
            if (size <= 0) {
                common.print_error(ast.filepath, array.expr.line_info, "array size can't be negative ({}).", .{size});
                std.os.exit(1);
            }

            array.count = @intCast(size);
            _type.size = array.count * array.subtype.size;
        },
        .Pointer => |subtype| {
            _type.size = 8;
            if (!flags.do_shallow_typecheck) {
                var new_flags = flags;
                new_flags.reject_void_type = false;
                typecheck_type_rec(ast, subtype, new_flags);
            }
        },
        .Void => {
            _type.size = 0;
            if (flags.reject_void_type) {
                common.print_error(ast.filepath, _type.line_info, "unexpected 'void' type.", .{});
                std.os.exit(1);
            }
        },
        .Bool => _type.size = 1,
        .Int64 => _type.size = 8,
        .Identifier => unreachable,
    }

    _type.typechecking_stage = if (flags.do_shallow_typecheck)
        .Shallow_Typechecked
    else
        .Fully_Typechecked;
}

fn typecheck_block(ast: *Ast, ctx: *const TypecheckerStmtContext, block: Ast.StmtBlock) void {
    var it = block.iterator();
    while (it.next()) |stmt| {
        typecheck_stmt(ast, ctx, stmt);
    }
}

fn typecheck_stmt(ast: *Ast, ctx: *const TypecheckerStmtContext, stmt: *Ast.Stmt) void {
    switch (stmt.payload) {
        .Print => |expr| {
            _ = typecheck_expr(ast, null, expr);
        },
        .Block => |block| {
            typecheck_block(ast, ctx, block);
        },
        .If => |_if| {
            var cond_type = typecheck_expr(ast, &BOOL_TYPE_HINT, _if.cond);
            if (cond_type.payload != .Bool) {
                common.print_error(ast.filepath, _if.cond.line_info, "expected 'bool', but got '{}'.", .{cond_type});
                std.os.exit(1);
            }

            // TODO: reject symbols definition in single statements.

            typecheck_stmt(ast, ctx, _if.if_true);
            if (_if.if_false) |if_false| typecheck_stmt(ast, ctx, if_false);
        },
        .While => |_while| {
            var cond_type = typecheck_expr(ast, &BOOL_TYPE_HINT, _while.cond);
            if (cond_type.payload != .Bool) {
                common.print_error(ast.filepath, _while.cond.line_info, "expected 'bool', but got '{}'.", .{cond_type});
                std.os.exit(1);
            }

            var new_ctx = ctx.*;
            new_ctx.is_in_loop = true;
            typecheck_stmt(ast, &new_ctx, _while.block);
        },
        .Break => {
            if (!ctx.is_in_loop) {
                common.print_error(ast.filepath, stmt.line_info, "'break' outside of loop.", .{});
                std.os.exit(1);
            }
        },
        .Continue => {
            if (!ctx.is_in_loop) {
                common.print_error(ast.filepath, stmt.line_info, "'continue' outside of loop.", .{});
                std.os.exit(1);
            }
        },
        .Switch => |_switch| {
            var cond_type = typecheck_expr(ast, null, _switch.cond);
            var cond_flags = cond_type.compare();

            if (!cond_flags.is_comparable) {
                common.print_error(ast.filepath, _switch.cond.line_info, "condition isn't comparable: '{}'.", .{cond_type});
                std.os.exit(1);
            }

            var it = _switch.cases.iterator();
            while (it.next()) |top_level_case| {
                if (top_level_case.payload != .Case) {
                    common.print_error(ast.filepath, top_level_case.line_info, "statement outside of 'case'.", .{});
                    std.os.exit(1);
                }

                var substmt = top_level_case;
                while (true) {
                    var case = &substmt.payload.Case;
                    var case_type = typecheck_expr(ast, cond_type, case.expr);
                    if (!cond_type.eql(case_type)) {
                        common.print_error(ast.filepath, case.expr.line_info, "mismatched types: '{}' and '{}'.", .{ cond_type, case_type });
                        common.print_note(ast.filepath, case.expr.line_info, "switch case is here.", .{});
                        common.print_note(ast.filepath, _switch.cond.line_info, "switch condition is here.", .{});
                        std.os.exit(1);
                    }

                    substmt = case.stmt;
                    if (substmt.payload != .Case) break;
                }
                typecheck_stmt(ast, ctx, substmt);
            }
        },
        .Case => |case| {
            common.print_error(ast.filepath, case.expr.line_info, "case outside of switch statement.", .{});
            std.os.exit(1);
        },
        .Return => {
            if (ctx.return_type.payload != .Void) {
                common.print_error(ast.filepath, stmt.line_info, "expected expression of type '{}'.", .{ctx.return_type});
                std.os.exit(1);
            }
        },
        .Return_Expr => |expr| {
            var expr_type = typecheck_expr(ast, ctx.return_type, expr);
            if (ctx.return_type.payload == .Void) {
                common.print_error(ast.filepath, expr.line_info, "unexpected expression here.", .{});
                std.os.exit(1);
            } else if (!ctx.return_type.eql(expr_type)) {
                common.print_error(ast.filepath, expr.line_info, "mismatched types: '{}' and '{}'.", .{ ctx.return_type, expr_type });
                common.print_note(ast.filepath, ctx.return_type.line_info, "return type is here.", .{});
                common.print_note(ast.filepath, expr.line_info, "expression is here.", .{});
                std.os.exit(1);
            }
        },
        .Symbol => |symbol| {
            typecheck_symbol(ast, symbol);
        },
        .Assign => |assign| {
            var lhs_type = typecheck_expr(ast, null, assign.lhs);
            var rhs_type = typecheck_expr(ast, lhs_type, assign.rhs);

            if (!assign.lhs.is_lvalue) {
                common.print_error(ast.filepath, assign.lhs.line_info, "expression is not an lvalue.", .{});
                std.os.exit(1);
            }

            if (rhs_type.payload == .Function) {
                if (rhs_type.payload.Function.scope != ast.global_scope) {
                    common.print_error(ast.filepath, assign.rhs.line_info, "can't use function pointers to local functions.", .{});
                    std.os.exit(1);
                }
            }

            if (!lhs_type.eql(rhs_type)) {
                common.print_error(ast.filepath, stmt.line_info, "expected '{}', but got '{}'.", .{ lhs_type, rhs_type });
                std.os.exit(1);
            }
        },
        .Expr => |expr| {
            _ = typecheck_expr_rec(ast, null, expr);
        },
    }
}

fn typecheck_expr(ast: *Ast, type_hint: ?*Ast.Type, expr: *Ast.Expr) *Ast.Type {
    var _type = typecheck_expr_rec(ast, type_hint, expr);
    if (_type.payload == .Void) {
        common.print_error(ast.filepath, expr.line_info, "unexpected 'void' type.", .{});
        std.os.exit(1);
    }
    return _type;
}

fn typecheck_expr_rec(ast: *Ast, type_hint: ?*Ast.Type, expr: *Ast.Expr) *Ast.Type {
    var result = typecheck_expr_rec_aux(ast, type_hint, expr);
    expr._type = result;
    return result;
}

fn typecheck_expr_rec_aux(ast: *Ast, type_hint: ?*Ast.Type, expr: *Ast.Expr) *Ast.Type {
    switch (expr.payload) {
        .Binary_Op => |op| {
            switch (op.tag) {
                .Or, .And => {
                    var lhs_type = typecheck_expr_rec(ast, &BOOL_TYPE_HINT, op.lhs);
                    var rhs_type = typecheck_expr_rec(ast, &BOOL_TYPE_HINT, op.rhs);

                    if (lhs_type.payload != .Bool or rhs_type.payload != .Bool) {
                        common.print_error(ast.filepath, op.lhs.line_info, "mismatched types: '{}' and '{}'.", .{ lhs_type, rhs_type });
                        std.os.exit(1);
                    }

                    return &BOOL_TYPE_HINT;
                },
                .Eq, .Neq => {
                    var lhs_type = typecheck_expr_rec(ast, null, op.lhs);
                    var rhs_type = typecheck_expr_rec(ast, lhs_type, op.rhs);
                    var lhs_flags = lhs_type.compare();

                    if (!lhs_flags.is_comparable) {
                        common.print_error(ast.filepath, op.lhs.line_info, "expression is not comparable: '{}'.", .{lhs_type});
                        std.os.exit(1);
                    } else if (!lhs_type.eql(rhs_type)) {
                        common.print_error(ast.filepath, op.lhs.line_info, "mismatched types: '{}' and '{}'.", .{ lhs_type, rhs_type });
                        std.os.exit(1);
                    }

                    return &BOOL_TYPE_HINT;
                },
                .Lt, .Leq, .Gt, .Geq => {
                    var lhs_type = typecheck_expr_rec(ast, null, op.lhs);
                    var rhs_type = typecheck_expr_rec(ast, lhs_type, op.rhs);
                    var lhs_flags = lhs_type.compare();

                    if (!lhs_flags.is_integer) {
                        common.print_error(ast.filepath, op.lhs.line_info, "expected integral type, but got '{}'.", .{lhs_type});
                        std.os.exit(1);
                    } else if (!lhs_type.eql(rhs_type)) {
                        common.print_error(ast.filepath, op.lhs.line_info, "mismatched types: '{}' and '{}'.", .{ lhs_type, rhs_type });
                        std.os.exit(1);
                    }

                    return &BOOL_TYPE_HINT;
                },
                .Add => {
                    var lhs_type = typecheck_expr_rec(ast, null, op.lhs);
                    var rhs_type = typecheck_expr_rec(ast, lhs_type, op.rhs);
                    var lhs_flags = lhs_type.compare();
                    var rhs_flags = rhs_type.compare();

                    if (lhs_flags.is_void_ptr or rhs_flags.is_void_ptr) {
                        common.print_error(ast.filepath, op.lhs.line_info, "can't add 'void' pointers.", .{});
                        std.os.exit(1);
                    } else if (lhs_flags.is_ptr and rhs_flags.is_integer) {
                        return lhs_type;
                    } else if (lhs_flags.is_integer and rhs_flags.is_ptr) {
                        return rhs_type;
                    } else if (lhs_flags.is_integer and lhs_type.eql(rhs_type)) {
                        return lhs_type;
                    } else {
                        common.print_error(ast.filepath, op.lhs.line_info, "expected integer/pointer: '{}' and '{}'", .{ lhs_type, rhs_type });
                        std.os.exit(1);
                    }
                },
                .Sub => {
                    var lhs_type = typecheck_expr_rec(ast, null, op.lhs);
                    var rhs_type = typecheck_expr_rec(ast, lhs_type, op.rhs);
                    var lhs_flags = lhs_type.compare();
                    var rhs_flags = rhs_type.compare();

                    if (lhs_flags.is_void_ptr) {
                        common.print_error(ast.filepath, op.lhs.line_info, "can't subtract 'void' pointer.", .{});
                        std.os.exit(1);
                    } else if (lhs_flags.is_ptr and rhs_flags.is_integer) {
                        return lhs_type;
                    } else if (lhs_flags.is_integer and lhs_type.eql(rhs_type)) {
                        return lhs_type;
                    } else {
                        common.print_error(ast.filepath, op.lhs.line_info, "expected integer/pointer: '{}' and '{}'", .{ lhs_type, rhs_type });
                        std.os.exit(1);
                    }
                },
                .Mul, .Div, .Mod => {
                    var lhs_type = typecheck_expr_rec(ast, null, op.lhs);
                    var rhs_type = typecheck_expr_rec(ast, lhs_type, op.rhs);
                    var lhs_flags = lhs_type.compare();

                    if (!lhs_flags.is_integer or !lhs_type.eql(rhs_type)) {
                        common.print_error(ast.filepath, op.lhs.line_info, "expected integer: '{}' and '{}'.", .{ lhs_type, rhs_type });
                        std.os.exit(1);
                    }

                    return lhs_type;
                },
            }
        },
        .Unary_Op => |op| {
            switch (op.tag) {
                .Not => {
                    var subexpr_type = typecheck_expr_rec(ast, &BOOL_TYPE_HINT, op.subexpr);

                    if (subexpr_type.payload != .Bool) {
                        common.print_error(ast.filepath, op.subexpr.line_info, "expected 'bool', but got '{}'.", .{subexpr_type});
                        std.os.exit(1);
                    }

                    return &BOOL_TYPE_HINT;
                },
                .Neg => {
                    var subexpr_type = typecheck_expr_rec(ast, null, op.subexpr);
                    var subexpr_flags = subexpr_type.compare();

                    if (!subexpr_flags.is_integer) {
                        common.print_error(ast.filepath, op.subexpr.line_info, "expected integer, but got '{}'.", .{subexpr_type});
                        std.os.exit(1);
                    }

                    return subexpr_type;
                },
                .Ref => {
                    var subexpr_type = typecheck_expr_rec(ast, null, op.subexpr);

                    if (!op.subexpr.is_lvalue) {
                        common.print_error(ast.filepath, op.subexpr.line_info, "expression is not an lvalue.", .{});
                        std.os.exit(1);
                    }

                    var result = ast.create(Ast.Type);
                    result.* = .{
                        .payload = .{ .Pointer = subexpr_type },
                        .size = 8,
                        .typechecking_stage = .Fully_Typechecked,
                        .line_info = expr.line_info,
                    };

                    return result;
                },
                .Deref => {
                    var subexpr_type = typecheck_expr_rec(ast, null, op.subexpr);
                    var subexpr_flags = subexpr_type.compare();

                    if (!subexpr_flags.can_be_dereferenced) {
                        common.print_error(ast.filepath, op.subexpr.line_info, "can't dereference value of type '{}'.", .{subexpr_type});
                        std.os.exit(1);
                    }

                    expr.is_lvalue = true;

                    return subexpr_type.payload.Pointer;
                },
            }
        },
        .If => |_if| {
            var cond_type = typecheck_expr_rec(ast, &BOOL_TYPE_HINT, _if.cond);

            if (cond_type.payload != .Bool) {
                common.print_error(ast.filepath, _if.cond.line_info, "expected 'bool', but got '{}'.", .{cond_type});
                std.os.exit(1);
            }

            var if_true_type = typecheck_expr_rec(ast, type_hint, _if.if_true);
            var if_false_type = typecheck_expr_rec(ast, type_hint, _if.if_false);

            if (!if_true_type.eql(if_false_type)) {
                common.print_error(ast.filepath, _if.cond.line_info, "mismatched types: '{}' and '{}'.", .{ if_true_type, if_false_type });
                std.os.exit(1);
            }

            return if_true_type;
        },
        .Call => |call| {
            var lhs_type = typecheck_expr_rec(ast, null, call.lhs);

            if (lhs_type.payload != .Function) {
                common.print_error(ast.filepath, call.lhs.line_info, "expected a function, but got '{}'.", .{lhs_type});
                std.os.exit(1);
            }

            var function = &lhs_type.payload.Function;
            var params = function.params;

            if (params.count != call.args.count) {
                common.print_error(ast.filepath, call.lhs.line_info, "expected {} arguments, but got {}.", .{ params.count, call.args.count });
                std.os.exit(1);
            }

            var pit = params.iterator();
            var ait = call.args.iterator();
            while (ait.next()) |arg| {
                var param = pit.next().?;
                var param_type = param.*.payload.Parameter._type;
                var arg_type = typecheck_expr(ast, param_type, arg);

                if (!param_type.eql(arg_type)) {
                    common.print_error(ast.filepath, arg.line_info, "expected '{}', but got '{}'.", .{ param_type, arg_type });
                    std.os.exit(1);
                }
            }

            return function.return_type;
        },
        .Index => |index| {
            var lhs_type = typecheck_expr_rec(ast, null, index.lhs);
            var index_type = typecheck_expr_rec(ast, &INT64_TYPE_HINT, index.index);
            var index_flags = index_type.compare();

            if (!index_flags.is_integer) {
                common.print_error(ast.filepath, index.lhs.line_info, "expected integer, but got '{}'.", .{index_type});
                std.os.exit(1);
            }

            expr.is_lvalue = true;

            switch (lhs_type.payload) {
                .Array => |array| return array.subtype,
                .Pointer => |subtype| {
                    switch (subtype.payload) {
                        .Array => |array| return array.subtype,
                        else => return subtype,
                    }
                },
                else => {
                    common.print_error(ast.filepath, index.lhs.line_info, "expected array or pointer to array, but got '{}'.", .{lhs_type});
                    std.os.exit(1);
                },
            }
        },
        .Field => |field| {
            var lhs_type = typecheck_expr_rec(ast, null, field.lhs);

            expr.is_lvalue = true;

            var _struct: *Ast.TypeStruct = _struct: {
                switch (lhs_type.payload) {
                    .Struct, .Union => |*struct_ptr| break :_struct struct_ptr,
                    .Pointer => |subtype| {
                        switch (subtype.payload) {
                            .Struct, .Union => |*struct_ptr| break :_struct struct_ptr,
                            else => {
                                common.print_error(ast.filepath, field.lhs.line_info, "expected pointer to struct/union, but got '{}'.", .{lhs_type});
                                std.os.exit(1);
                            },
                        }
                    },
                    else => {
                        common.print_error(ast.filepath, field.lhs.line_info, "expected struct/union, but got '{}'.", .{lhs_type});
                        std.os.exit(1);
                    },
                }
            };

            var key = Ast.SymbolKey{
                .text = field.id.text,
                .scope = _struct.scope,
            };
            var found_symbol = ast.symbols.get(key);

            if (found_symbol) |symbol| {
                switch (symbol.payload) {
                    .Struct_Field => |struct_field| {
                        return struct_field._type;
                    },
                    else => unreachable,
                }
            } else {
                common.print_error(ast.filepath, field.lhs.line_info, "field '{s}' is not a member of a struct/union.", .{key.text});
                std.os.exit(1);
            }
        },
        .Initializer => |init| {
            typecheck_type(ast, init._type);

            var subexpr = Ast.Expr{
                .payload = .{ .Expr_List = init.expr_list },
                ._type = undefined,
                .line_info = expr.line_info,
            };
            _ = typecheck_expr_rec(ast, init._type, &subexpr);

            return init._type;
        },
        .Expr_List => |list| {
            if (type_hint) |hint| {
                switch (hint.payload) {
                    .Array => |array| {
                        if (list.count != array.count) {
                            common.print_error(ast.filepath, expr.line_info, "expected {} elements, but got {}.", .{ array.count, list.count });
                            std.os.exit(1);
                        }

                        var subhint = array.subtype;
                        var it = list.iterator();
                        while (it.next()) |subexpr| {
                            if (subexpr.payload == .Designator) {
                                common.print_error(ast.filepath, subexpr.line_info, "expected expression, but got designator.", .{});
                                std.os.exit(1);
                            }

                            var subexpr_type = typecheck_expr_rec(ast, subhint, subexpr);
                            if (!subexpr_type.eql(subhint)) {
                                common.print_error(ast.filepath, subexpr.line_info, "expected '{}', but got '{}'.", .{ subhint, subexpr_type });
                                std.os.exit(1);
                            }
                        }
                    },
                    .Struct, .Union => |_struct| {
                        var it = list.iterator();
                        while (it.next()) |subexpr| {
                            if (subexpr.payload != .Designator) {
                                common.print_error(ast.filepath, subexpr.line_info, "expected designator, but got expression.", .{});
                                std.os.exit(1);
                            }

                            var designator = &subexpr.payload.Designator;
                            var key = Ast.SymbolKey{
                                .text = designator.id.text,
                                .scope = _struct.scope,
                            };
                            var found_symbol = ast.symbols.get(key);

                            if (found_symbol) |symbol| {
                                var subhint = switch (symbol.payload) {
                                    .Struct_Field => |field| field._type,
                                    else => unreachable,
                                };
                                var subexpr_type = typecheck_expr_rec(ast, subhint, designator.expr);
                                if (!subhint.eql(subexpr_type)) {
                                    common.print_error(ast.filepath, designator.expr.line_info, "expected '{}', but got '{}'.", .{ subhint, subexpr_type });
                                    std.os.exit(1);
                                }
                            } else {
                                common.print_error(ast.filepath, designator.id.line_info, "field '{s}' is not a member of a struct/union.", .{designator.id.text});
                                std.os.exit(1);
                            }
                        }
                    },
                    else => {
                        common.print_error(ast.filepath, expr.line_info, "expected '{}', but got expression list.", .{hint});
                        std.os.exit(1);
                    },
                }

                return hint;
            } else {
                common.print_error(ast.filepath, expr.line_info, "can't infere the type of expression list.", .{});
                std.os.exit(1);
            }
        },
        .Designator => unreachable,
        .Enum_Field_From_Type => |field| {
            typecheck_type(ast, field._type);

            var _type = field._type;
            var subexpr = Ast.Expr{
                .payload = .{ .Enum_Field = field.id },
                ._type = undefined,
                .line_info = field.id.line_info,
            };
            _ = typecheck_expr_rec(ast, _type, &subexpr);
            expr.payload = subexpr.payload;
            return _type;
        },
        .Enum_Field => |id| {
            if (type_hint) |hint| {
                switch (hint.payload) {
                    .Enum => |_enum| {
                        var key = Ast.SymbolKey{
                            .text = id.text,
                            .scope = _enum.scope,
                        };
                        var found_symbol = ast.symbols.get(key);

                        if (found_symbol) |symbol| {
                            expr.payload = .{ .Symbol = symbol };
                            return hint;
                        } else {
                            common.print_error(ast.filepath, expr.line_info, "enumerator '{s}' is not defined.", .{id.text});
                            std.os.exit(1);
                        }
                    },
                    else => {
                        common.print_error(ast.filepath, expr.line_info, "expected '{}', but got enum value.", .{hint});
                        std.os.exit(1);
                    },
                }
            } else {
                common.print_error(ast.filepath, expr.line_info, "can't infere the type of enumerator.", .{});
                std.os.exit(1);
            }
        },
        .Cast1 => |subexpr| {
            if (type_hint) |hint| {
                var payload = cast_expr(ast, hint, subexpr);
                expr.payload = payload;
                expr._type = hint;
                return hint;
            } else {
                common.print_error(ast.filepath, expr.line_info, "can't infere the type to cast to.", .{});
                std.os.exit(1);
            }
        },
        .Cast2 => |cast| {
            typecheck_type(ast, cast._type);
            var payload = cast_expr(ast, cast._type, cast.expr);
            expr.payload = payload;
            expr._type = cast._type;
            return cast._type;
        },
        .Bool => {
            return &BOOL_TYPE_HINT;
        },
        .Int64 => {
            return &INT64_TYPE_HINT;
        },
        .Null => {
            return &VOID_PTR_TYPE_HINT;
        },
        .Type => {
            common.print_error(ast.filepath, expr.line_info, "unexpected type.", .{});
            std.os.exit(1);
        },
        .Symbol => |symbol| {
            switch (symbol.payload) {
                .Variable => |variable| {
                    if (!variable.was_visited) {
                        common.print_error(ast.filepath, expr.line_info, "can't use variable in its own definition.", .{});
                        std.os.exit(1);
                    }

                    expr.is_lvalue = true;

                    return variable._type.?;
                },
                .Parameter => |parameter| {
                    expr.is_lvalue = true;
                    return parameter._type;
                },
                .Function => |function| {
                    typecheck_type(ast, function._type);
                    return function._type;
                },
                .Type, .Struct_Field, .Enum_Field, .Definition => unreachable,
            }
        },
        .Identifier => unreachable,
    }
}

fn cast_expr(ast: *Ast, _type: *Ast.Type, expr: *Ast.Expr) Ast.ExprPayload {
    var expr_type = typecheck_expr_rec(ast, null, expr);

    switch (_type.payload) {
        .Array => {
            switch (expr_type.payload) {
                .Pointer, .Array => return expr.payload,
                else => {},
            }
        },
        .Pointer => {
            switch (expr_type.payload) {
                .Array, .Enum, .Function, .Pointer, .Bool, .Int64 => return expr.payload,
                else => {},
            }
        },
        .Enum, .Function, .Bool, .Int64 => {
            switch (expr_type.payload) {
                .Enum, .Function, .Pointer, .Bool, .Int64 => return expr.payload,
                else => {},
            }
        },
        .Struct, .Union, .Void => {},
        .Identifier => unreachable,
    }

    common.print_error(ast.filepath, expr.line_info, "can't cast '{}' to '{}'.", .{ expr_type, _type });
    std.os.exit(1);
}
