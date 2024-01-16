const std = @import("std");
const common = @import("common.zig");
const Ast = @import("ast.zig");

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

const TypecheckerStmtContext = struct {
    return_type: *Ast.Type,
    is_in_loop: bool,
};

inline fn min_enum(x: anytype, y: @TypeOf(x)) @TypeOf(x) {
    return @enumFromInt(@min(@intFromEnum(x), @intFromEnum(y)));
}

inline fn check_flags(actual: anytype, expected: @TypeOf(actual)) bool {
    return actual & expected == expected;
}

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

            var return_type = _type.return_type.extract_ptr();

            if (!return_type.is(.Void)) {
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
                    var expected_type = _type.extract_ptr();
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
                .return_type = function._type.payload.Function.return_type.extract_ptr(),
                .is_in_loop = false,
            };
            var it = function.block.iterator();
            while (it.next()) |stmt| {
                typecheck_stmt(ast, &ctx, stmt);
            }
        },
        .Type => |_type| {
            typecheck_type_flags(ast, _type, 0);
        },
        .Struct_Field,
        .Union_Field,
        .Enum_Field,
        .Definition,
        => unreachable,
    }
}

const REJECT_VOID_TYPE: u8 = 0x1;
const DO_SHALLOW_TYPECHECK: u8 = 0x2;

inline fn typecheck_type(ast: *Ast, _type: *Ast.Type) void {
    typecheck_type_flags(ast, _type, REJECT_VOID_TYPE);
}

fn typecheck_type_flags(ast: *Ast, _type: *Ast.Type, flags: u8) void {
    _ = typecheck_type_aux(ast, _type, flags | DO_SHALLOW_TYPECHECK);
    _ = typecheck_type_aux(ast, _type, flags & ~DO_SHALLOW_TYPECHECK);
}

fn typecheck_type_aux(ast: *Ast, _type: *Ast.Type, flags: u8) Ast.TypecheckingStage {
    var _flags = flags;
    var result: Ast.TypecheckingStage = .Fully_Typechecked;

    switch (_type.typechecking_stage) {
        .Not_Typechecked => {
            _type.typechecking_stage = .Being_Typechecked;
        },
        .Being_Typechecked => {
            common.print_error(ast.filepath, _type.line_info, "cyclic reference detected.", .{});
            std.os.exit(1);
        },
        .Shallow_Typechecked => {
            if (check_flags(_flags, DO_SHALLOW_TYPECHECK)) {
                return .Shallow_Typechecked;
            }
        },
        .Fully_Typechecked => {
            return .Fully_Typechecked;
        },
    }

    switch (_type.payload) {
        .Struct => |_struct| {
            var size: Ast.TypeSize = 0;

            _flags |= REJECT_VOID_TYPE;

            var it = _struct.fields.iterator();
            while (it.next()) |field_ptr| {
                var field = &field_ptr.*.payload.Struct_Field;
                var stage = typecheck_type_aux(ast, field._type, _flags);
                result = min_enum(result, stage);
                size += field._type.size;
            }

            _type.size = size;
        },
        .Union => |_union| {
            var size: Ast.TypeSize = 0;

            _flags |= REJECT_VOID_TYPE;

            var it = _union.fields.iterator();
            while (it.next()) |field_ptr| {
                var field = &field_ptr.*.payload.Union_Field;
                var stage = typecheck_type_aux(ast, field._type, _flags);
                result = min_enum(result, stage);
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

            if (check_flags(_flags, DO_SHALLOW_TYPECHECK)) {
                result = .Shallow_Typechecked;
            } else {
                _flags |= REJECT_VOID_TYPE;

                var it = function.params.iterator();
                while (it.next()) |param| {
                    var stage = typecheck_type_aux(ast, param.*.payload.Parameter._type, _flags);
                    result = min_enum(result, stage);
                }

                _flags &= ~REJECT_VOID_TYPE;
                var stage = typecheck_type_aux(ast, function.return_type, _flags);
                result = min_enum(result, stage);
            }
        },
        .Array => |*array| {
            var size_type = typecheck_expr(ast, &INT64_TYPE_HINT, array.expr);
            var size_flags = size_type.compare();

            if (!check_flags(size_flags, Ast.Type.Is_Integer)) {
                common.print_error(ast.filepath, array.expr.line_info, "expected integer, but got '{}'.", .{size_type});
                std.os.exit(1);
            }

            _flags |= REJECT_VOID_TYPE;
            var stage = typecheck_type_aux(ast, array.subtype, _flags);
            result = min_enum(result, stage);

            reduce_expr(ast, array.expr);
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

            if (check_flags(_flags, DO_SHALLOW_TYPECHECK)) {
                result = .Shallow_Typechecked;
            } else {
                _flags &= ~REJECT_VOID_TYPE;
                var stage = typecheck_type_aux(ast, subtype, _flags);
                result = min_enum(result, stage);
            }
        },
        .Void => {
            if (check_flags(_flags, REJECT_VOID_TYPE)) {
                common.print_error(ast.filepath, _type.line_info, "unexpected 'void' type.", .{});
                std.os.exit(1);
            }

            _type.size = 0;
        },
        .Bool => {
            _type.size = 1;
        },
        .Int64 => {
            _type.size = 8;
        },
        .Symbol => |symbol| {
            var subtype = symbol.payload.Type;
            switch (subtype.typechecking_stage) {
                .Not_Typechecked => {
                    result = typecheck_type_aux(ast, subtype, _flags);
                },
                .Being_Typechecked => {
                    common.print_error(ast.filepath, symbol.line_info, "cyclic reference detected.", .{});
                    std.os.exit(1);
                },
                .Shallow_Typechecked,
                .Fully_Typechecked,
                => {},
            }
            _type.size = subtype.size;

            if (check_flags(_flags, REJECT_VOID_TYPE) and subtype.is(.Void)) {
                common.print_error(ast.filepath, symbol.line_info, "unexpected 'void' type.", .{});
                std.os.exit(1);
            }
        },
        .Identifier => unreachable,
    }

    _type.typechecking_stage = result;

    return result;
}

fn typecheck_block(ast: *Ast, ctx: *const TypecheckerStmtContext, block: Ast.StmtBlock) void {
    var it = block.stmts.iterator();
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
            if (!cond_type.is(.Bool)) {
                common.print_error(ast.filepath, _if.cond.line_info, "expected 'bool', but got '{}'.", .{cond_type});
                std.os.exit(1);
            }

            typecheck_block(ast, ctx, _if.if_true);
            typecheck_block(ast, ctx, _if.if_false);
        },
        .While => |_while| {
            var cond_type = typecheck_expr(ast, &BOOL_TYPE_HINT, _while.cond);
            if (!cond_type.is(.Bool)) {
                common.print_error(ast.filepath, _while.cond.line_info, "expected 'bool', but got '{}'.", .{cond_type});
                std.os.exit(1);
            }

            var new_ctx = ctx.*;
            new_ctx.is_in_loop = true;
            typecheck_block(ast, &new_ctx, _while.block);
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

            if (!check_flags(cond_flags, Ast.Type.Is_Comparable)) {
                common.print_error(ast.filepath, _switch.cond.line_info, "condition isn't comparable: '{}'.", .{cond_type});
                std.os.exit(1);
            }

            var it = _switch.cases.iterator();
            while (it.next()) |case| {
                var case_type = typecheck_expr(ast, cond_type, case.value);
                if (!cond_type.eql(case_type)) {
                    common.print_error(ast.filepath, case.value.line_info, "mismatched types: '{}' and '{}'.", .{ cond_type, case_type });
                    common.print_note(ast.filepath, case.value.line_info, "switch case is here.", .{});
                    common.print_note(ast.filepath, _switch.cond.line_info, "switch condition is here.", .{});
                    std.os.exit(1);
                }

                typecheck_block(ast, ctx, case.block);
            }
        },
        .Return => {
            if (!ctx.return_type.is(.Void)) {
                common.print_error(ast.filepath, stmt.line_info, "expected expression of type '{}'.", .{ctx.return_type});
                std.os.exit(1);
            }
        },
        .Return_Expr => |expr| {
            var expr_type = typecheck_expr(ast, ctx.return_type, expr);
            if (ctx.return_type.is(.Void)) {
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
            _ = typecheck_expr_allow_void(ast, null, expr);
        },
    }
}

fn typecheck_expr(ast: *Ast, type_hint: ?*Ast.Type, expr: *Ast.Expr) *Ast.Type {
    var _type = typecheck_expr_allow_void(ast, type_hint, expr);
    if (_type.is(.Void)) {
        common.print_error(ast.filepath, expr.line_info, "unexpected 'void' type.", .{});
        std.os.exit(1);
    }
    return _type;
}

fn typecheck_expr_allow_void(ast: *Ast, type_hint: ?*Ast.Type, expr: *Ast.Expr) *Ast.Type {
    switch (expr.payload) {
        .Binary_Op => |op| {
            switch (op.tag) {
                .Or,
                .And,
                => {
                    var lhs_type = typecheck_expr(ast, &BOOL_TYPE_HINT, op.lhs);
                    var rhs_type = typecheck_expr(ast, &BOOL_TYPE_HINT, op.rhs);

                    if (!lhs_type.is(.Bool) or !rhs_type.is(.Bool)) {
                        common.print_error(ast.filepath, op.lhs.line_info, "mismatched types: '{}' and '{}'.", .{ lhs_type, rhs_type });
                        std.os.exit(1);
                    }

                    expr._type = &BOOL_TYPE_HINT;
                },
                .Eq,
                .Neq,
                => {
                    var lhs_type = typecheck_expr(ast, null, op.lhs);
                    var rhs_type = typecheck_expr(ast, lhs_type, op.rhs);
                    var lhs_flags = lhs_type.compare();

                    if (!check_flags(lhs_flags, Ast.Type.Is_Comparable)) {
                        common.print_error(ast.filepath, op.lhs.line_info, "expression is not comparable: '{}'.", .{lhs_type});
                        std.os.exit(1);
                    } else if (!lhs_type.eql(rhs_type)) {
                        common.print_error(ast.filepath, op.lhs.line_info, "mismatched types: '{}' and '{}'.", .{ lhs_type, rhs_type });
                        std.os.exit(1);
                    }

                    expr._type = &BOOL_TYPE_HINT;
                },
                .Lt,
                .Leq,
                .Gt,
                .Geq,
                => {
                    var lhs_type = typecheck_expr(ast, null, op.lhs);
                    var rhs_type = typecheck_expr(ast, lhs_type, op.rhs);
                    var lhs_flags = lhs_type.compare();

                    if (!check_flags(lhs_flags, Ast.Type.Is_Integral)) {
                        common.print_error(ast.filepath, op.lhs.line_info, "expected integral type, but got '{}'.", .{lhs_type});
                        std.os.exit(1);
                    } else if (!lhs_type.eql(rhs_type)) {
                        common.print_error(ast.filepath, op.lhs.line_info, "mismatched types: '{}' and '{}'.", .{ lhs_type, rhs_type });
                        std.os.exit(1);
                    }

                    expr._type = &BOOL_TYPE_HINT;
                },
                .Add => {
                    var lhs_type = typecheck_expr(ast, null, op.lhs);
                    var rhs_type = typecheck_expr(ast, lhs_type, op.rhs);
                    var lhs_flags = lhs_type.compare();
                    var rhs_flags = rhs_type.compare();

                    if (check_flags(lhs_flags, Ast.Type.Is_Void_Ptr) or check_flags(rhs_flags, Ast.Type.Is_Void_Ptr)) {
                        common.print_error(ast.filepath, op.lhs.line_info, "can't add 'void' pointers.", .{});
                        std.os.exit(1);
                    } else if (check_flags(lhs_flags, Ast.Type.Is_Ptr) and check_flags(rhs_flags, Ast.Type.Is_Integer)) {
                        expr._type = lhs_type;
                    } else if (check_flags(lhs_flags, Ast.Type.Is_Integer) and check_flags(rhs_flags, Ast.Type.Is_Ptr)) {
                        expr._type = rhs_type;
                    } else if (check_flags(lhs_flags, Ast.Type.Is_Integer) and lhs_type.eql(rhs_type)) {
                        expr._type = lhs_type;
                    } else {
                        common.print_error(ast.filepath, op.lhs.line_info, "expected integer/pointer: '{}' and '{}'", .{ lhs_type, rhs_type });
                        std.os.exit(1);
                    }
                },
                .Sub => {
                    var lhs_type = typecheck_expr(ast, null, op.lhs);
                    var rhs_type = typecheck_expr(ast, lhs_type, op.rhs);
                    var lhs_flags = lhs_type.compare();
                    var rhs_flags = rhs_type.compare();

                    if (check_flags(lhs_flags, Ast.Type.Is_Void_Ptr)) {
                        common.print_error(ast.filepath, op.lhs.line_info, "can't subtract 'void' pointer.", .{});
                        std.os.exit(1);
                    } else if (check_flags(lhs_flags, Ast.Type.Is_Ptr) and check_flags(rhs_flags, Ast.Type.Is_Integer)) {
                        expr._type = lhs_type;
                    } else if (check_flags(lhs_flags, Ast.Type.Is_Integer) and lhs_type.eql(rhs_type)) {
                        expr._type = lhs_type;
                    } else {
                        common.print_error(ast.filepath, op.lhs.line_info, "expected integer/pointer: '{}' and '{}'", .{ lhs_type, rhs_type });
                        std.os.exit(1);
                    }
                },
                .Mul,
                .Div,
                .Mod,
                => {
                    var lhs_type = typecheck_expr(ast, null, op.lhs);
                    var rhs_type = typecheck_expr(ast, lhs_type, op.rhs);
                    var lhs_flags = lhs_type.compare();

                    if (!check_flags(lhs_flags, Ast.Type.Is_Integer) or !lhs_type.eql(rhs_type)) {
                        common.print_error(ast.filepath, op.lhs.line_info, "expected integer: '{}' and '{}'.", .{ lhs_type, rhs_type });
                        std.os.exit(1);
                    }

                    expr._type = lhs_type;
                },
            }
        },
        .Unary_Op => |op| {
            switch (op.tag) {
                .Not => {
                    var subexpr_type = typecheck_expr(ast, &BOOL_TYPE_HINT, op.subexpr);

                    if (!subexpr_type.is(.Bool)) {
                        common.print_error(ast.filepath, op.subexpr.line_info, "expected 'bool', but got '{}'.", .{subexpr_type});
                        std.os.exit(1);
                    }

                    expr._type = &BOOL_TYPE_HINT;
                },
                .Neg => {
                    var subexpr_type = typecheck_expr(ast, null, op.subexpr);
                    var subexpr_flags = subexpr_type.compare();

                    if (!check_flags(subexpr_flags, Ast.Type.Is_Integer)) {
                        common.print_error(ast.filepath, op.subexpr.line_info, "expected integer, but got '{}'.", .{subexpr_type});
                        std.os.exit(1);
                    }

                    expr._type = subexpr_type;
                },
                .Ref => {
                    var subexpr_type = typecheck_expr(ast, null, op.subexpr);

                    if (!op.subexpr.is_lvalue) {
                        common.print_error(ast.filepath, op.subexpr.line_info, "expression is not an lvalue.", .{});
                        std.os.exit(1);
                    }

                    expr._type = ast.ast_create(Ast.Type);
                    expr._type.* = .{
                        .payload = .{ .Pointer = subexpr_type },
                        .size = 8,
                        .typechecking_stage = .Fully_Typechecked,
                        .line_info = expr.line_info,
                    };
                },
                .Deref => {
                    var subexpr_type = typecheck_expr(ast, null, op.subexpr);
                    var subexpr_flags = subexpr_type.compare();

                    if (!check_flags(subexpr_flags, Ast.Type.Can_Be_Dereferenced)) {
                        common.print_error(ast.filepath, op.subexpr.line_info, "can't dereference value of type '{}'.", .{subexpr_type});
                        std.os.exit(1);
                    }

                    expr.is_lvalue = true;

                    expr._type = subexpr_type.payload.Pointer.extract_ptr();
                },
            }
        },
        .If => |_if| {
            var cond_type = typecheck_expr(ast, &BOOL_TYPE_HINT, _if.cond);
            if (!cond_type.is(.Bool)) {
                common.print_error(ast.filepath, _if.cond.line_info, "expected 'bool', but got '{}'.", .{cond_type});
                std.os.exit(1);
            }

            var if_true_type = typecheck_expr(ast, type_hint, _if.if_true);
            var if_false_type = typecheck_expr(ast, type_hint, _if.if_false);

            if (!if_true_type.eql(if_false_type)) {
                common.print_error(ast.filepath, _if.cond.line_info, "mismatched types: '{}' and '{}'.", .{ if_true_type, if_false_type });
                std.os.exit(1);
            }

            expr._type = if_true_type;
        },
        .Call => |call| {
            var lhs_type = typecheck_expr(ast, null, call.lhs);
            if (!lhs_type.is(.Function)) {
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
                var param_type = param.*.payload.Parameter._type.extract_ptr();
                var arg_type = typecheck_expr(ast, param_type, arg);

                if (!param_type.eql(arg_type)) {
                    common.print_error(ast.filepath, arg.line_info, "expected '{}', but got '{}'.", .{ param_type, arg_type });
                    std.os.exit(1);
                }
            }

            expr._type = function.return_type.extract_ptr();
        },
        .Index => |index| {
            var lhs_type = typecheck_expr(ast, null, index.lhs);
            var index_type = typecheck_expr(ast, &INT64_TYPE_HINT, index.index);
            var index_flags = index_type.compare();

            if (!check_flags(index_flags, Ast.Type.Is_Integer)) {
                common.print_error(ast.filepath, index.lhs.line_info, "expected integer, but got '{}'.", .{index_type});
                std.os.exit(1);
            }

            expr.is_lvalue = true;

            switch (lhs_type.payload) {
                .Array => |array| {
                    expr._type = array.subtype.extract_ptr();
                },
                .Pointer => {
                    var subtype = lhs_type.payload.Pointer.extract_ptr();
                    if (subtype.payload == .Array) {
                        expr._type = subtype.payload.Array.subtype.extract_ptr();
                    } else {
                        expr._type = subtype;
                    }
                },
                else => {
                    common.print_error(ast.filepath, index.lhs.line_info, "expected array or pointer to array, but got '{}'.", .{lhs_type});
                    std.os.exit(1);
                },
            }
        },
        .Field => |field| {
            var lhs_type = typecheck_expr(ast, null, field.lhs);

            expr.is_lvalue = true;

            var _struct: *Ast.TypeStruct = undefined;

            switch (lhs_type.payload) {
                .Struct, .Union => |*struct_ptr| {
                    _struct = struct_ptr;
                },
                .Pointer => {
                    var subtype = lhs_type.payload.Pointer.extract_ptr();
                    switch (subtype.payload) {
                        .Struct,
                        .Union,
                        => |*struct_ptr| {
                            _struct = struct_ptr;
                        },
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

            var key = Ast.SymbolKey{
                .text = field.id.text,
                .scope = _struct.scope,
            };
            var found_symbol = ast.symbols.get(key);

            if (found_symbol) |symbol| {
                switch (symbol.payload) {
                    .Struct_Field => |struct_field| {
                        expr._type = struct_field._type.extract_ptr();
                    },
                    .Union_Field => |struct_field| {
                        expr._type = struct_field._type.extract_ptr();
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

            var _type = init._type.extract_ptr();
            var subexpr = Ast.Expr{
                .payload = .{ .Expr_List = init.expr_list },
                ._type = undefined,
                .line_info = expr.line_info,
            };
            _ = typecheck_expr_allow_void(ast, _type, &subexpr);

            expr._type = _type;
        },
        .Expr_List => |list| {
            if (type_hint == null) {
                common.print_error(ast.filepath, expr.line_info, "can't infere the type of expression list.", .{});
                std.os.exit(1);
            }

            var hint = type_hint.?;
            switch (hint.payload) {
                .Array => |array| {
                    if (list.count != array.count) {
                        common.print_error(ast.filepath, expr.line_info, "expected {} elements, but got {}.", .{ array.count, list.count });
                        std.os.exit(1);
                    }

                    var subhint = array.subtype.extract_ptr();
                    var it = list.iterator();
                    while (it.next()) |subexpr| {
                        if (subexpr.payload == .Designator) {
                            common.print_error(ast.filepath, subexpr.line_info, "expected expression, but got designator.", .{});
                            std.os.exit(1);
                        }

                        var subexpr_type = typecheck_expr(ast, subhint, subexpr);
                        if (!subexpr_type.eql(subhint)) {
                            common.print_error(ast.filepath, subexpr.line_info, "expected '{}', but got '{}'.", .{ subhint, subexpr_type });
                            std.os.exit(1);
                        }
                    }
                },
                .Struct,
                .Union,
                => |_struct| {
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
                                .Struct_Field => |field| field._type.extract_ptr(),
                                .Union_Field => |field| field._type.extract_ptr(),
                                else => unreachable,
                            };
                            var subexpr_type = typecheck_expr(ast, subhint, designator.expr);
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

            expr._type = hint;
        },
        .Designator => unreachable,
        .Enum_Field_From_Type => |field| {
            typecheck_type(ast, field._type);

            var _type = field._type.extract_ptr();
            var subexpr = Ast.Expr{
                .payload = .{ .Enum_Field = field.id },
                ._type = undefined,
                .line_info = field.id.line_info,
            };
            _ = typecheck_expr_allow_void(ast, _type, &subexpr);
            expr.payload = subexpr.payload;
            expr._type = _type;
        },
        .Enum_Field => |id| {
            if (type_hint == null) {
                common.print_error(ast.filepath, expr.line_info, "can't infere the type of enumerator.", .{});
                std.os.exit(1);
            }

            var hint = type_hint.?;
            switch (hint.payload) {
                .Enum => |_enum| {
                    var key = Ast.SymbolKey{
                        .text = id.text,
                        .scope = _enum.scope,
                    };
                    var found_symbol = ast.symbols.get(key);

                    if (found_symbol) |symbol| {
                        expr.payload = .{ .Symbol = symbol };
                        expr._type = hint;
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
        },
        .Cast1 => |subexpr| {
            var expr_type = typecheck_expr(ast, type_hint, subexpr);
            _ = expr_type;
            unreachable;
        },
        .Cast2 => |cast| {
            typecheck_type(ast, cast._type);
            var expr_type = typecheck_expr(ast, cast._type, cast.expr);
            _ = expr_type;
            unreachable;
        },
        .Bool => {
            expr._type = &BOOL_TYPE_HINT;
        },
        .Int64 => {
            expr._type = &INT64_TYPE_HINT;
        },
        .Null => {
            expr._type = &VOID_PTR_TYPE_HINT;
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

                    expr._type = variable._type.?;
                },
                .Parameter => |parameter| {
                    expr.is_lvalue = true;
                    expr._type = parameter._type;
                },
                .Function => |function| {
                    typecheck_type(ast, function._type);
                    expr._type = function._type;
                },
                .Type,
                .Struct_Field,
                .Union_Field,
                .Enum_Field,
                .Definition,
                => unreachable,
            }
        },
        .Identifier => unreachable,
    }

    expr._type = expr._type.extract_ptr();

    return expr._type;
}
