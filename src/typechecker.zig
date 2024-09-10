const std = @import("std");
const utils = @import("utils.zig");
const Compiler = @import("compiler.zig");
const IRCGen = @import("generate-ir.zig");

const Ast = Compiler.Ast;
const Alignment = utils.Alignment;

const TypecheckExprResult = struct {
    typ: *Ast.Type,
    tag: Tag,

    pub const Tag = enum {
        Value,
        Type,
        Non_Value,
    };
};

const Sign = enum {
    Signed,
    Unsigned,
    Both,
};

pub fn typecheck(c: *Compiler) void {
    typecheck_top_level(c);
}

fn typecheck_top_level(c: *Compiler) void {
    var it = c.ast.globals.first;
    while (it) |node| {
        typecheck_symbol(c, node.data);
        it = node.next;
    }

    const has_main = c.find_symbol(.{
        .name = "main",
        .scope = &Compiler.global_scope,
    }, 0);

    if (has_main) |main| {
        c.ast.main = main;
        switch (main.as) {
            .Procedure => |Procedure| {
                const Proc = &Procedure.typ.data.as.Proc;

                if (Proc.params.len != 0) {
                    c.report_error(Procedure.typ.line_info, "expected 0 arguments, but got {}", .{Proc.params.len});
                    Compiler.exit(1);
                }

                if (!Proc.return_type.equal(Ast.void_type)) {
                    c.report_error(Proc.return_type.line_info, "expected 'void', but got '{}'", .{Proc.return_type});
                    Compiler.exit(1);
                }
            },
            else => {
                c.report_error(main.line_info, "'main' isn't a procedure", .{});
                Compiler.exit(1);
            },
        }
    }
}

fn compute_const_expr(c: *Compiler, expr: *Ast.Expr) u64 {
    if (expr.as != .Integer) {
        c.report_error(expr.line_info, "TODO: evaluate expression in compile-time", .{});
        Compiler.exit(1);
    }
    return expr.as.Integer;
}

fn typecheck_symbol_type(c: *Compiler, symbol: *Ast.Symbol) void {
    switch (symbol.as) {
        .Variable => |Variable| {
            if (Variable.typ) |typ| {
                typecheck_type(c, typ);
                reject_void_type(c, typ);
            }
        },
        .Parameter => |Parameter| {
            typecheck_type(c, Parameter.typ);
            reject_void_type(c, Parameter.typ);
            if (Parameter.value) |value| {
                c.report_error(value.line_info, "default values are not supported", .{});
                Compiler.exit(1);
            }
        },
        .Procedure => |Procedure| {
            typecheck_type(c, Procedure.typ);
        },
        .Struct_Field => |Field| {
            typecheck_type(c, Field.typ);
            reject_void_type(c, Field.typ);
            if (Field.value) |value| {
                c.report_error(value.line_info, "default values are not supported", .{});
                Compiler.exit(1);
            }
        },
        .Union_Field => |Field| {
            typecheck_type(c, Field.typ);
            reject_void_type(c, Field.typ);
            if (Field.value) |value| {
                c.report_error(value.line_info, "default values are not supported", .{});
                Compiler.exit(1);
            }
        },
        .Enum_Field => {},
        .Type => |typ| {
            typ.symbol = symbol;
        },
    }
}

fn typecheck_symbol(c: *Compiler, symbol: *Ast.Symbol) void {
    switch (symbol.typechecking) {
        .None => symbol.typechecking = .Going,
        .Going => {
            c.report_error(symbol.line_info, "found cyclic reference", .{});
            Compiler.exit(1);
        },
        .Done => return,
    }
    defer symbol.typechecking = .Done;

    typecheck_symbol_type(c, symbol);

    switch (symbol.as) {
        .Variable => |*Variable| {
            if (Variable.typ) |typ| {
                if (Variable.value) |value| {
                    const value_type = typecheck_expr_only(c, value);
                    if (!safe_cast(c, value, value_type, typ)) {
                        c.report_error(value.line_info, "expected '{}', but got '{}'", .{ typ, value_type });
                        c.report_note(typ.line_info, "expected type is here", .{});
                        c.report_note(value.line_info, "expression is here", .{});
                        Compiler.exit(1);
                    }
                }
            } else if (Variable.value) |value| {
                const value_type = typecheck_expr_only(c, value);
                reject_void_type(c, value_type);

                Variable.typ = value_type;
            } else {
                unreachable;
            }
        },
        .Procedure => |*Procedure| {
            const old_return_type = c.typechecker.return_type;
            c.typechecker.return_type = Procedure.typ.data.as.Proc.return_type;

            var it = Procedure.block.first;
            while (it) |node| {
                typecheck_stmt(c, node.data);
                it = node.next;
            }

            c.typechecker.return_type = old_return_type;

            std.debug.assert(Procedure.start_label == null);
            Procedure.start_label = IRCGen.grab_label(c);
            Procedure.end_label = IRCGen.grab_label(c);
        },
        .Type => |typ| {
            typecheck_type(c, typ);
        },
        .Parameter, .Struct_Field, .Union_Field, .Enum_Field => unreachable,
    }
}

fn unpack(c: *Compiler, typ: *Ast.Type) void {
    const data = typ.data;
    switch (data.stages.unpacking) {
        .None => data.stages.unpacking = .Going,
        .Going => {
            c.report_error(typ.line_info, "found cyclic reference", .{});
            Compiler.exit(1);
        },
        .Done => return,
    }
    defer data.stages.unpacking = .Done;

    switch (data.as) {
        .Array => |Array| {
            unpack(c, Array.subtype);
        },
        .Pointer => |subtype| {
            unpack(c, subtype);
        },
        .Identifier => |Identifier| {
            const has_symbol = c.find_symbol(.{
                .name = Identifier.name,
                .scope = Identifier.scope,
            }, typ.line_info.offset);

            if (has_symbol) |symbol| {
                if (symbol.as != .Type) {
                    c.report_error(typ.line_info, "symbol '{s}' is not a type", .{Identifier.name});
                    Compiler.exit(1);
                }

                const new_typ = symbol.as.Type;
                unpack(c, new_typ);
                typ.data = new_typ.data;
                typ.symbol = symbol;
            } else {
                c.report_error(typ.line_info, "symbol '{s}' is not defined", .{Identifier.name});
                Compiler.exit(1);
            }
        },
        .Type_Of => |expr| {
            const expr_result = typecheck_expr(c, expr);
            if (expr_result.tag == .Type) {
                unpack(c, expr_result.typ);
            }
            typ.data = expr_result.typ.data;
        },
        .Struct,
        .Union,
        .Enum,
        .Proc,
        .Integer,
        .Bool,
        .Void,
        => {},
    }
}

fn shallow_check(c: *Compiler, typ: *Ast.Type) void {
    unpack(c, typ);

    const data = typ.data;
    switch (data.stages.shallow_check) {
        .None => data.stages.shallow_check = .Going,
        .Going => {
            c.report_error(typ.line_info, "found cyclic reference", .{});
            Compiler.exit(1);
        },
        .Done => return,
    }
    defer data.stages.shallow_check = .Done;

    switch (data.as) {
        .Struct, .Union => |Struct| {
            var it = Struct.fields.first;
            while (it) |node| {
                switch (node.data.as) {
                    .Struct_Field, .Union_Field => |Field| {
                        shallow_check(c, Field.typ);
                    },
                    else => unreachable,
                }
                it = node.next;
            }
        },
        .Array => |Array| {
            shallow_check(c, Array.subtype);
        },
        .Enum,
        .Proc,
        .Pointer,
        .Integer,
        .Bool,
        .Void,
        => {},
        .Identifier, .Type_Of => unreachable,
    }
}

fn void_array_check(c: *Compiler, typ: *Ast.Type) void {
    shallow_check(c, typ);

    const data = typ.data;
    switch (data.stages.void_array_check) {
        .None => data.stages.void_array_check = .Going,
        .Going, .Done => return,
    }
    defer data.stages.void_array_check = .Done;

    switch (data.as) {
        .Struct, .Union => |Struct| {
            var it = Struct.fields.first;
            while (it) |node| {
                switch (node.data.as) {
                    .Struct_Field, .Union_Field => |Field| {
                        void_array_check(c, Field.typ);
                    },
                    else => unreachable,
                }
                it = node.next;
            }
        },
        .Proc => |Proc| {
            var it = Proc.params.first;
            while (it) |node| {
                const Parameter = &node.data.as.Parameter;
                void_array_check(c, Parameter.typ);
                it = node.next;
            }
            void_array_check(c, Proc.return_type);
        },
        .Array => |Array| {
            void_array_check(c, Array.subtype);
            reject_void_type(c, Array.subtype);
        },
        .Pointer => |subtype| {
            void_array_check(c, subtype);
        },
        .Enum,
        .Integer,
        .Bool,
        .Void,
        => {},
        .Identifier, .Type_Of => unreachable,
    }
}

// TODO: don't go through all stages when the top one is done.
fn typecheck_type(c: *Compiler, typ: *Ast.Type) void {
    void_array_check(c, typ);

    const data = typ.data;
    switch (data.stages.full_check) {
        .None => data.stages.full_check = .Going,
        .Going, .Done => return,
    }
    defer data.stages.full_check = .Done;

    switch (data.as) {
        .Struct => |Struct| {
            var size: u64 = 0;
            var alignment: Alignment = .BYTE;

            var it = Struct.fields.first;
            while (it) |node| {
                const symbol = node.data;
                typecheck_symbol_type(c, symbol);

                const Field = &symbol.as.Struct_Field;
                const field_alignment = Field.typ.data.alignment;
                size = utils.align_u64(size, field_alignment);
                Field.offset = size;

                size += Field.typ.data.byte_size;
                alignment = @enumFromInt(@max(@intFromEnum(alignment), @intFromEnum(field_alignment)));

                it = node.next;
            }

            data.byte_size = size;
            data.alignment = alignment;
        },
        .Union => |Union| {
            var size: u64 = 0;
            var alignment: Alignment = .BYTE;

            var it = Union.fields.first;
            while (it) |node| {
                const symbol = node.data;
                typecheck_symbol_type(c, symbol);

                const Field = &node.data.as.Union_Field;
                Field.offset = 0;

                size = @max(size, Field.typ.data.byte_size);
                alignment = @enumFromInt(@max(@intFromEnum(alignment), @intFromEnum(Field.typ.data.alignment)));

                it = node.next;
            }

            data.byte_size = size;
            data.alignment = alignment;
        },
        .Enum => |*Enum| {
            const old_enum_type = c.typechecker.enum_type;
            c.typechecker.enum_type = typ;

            var next_value: u64 = 0;
            var max_value: u64 = 0;
            var is_signed = false;

            var it = Enum.fields.first;
            while (it) |node| {
                const enum_field = &node.data.as.Enum_Field;

                if (enum_field.value) |value| {
                    const value_type = typecheck_expr_only(c, value);
                    const new_value_type = safe_cast_to_integer(c, value, value_type, .Both);
                    if (new_value_type == null) {
                        c.report_error(value.line_info, "expected integer, but got '{}'\n", .{value_type});
                        Compiler.exit(1);
                    }
                    switch (new_value_type.?.data.as) {
                        .Integer => |Integer| {
                            if (Integer.is_signed) {
                                is_signed = true;
                            }
                        },
                        else => unreachable,
                    }
                    next_value = compute_const_expr(c, value);
                    enum_field.computed_value = next_value;
                } else {
                    enum_field.computed_value = next_value;
                }

                max_value = @max(max_value, next_value);
                next_value += 1;

                it = node.next;
            }

            const bits = utils.count_bits(max_value) + @intFromBool(is_signed);
            if (bits > 64) {
                c.report_error(typ.line_info, "enum can't be represented in <= 64 bits", .{});
                Compiler.exit(1);
            }
            const integer_type = Ast.lookup_integer_type(bits, is_signed);

            Enum.integer_type = integer_type;
            data.byte_size = integer_type.data.byte_size;
            data.alignment = integer_type.data.alignment;

            c.typechecker.enum_type = old_enum_type;
        },
        .Proc => |Proc| {
            std.debug.assert(data.byte_size == Ast.pointer_byte_size and
                data.alignment == Ast.pointer_alignment);

            var it = Proc.params.first;
            while (it) |node| {
                const symbol = node.data;
                typecheck_symbol_type(c, symbol);

                it = node.next;
            }
            typecheck_type(c, Proc.return_type);
        },
        .Array => |*Array| {
            typecheck_type(c, Array.subtype);

            const size_type = typecheck_expr_only(c, Array.size);
            if (safe_cast_to_integer(c, Array.size, size_type, .Unsigned) == null) {
                c.report_error(typ.line_info, "expected unsigned integer, but got '{}'", .{size_type});
                Compiler.exit(1);
            }

            const count = compute_const_expr(c, Array.size);
            Array.computed_size = count;
            data.byte_size = count * Array.subtype.data.byte_size;
            data.alignment = Array.subtype.data.alignment;
        },
        .Pointer => |subtype| {
            std.debug.assert(data.byte_size == Ast.pointer_byte_size and
                data.alignment == Ast.pointer_alignment);
            typecheck_type(c, subtype);
        },
        .Integer => {
            std.debug.assert(data.byte_size != 0);
        },
        .Bool => {
            std.debug.assert(data.byte_size == 1 and
                data.alignment == .BYTE);
        },
        .Void => {
            std.debug.assert(data.byte_size == 0 and
                data.alignment == .BYTE);
        },
        .Identifier, .Type_Of => unreachable,
    }
}

fn typecheck_stmt_list(c: *Compiler, list: Ast.StmtList) void {
    var it = list.first;
    while (it) |node| {
        typecheck_stmt(c, node.data);
        it = node.next;
    }
}

fn typecheck_stmt(c: *Compiler, stmt: *Ast.Stmt) void {
    switch (stmt.as) {
        .Print => |expr| {
            const expr_type = typecheck_expr_only(c, expr);
            switch (expr_type.data.as) {
                .Struct, .Union, .Proc, .Array, .Void => {
                    c.report_error(expr.line_info, "can't print value of type '{}'", .{expr.typ});
                    Compiler.exit(1);
                },
                .Enum,
                .Pointer,
                .Integer,
                .Bool,
                => {},
                .Identifier, .Type_Of => unreachable,
            }
        },
        .Block => |block| {
            typecheck_stmt_list(c, block);
        },
        .If => |If| {
            const condition_type = typecheck_expr_only(c, If.condition);
            if (!safe_cast(c, If.condition, condition_type, Ast.bool_type)) {
                c.report_error(If.condition.line_info, "expected 'bool', but got '{}'", .{condition_type});
                Compiler.exit(1);
            }

            reject_symbol(c, If.true_branch);
            typecheck_stmt(c, If.true_branch);

            if (If.false_branch) |false_branch| {
                reject_symbol(c, false_branch);
                typecheck_stmt(c, false_branch);
            }
        },
        .While, .Do_While => |While| {
            const condition_type = typecheck_expr_only(c, While.condition);
            if (!safe_cast(c, While.condition, condition_type, Ast.bool_type)) {
                c.report_error(While.condition.line_info, "expected 'bool', but got '{}'", .{condition_type});
                Compiler.exit(1);
            }

            const old_is_in_loop = c.typechecker.is_in_loop;

            reject_symbol(c, While.body);
            c.typechecker.is_in_loop = true;
            typecheck_stmt(c, While.body);
            c.typechecker.is_in_loop = old_is_in_loop;
        },
        .Break => {
            if (!c.typechecker.is_in_loop) {
                c.report_error(stmt.line_info, "'break' outside of loop", .{});
                Compiler.exit(1);
            }
        },
        .Continue => {
            if (!c.typechecker.is_in_loop) {
                c.report_error(stmt.line_info, "'continue' outside of loop", .{});
                Compiler.exit(1);
            }
        },
        .Switch => |Switch| {
            const condition_type = typecheck_expr_only(c, Switch.condition);
            const is_comparable = condition_type.compare().is_comparable;

            if (!is_comparable) {
                c.report_error(Switch.condition.line_info, "value of type '{}' isn't comparable", .{condition_type});
                Compiler.exit(1);
            }

            var it = Switch.cases.first;
            while (it) |node| {
                var Case = node.data;
                while (true) {
                    switch (Case.*) {
                        .Case => |case| {
                            const value_type = typecheck_expr_only(c, case.value);

                            if (!case.value.flags.is_const) {
                                c.report_error(case.value.line_info, "expression is not constant", .{});
                                Compiler.exit(1);
                            } else if (!safe_cast(c, case.value, value_type, condition_type)) {
                                c.report_error(case.value.line_info, "mismatched types: '{}' and '{}'", .{ condition_type, value_type });
                                c.report_note(case.value.line_info, "switch case is here", .{});
                                c.report_note(Switch.condition.line_info, "switch condition is here", .{});
                                Compiler.exit(1);
                            }

                            Case = case.subcase;
                        },
                        .Stmt => |substmt| {
                            reject_symbol(c, substmt);
                            typecheck_stmt(c, substmt);
                            break;
                        },
                    }
                }

                it = node.next;
            }
        },
        .Return => |has_expr| {
            const return_type = c.typechecker.return_type.?;

            if (has_expr) |expr| {
                if (return_type.equal(Ast.void_type)) {
                    c.report_error(expr.line_info, "unexpected expression here", .{});
                    Compiler.exit(1);
                } else {
                    const expr_type = typecheck_expr_only(c, expr);

                    if (!safe_cast(c, expr, expr_type, return_type)) {
                        c.report_error(expr.line_info, "mismatched types: '{}' and '{}'", .{ return_type, expr_type });
                        c.report_note(return_type.line_info, "return type is here", .{});
                        c.report_note(expr.line_info, "expression is here", .{});
                        Compiler.exit(1);
                    }
                }
            } else if (!return_type.equal(Ast.void_type)) {
                c.report_error(stmt.line_info, "expected expression of type '{}'", .{return_type});
                Compiler.exit(1);
            }
        },
        .Symbol => |symbol| {
            typecheck_symbol(c, symbol);
            if (symbol.as == .Procedure) {
                c.report_error(stmt.line_info, "no local function allowed, yet", .{});
                Compiler.exit(1);
            }
        },
        .Assign => |Assign| {
            const lhs_type = typecheck_expr_only(c, Assign.lhs);

            if (!Assign.lhs.flags.is_lvalue) {
                c.report_error(Assign.lhs.line_info, "expression is not an lvalue", .{});
                Compiler.exit(1);
            } else if (Assign.lhs.flags.is_const) {
                c.report_error(Assign.lhs.line_info, "can't assign to constant expression", .{});
                Compiler.exit(1);
            }

            const rhs_type = typecheck_expr_only(c, Assign.rhs);

            if (!safe_cast(c, Assign.rhs, rhs_type, lhs_type)) {
                c.report_error(stmt.line_info, "expected '{}', but got '{}'", .{ lhs_type, rhs_type });
                Compiler.exit(1);
            }
        },
        .Expr => |expr| {
            _ = typecheck_expr_only(c, expr);
        },
    }
}

fn typecheck_expr_only(c: *Compiler, expr: *Ast.Expr) *Ast.Type {
    const result = typecheck_expr(c, expr);
    if (result.tag != .Value) {
        c.report_error(expr.line_info, "expected expression", .{});
        Compiler.exit(1);
    }
    return result.typ;
}

// Types are typechecked lazily (only when used). 'cause if a type is not fully formed (like ambiguous pointer/array), we need to typecheck it from the top level.
fn typecheck_expr(c: *Compiler, expr: *Ast.Expr) TypecheckExprResult {
    switch (expr.typechecking) {
        .None => expr.typechecking = .Going,
        .Going => {
            c.report_error(expr.line_info, "found cyclic reference", .{});
            Compiler.exit(1);
        },
        .Done => return .{ .typ = expr.typ, .tag = if (expr.as == .Type) .Type else .Value }, // Non values should be filtered at  this point?
    }
    defer expr.typechecking = .Done;

    const result: TypecheckExprResult = result: {
        switch (expr.as) {
            .Binary_Op => |*Binary_Op| {
                const lhs_type = typecheck_expr_only(c, Binary_Op.lhs);
                const rhs_type = typecheck_expr_only(c, Binary_Op.rhs);
                const lhs_flags = lhs_type.compare();
                const rhs_flags = rhs_type.compare();

                expr.flags.is_const = Binary_Op.lhs.flags.is_const and
                    Binary_Op.rhs.flags.is_const;

                switch (Binary_Op.tag) {
                    .Or, .And => {
                        if (!safe_cast(c, Binary_Op.lhs, lhs_type, Ast.bool_type) or
                            !safe_cast(c, Binary_Op.rhs, rhs_type, Ast.bool_type))
                        {
                            c.report_error(Binary_Op.line_info, "mismatched types: '{}' and '{}'", .{ lhs_type, rhs_type });
                            Compiler.exit(1);
                        }

                        switch (Binary_Op.tag) {
                            .Or => {
                                const true_branch = c.ast.create(Ast.Expr);
                                true_branch.* = .{
                                    .line_info = expr.line_info,
                                    .as = .{ .Boolean = true },
                                    .typ = Ast.bool_type,
                                    .flags = .{
                                        .is_const = true,
                                    },
                                    .typechecking = .Done,
                                };
                                expr.as = .{ .If = .{
                                    .condition = Binary_Op.lhs,
                                    .true_branch = true_branch,
                                    .false_branch = Binary_Op.rhs,
                                } };
                            },
                            .And => {
                                const false_branch = c.ast.create(Ast.Expr);
                                false_branch.* = .{
                                    .line_info = expr.line_info,
                                    .as = .{ .Boolean = false },
                                    .typ = Ast.bool_type,
                                    .flags = .{
                                        .is_const = true,
                                    },
                                    .typechecking = .Done,
                                };
                                expr.as = .{ .If = .{
                                    .condition = Binary_Op.lhs,
                                    .true_branch = Binary_Op.rhs,
                                    .false_branch = false_branch,
                                } };
                            },
                            else => unreachable,
                        }

                        break :result .{ .typ = Ast.bool_type, .tag = .Value };
                    },
                    .Eq, .Neq => {
                        if (!lhs_flags.is_comparable or !rhs_flags.is_comparable) { // TODO: only one side needs to be comparable?
                            c.report_error(Binary_Op.line_info, "errror messages suuuuuuuuuuuuuuuuuck. One of the {}/{} is not comparable", .{ lhs_type, rhs_type });
                            Compiler.exit(1);
                        } else if (!symetric_safe_cast(c, Binary_Op.lhs, lhs_type, Binary_Op.rhs, rhs_type)) {
                            c.report_error(Binary_Op.line_info, "mismatched types: '{}' and '{}'", .{ lhs_type, rhs_type });
                            Compiler.exit(1);
                        }

                        break :result .{ .typ = Ast.bool_type, .tag = .Value };
                    },
                    .Lt, .Leq, .Gt, .Geq => {
                        if (!lhs_flags.is_ordered or !rhs_flags.is_ordered) {
                            c.report_error(Binary_Op.line_info, "expression of type '{}' is not comparable", .{lhs_type});
                            Compiler.exit(1);
                        } else if (!symetric_safe_cast(c, Binary_Op.lhs, lhs_type, Binary_Op.rhs, rhs_type)) {
                            c.report_error(Binary_Op.line_info, "mismatched types: '{}' and '{}'", .{ lhs_type, rhs_type });
                            Compiler.exit(1);
                        }

                        break :result .{ .typ = Ast.bool_type, .tag = .Value };
                    },
                    .Add => {
                        if (lhs_flags.is_void_pointer or rhs_flags.is_void_pointer) {
                            c.report_error(Binary_Op.line_info, "can't add 'void' pointers", .{});
                            Compiler.exit(1);
                        } else if (lhs_flags.is_pointer and safe_cast_to_integer(c, Binary_Op.rhs, rhs_type, .Unsigned) != null) {
                            Binary_Op.rhs = make_expr_pointer_mul_integer(c, Binary_Op.rhs, lhs_type.data.as.Pointer.data);
                            break :result .{ .typ = lhs_type, .tag = .Value };
                        } else if (rhs_flags.is_pointer and safe_cast_to_integer(c, Binary_Op.lhs, lhs_type, .Unsigned) != null) {
                            Binary_Op.lhs = make_expr_pointer_mul_integer(c, Binary_Op.lhs, rhs_type.data.as.Pointer.data);
                            break :result .{ .typ = rhs_type, .tag = .Value };
                        } else {
                            const casted = safe_cast_two_integers(c, Binary_Op.lhs, lhs_type, Binary_Op.rhs, rhs_type);
                            if (casted == null) {
                                c.report_error(Binary_Op.line_info, "expected integer/pointer: '{}' and '{}'", .{ lhs_type, rhs_type });
                                Compiler.exit(1);
                            }
                            break :result .{ .typ = casted.?, .tag = .Value };
                        }
                    },
                    .Sub => {
                        if (lhs_flags.is_void_pointer) {
                            c.report_error(Binary_Op.line_info, "can't subtract 'void' pointer", .{});
                            Compiler.exit(1);
                        } else if (lhs_flags.is_pointer and safe_cast_to_integer(c, Binary_Op.rhs, rhs_type, .Unsigned) != null) {
                            Binary_Op.rhs = make_expr_pointer_mul_integer(c, Binary_Op.rhs, lhs_type.data.as.Pointer.data);
                            break :result .{ .typ = lhs_type, .tag = .Value };
                        } else {
                            const casted = safe_cast_two_integers(c, Binary_Op.lhs, lhs_type, Binary_Op.rhs, rhs_type);
                            if (casted == null) {
                                c.report_error(Binary_Op.line_info, "expected integer/pointer: '{}' and '{}'", .{ lhs_type, rhs_type });
                                Compiler.exit(1);
                            }
                            break :result .{ .typ = casted.?, .tag = .Value };
                        }
                    },
                    .Mul, .Div, .Mod => {
                        const casted = safe_cast_two_integers(c, Binary_Op.lhs, lhs_type, Binary_Op.rhs, rhs_type);
                        if (casted == null) {
                            c.report_error(Binary_Op.line_info, "expected integer/pointer: '{}' and '{}'", .{ lhs_type, rhs_type });
                            Compiler.exit(1);
                        }
                        break :result .{ .typ = casted.?, .tag = .Value };
                    },
                }
            },
            .Unary_Op => |Unary_Op| {
                const subexpr_type = typecheck_expr_only(c, Unary_Op.subexpr);

                expr.flags.is_const = Unary_Op.subexpr.flags.is_const;

                switch (Unary_Op.tag) {
                    .Pos => {
                        const casted = safe_cast_to_integer(c, Unary_Op.subexpr, subexpr_type, .Both);
                        if (casted == null) {
                            c.report_error(Unary_Op.subexpr.line_info, "expected integer, but got '{}'", .{subexpr_type});
                            Compiler.exit(1);
                        }
                        break :result .{ .typ = casted.?, .tag = .Value };
                    },
                    .Neg => {
                        const casted = safe_cast_to_integer(c, Unary_Op.subexpr, subexpr_type, .Signed);
                        if (casted == null) {
                            c.report_error(Unary_Op.subexpr.line_info, "expected integer, but got '{}'", .{subexpr_type});
                            Compiler.exit(1);
                        }
                        break :result .{ .typ = casted.?, .tag = .Value };
                    },
                    .Not => {
                        if (!safe_cast(c, Unary_Op.subexpr, subexpr_type, Ast.bool_type)) {
                            c.report_error(Unary_Op.subexpr.line_info, "expected 'bool', but got '{}'", .{subexpr_type});
                            Compiler.exit(1);
                        }

                        break :result .{ .typ = Ast.bool_type, .tag = .Value };
                    },
                }
            },
            .Ref => |subexpr| {
                const subexpr_type = typecheck_expr_only(c, subexpr);

                if (!subexpr.flags.is_lvalue) {
                    c.report_error(subexpr.line_info, "expression is not an lvalue", .{});
                    Compiler.exit(1);
                } else if (subexpr.flags.is_const) {
                    c.report_error(subexpr.line_info, "can't take raferences to constant expression", .{});
                    Compiler.exit(1);
                }

                const data = c.ast.create(Ast.Type.SharedData);
                data.* = .{
                    .as = .{ .Pointer = subexpr_type },
                    .byte_size = Ast.pointer_byte_size,
                    .alignment = Ast.pointer_alignment,
                    .stages = Ast.default_stages_done,
                };
                const typ = c.ast.create(Ast.Type);
                typ.* = .{
                    .line_info = expr.line_info,
                    .data = data,
                    .symbol = null,
                };

                expr.flags.is_const = subexpr.flags.is_const;

                break :result .{ .typ = typ, .tag = .Value };
            },
            .Deref => |subexpr| {
                const subexpr_result = typecheck_expr(c, subexpr);

                if (subexpr_result.tag == .Type) {
                    const data = c.ast.create(Ast.Type.SharedData);
                    data.* = .{
                        .as = .{ .Pointer = subexpr_result.typ },
                        .byte_size = Ast.pointer_byte_size,
                        .alignment = Ast.pointer_alignment,
                        .stages = Ast.default_stages_none,
                    };

                    expr.as = .{ .Type = .{
                        .line_info = expr.line_info,
                        .data = data,
                        .symbol = null,
                    } };

                    expr.flags.is_const = true;

                    break :result .{ .typ = &expr.as.Type, .tag = .Type };
                } else {
                    const subexpr_type = subexpr_result.typ;
                    const can_be_dereferenced = subexpr_type.compare().can_be_dereferenced;

                    if (!can_be_dereferenced) {
                        c.report_error(subexpr.line_info, "can't dereference expression of type '{}'", .{subexpr_type});
                        Compiler.exit(1);
                    }

                    expr.flags.is_lvalue = true;
                    expr.flags.is_const = false;

                    break :result .{ .typ = subexpr_type.data.as.Pointer, .tag = .Value };
                }
            },
            .If => |If| {
                const condition_type = typecheck_expr_only(c, If.condition);

                if (!safe_cast(c, If.condition, condition_type, Ast.bool_type)) {
                    c.report_error(If.condition.line_info, "expected 'bool', but got '{}'", .{condition_type});
                    Compiler.exit(1);
                }

                const true_branch_type = typecheck_expr_only(c, If.true_branch);
                const false_branch_type = typecheck_expr_only(c, If.false_branch);

                if (!symetric_safe_cast(c, If.true_branch, true_branch_type, If.false_branch, false_branch_type)) {
                    c.report_error(If.condition.line_info, "mismatched types: '{}' and '{}'", .{ true_branch_type, false_branch_type });
                    Compiler.exit(1);
                }

                expr.flags.is_const = If.condition.flags.is_const and
                    If.true_branch.flags.is_const and
                    If.false_branch.flags.is_const;

                break :result .{ .typ = true_branch_type, .tag = .Value };
            },
            .Call => |Call| {
                const subexpr_result = typecheck_expr(c, Call.subexpr);

                if (subexpr_result.tag == .Type) {
                    const typ = subexpr_result.typ;

                    var is_const = false;

                    typecheck_type(c, typ);
                    switch (typ.data.as) {
                        .Struct, .Union => |Struct| {
                            is_const = true;

                            var it = Call.args.first;
                            while (it) |node| {
                                switch (node.data.*) {
                                    .Designator => |Designator| {
                                        const symbol = resolve_identifier(c, Designator.lhs, Struct.scope);

                                        switch (symbol.as) {
                                            .Struct_Field, .Union_Field => |Field| {
                                                const rhs_type = typecheck_expr_only(c, Designator.rhs);
                                                if (!safe_cast(c, Designator.rhs, rhs_type, Field.typ)) {
                                                    c.report_error(expr.line_info, "expected '{}', but got '{}'", .{ Field.typ, rhs_type });
                                                    Compiler.exit(1);
                                                }
                                                is_const = is_const and Designator.rhs.flags.is_const;
                                            },
                                            else => {
                                                c.report_error(typ.line_info, "expected a structure or union field", .{});
                                                Compiler.exit(1);
                                            },
                                        }
                                    },
                                    .Expr => {
                                        c.report_error(expr.line_info, "unexpected expression", .{});
                                        Compiler.exit(1);
                                    },
                                }

                                it = node.next;
                            }
                        },
                        .Array => |Array| {
                            if (Array.computed_size != Call.args.len) {
                                c.report_error(expr.line_info, "expected {} values, but got {}", .{ Array.computed_size, Call.args.len });
                                Compiler.exit(1);
                            }

                            is_const = true;

                            var it = Call.args.first;
                            while (it) |node| {
                                switch (node.data.*) {
                                    .Designator => |Designator| {
                                        c.report_error(Designator.lhs.line_info, "unexpected designator", .{});
                                        Compiler.exit(1);
                                    },
                                    .Expr => |arg| {
                                        const arg_type = typecheck_expr_only(c, arg);
                                        if (!safe_cast(c, arg, arg_type, Array.subtype)) {
                                            c.report_error(arg.line_info, "expected '{}', but got '{}'", .{ Array.subtype, arg_type });
                                            Compiler.exit(1);
                                        }
                                        is_const = is_const and arg.flags.is_const;
                                    },
                                }

                                it = node.next;
                            }
                        },
                        .Enum,
                        .Proc,
                        .Pointer,
                        .Integer,
                        .Bool,
                        => {
                            if (Call.args.len != 1) {
                                c.report_error(expr.line_info, "expected 1 argument for scalar type, but got {}", .{Call.args.len});
                                Compiler.exit(1);
                            }

                            switch (Call.args.first.?.data.*) {
                                .Designator => |Designator| {
                                    c.report_error(Designator.lhs.line_info, "unexpected designator", .{});
                                    Compiler.exit(1);
                                },
                                .Expr => |arg| {
                                    const arg_type = typecheck_expr_only(c, arg);

                                    if (!safe_cast(c, arg, arg_type, typ)) {
                                        c.report_error(expr.line_info, "expected '{}', but got '{}'", .{ typ, arg_type });
                                        Compiler.exit(1);
                                    }

                                    is_const = arg.flags.is_const;
                                },
                            }
                        },
                        .Void => {
                            c.report_error(expr.line_info, "can't construct expression of type 'void'", .{});
                            Compiler.exit(1);
                        },
                        .Identifier, .Type_Of => unreachable,
                    }

                    const args = Call.args;
                    expr.as = .{ .Constructor = .{
                        .typ = typ,
                        .args = args,
                    } };
                    expr.flags.is_const = is_const;

                    break :result .{ .typ = typ, .tag = .Value };
                } else {
                    const subexpr_type = subexpr_result.typ;

                    if (subexpr_type.data.as != .Proc) {
                        c.report_error(Call.subexpr.line_info, "expected a procedure, but got '{}'", .{subexpr_type});
                        Compiler.exit(1);
                    }

                    const Proc = &subexpr_type.data.as.Proc;

                    if (Proc.params.len != Call.args.len) {
                        c.report_error(Call.subexpr.line_info, "expected {} arguments, but got {}", .{ Proc.params.len, Call.args.len });
                        Compiler.exit(1);
                    }

                    var is_const = true;

                    var pit = Proc.params.first;
                    var ait = Call.args.first;
                    while (pit != null) {
                        const pnode = pit.?;
                        const anode = ait.?;

                        const Parameter = &pnode.data.as.Parameter;
                        const param_type = Parameter.typ;

                        switch (anode.data.*) {
                            .Designator => |Designator| {
                                c.report_error(Designator.lhs.line_info, "unexpected designator", .{});
                                Compiler.exit(1);
                            },
                            .Expr => |arg| {
                                const arg_type = typecheck_expr_only(c, arg);

                                if (!safe_cast(c, arg, arg_type, param_type)) {
                                    c.report_error(arg.line_info, "expected '{}', but got '{}'", .{ param_type, arg_type });
                                    Compiler.exit(1);
                                }

                                is_const = is_const and arg.flags.is_const;
                            },
                        }

                        pit = pnode.next;
                        ait = anode.next;
                    }

                    expr.flags.is_const = is_const;

                    break :result .{ .typ = Proc.return_type, .tag = .Value };
                }
            },
            .Constructor => unreachable,
            .Subscript => |Subscript| {
                const subexpr_result = typecheck_expr(c, Subscript.subexpr);

                if (subexpr_result.tag == .Type) {
                    const data = c.ast.create(Ast.Type.SharedData);
                    data.* = .{
                        .as = .{ .Array = .{
                            .subtype = subexpr_result.typ,
                            .size = Subscript.index,
                            .computed_size = 0,
                        } },
                        .byte_size = 0,
                        .alignment = .BYTE,
                        .stages = Ast.default_stages_none,
                    };

                    expr.as = .{ .Type = .{
                        .line_info = expr.line_info,
                        .data = data,
                        .symbol = null,
                    } };

                    expr.flags.is_const = true;

                    break :result .{ .typ = &expr.as.Type, .tag = .Type };
                } else {
                    const subexpr_type = subexpr_result.typ;
                    const index_type = typecheck_expr_only(c, Subscript.index);

                    if (safe_cast_to_integer(c, Subscript.index, index_type, .Both) == null) {
                        c.report_error(Subscript.subexpr.line_info, "expected integer, but got '{}'", .{index_type});
                        Compiler.exit(1);
                    }

                    expr.flags.is_lvalue = Subscript.subexpr.flags.is_lvalue;
                    expr.flags.is_const = Subscript.subexpr.flags.is_const and Subscript.index.flags.is_const;

                    switch (subexpr_type.data.as) {
                        .Array => |Array| {
                            break :result .{ .typ = Array.subtype, .tag = .Value };
                        },
                        .Pointer => |subtype| {
                            switch (subtype.data.as) {
                                .Array => |Array| break :result .{ .typ = Array.subtype, .tag = .Value },
                                else => break :result .{ .typ = subtype, .tag = .Value },
                            }
                        },
                        else => {
                            c.report_error(Subscript.subexpr.line_info, "expected array or pointer to array, but got '{}'", .{subexpr_type});
                            Compiler.exit(1);
                        },
                    }
                }
            },
            .Field => |Field| {
                const subexpr_result = typecheck_expr(c, Field.subexpr);

                if (subexpr_result.tag == .Type) {
                    typecheck_type(c, subexpr_result.typ);

                    const scope: *Ast.Scope = switch (subexpr_result.typ.data.as) {
                        .Struct, .Union => |Struct| Struct.scope,
                        .Enum => |Enum| Enum.scope,
                        .Proc,
                        .Array,
                        .Pointer,
                        .Integer,
                        .Bool,
                        .Void,
                        => {
                            c.report_error(subexpr_result.typ.line_info, "expected struct, union, or enum type, but got '{}'", .{subexpr_result.typ});
                            Compiler.exit(1);
                        },
                        .Identifier, .Type_Of => unreachable,
                    };

                    const symbol = resolve_identifier(c, Field.field, scope);

                    switch (symbol.as) {
                        .Variable, .Parameter, .Procedure => unreachable,
                        .Struct_Field, .Union_Field => |Struct_Field| {
                            expr.as = .{ .Symbol = symbol };
                            break :result .{ .typ = Struct_Field.typ, .tag = .Non_Value };
                        },
                        .Enum_Field => {
                            expr.as = .{ .Symbol = symbol };
                            break :result .{ .typ = subexpr_result.typ, .tag = .Value };
                        },
                        .Type => |Type| {
                            expr.as = .{ .Type = Type.* };
                            expr.flags.is_const = true;
                            break :result .{ .typ = &expr.as.Type, .tag = .Type };
                        },
                    }
                } else {
                    expr.flags.is_lvalue = Field.subexpr.flags.is_lvalue;
                    expr.flags.is_const = Field.subexpr.flags.is_const;

                    const scope: *Ast.Scope = switch (subexpr_result.typ.data.as) {
                        .Struct, .Union => |Struct| Struct.scope,
                        .Pointer => |subtype| switch (subtype.data.as) {
                            .Struct, .Union => |Struct| Struct.scope,
                            else => {
                                c.report_error(Field.subexpr.line_info, "expected pointer to struct/union, but got '{}'", .{subexpr_result.typ});
                                Compiler.exit(1);
                            },
                        },
                        else => {
                            c.report_error(Field.subexpr.line_info, "expected struct, union, or enum, but got '{}'", .{subexpr_result.typ});
                            Compiler.exit(1);
                        },
                    };

                    const symbol = resolve_identifier(c, Field.field, scope);

                    switch (symbol.as) {
                        .Struct_Field, .Union_Field => |Struct_Field| {
                            break :result .{ .typ = Struct_Field.typ, .tag = .Value };
                        },
                        else => unreachable,
                    }
                }
            },
            .Byte_Size_Of => |subexpr| {
                const subexpr_result = typecheck_expr(c, subexpr);
                if (subexpr_result.tag == .Type) {
                    typecheck_type(c, subexpr_result.typ);
                }
                const byte_size = subexpr_result.typ.data.byte_size;

                expr.as = .{ .Integer = byte_size };
                expr.flags.is_const = true;

                break :result .{ .typ = Ast.integer_type_from_u64(byte_size), .tag = .Value };
            },
            .Alignment_Of => |subexpr| {
                const subexpr_result = typecheck_expr(c, subexpr);
                if (subexpr_result.tag == .Type) {
                    typecheck_type(c, subexpr_result.typ);
                }
                const alignment = subexpr_result.typ.data.alignment.to_byte_size();

                expr.as = .{ .Integer = alignment };
                expr.flags.is_const = true;

                break :result .{ .typ = Ast.integer_type_from_u64(alignment), .tag = .Value };
            },
            .As => |As| {
                typecheck_type(c, As.typ);
                const expr_type = typecheck_expr_only(c, As.expr);
                if (!safe_cast(c, As.expr, expr_type, As.typ)) {
                    c.report_error(As.expr.line_info, "can't reinterpret '{}' as '{}'", .{ As.typ, expr_type });
                    Compiler.exit(1);
                }
                expr.flags.is_const = As.expr.flags.is_const;
                break :result .{ .typ = As.typ, .tag = .Value };
            },
            .Cast => |Cast| {
                typecheck_type(c, Cast.typ);
                const expr_type = typecheck_expr_only(c, Cast.expr);
                if (!can_unsafe_cast(c, Cast.expr, expr_type, Cast.typ)) {
                    c.report_error(Cast.expr.line_info, "can't cast '{}' to '{}'", .{ Cast.typ, expr_type });
                    Compiler.exit(1);
                }
                expr.flags.is_const = Cast.expr.flags.is_const;
                break :result .{ .typ = Cast.typ, .tag = .Value };
            },
            .Boolean => {
                expr.flags.is_const = true;
                break :result .{ .typ = expr.typ, .tag = .Value };
            },
            .Integer => {
                expr.flags.is_const = true;
                break :result .{ .typ = expr.typ, .tag = .Value };
            },
            .Null => {
                expr.flags.is_const = true;
                break :result .{ .typ = expr.typ, .tag = .Value };
            },
            .Type => |*Type| {
                expr.flags.is_const = true;
                break :result .{ .typ = Type, .tag = .Type };
            },
            .Symbol => unreachable,
            .Identifier => |Identifier| {
                const has_symbol = c.find_symbol(.{
                    .name = Identifier.name,
                    .scope = Identifier.scope,
                }, expr.line_info.offset);

                if (has_symbol) |symbol| {
                    expr.as = .{ .Symbol = symbol };

                    typecheck_symbol_type(c, symbol);

                    switch (symbol.as) {
                        .Variable => |*Variable| {
                            typecheck_symbol(c, symbol); // TODO: allow cyclic references when using #type_of/#alignment_of, etc.

                            expr.flags.is_lvalue = true;

                            break :result .{ .typ = Variable.typ.?, .tag = .Value };
                        },
                        .Parameter => |Parameter| {
                            expr.flags.is_lvalue = true;
                            break :result .{ .typ = Parameter.typ, .tag = .Value };
                        },
                        .Procedure => |Procedure| {
                            break :result .{ .typ = Procedure.typ, .tag = .Value };
                        },
                        .Type => |Type| {
                            break :result .{ .typ = Type, .tag = .Type };
                        },
                        .Struct_Field, .Union_Field => |Field| {
                            break :result .{ .typ = Field.typ, .tag = .Non_Value };
                        },
                        .Enum_Field => {
                            break :result .{ .typ = c.typechecker.enum_type.?, .tag = .Value };
                        },
                    }
                } else {
                    c.report_error(expr.line_info, "symbol '{s}' is not defined", .{Identifier.name});
                    Compiler.exit(1);
                }
            },
        }
    };

    expr.typ = result.typ;

    return result;
}

fn resolve_identifier(c: *Compiler, expr: *Ast.Expr, scope: *Ast.Scope) *Ast.Symbol {
    switch (expr.as) {
        .Identifier => |*Identifier| {
            Identifier.scope = scope;

            const has_symbol = c.find_symbol_in_scope(.{
                .name = Identifier.name,
                .scope = Identifier.scope,
            }, expr.line_info.offset);

            if (has_symbol) |symbol| {
                expr.as = .{ .Symbol = symbol };
                return symbol;
            } else {
                c.report_error(expr.line_info, "symbol '{s}' is not defined", .{Identifier.name});
                Compiler.exit(1);
            }
        },
        else => {
            c.report_error(expr.line_info, "expected identifier", .{});
            Compiler.exit(1);
        },
    }
}

fn reject_void_type(c: *Compiler, typ: *Ast.Type) void {
    if (typ.equal(Ast.void_type)) {
        c.report_error(typ.line_info, "unexpected 'void' type", .{});
        Compiler.exit(1);
    }
}

fn reject_symbol(c: *Compiler, stmt: *Ast.Stmt) void {
    if (stmt.as == .Symbol) {
        c.report_error(stmt.line_info, "unexpected symbol definition", .{});
        Compiler.exit(1);
    }
}

fn safe_cast_two_integers(c: *Compiler, lhs: *Ast.Expr, lhs_type: *Ast.Type, rhs: *Ast.Expr, rhs_type: *Ast.Type) ?*Ast.Type {
    if (lhs_type.equal(rhs_type)) {
        return lhs_type;
    }

    const casted_lhs = safe_cast_to_integer(c, lhs, lhs_type, .Both);
    const casted_rhs = safe_cast_to_integer(c, rhs, rhs_type, .Both);

    if (casted_lhs == null and casted_rhs == null) {
        return null;
    }

    const new_lhs_type = casted_lhs.?;
    const new_rhs_type = casted_rhs.?;
    const lInteger = &new_lhs_type.data.as.Integer;
    const rInteger = &new_rhs_type.data.as.Integer;

    const Case = enum(u4) {
        ULU = 0,
        UGU = 1,
        UEU = 2,

        ULI = 3,
        UGI = 4,
        UEI = 5,

        ILU = 6,
        IGU = 7,
        IEU = 8,

        ILI = 9,
        IGI = 10,
        IEI = 11,
    };

    const case: Case = case: {
        const order = utils.compare(lInteger.bits, rInteger.bits);
        const index = 3 * ((2 * @as(u8, @intFromBool(lInteger.is_signed))) + @intFromBool(rInteger.is_signed)) + @intFromEnum(order);
        break :case @enumFromInt(index);
    };

    const Side = enum {
        None,
        Left,
        Right,
        Both,
    };

    var casted: ?*Ast.Type = null;
    var which_side_to_cast: Side = .None;
    switch (case) {
        .ULU, .ILI, .ULI => {
            which_side_to_cast = .Left;
            casted = new_rhs_type;
        },
        .UGU, .IGI, .IGU => {
            which_side_to_cast = .Right;
            casted = new_lhs_type;
        },
        .UGI, .UEI, .ILU, .IEU => {
            const bits = @max(lInteger.bits, rInteger.bits) + 1;
            if (bits <= 64) {
                which_side_to_cast = .Both;
                casted = Ast.lookup_integer_type(bits, true);
            }
        },
        .UEU, .IEI => {
            which_side_to_cast = .None;
            casted = new_lhs_type;
        },
    }

    // Should we undo the cast if integers are incompatible?

    if (which_side_to_cast == .Left or which_side_to_cast == .Both) {
        const typ = casted.?;
        const subexpr = c.ast.create(Ast.Expr);
        subexpr.* = lhs.*;
        lhs.* = .{
            .line_info = subexpr.line_info,
            .as = .{ .Cast = .{
                .typ = typ,
                .expr = subexpr,
            } },
            .typ = typ,
            .flags = .{
                .is_const = subexpr.flags.is_const,
            },
            .typechecking = .Done,
        };
    }

    if (which_side_to_cast == .Right or which_side_to_cast == .Both) {
        const typ = casted.?;
        const subexpr = c.ast.create(Ast.Expr);
        subexpr.* = rhs.*;
        rhs.* = .{
            .line_info = subexpr.line_info,
            .as = .{ .Cast = .{
                .typ = typ,
                .expr = subexpr,
            } },
            .typ = typ,
            .flags = .{
                .is_const = subexpr.flags.is_const,
            },
            .typechecking = .Done,
        };
    }

    return casted;
}

fn safe_cast_to_integer(c: *Compiler, expr: *Ast.Expr, expr_type: *Ast.Type, what_sign: Sign) ?*Ast.Type {
    var should_cast = false;
    var typ: *Ast.Type = switch (expr_type.data.as) {
        .Struct,
        .Union,
        .Array,
        .Void,
        => return null,
        .Enum => |Enum| Enum.integer_type,
        .Proc, .Pointer => Ast.lookup_integer_type(64, false),
        .Integer => expr_type,
        .Bool => Ast.lookup_integer_type(1, false),
        .Identifier, .Type_Of => unreachable,
    };

    {
        const Integer = &typ.data.as.Integer;
        switch (what_sign) {
            .Signed => {
                if (Integer.is_signed) {
                    // NOTE[reinterp-expr]: Reinterpret the value, no need to cast it.
                } else if (Integer.bits < 64) {
                    should_cast = true;
                    typ = Ast.lookup_integer_type(Integer.bits + 1, true);
                } else {
                    return null;
                }
            },
            .Unsigned => {
                if (Integer.is_signed) {
                    return null;
                }
                // NOTE[reinterp-expr].
            },
            .Both => {
                // NOTE[reinterp-expr].
            },
        }
    }

    if (should_cast) {
        const subexpr = c.ast.create(Ast.Expr);
        subexpr.* = expr.*;
        expr.* = .{
            .line_info = subexpr.line_info,
            .as = .{ .Cast = .{
                .typ = typ,
                .expr = subexpr,
            } },
            .typ = typ,
            .flags = .{
                .is_const = subexpr.flags.is_const,
            },
            .typechecking = .Done,
        };
    }

    expr.typ = typ;

    return typ;
}

fn safe_cast(c: *Compiler, expr: *Ast.Expr, expr_type: *Ast.Type, cast_to: *Ast.Type) bool {
    if (expr_type.equal(cast_to)) {
        return true;
    }

    const can_cast = can_cast: {
        switch (cast_to.data.as) {
            .Struct,
            .Union,
            .Enum,
            .Array,
            .Void,
            => {},
            .Proc => {
                const is_void_pointer = expr_type.compare().is_void_pointer;
                if (is_void_pointer) {
                    break :can_cast true;
                }
            },
            .Pointer => |dsubtype| {
                switch (expr_type.data.as) {
                    .Array => |Array| {
                        if (Array.subtype.equal(dsubtype)) {
                            break :can_cast true;
                        }
                    },
                    .Pointer => |ssubtype| {
                        if (dsubtype.equal(Ast.void_type) or ssubtype.equal(Ast.void_type)) {
                            return true; // Don't need to cast.
                        }
                    },
                    else => {},
                }
            },
            .Integer => |dInteger| {
                const casted = safe_cast_to_integer(c, expr, expr_type, if (dInteger.is_signed) .Signed else .Unsigned);

                if (casted) |typ| {
                    const sInteger = &typ.data.as.Integer;
                    if (dInteger.bits >= sInteger.bits) { // TODO: 'safe_cast_two_integers' does more general version of this. Should reuse it somehow?
                        if (dInteger.is_signed == sInteger.is_signed) {
                            break :can_cast true;
                        } else if (dInteger.is_signed and sInteger.bits < 64) {
                            break :can_cast true;
                        }
                    }
                }
            },
            .Bool => {
                switch (expr_type.data.as) {
                    .Integer => |Integer| {
                        if (Integer.bits == 0 or (Integer.bits == 1 and !Integer.is_signed)) {
                            break :can_cast true;
                        }
                    },
                    else => {},
                }
            },
            .Identifier, .Type_Of => unreachable,
        }

        break :can_cast false;
    };

    if (can_cast) {
        const subexpr = c.ast.create(Ast.Expr);
        subexpr.* = expr.*;
        expr.* = .{
            .line_info = subexpr.line_info,
            .as = .{ .Cast = .{
                .typ = cast_to,
                .expr = subexpr,
            } },
            .typ = cast_to,
            .flags = .{
                .is_const = subexpr.flags.is_const,
            },
            .typechecking = .Done,
        };
    }

    return can_cast;
}

fn symetric_safe_cast(c: *Compiler, lhs: *Ast.Expr, lhs_type: *Ast.Type, rhs: *Ast.Expr, rhs_type: *Ast.Type) bool {
    return safe_cast(c, lhs, lhs_type, rhs_type) or safe_cast(c, rhs, rhs_type, lhs_type);
}

fn can_unsafe_cast(c: *Compiler, expr: *Ast.Expr, expr_type: *Ast.Type, cast_to: *Ast.Type) bool {
    if (safe_cast(c, expr, expr_type, cast_to)) {
        return true;
    }

    var can_cast = false;

    switch (cast_to.data.as) {
        .Struct, .Union, .Proc, .Array, .Void => {},
        .Pointer, .Enum, .Integer, .Bool => {
            switch (expr_type.data.as) {
                .Pointer,
                .Enum,
                .Integer,
                .Bool,
                => can_cast = true,
                else => {},
            }
        },
        .Identifier, .Type_Of => unreachable,
    }

    return can_cast;
}

fn make_expr_pointer_mul_integer(c: *Compiler, expr: *Ast.Expr, data: *Ast.Type.SharedData) *Ast.Expr {
    const size = utils.align_u64(data.byte_size, data.alignment);
    const rhs = c.ast.create(Ast.Expr);
    rhs.* = .{
        .line_info = expr.line_info,
        .as = .{ .Integer = size },
        .typ = Ast.integer_type_from_u64(size),
        .flags = .{
            .is_const = true,
        },
        .typechecking = .Done,
    };
    std.debug.assert(symetric_safe_cast(c, expr, expr.typ, rhs, rhs.typ));
    const new_expr = c.ast.create(Ast.Expr);
    new_expr.* = .{
        .line_info = expr.line_info,
        .as = .{ .Binary_Op = .{
            .lhs = expr,
            .rhs = rhs,
            .tag = .Mul,
            .line_info = undefined,
        } },
        .typ = expr.typ,
        .flags = .{
            .is_const = expr.flags.is_const,
        },
        .typechecking = .Done,
    };
    return new_expr;
}
