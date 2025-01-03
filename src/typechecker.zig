enum_type: ?*Ast.Type,
return_type: ?*Ast.Type,
scope_bound: *Ast.Scope,
is_in_loop: bool,
interp: Interpreter,
ast: *Ast,
irgen: *IRGen,
c: *Compiler,

const Typechecker = @This();

pub fn typecheck(c: *Compiler, ast: *Ast, irgen: *IRGen) void {
    var typechecker = Typechecker{
        .enum_type = null,
        .return_type = null,
        .scope_bound = &Ast.global_scope,
        .is_in_loop = false,
        .ast = ast,
        .irgen = irgen,
        .interp = Interpreter.init(irgen.ir),
        .c = c,
    };

    typecheck_top_level(&typechecker);

    typechecker.interp.deinit();
}

fn typecheck_top_level(t: *Typechecker) void {
    var it = t.ast.globals.first;
    while (it) |node| {
        typecheck_symbol(t, node.data);
        it = node.next;
    }

    const has_main = t.c.find_symbol_in_scope(.{
        .name = "main",
        .scope = &Ast.global_scope,
    }, false, 0);

    if (has_main) |main| {
        t.ast.main = main;
        switch (main.as) {
            .Procedure => |Procedure| {
                const Proc = &Procedure.typ.data.as.Proc;

                if (Proc.params.len != 0) {
                    t.c.report_error(Procedure.typ.line_info, "expected 0 arguments, but got {}", .{Proc.params.len});
                    Compiler.exit(1);
                }

                if (!Proc.return_type.equal(Ast.void_type)) {
                    t.c.report_error(Proc.return_type.line_info, "expected 'void', but got '{}'", .{Proc.return_type});
                    Compiler.exit(1);
                }
            },
            else => {
                t.c.report_error(main.line_info, "'main' isn't a procedure", .{});
                Compiler.exit(1);
            },
        }
    } else {
        t.c.report_error(.{ .line = 1, .column = 1, .offset = 0 }, "'main' isn't defined", .{});
        Compiler.exit(1);
    }

    if (t.c.had_error) {
        Compiler.exit(1);
    }
}

// Expression is not struct/union/array.
fn compute_simple_expr(t: *Typechecker, expr: *Ast.Expr) u64 {
    const op = compute_expr(t, expr);
    const address = t.interp.grab_value_from_operand(op, false);
    return t.interp.read(address, @intCast(expr.typ.data.byte_size), expr.typ.is_signed());
}

fn compute_expr_to_operand(t: *Typechecker, dst: IRE.Operand, expr: *Ast.Expr) void {
    std.debug.assert(dst.is_lvalue());
    const op = compute_expr(t, expr);
    const dst_address = t.interp.grab_value_from_operand(dst.addr_of().decode(), false);
    const src_address = t.interp.grab_value_from_operand(op, false);
    t.interp.write_big(dst_address, src_address, expr.typ.data.byte_size);
}

fn compute_expr(t: *Typechecker, expr: *Ast.Expr) IRD.Operand {
    std.debug.assert(expr.flags.is_const);

    var old_irgen = t.irgen.*;

    const dst = t.irgen.grab_local_from_type(expr.typ.data);
    std.debug.assert(dst.is_lvalue());
    _ = t.irgen.generate_ir_expr(dst, expr);
    t.irgen.generate_ir_instr0(.exit);
    t.irgen.remove_labels();
    t.interp.reset();
    t.interp.interpret();

    t.irgen.ir.instrs.clearRetainingCapacity();

    old_irgen.labels = t.irgen.labels;
    t.irgen.* = old_irgen;

    return dst.addr_of().decode();
}

fn typecheck_symbol_type(t: *Typechecker, symbol: *Ast.Symbol) void {
    switch (symbol.as) {
        .Variable => |Variable| {
            if (Variable.typ) |typ| {
                typecheck_type(t, typ);
                reject_void_type(t, typ);
            }
        },
        .Parameter => |Parameter| {
            typecheck_type(t, Parameter.typ);
            reject_void_type(t, Parameter.typ);
            if (Parameter.value) |value| {
                t.c.report_error(value.line_info, "default values are not supported", .{});
                Compiler.exit(1);
            }
        },
        .Procedure => |Procedure| {
            typecheck_type(t, Procedure.typ);
        },
        .Struct_Field => |Field| {
            typecheck_type(t, Field.typ);
            reject_void_type(t, Field.typ);
            if (Field.value) |value| {
                t.c.report_error(value.line_info, "default values are not supported", .{});
                Compiler.exit(1);
            }
        },
        .Union_Field => |Field| {
            typecheck_type(t, Field.typ);
            reject_void_type(t, Field.typ);
            if (Field.value) |value| {
                t.c.report_error(value.line_info, "default values are not supported", .{});
                Compiler.exit(1);
            }
        },
        .Enum_Field => {},
        .Type => |typ| {
            typ.symbol = symbol;
        },
    }
}

fn typecheck_symbol(t: *Typechecker, symbol: *Ast.Symbol) void {
    switch (symbol.typechecking) {
        .None => symbol.typechecking = .Going,
        .Going => {
            t.c.report_error(symbol.line_info, "found cyclic reference", .{});
            Compiler.exit(1);
        },
        .Done => return,
    }
    defer symbol.typechecking = .Done;

    typecheck_symbol_type(t, symbol);

    const fns = struct {
        pub fn compute_variable_initializer(_t: *Typechecker, _symbol: *Ast.Symbol) void {
            if (_symbol.attributes.is_global or _symbol.attributes.is_const) {
                const Variable = &_symbol.as.Variable;
                const value = Variable.value.?;

                // TODO: compute initializer always when it's constant.
                if (value.flags.is_const) {
                    const storage = _t.irgen.grab_static_variable_storage(Variable);
                    compute_expr_to_operand(_t, storage, value);
                } else {
                    _t.c.report_error(value.line_info, "expression is not a constant", .{});
                    Compiler.exit(1);
                }
            }
        }
    };

    switch (symbol.as) {
        .Variable => |*Variable| {
            if (Variable.typ) |typ| {
                if (Variable.value) |value| {
                    const value_type = typecheck_expr_only(t, value);

                    if (!safe_cast(t, value, value_type, typ)) {
                        t.c.report_error(value.line_info, "expected '{}', but got '{}'", .{ typ, value_type });
                        t.c.report_note(typ.line_info, "expected type is here", .{});
                        t.c.report_note(value.line_info, "expression is here", .{});
                        Compiler.exit(1);
                    }

                    fns.compute_variable_initializer(t, symbol);
                } else if (symbol.attributes.is_const) {
                    t.c.report_error(symbol.line_info, "constant expression needs initializer", .{});
                    Compiler.exit(1);
                }
            } else if (Variable.value) |value| {
                const value_type = typecheck_expr_only(t, value);
                reject_void_type(t, value_type);

                Variable.typ = value_type;

                fns.compute_variable_initializer(t, symbol);
            } else {
                unreachable;
            }
        },
        .Procedure => |Procedure| {
            const old_scope_bound = t.scope_bound;
            defer t.scope_bound = old_scope_bound;

            t.scope_bound = Procedure.typ.data.as.Proc.scope;

            const old_return_type = t.return_type;
            t.return_type = Procedure.typ.data.as.Proc.return_type;

            var it = Procedure.block.first;
            while (it) |node| {
                typecheck_stmt(t, node.data);
                it = node.next;
            }

            t.return_type = old_return_type;
        },
        .Type => |typ| {
            typecheck_type(t, typ);
        },
        .Parameter,
        .Struct_Field,
        .Union_Field,
        .Enum_Field,
        => unreachable,
    }
}

fn unpack(t: *Typechecker, typ: *Ast.Type) void {
    const old_data = typ.data;
    switch (old_data.stages.unpacking) {
        .None => old_data.stages.unpacking = .Going,
        .Going => {
            t.c.report_error(typ.line_info, "found cyclic reference", .{});
            Compiler.exit(1);
        },
        .Done => return,
    }
    defer old_data.stages.unpacking = .Done;

    const fns = struct {
        pub fn unpack_symbol(_t: *Typechecker, _typ: *Ast.Type, symbol: *Ast.Symbol) void {
            if (symbol.as != .Type) {
                _t.c.report_error(_typ.line_info, "symbol '{s}' is not a type", .{symbol.key.name});
                Compiler.exit(1);
            }

            const new_typ = symbol.as.Type;
            unpack(_t, new_typ);
            _typ.data = new_typ.data;
            _typ.symbol = symbol;
        }
    };

    switch (old_data.as) {
        .Array => |Array| {
            unpack(t, Array.subtype);
        },
        .Field => |Field| {
            unpack(t, Field.subtype);

            const scope: *Ast.Scope = switch (Field.subtype.data.as) {
                .Struct, .Union => |Struct| Struct.scope,
                .Enum => |Enum| Enum.scope,
                .Proc,
                .Array,
                .Pointer,
                .Integer,
                .Bool,
                .Void,
                => {
                    t.c.report_error(Field.subtype.line_info, "expected struct, union, or enum type, but got '{}'", .{Field.subtype});
                    Compiler.exit(1);
                },
                .Field,
                .Identifier,
                .Type_Of,
                => unreachable,
            };

            const symbol = resolve_identifier(t, Field.field, scope);

            fns.unpack_symbol(t, typ, symbol);
        },
        .Pointer => |subtype| {
            unpack(t, subtype);
        },
        .Identifier => |Identifier| {
            const has_symbol = t.c.find_symbol_with_scope_bound(.{
                .name = Identifier.name,
                .scope = Identifier.scope,
            }, t.scope_bound, typ.line_info.offset);

            if (has_symbol) |symbol| {
                fns.unpack_symbol(t, typ, symbol);
            } else {
                t.c.report_error(typ.line_info, "symbol '{s}' is not defined", .{Identifier.name});
                Compiler.exit(1);
            }
        },
        .Type_Of => |expr| {
            const expr_result = typecheck_expr(t, expr);
            if (expr_result.tag == .Type) {
                unpack(t, expr_result.typ);
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

fn shallow_check(t: *Typechecker, typ: *Ast.Type) void {
    unpack(t, typ);

    const data = typ.data;
    switch (data.stages.shallow_check) {
        .None => data.stages.shallow_check = .Going,
        .Going => {
            t.c.report_error(typ.line_info, "found cyclic reference", .{});
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
                        shallow_check(t, Field.typ);
                    },
                    else => unreachable,
                }
                it = node.next;
            }
        },
        .Array => |Array| {
            shallow_check(t, Array.subtype);
        },
        .Enum,
        .Proc,
        .Pointer,
        .Integer,
        .Bool,
        .Void,
        => {},
        .Field,
        .Identifier,
        .Type_Of,
        => unreachable,
    }
}

fn void_array_check(t: *Typechecker, typ: *Ast.Type) void {
    shallow_check(t, typ);

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
                        void_array_check(t, Field.typ);
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
                void_array_check(t, Parameter.typ);
                it = node.next;
            }
            void_array_check(t, Proc.return_type);
        },
        .Array => |Array| {
            void_array_check(t, Array.subtype);
            reject_void_type(t, Array.subtype);
        },
        .Pointer => |subtype| {
            void_array_check(t, subtype);
        },
        .Enum,
        .Integer,
        .Bool,
        .Void,
        => {},
        .Field,
        .Identifier,
        .Type_Of,
        => unreachable,
    }
}

// TODO: don't go through all stages when the top one is done.
fn typecheck_type(t: *Typechecker, typ: *Ast.Type) void {
    void_array_check(t, typ);

    const data = typ.data;
    switch (data.stages.full_check) {
        .None => data.stages.full_check = .Going,
        .Going, .Done => return,
    }
    defer data.stages.full_check = .Done;

    switch (data.as) {
        .Struct => |Struct| {
            const old_scope_bound = t.scope_bound;
            defer t.scope_bound = old_scope_bound;

            t.scope_bound = Struct.scope;

            var size: u64 = 0;
            var alignment: Alignment = .BYTE;

            var it = Struct.fields.first;
            while (it) |node| {
                const symbol = node.data;
                typecheck_symbol_type(t, symbol);

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

            // NOTE[check-fields-in-namespace]: should deduplicate?
            it = Struct.rest.first;
            while (it) |node| {
                typecheck_symbol(t, node.data);
                it = node.next;
            }
        },
        .Union => |Union| {
            const old_scope_bound = t.scope_bound;
            defer t.scope_bound = old_scope_bound;

            t.scope_bound = Union.scope;

            var size: u64 = 0;
            var alignment: Alignment = .BYTE;

            var it = Union.fields.first;
            while (it) |node| {
                const symbol = node.data;
                typecheck_symbol_type(t, symbol);

                const Field = &node.data.as.Union_Field;
                Field.offset = 0;

                size = @max(size, Field.typ.data.byte_size);
                alignment = @enumFromInt(@max(@intFromEnum(alignment), @intFromEnum(Field.typ.data.alignment)));

                it = node.next;
            }

            data.byte_size = size;
            data.alignment = alignment;

            // NOTE[check-fields-in-namespace].
            it = Union.rest.first;
            while (it) |node| {
                typecheck_symbol(t, node.data);
                it = node.next;
            }
        },
        .Enum => |*Enum| {
            const old_scope_bound = t.scope_bound;
            defer t.scope_bound = old_scope_bound;

            t.scope_bound = Enum.scope;

            const old_enum_type = t.enum_type;
            t.enum_type = typ;

            var next_value: u64 = 0;
            var max_value: u64 = 0;
            var is_signed = false;

            // Need to find if the enum is signed or not.
            {
                var it = Enum.fields.first;
                while (it) |node| {
                    const enum_field = &node.data.as.Enum_Field;

                    if (enum_field.value) |value| {
                        const value_type = typecheck_expr_only(t, value);
                        const new_value_type = safe_cast_to_integer(t, value, value_type, .Both);
                        if (new_value_type == null) {
                            t.c.report_error(value.line_info, "expected integer, but got '{}'\n", .{value_type});
                            Compiler.exit(1);
                        }
                        is_signed = is_signed or new_value_type.?.is_signed();
                        enum_field.computed_value = compute_simple_expr(t, value);
                    }

                    it = node.next;
                }
            }

            {
                var it = Enum.fields.first;
                while (it) |node| {
                    const enum_field = &node.data.as.Enum_Field;

                    if (enum_field.value != null) {
                        next_value = enum_field.computed_value;
                    } else {
                        enum_field.computed_value = next_value;
                    }

                    max_value = @max(max_value, next_value);
                    next_value = if (is_signed) @bitCast(@as(i64, @bitCast(next_value)) + 1) else next_value + 1;

                    it = node.next;
                }
            }

            const bits = utils.count_bits(max_value) + @intFromBool(is_signed);
            if (bits > 64) {
                t.c.report_error(typ.line_info, "enum can't be represented in <= 64 bits", .{});
                Compiler.exit(1);
            }
            const integer_type = Ast.lookup_integer_type(bits, is_signed);

            Enum.integer_type = integer_type;
            data.byte_size = integer_type.data.byte_size;
            data.alignment = integer_type.data.alignment;

            t.enum_type = old_enum_type;

            // NOTE[check-fields-in-namespace].
            var it = Enum.rest.first;
            while (it) |node| {
                typecheck_symbol(t, node.data);
                it = node.next;
            }
        },
        .Proc => |Proc| {
            std.debug.assert(data.byte_size == Ast.pointer_byte_size and
                data.alignment == Ast.pointer_alignment);

            var it = Proc.params.first;
            while (it) |node| {
                const symbol = node.data;
                typecheck_symbol_type(t, symbol);

                it = node.next;
            }
            typecheck_type(t, Proc.return_type);
        },
        .Array => |*Array| {
            typecheck_type(t, Array.subtype);

            const size_type = typecheck_expr_only(t, Array.size);
            if (safe_cast_to_integer(t, Array.size, size_type, .Unsigned) == null) {
                t.c.report_error(typ.line_info, "expected unsigned integer, but got '{}'", .{size_type});
                Compiler.exit(1);
            }

            const count = compute_simple_expr(t, Array.size);
            Array.computed_size = count;
            data.byte_size = count * Array.subtype.data.byte_size;
            data.alignment = Array.subtype.data.alignment;
        },
        .Pointer => |subtype| {
            std.debug.assert(data.byte_size == Ast.pointer_byte_size and
                data.alignment == Ast.pointer_alignment);
            typecheck_type(t, subtype);
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
        .Field,
        .Identifier,
        .Type_Of,
        => unreachable,
    }
}

fn typecheck_stmt_list(t: *Typechecker, list: Ast.StmtList) void {
    var it = list.first;
    while (it) |node| {
        typecheck_stmt(t, node.data);
        it = node.next;
    }
}

fn typecheck_stmt(t: *Typechecker, stmt: *Ast.Stmt) void {
    switch (stmt.as) {
        .Print => |expr| {
            const expr_type = typecheck_expr_only(t, expr);
            switch (expr_type.data.as) {
                .Struct, .Union, .Proc, .Array, .Void => {
                    t.c.report_error(expr.line_info, "can't print value of type '{}'", .{expr.typ});
                    Compiler.exit(1);
                },
                .Enum,
                .Pointer,
                .Integer,
                .Bool,
                => {},
                .Field, .Identifier, .Type_Of => unreachable,
            }
        },
        .Block => |block| {
            typecheck_stmt_list(t, block);
        },
        .If => |If| {
            const condition_type = typecheck_expr_only(t, If.condition);
            if (!safe_cast(t, If.condition, condition_type, Ast.bool_type)) {
                t.c.report_error(If.condition.line_info, "expected 'bool', but got '{}'", .{condition_type});
                Compiler.exit(1);
            }

            reject_symbol(t, If.true_branch);
            typecheck_stmt(t, If.true_branch);

            if (If.false_branch) |false_branch| {
                reject_symbol(t, false_branch);
                typecheck_stmt(t, false_branch);
            }
        },
        .While, .Do_While => |While| {
            const condition_type = typecheck_expr_only(t, While.condition);
            if (!safe_cast(t, While.condition, condition_type, Ast.bool_type)) {
                t.c.report_error(While.condition.line_info, "expected 'bool', but got '{}'", .{condition_type});
                Compiler.exit(1);
            }

            const old_is_in_loop = t.is_in_loop;

            reject_symbol(t, While.body);
            t.is_in_loop = true;
            typecheck_stmt(t, While.body);
            t.is_in_loop = old_is_in_loop;
        },
        .Break => {
            if (!t.is_in_loop) {
                t.c.report_error(stmt.line_info, "'break' outside of loop", .{});
                Compiler.exit(1);
            }
        },
        .Continue => {
            if (!t.is_in_loop) {
                t.c.report_error(stmt.line_info, "'continue' outside of loop", .{});
                Compiler.exit(1);
            }
        },
        .Switch => |Switch| {
            const condition_type = typecheck_expr_only(t, Switch.condition);
            const is_comparable = condition_type.compare().is_comparable;

            if (!is_comparable) {
                t.c.report_error(Switch.condition.line_info, "value of type '{}' isn't comparable", .{condition_type});
                Compiler.exit(1);
            }

            var it = Switch.cases.first;
            while (it) |node| {
                var Case = node.data;
                while (true) {
                    switch (Case.*) {
                        .Case => |case| {
                            const value_type = typecheck_expr_only(t, case.value);

                            if (!safe_cast(t, case.value, value_type, condition_type)) {
                                t.c.report_error(case.value.line_info, "mismatched types: '{}' and '{}'", .{ condition_type, value_type });
                                t.c.report_note(case.value.line_info, "switch case is here", .{});
                                t.c.report_note(Switch.condition.line_info, "switch condition is here", .{});
                                Compiler.exit(1);
                            }

                            const value = compute_simple_expr(t, case.value);
                            case.value.as = .{
                                .Integer = value,
                            };

                            Case = case.subcase;
                        },
                        .Stmt => |substmt| {
                            reject_symbol(t, substmt);
                            typecheck_stmt(t, substmt);
                            break;
                        },
                    }
                }

                it = node.next;
            }
        },
        .Return => |has_expr| {
            const return_type = t.return_type.?;

            if (has_expr) |expr| {
                if (return_type.equal(Ast.void_type)) {
                    t.c.report_error(expr.line_info, "unexpected expression here", .{});
                    Compiler.exit(1);
                } else {
                    const expr_type = typecheck_expr_only(t, expr);

                    if (!safe_cast(t, expr, expr_type, return_type)) {
                        t.c.report_error(expr.line_info, "mismatched types: '{}' and '{}'", .{ return_type, expr_type });
                        t.c.report_note(return_type.line_info, "return type is here", .{});
                        t.c.report_note(expr.line_info, "expression is here", .{});
                        Compiler.exit(1);
                    }
                }
            } else if (!return_type.equal(Ast.void_type)) {
                t.c.report_error(stmt.line_info, "expected expression of type '{}'", .{return_type});
                Compiler.exit(1);
            }
        },
        .Symbol => |symbol| {
            typecheck_symbol(t, symbol);
        },
        .Assign => |Assign| {
            const lhs_type = typecheck_expr_only(t, Assign.lhs);

            if (!Assign.lhs.flags.is_lvalue) {
                t.c.report_error(Assign.lhs.line_info, "expression is not an lvalue", .{});
                Compiler.exit(1);
            } else if (Assign.lhs.flags.is_const) {
                t.c.report_error(Assign.lhs.line_info, "can't assign to constant expression", .{});
                Compiler.exit(1);
            }

            const rhs_type = typecheck_expr_only(t, Assign.rhs);

            if (!safe_cast(t, Assign.rhs, rhs_type, lhs_type)) {
                t.c.report_error(stmt.line_info, "expected '{}', but got '{}'", .{ lhs_type, rhs_type });
                Compiler.exit(1);
            }
        },
        .Expr => |expr| {
            _ = typecheck_expr_only(t, expr);
        },
    }
}

fn typecheck_expr_only(t: *Typechecker, expr: *Ast.Expr) *Ast.Type {
    const result = typecheck_expr(t, expr);
    if (result.tag != .Value) {
        t.c.report_error(expr.line_info, "expected expression", .{});
        Compiler.exit(1);
    }
    return result.typ;
}

// Types are typechecked lazily (only when used). 'cause if a type is not fully formed (like ambiguous pointer/array), we need to typecheck it from the top level.
fn typecheck_expr(t: *Typechecker, expr: *Ast.Expr) TypecheckExprResult {
    switch (expr.typechecking) {
        .None => expr.typechecking = .Going,
        .Going => {
            t.c.report_error(expr.line_info, "found cyclic reference", .{});
            Compiler.exit(1);
        },
        .Done => return .{ .typ = expr.typ, .tag = if (expr.as == .Type) .Type else .Value }, // Non values should be filtered at this point?
    }
    defer expr.typechecking = .Done;

    const fns = struct {
        pub fn typecheck_expr_symbol(_t: *Typechecker, _expr: *Ast.Expr, symbol: *Ast.Symbol) TypecheckExprResult {
            _expr.as = .{ .Symbol = symbol };
            _expr.flags.is_const = symbol.attributes.is_const;
            _expr.flags.is_static = symbol.attributes.is_static;

            switch (symbol.as) {
                .Variable => |*Variable| {
                    typecheck_symbol(_t, symbol); // TODO: allow cyclic references when using #type_of/#alignment_of, etc.

                    _expr.flags.is_lvalue = true;

                    return .{ .typ = Variable.typ.?, .tag = .Value };
                },
                .Parameter => |Parameter| {
                    _expr.flags.is_lvalue = true;
                    return .{ .typ = Parameter.typ, .tag = .Value };
                },
                .Procedure => |Procedure| {
                    return .{ .typ = Procedure.typ, .tag = .Value };
                },
                .Type => |Type| {
                    return .{ .typ = Type, .tag = .Type };
                },
                .Struct_Field, .Union_Field => |Field| {
                    return .{ .typ = Field.typ, .tag = .Non_Value };
                },
                .Enum_Field => {
                    return .{ .typ = _t.enum_type.?, .tag = .Value };
                },
            }
        }
    };

    const result: TypecheckExprResult = result: {
        switch (expr.as) {
            .Binary_Op => |*Binary_Op| {
                const lhs_type = typecheck_expr_only(t, Binary_Op.lhs);
                const rhs_type = typecheck_expr_only(t, Binary_Op.rhs);
                const lhs_flags = lhs_type.compare();
                const rhs_flags = rhs_type.compare();

                expr.flags.is_const = Binary_Op.lhs.flags.is_const and
                    Binary_Op.rhs.flags.is_const;

                switch (Binary_Op.tag) {
                    .Or, .And => {
                        if (!safe_cast(t, Binary_Op.lhs, lhs_type, Ast.bool_type) or
                            !safe_cast(t, Binary_Op.rhs, rhs_type, Ast.bool_type))
                        {
                            t.c.report_error(Binary_Op.line_info, "mismatched types: '{}' and '{}'", .{ lhs_type, rhs_type });
                            break :result .{ .typ = Ast.bool_type, .tag = .Value };
                        }

                        switch (Binary_Op.tag) {
                            .Or => {
                                const true_branch = t.ast.create(Ast.Expr);
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
                                const false_branch = t.ast.create(Ast.Expr);
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
                        if (!((lhs_flags.is_comparable and safe_cast(t, Binary_Op.rhs, rhs_type, lhs_type)) or
                            (rhs_flags.is_comparable and safe_cast(t, Binary_Op.lhs, lhs_type, rhs_type))))
                        {
                            t.c.report_error(Binary_Op.line_info, "can't compare '{}' and '{}'", .{ lhs_type, rhs_type });
                        }

                        break :result .{ .typ = Ast.bool_type, .tag = .Value };
                    },
                    .Lt, .Leq, .Gt, .Geq => {
                        if (!lhs_flags.is_ordered or !rhs_flags.is_ordered) {
                            t.c.report_error(Binary_Op.line_info, "expression of type '{}' is not comparable", .{lhs_type});
                        } else if (!symetric_safe_cast(t, Binary_Op.lhs, lhs_type, Binary_Op.rhs, rhs_type)) {
                            t.c.report_error(Binary_Op.line_info, "mismatched types: '{}' and '{}'", .{ lhs_type, rhs_type });
                        }

                        break :result .{ .typ = Ast.bool_type, .tag = .Value };
                    },
                    .Add => {
                        if (lhs_flags.is_void_pointer or rhs_flags.is_void_pointer) {
                            t.c.report_error(Binary_Op.line_info, "can't add 'void' pointers", .{});
                            Compiler.exit(1);
                        } else if (lhs_flags.is_pointer and safe_cast_to_integer(t, Binary_Op.rhs, rhs_type, .Unsigned) != null) {
                            Binary_Op.rhs = make_expr_pointer_mul_integer(t, Binary_Op.rhs, lhs_type.data.as.Pointer.data);
                            break :result .{ .typ = lhs_type, .tag = .Value };
                        } else if (rhs_flags.is_pointer and safe_cast_to_integer(t, Binary_Op.lhs, lhs_type, .Unsigned) != null) {
                            Binary_Op.lhs = make_expr_pointer_mul_integer(t, Binary_Op.lhs, rhs_type.data.as.Pointer.data);
                            break :result .{ .typ = rhs_type, .tag = .Value };
                        } else {
                            const casted = safe_cast_two_integers(t, Binary_Op.lhs, lhs_type, Binary_Op.rhs, rhs_type);
                            if (casted == null) {
                                t.c.report_error(Binary_Op.line_info, "expected integer/pointer: '{}' and '{}'", .{ lhs_type, rhs_type });
                                Compiler.exit(1);
                            }
                            break :result .{ .typ = casted.?, .tag = .Value };
                        }
                    },
                    .Sub => {
                        if (lhs_flags.is_void_pointer) {
                            t.c.report_error(Binary_Op.line_info, "can't subtract 'void' pointer", .{});
                            Compiler.exit(1);
                        } else if (lhs_flags.is_pointer and safe_cast_to_integer(t, Binary_Op.rhs, rhs_type, .Unsigned) != null) {
                            Binary_Op.rhs = make_expr_pointer_mul_integer(t, Binary_Op.rhs, lhs_type.data.as.Pointer.data);
                            break :result .{ .typ = lhs_type, .tag = .Value };
                        } else {
                            const casted = safe_cast_two_integers(t, Binary_Op.lhs, lhs_type, Binary_Op.rhs, rhs_type);
                            if (casted == null) {
                                t.c.report_error(Binary_Op.line_info, "expected integer/pointer: '{}' and '{}'", .{ lhs_type, rhs_type });
                                Compiler.exit(1);
                            }
                            break :result .{ .typ = casted.?, .tag = .Value };
                        }
                    },
                    .Mul, .Div, .Mod => {
                        const casted = safe_cast_two_integers(t, Binary_Op.lhs, lhs_type, Binary_Op.rhs, rhs_type);
                        if (casted == null) {
                            t.c.report_error(Binary_Op.line_info, "expected integer/pointer: '{}' and '{}'", .{ lhs_type, rhs_type });
                            Compiler.exit(1);
                        }
                        break :result .{ .typ = casted.?, .tag = .Value };
                    },
                }
            },
            .Unary_Op => |Unary_Op| {
                const subexpr_type = typecheck_expr_only(t, Unary_Op.subexpr);

                expr.flags.is_const = Unary_Op.subexpr.flags.is_const;

                switch (Unary_Op.tag) {
                    .Pos => {
                        const casted = safe_cast_to_integer(t, Unary_Op.subexpr, subexpr_type, .Both);
                        if (casted == null) {
                            t.c.report_error(Unary_Op.subexpr.line_info, "expected integer, but got '{}'", .{subexpr_type});
                            Compiler.exit(1);
                        }
                        break :result .{ .typ = casted.?, .tag = .Value };
                    },
                    .Neg => {
                        const casted = safe_cast_to_integer(t, Unary_Op.subexpr, subexpr_type, .Signed);
                        if (casted == null) {
                            t.c.report_error(Unary_Op.subexpr.line_info, "expected integer, but got '{}'", .{subexpr_type});
                            Compiler.exit(1);
                        }
                        break :result .{ .typ = casted.?, .tag = .Value };
                    },
                    .Not => {
                        if (!safe_cast(t, Unary_Op.subexpr, subexpr_type, Ast.bool_type)) {
                            t.c.report_error(Unary_Op.subexpr.line_info, "expected 'bool', but got '{}'", .{subexpr_type});
                        }

                        break :result .{ .typ = Ast.bool_type, .tag = .Value };
                    },
                }
            },
            .Ref => |subexpr| {
                const subexpr_type = typecheck_expr_only(t, subexpr);

                if (!subexpr.flags.is_lvalue) {
                    t.c.report_error(subexpr.line_info, "expression is not an lvalue", .{});
                    Compiler.exit(1);
                } else if (subexpr.flags.is_const) {
                    t.c.report_error(subexpr.line_info, "can't take raferences to constant expression", .{});
                    Compiler.exit(1);
                }

                const data = t.ast.create(Ast.Type.SharedData);
                data.* = .{
                    .as = .{ .Pointer = subexpr_type },
                    .byte_size = Ast.pointer_byte_size,
                    .alignment = Ast.pointer_alignment,
                    .stages = Ast.default_stages_done,
                };
                const typ = t.ast.create(Ast.Type);
                typ.* = .{
                    .line_info = expr.line_info,
                    .data = data,
                    .symbol = null,
                };

                expr.flags.is_const = subexpr.flags.is_const or subexpr.flags.is_static;

                break :result .{ .typ = typ, .tag = .Value };
            },
            .Deref => |subexpr| {
                const subexpr_result = typecheck_expr(t, subexpr);

                if (subexpr_result.tag == .Type) {
                    const data = t.ast.create(Ast.Type.SharedData);
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
                        t.c.report_error(subexpr.line_info, "can't dereference expression of type '{}'", .{subexpr_type});
                        Compiler.exit(1);
                    }

                    expr.flags.is_lvalue = true;
                    expr.flags.is_const = subexpr.flags.is_static;

                    break :result .{ .typ = subexpr_type.data.as.Pointer, .tag = .Value };
                }
            },
            .If => |If| {
                const condition_type = typecheck_expr_only(t, If.condition);

                if (!safe_cast(t, If.condition, condition_type, Ast.bool_type)) {
                    t.c.report_error(If.condition.line_info, "expected 'bool', but got '{}'", .{condition_type});
                    Compiler.exit(1);
                }

                const true_branch_type = typecheck_expr_only(t, If.true_branch);
                const false_branch_type = typecheck_expr_only(t, If.false_branch);

                if (!symetric_safe_cast(t, If.true_branch, true_branch_type, If.false_branch, false_branch_type)) {
                    t.c.report_error(If.condition.line_info, "mismatched types: '{}' and '{}'", .{ true_branch_type, false_branch_type });
                    Compiler.exit(1);
                }

                expr.flags.is_const = If.condition.flags.is_const and
                    If.true_branch.flags.is_const and
                    If.false_branch.flags.is_const;

                break :result .{ .typ = true_branch_type, .tag = .Value };
            },
            .Call => |Call| {
                const subexpr_result = typecheck_expr(t, Call.subexpr);

                if (subexpr_result.tag == .Type) {
                    const typ = subexpr_result.typ;

                    var is_const = false;

                    typecheck_type(t, typ);
                    switch (typ.data.as) {
                        .Struct, .Union => |Struct| {
                            is_const = true;

                            var it = Call.args.first;
                            while (it) |node| {
                                switch (node.data.*) {
                                    .Designator => |Designator| {
                                        const symbol = resolve_identifier(t, Designator.lhs, Struct.scope);

                                        switch (symbol.as) {
                                            .Struct_Field, .Union_Field => |Field| {
                                                const rhs_type = typecheck_expr_only(t, Designator.rhs);
                                                if (!safe_cast(t, Designator.rhs, rhs_type, Field.typ)) {
                                                    t.c.report_error(Designator.rhs.line_info, "expected '{}', but got '{}'", .{ Field.typ, rhs_type });
                                                    Compiler.exit(1);
                                                }
                                                is_const = is_const and Designator.rhs.flags.is_const;
                                            },
                                            else => {
                                                t.c.report_error(typ.line_info, "expected a structure or union field", .{});
                                                Compiler.exit(1);
                                            },
                                        }
                                    },
                                    .Expr => |subexpr| {
                                        t.c.report_error(subexpr.line_info, "unexpected expression", .{});
                                        Compiler.exit(1);
                                    },
                                }

                                it = node.next;
                            }
                        },
                        .Array => |Array| {
                            if (Array.computed_size != Call.args.len) {
                                t.c.report_error(expr.line_info, "expected {} values, but got {}", .{ Array.computed_size, Call.args.len });
                                Compiler.exit(1);
                            }

                            is_const = true;

                            var it = Call.args.first;
                            while (it) |node| {
                                switch (node.data.*) {
                                    .Designator => |Designator| {
                                        t.c.report_error(Designator.lhs.line_info, "unexpected designator", .{});
                                        Compiler.exit(1);
                                    },
                                    .Expr => |arg| {
                                        const arg_type = typecheck_expr_only(t, arg);
                                        if (!safe_cast(t, arg, arg_type, Array.subtype)) {
                                            t.c.report_error(arg.line_info, "expected '{}', but got '{}'", .{ Array.subtype, arg_type });
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
                                t.c.report_error(expr.line_info, "expected 1 argument for scalar type, but got {}", .{Call.args.len});
                                Compiler.exit(1);
                            }

                            switch (Call.args.first.?.data.*) {
                                .Designator => |Designator| {
                                    t.c.report_error(Designator.lhs.line_info, "unexpected designator", .{});
                                    Compiler.exit(1);
                                },
                                .Expr => |arg| {
                                    const arg_type = typecheck_expr_only(t, arg);

                                    if (!safe_cast(t, arg, arg_type, typ)) {
                                        t.c.report_error(expr.line_info, "expected '{}', but got '{}'", .{ typ, arg_type });
                                        Compiler.exit(1);
                                    }

                                    is_const = arg.flags.is_const;
                                },
                            }
                        },
                        .Void => {
                            t.c.report_error(expr.line_info, "can't construct expression of type 'void'", .{});
                            Compiler.exit(1);
                        },
                        .Field,
                        .Identifier,
                        .Type_Of,
                        => unreachable,
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
                        t.c.report_error(Call.subexpr.line_info, "expected a procedure, but got '{}'", .{subexpr_type});
                        Compiler.exit(1);
                    }

                    const Proc = &subexpr_type.data.as.Proc;

                    if (Proc.params.len != Call.args.len) {
                        t.c.report_error(Call.subexpr.line_info, "expected {} arguments, but got {}", .{ Proc.params.len, Call.args.len });
                        Compiler.exit(1);
                    }

                    var pit = Proc.params.first;
                    var ait = Call.args.first;
                    while (pit != null) {
                        const pnode = pit.?;
                        const anode = ait.?;

                        const Parameter = &pnode.data.as.Parameter;
                        const param_type = Parameter.typ;

                        switch (anode.data.*) {
                            .Designator => |Designator| {
                                t.c.report_error(Designator.lhs.line_info, "unexpected designator", .{});
                                Compiler.exit(1);
                            },
                            .Expr => |arg| {
                                const arg_type = typecheck_expr_only(t, arg);

                                if (!safe_cast(t, arg, arg_type, param_type)) {
                                    t.c.report_error(arg.line_info, "expected '{}', but got '{}'", .{ param_type, arg_type });
                                    Compiler.exit(1);
                                }
                            },
                        }

                        pit = pnode.next;
                        ait = anode.next;
                    }

                    break :result .{ .typ = Proc.return_type, .tag = .Value };
                }
            },
            .Constructor => unreachable,
            .Subscript => |Subscript| {
                const subexpr_result = typecheck_expr(t, Subscript.subexpr);

                if (subexpr_result.tag == .Type) {
                    const data = t.ast.create(Ast.Type.SharedData);
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
                    const index_type = typecheck_expr_only(t, Subscript.index);

                    if (safe_cast_to_integer(t, Subscript.index, index_type, .Both) == null) {
                        t.c.report_error(Subscript.subexpr.line_info, "expected integer, but got '{}'", .{index_type});
                        Compiler.exit(1);
                    }

                    expr.flags.is_lvalue = Subscript.subexpr.flags.is_lvalue;
                    expr.flags.is_const = Subscript.subexpr.flags.is_const and Subscript.index.flags.is_const;
                    expr.flags.is_static = Subscript.subexpr.flags.is_static;

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
                            t.c.report_error(Subscript.subexpr.line_info, "expected array or pointer to array, but got '{}'", .{subexpr_type});
                            Compiler.exit(1);
                        },
                    }
                }
            },
            .Field => |Field| {
                const subexpr_result = typecheck_expr(t, Field.subexpr);

                if (subexpr_result.tag == .Type) {
                    typecheck_type(t, subexpr_result.typ);

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
                            t.c.report_error(subexpr_result.typ.line_info, "expected struct, union, or enum type, but got '{}'", .{subexpr_result.typ});
                            Compiler.exit(1);
                        },
                        .Field,
                        .Identifier,
                        .Type_Of,
                        => unreachable,
                    };

                    const symbol = resolve_identifier(t, Field.field, scope);

                    switch (symbol.as) {
                        .Type => |Type| {
                            expr.as = .{ .Type = .{
                                .line_info = expr.line_info,
                                .data = Type.data,
                                .symbol = symbol,
                            } };

                            expr.flags.is_const = true;

                            break :result .{ .typ = &expr.as.Type, .tag = .Type };
                        },
                        else => {
                            break :result fns.typecheck_expr_symbol(t, expr, symbol);
                        },
                    }
                } else {
                    expr.flags.is_lvalue = Field.subexpr.flags.is_lvalue;
                    expr.flags.is_const = Field.subexpr.flags.is_const;
                    expr.flags.is_static = Field.subexpr.flags.is_static;

                    const scope: *Ast.Scope = switch (subexpr_result.typ.data.as) {
                        .Struct, .Union => |Struct| Struct.scope,
                        .Pointer => |subtype| switch (subtype.data.as) {
                            .Struct, .Union => |Struct| Struct.scope,
                            else => {
                                t.c.report_error(Field.subexpr.line_info, "expected pointer to struct/union, but got '{}'", .{subexpr_result.typ});
                                Compiler.exit(1);
                            },
                        },
                        else => {
                            t.c.report_error(Field.subexpr.line_info, "expected struct, union, or enum, but got '{}'", .{subexpr_result.typ});
                            Compiler.exit(1);
                        },
                    };

                    const symbol = resolve_identifier(t, Field.field, scope);

                    switch (symbol.as) {
                        .Struct_Field, .Union_Field => |Struct_Field| {
                            break :result .{ .typ = Struct_Field.typ, .tag = .Value };
                        },
                        else => unreachable,
                    }
                }
            },
            .Byte_Size_Of => |subexpr| {
                const subexpr_result = typecheck_expr(t, subexpr);
                if (subexpr_result.tag == .Type) {
                    typecheck_type(t, subexpr_result.typ);
                }
                const byte_size = subexpr_result.typ.data.byte_size;

                expr.as = .{ .Integer = byte_size };
                expr.flags.is_const = true;

                break :result .{ .typ = Ast.integer_type_from_u64(byte_size), .tag = .Value };
            },
            .Alignment_Of => |subexpr| {
                const subexpr_result = typecheck_expr(t, subexpr);
                if (subexpr_result.tag == .Type) {
                    typecheck_type(t, subexpr_result.typ);
                }
                const alignment = subexpr_result.typ.data.alignment.to_byte_size();

                expr.as = .{ .Integer = alignment };
                expr.flags.is_const = true;

                break :result .{ .typ = Ast.integer_type_from_u64(alignment), .tag = .Value };
            },
            .As => |As| {
                typecheck_type(t, As.typ);
                const expr_type = typecheck_expr_only(t, As.expr);
                if (!safe_cast(t, As.expr, expr_type, As.typ)) {
                    t.c.report_error(As.expr.line_info, "can't reinterpret '{}' as '{}'", .{ As.typ, expr_type });
                    Compiler.exit(1);
                }
                expr.flags.is_const = As.expr.flags.is_const;
                break :result .{ .typ = As.typ, .tag = .Value };
            },
            .Cast => |Cast| {
                typecheck_type(t, Cast.typ);
                const expr_type = typecheck_expr_only(t, Cast.expr);
                if (!can_unsafe_cast(t, Cast.expr, expr_type, Cast.typ)) {
                    t.c.report_error(Cast.expr.line_info, "can't cast '{}' to '{}'", .{ expr_type, Cast.typ });
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
                const has_symbol = t.c.find_symbol_with_scope_bound(.{
                    .name = Identifier.name,
                    .scope = Identifier.scope,
                }, t.scope_bound, expr.line_info.offset);

                if (has_symbol) |symbol| {
                    typecheck_symbol_type(t, symbol);

                    break :result fns.typecheck_expr_symbol(t, expr, symbol);
                } else {
                    t.c.report_error(expr.line_info, "symbol '{s}' is not defined", .{Identifier.name});
                    Compiler.exit(1);
                }
            },
        }
    };

    expr.typ = result.typ;

    return result;
}

fn resolve_identifier(t: *Typechecker, expr: *Ast.Expr, scope: *Ast.Scope) *Ast.Symbol {
    switch (expr.as) {
        .Identifier => |*Identifier| {
            Identifier.scope = scope;

            const has_symbol = t.c.find_symbol_in_scope(.{
                .name = Identifier.name,
                .scope = Identifier.scope,
            }, false, expr.line_info.offset);

            if (has_symbol) |symbol| {
                expr.as = .{ .Symbol = symbol };
                return symbol;
            } else {
                t.c.report_error(expr.line_info, "symbol '{s}' is not defined", .{Identifier.name});
                Compiler.exit(1);
            }
        },
        else => {
            t.c.report_error(expr.line_info, "expected identifier", .{});
            Compiler.exit(1);
        },
    }
}

// TODO: line info is not properly reported.
fn reject_void_type(t: *Typechecker, typ: *Ast.Type) void {
    if (typ.equal(Ast.void_type)) {
        t.c.report_error(typ.line_info, "unexpected 'void' type", .{});
        Compiler.exit(1);
    }
}

fn reject_symbol(t: *Typechecker, stmt: *Ast.Stmt) void {
    if (stmt.as == .Symbol) {
        t.c.report_error(stmt.line_info, "unexpected symbol definition", .{});
        Compiler.exit(1);
    }
}

fn safe_cast_two_integers(t: *Typechecker, lhs: *Ast.Expr, lhs_type: *Ast.Type, rhs: *Ast.Expr, rhs_type: *Ast.Type) ?*Ast.Type {
    const casted_lhs = safe_cast_to_integer(t, lhs, lhs_type, .Both);
    const casted_rhs = safe_cast_to_integer(t, rhs, rhs_type, .Both);

    if (casted_lhs == null and casted_rhs == null) {
        return null;
    }

    const new_lhs_type = casted_lhs.?;
    const new_rhs_type = casted_rhs.?;

    if (new_lhs_type.equal(new_rhs_type)) {
        return new_lhs_type;
    }

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
        const subexpr = t.ast.create(Ast.Expr);
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
        const subexpr = t.ast.create(Ast.Expr);
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

fn safe_cast_to_integer(t: *Typechecker, expr: *Ast.Expr, expr_type: *Ast.Type, what_sign: Sign) ?*Ast.Type {
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
        .Field, .Identifier, .Type_Of => unreachable,
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
        const subexpr = t.ast.create(Ast.Expr);
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

fn safe_cast(t: *Typechecker, expr: *Ast.Expr, expr_type: *Ast.Type, cast_to: *Ast.Type) bool {
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
                const casted = safe_cast_to_integer(t, expr, expr_type, if (dInteger.is_signed) .Signed else .Unsigned);

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
            .Field, .Identifier, .Type_Of => unreachable,
        }

        break :can_cast false;
    };

    if (can_cast) {
        const subexpr = t.ast.create(Ast.Expr);
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

fn symetric_safe_cast(t: *Typechecker, lhs: *Ast.Expr, lhs_type: *Ast.Type, rhs: *Ast.Expr, rhs_type: *Ast.Type) bool {
    return safe_cast(t, lhs, lhs_type, rhs_type) or safe_cast(t, rhs, rhs_type, lhs_type);
}

fn can_unsafe_cast(t: *Typechecker, expr: *Ast.Expr, expr_type: *Ast.Type, cast_to: *Ast.Type) bool {
    if (safe_cast(t, expr, expr_type, cast_to)) {
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
        .Field, .Identifier, .Type_Of => unreachable,
    }

    return can_cast;
}

fn make_expr_pointer_mul_integer(t: *Typechecker, expr: *Ast.Expr, data: *Ast.Type.SharedData) *Ast.Expr {
    const size = utils.align_u64(data.byte_size, data.alignment);
    const rhs = t.ast.create(Ast.Expr);
    rhs.* = .{
        .line_info = expr.line_info,
        .as = .{ .Integer = size },
        .typ = Ast.integer_type_from_u64(size),
        .flags = .{
            .is_const = true,
        },
        .typechecking = .Done,
    };
    std.debug.assert(symetric_safe_cast(t, expr, expr.typ, rhs, rhs.typ));
    const new_expr = t.ast.create(Ast.Expr);
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

const std = @import("std");
const utils = @import("utils.zig");
const Compiler = @import("compiler.zig");
const Ast = @import("ast.zig");
const IR = @import("ir.zig");
const IRGen = @import("ir-generator.zig");
const Interpreter = @import("interpreter.zig");

const Alignment = utils.Alignment;
const IRD = IR.Decoded;
const IRE = IR.Encoded;

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
