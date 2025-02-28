enum_type: ?*Ast.Type,
return_type: ?*Ast.Type,
scope_bound: *Ast.Scope,
is_in_loop: bool,
interp: Interpreter,
ast: *Ast,
generator: *IRGenerator,
c: *Compiler,

const TypeChecker = @This();

pub fn check(c: *Compiler, ast: *Ast, generator: *IRGenerator) void {
    var typechecker = TypeChecker{
        .enum_type = null,
        .return_type = null,
        .scope_bound = &Ast.global_scope,
        .is_in_loop = false,
        .ast = ast,
        .generator = generator,
        .interp = Interpreter.init(generator.ir),
        .c = c,
    };

    check_top_level(&typechecker);

    typechecker.interp.deinit();
}

fn check_top_level(t: *TypeChecker) void {
    var it = t.ast.globals.first;
    while (it) |node| {
        check_symbol(t, node.data);
        it = node.next;
    }

    const has_main = t.ast.find_symbol_in_scope(.{
        .name = "main",
        .scope = &Ast.global_scope,
    }, false, 0);

    if (has_main) |main| {
        t.ast.main = main;
        switch (main.as) {
            .Procedure => |Procedure| {
                const Proc = &Procedure.typ.data.as.Proc;

                if (Proc.params.len != 0) {
                    t.c.report_error(Procedure.typ.position, "expected 0 arguments, but got {}", .{Proc.params.len});
                    exit(1);
                }

                if (!Proc.return_type.equal(Ast.void_type)) {
                    t.c.report_error(Proc.return_type.position, "expected 'void', but got '{}'", .{Proc.return_type});
                    exit(1);
                }
            },
            else => {
                t.c.report_error(main.position, "'main' isn't a procedure", .{});
                exit(1);
            },
        }
    } else {
        t.c.report_error(.{ .line = 1, .column = 1, .offset = 0 }, "'main' isn't defined", .{});
        exit(1);
    }

    if (t.c.had_error) {
        exit(1);
    }
}

// Expression is not struct/union/array.
fn compute_simple_expression(t: *TypeChecker, expression: *Ast.Expression) u64 {
    const op = compute_expression(t, expression);
    const address = t.interp.grab_value_from_operand(op, false);
    return t.interp.read(address, @intCast(expression.typ.data.byte_size), expression.typ.is_signed());
}

fn compute_expression_to_operand(t: *TypeChecker, dst: IRE.Operand, expression: *Ast.Expression) void {
    std.debug.assert(dst.is_lvalue());
    const op = compute_expression(t, expression);
    const dst_address = t.interp.grab_value_from_operand(dst.addr_of().decode(), false);
    const src_address = t.interp.grab_value_from_operand(op, false);
    t.interp.write_big(dst_address, src_address, expression.typ.data.byte_size);
}

fn compute_expression(t: *TypeChecker, expression: *Ast.Expression) IRD.Operand {
    std.debug.assert(expression.flags.is_const);

    var old_generator = t.generator.*;

    const dst = t.generator.grab_local_from_type(expression.typ.data);
    std.debug.assert(dst.is_lvalue());
    _ = t.generator.generate_expression(dst, expression);
    t.generator.generate_instr0(.exit);
    t.generator.remove_labels();
    t.interp.reset();
    t.interp.interpret();

    t.generator.ir.instrs.clearRetainingCapacity();

    old_generator.labels = t.generator.labels;
    t.generator.* = old_generator;

    return dst.addr_of().decode();
}

fn check_symbol_type(t: *TypeChecker, symbol: *Ast.Symbol) void {
    switch (symbol.as) {
        .Variable => |Variable| {
            if (Variable.typ) |typ| {
                check_type(t, typ);
                reject_void_type(t, typ);
            }
        },
        .Parameter => |Parameter| {
            check_type(t, Parameter.typ);
            reject_void_type(t, Parameter.typ);
            if (Parameter.value) |value| {
                t.c.report_error(value.position, "default values are not supported", .{});
                exit(1);
            }
        },
        .Procedure => |Procedure| {
            check_type(t, Procedure.typ);
        },
        .Struct_Field => |Field| {
            check_type(t, Field.typ);
            reject_void_type(t, Field.typ);
            if (Field.value) |value| {
                t.c.report_error(value.position, "default values are not supported", .{});
                exit(1);
            }
        },
        .Union_Field => |Field| {
            check_type(t, Field.typ);
            reject_void_type(t, Field.typ);
            if (Field.value) |value| {
                t.c.report_error(value.position, "default values are not supported", .{});
                exit(1);
            }
        },
        .Enum_Field => {},
        .Type => |typ| {
            typ.symbol = symbol;
        },
    }
}

fn check_symbol(t: *TypeChecker, symbol: *Ast.Symbol) void {
    switch (symbol.typechecking) {
        .None => symbol.typechecking = .Going,
        .Going => {
            t.c.report_error(symbol.position, "found cyclic reference", .{});
            exit(1);
        },
        .Done => return,
    }
    defer symbol.typechecking = .Done;

    check_symbol_type(t, symbol);

    const fns = struct {
        pub fn compute_variable_initializer(_t: *TypeChecker, _symbol: *Ast.Symbol) void {
            const Variable = &_symbol.as.Variable;

            if (Variable.attributes.is_static or Variable.attributes.is_const) {
                const value = Variable.value.?;

                // TODO: compute initializer always when it's constant.
                if (value.flags.is_const) {
                    const storage = _t.generator.grab_static_variable_storage(Variable);
                    compute_expression_to_operand(_t, storage, value);
                } else {
                    _t.c.report_error(value.position, "expression is not a constant", .{});
                    exit(1);
                }
            }
        }
    };

    switch (symbol.as) {
        .Variable => |*Variable| {
            if (Variable.typ) |typ| {
                if (Variable.value) |value| {
                    const value_type = check_expression_only(t, value);

                    if (!safe_cast(t, value, value_type, typ)) {
                        t.c.report_error(value.position, "expected '{}', but got '{}'", .{ typ, value_type });
                        t.c.report_note(typ.position, "expected type is here", .{});
                        t.c.report_note(value.position, "expression is here", .{});
                        exit(1);
                    }

                    fns.compute_variable_initializer(t, symbol);
                } else if (Variable.attributes.is_const) {
                    t.c.report_error(symbol.position, "constant expression needs initializer", .{});
                    exit(1);
                }
            } else if (Variable.value) |value| {
                const value_type = check_expression_only(t, value);
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
                check_statement(t, node.data);
                it = node.next;
            }

            t.return_type = old_return_type;
        },
        .Type => |typ| {
            check_type(t, typ);
        },
        .Parameter,
        .Struct_Field,
        .Union_Field,
        .Enum_Field,
        => unreachable,
    }
}

fn unpack(t: *TypeChecker, typ: *Ast.Type) void {
    const old_data = typ.data;
    switch (old_data.stages.unpacking) {
        .None => old_data.stages.unpacking = .Going,
        .Going => {
            t.c.report_error(typ.position, "found cyclic reference", .{});
            exit(1);
        },
        .Done => return,
    }
    defer old_data.stages.unpacking = .Done;

    const fns = struct {
        pub fn unpack_symbol(_t: *TypeChecker, _typ: *Ast.Type, symbol: *Ast.Symbol) void {
            if (symbol.as != .Type) {
                _t.c.report_error(_typ.position, "symbol '{s}' is not a type", .{symbol.key.name});
                exit(1);
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
                    t.c.report_error(Field.subtype.position, "expected struct, union, or enum type, but got '{}'", .{Field.subtype});
                    exit(1);
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
            const has_symbol = t.ast.find_symbol_with_scope_bound(.{
                .name = Identifier.name,
                .scope = Identifier.scope,
            }, t.scope_bound, typ.position.offset);

            if (has_symbol) |symbol| {
                fns.unpack_symbol(t, typ, symbol);
            } else {
                t.c.report_error(typ.position, "symbol '{s}' is not defined", .{Identifier.name});
                exit(1);
            }
        },
        .Type_Of => |expression| {
            const expression_result = check_expression(t, expression);
            if (expression_result.tag == .Type) {
                unpack(t, expression_result.typ);
            }
            typ.data = expression_result.typ.data;
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

fn shallow_check(t: *TypeChecker, typ: *Ast.Type) void {
    unpack(t, typ);

    const data = typ.data;
    switch (data.stages.shallow_check) {
        .None => data.stages.shallow_check = .Going,
        .Going => {
            t.c.report_error(typ.position, "found cyclic reference", .{});
            exit(1);
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

fn void_array_check(t: *TypeChecker, typ: *Ast.Type) void {
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
fn check_type(t: *TypeChecker, typ: *Ast.Type) void {
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
            var alignment: Alignment = .Byte;

            var it = Struct.fields.first;
            while (it) |node| {
                const symbol = node.data;
                check_symbol_type(t, symbol);

                const Field = &symbol.as.Struct_Field;
                const field_alignment = Field.typ.data.alignment;
                size = nostd.align_up(size, field_alignment);
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
                check_symbol(t, node.data);
                it = node.next;
            }
        },
        .Union => |Union| {
            const old_scope_bound = t.scope_bound;
            defer t.scope_bound = old_scope_bound;

            t.scope_bound = Union.scope;

            var size: u64 = 0;
            var alignment: Alignment = .Byte;

            var it = Union.fields.first;
            while (it) |node| {
                const symbol = node.data;
                check_symbol_type(t, symbol);

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
                check_symbol(t, node.data);
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
                        const value_type = check_expression_only(t, value);
                        const new_value_type = safe_cast_to_integer(t, value, value_type, .Both);
                        if (new_value_type == null) {
                            t.c.report_error(value.position, "expected integer, but got '{}'\n", .{value_type});
                            exit(1);
                        }
                        is_signed = is_signed or new_value_type.?.is_signed();
                        enum_field.computed_value = compute_simple_expression(t, value);
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

            const bits = nostd.highest_bit_count(max_value) + @intFromBool(is_signed);
            if (bits > 64) {
                t.c.report_error(typ.position, "enum can't be represented in <= 64 bits", .{});
                exit(1);
            }
            const integer_type = Ast.lookup_integer_type(bits, is_signed);

            Enum.integer_type = integer_type;
            data.byte_size = integer_type.data.byte_size;
            data.alignment = integer_type.data.alignment;

            t.enum_type = old_enum_type;

            // NOTE[check-fields-in-namespace].
            var it = Enum.rest.first;
            while (it) |node| {
                check_symbol(t, node.data);
                it = node.next;
            }
        },
        .Proc => |Proc| {
            std.debug.assert(data.byte_size == Ast.pointer_byte_size and
                data.alignment == Ast.pointer_alignment);

            var it = Proc.params.first;
            while (it) |node| {
                const symbol = node.data;
                check_symbol_type(t, symbol);

                it = node.next;
            }
            check_type(t, Proc.return_type);
        },
        .Array => |*Array| {
            check_type(t, Array.subtype);

            const size_type = check_expression_only(t, Array.size);
            if (safe_cast_to_integer(t, Array.size, size_type, .Unsigned) == null) {
                t.c.report_error(typ.position, "expected unsigned integer, but got '{}'", .{size_type});
                exit(1);
            }

            const count = compute_simple_expression(t, Array.size);
            Array.computed_size = count;
            data.byte_size = count * Array.subtype.data.byte_size;
            data.alignment = Array.subtype.data.alignment;
        },
        .Pointer => |subtype| {
            std.debug.assert(data.byte_size == Ast.pointer_byte_size and
                data.alignment == Ast.pointer_alignment);
            check_type(t, subtype);
        },
        .Integer => {
            std.debug.assert(data.byte_size != 0);
        },
        .Bool => {
            std.debug.assert(data.byte_size == 1 and
                data.alignment == .Byte);
        },
        .Void => {
            std.debug.assert(data.byte_size == 0 and
                data.alignment == .Byte);
        },
        .Field,
        .Identifier,
        .Type_Of,
        => unreachable,
    }
}

fn check_statement_list(t: *TypeChecker, list: Ast.StatementList) void {
    var it = list.first;
    while (it) |node| {
        check_statement(t, node.data);
        it = node.next;
    }
}

fn check_statement(t: *TypeChecker, statement: *Ast.Statement) void {
    switch (statement.as) {
        .Print => |expression| {
            const expression_type = check_expression_only(t, expression);
            switch (expression_type.data.as) {
                .Struct, .Union, .Proc, .Array, .Void => {
                    t.c.report_error(expression.position, "can't print value of type '{}'", .{expression.typ});
                    exit(1);
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
            check_statement_list(t, block);
        },
        .If => |If| {
            const condition_type = check_expression_only(t, If.condition);
            if (!safe_cast(t, If.condition, condition_type, Ast.bool_type)) {
                t.c.report_error(If.condition.position, "expected 'bool', but got '{}'", .{condition_type});
                exit(1);
            }

            reject_symbol(t, If.true_branch);
            check_statement(t, If.true_branch);

            if (If.false_branch) |false_branch| {
                reject_symbol(t, false_branch);
                check_statement(t, false_branch);
            }
        },
        .While, .Do_While => |While| {
            const condition_type = check_expression_only(t, While.condition);
            if (!safe_cast(t, While.condition, condition_type, Ast.bool_type)) {
                t.c.report_error(While.condition.position, "expected 'bool', but got '{}'", .{condition_type});
                exit(1);
            }

            const old_is_in_loop = t.is_in_loop;

            reject_symbol(t, While.body);
            t.is_in_loop = true;
            check_statement(t, While.body);
            t.is_in_loop = old_is_in_loop;
        },
        .Break => {
            if (!t.is_in_loop) {
                t.c.report_error(statement.position, "'break' outside of loop", .{});
                exit(1);
            }
        },
        .Continue => {
            if (!t.is_in_loop) {
                t.c.report_error(statement.position, "'continue' outside of loop", .{});
                exit(1);
            }
        },
        .Switch => |Switch| {
            const condition_type = check_expression_only(t, Switch.condition);
            const is_comparable = condition_type.compare().is_comparable;

            if (!is_comparable) {
                t.c.report_error(Switch.condition.position, "value of type '{}' isn't comparable", .{condition_type});
                exit(1);
            }

            var it = Switch.cases.first;
            while (it) |node| {
                var Case = node.data;
                while (true) {
                    switch (Case.*) {
                        .Case => |case| {
                            const value_type = check_expression_only(t, case.value);

                            if (!safe_cast(t, case.value, value_type, condition_type)) {
                                t.c.report_error(case.value.position, "mismatched types: '{}' and '{}'", .{ condition_type, value_type });
                                t.c.report_note(case.value.position, "switch case is here", .{});
                                t.c.report_note(Switch.condition.position, "switch condition is here", .{});
                                exit(1);
                            }

                            const value = compute_simple_expression(t, case.value);
                            case.value.as = .{
                                .Integer = value,
                            };

                            Case = case.subcase;
                        },
                        .Statement => |substatement| {
                            reject_symbol(t, substatement);
                            check_statement(t, substatement);
                            break;
                        },
                    }
                }

                it = node.next;
            }
        },
        .Return => |has_expression| {
            const return_type = t.return_type.?;

            if (has_expression) |expression| {
                if (return_type.equal(Ast.void_type)) {
                    t.c.report_error(expression.position, "unexpected expression here", .{});
                    exit(1);
                } else {
                    const expression_type = check_expression_only(t, expression);

                    if (!safe_cast(t, expression, expression_type, return_type)) {
                        t.c.report_error(expression.position, "mismatched types: '{}' and '{}'", .{ return_type, expression_type });
                        t.c.report_note(return_type.position, "return type is here", .{});
                        t.c.report_note(expression.position, "expression is here", .{});
                        exit(1);
                    }
                }
            } else if (!return_type.equal(Ast.void_type)) {
                t.c.report_error(statement.position, "expected expression of type '{}'", .{return_type});
                exit(1);
            }
        },
        .Symbol => |symbol| {
            check_symbol(t, symbol);
        },
        .Assign => |Assign| {
            const lhs_type = check_expression_only(t, Assign.lhs);

            if (!Assign.lhs.flags.is_lvalue) {
                t.c.report_error(Assign.lhs.position, "expression is not an lvalue", .{});
                exit(1);
            } else if (Assign.lhs.flags.is_const) {
                t.c.report_error(Assign.lhs.position, "can't assign to constant expression", .{});
                exit(1);
            }

            const rhs_type = check_expression_only(t, Assign.rhs);

            if (!safe_cast(t, Assign.rhs, rhs_type, lhs_type)) {
                t.c.report_error(statement.position, "expected '{}', but got '{}'", .{ lhs_type, rhs_type });
                exit(1);
            }
        },
        .Expression => |expression| {
            _ = check_expression_only(t, expression);
        },
    }
}

fn check_expression_only(t: *TypeChecker, expression: *Ast.Expression) *Ast.Type {
    const result = check_expression(t, expression);
    if (result.tag != .Value) {
        t.c.report_error(expression.position, "expected expression", .{});
        exit(1);
    }
    return result.typ;
}

// Types are typechecked lazily (only when used). 'cause if a type is not fully formed (like ambiguous pointer/array), we need to typecheck it from the top level.
fn check_expression(t: *TypeChecker, expression: *Ast.Expression) TypecheckExpressionResult {
    switch (expression.typechecking) {
        .None => expression.typechecking = .Going,
        .Going => {
            t.c.report_error(expression.position, "found cyclic reference", .{});
            exit(1);
        },
        .Done => return .{ .typ = expression.typ, .tag = if (expression.as == .Type) .Type else .Value }, // Non values should be filtered at this point?
    }
    defer expression.typechecking = .Done;

    const fns = struct {
        pub fn check_expression_symbol(_t: *TypeChecker, _expression: *Ast.Expression, symbol: *Ast.Symbol) TypecheckExpressionResult {
            _expression.as = .{ .Symbol = symbol };

            switch (symbol.as) {
                .Variable => |*Variable| {
                    _expression.flags.is_const = Variable.attributes.is_const;
                    _expression.flags.is_static = Variable.attributes.is_static;

                    check_symbol(_t, symbol); // TODO: allow cyclic references when using #type_of/#alignment_of, etc.

                    _expression.flags.is_lvalue = true;

                    return .{ .typ = Variable.typ.?, .tag = .Value };
                },
                .Parameter => |Parameter| {
                    _expression.flags.is_lvalue = true;
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

    const result: TypecheckExpressionResult = result: {
        switch (expression.as) {
            .Binary_Op => |*Binary_Op| {
                const lhs_type = check_expression_only(t, Binary_Op.lhs);
                const rhs_type = check_expression_only(t, Binary_Op.rhs);
                const lhs_flags = lhs_type.compare();
                const rhs_flags = rhs_type.compare();

                expression.flags.is_const = Binary_Op.lhs.flags.is_const and
                    Binary_Op.rhs.flags.is_const;

                switch (Binary_Op.tag) {
                    .Or, .And => {
                        if (!safe_cast(t, Binary_Op.lhs, lhs_type, Ast.bool_type) or
                            !safe_cast(t, Binary_Op.rhs, rhs_type, Ast.bool_type))
                        {
                            t.c.report_error(Binary_Op.position, "mismatched types: '{}' and '{}'", .{ lhs_type, rhs_type });
                            break :result .{ .typ = Ast.bool_type, .tag = .Value };
                        }

                        switch (Binary_Op.tag) {
                            .Or => {
                                const true_branch = t.ast.create(Ast.Expression);
                                true_branch.* = .{
                                    .position = expression.position,
                                    .as = .{ .Boolean = true },
                                    .typ = Ast.bool_type,
                                    .flags = .{
                                        .is_const = true,
                                    },
                                    .typechecking = .Done,
                                };
                                expression.as = .{ .If = .{
                                    .condition = Binary_Op.lhs,
                                    .true_branch = true_branch,
                                    .false_branch = Binary_Op.rhs,
                                } };
                            },
                            .And => {
                                const false_branch = t.ast.create(Ast.Expression);
                                false_branch.* = .{
                                    .position = expression.position,
                                    .as = .{ .Boolean = false },
                                    .typ = Ast.bool_type,
                                    .flags = .{
                                        .is_const = true,
                                    },
                                    .typechecking = .Done,
                                };
                                expression.as = .{ .If = .{
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
                            t.c.report_error(Binary_Op.position, "can't compare '{}' and '{}'", .{ lhs_type, rhs_type });
                        }

                        break :result .{ .typ = Ast.bool_type, .tag = .Value };
                    },
                    .Lt, .Leq, .Gt, .Geq => {
                        if (!lhs_flags.is_ordered or !rhs_flags.is_ordered) {
                            t.c.report_error(Binary_Op.position, "expression of type '{}' is not comparable", .{lhs_type});
                        } else if (!symetric_safe_cast(t, Binary_Op.lhs, lhs_type, Binary_Op.rhs, rhs_type)) {
                            t.c.report_error(Binary_Op.position, "mismatched types: '{}' and '{}'", .{ lhs_type, rhs_type });
                        }

                        break :result .{ .typ = Ast.bool_type, .tag = .Value };
                    },
                    .Add => {
                        if (lhs_flags.is_void_pointer or rhs_flags.is_void_pointer) {
                            t.c.report_error(Binary_Op.position, "can't add 'void' pointers", .{});
                            exit(1);
                        } else if (lhs_flags.is_pointer and safe_cast_to_integer(t, Binary_Op.rhs, rhs_type, .Unsigned) != null) {
                            Binary_Op.rhs = make_expression_pointer_mul_integer(t, Binary_Op.rhs, lhs_type.data.as.Pointer.data);
                            break :result .{ .typ = lhs_type, .tag = .Value };
                        } else if (rhs_flags.is_pointer and safe_cast_to_integer(t, Binary_Op.lhs, lhs_type, .Unsigned) != null) {
                            Binary_Op.lhs = make_expression_pointer_mul_integer(t, Binary_Op.lhs, rhs_type.data.as.Pointer.data);
                            break :result .{ .typ = rhs_type, .tag = .Value };
                        } else {
                            const casted = safe_cast_two_integers(t, Binary_Op.lhs, lhs_type, Binary_Op.rhs, rhs_type);
                            if (casted == null) {
                                t.c.report_error(Binary_Op.position, "expected integer/pointer: '{}' and '{}'", .{ lhs_type, rhs_type });
                                exit(1);
                            }
                            break :result .{ .typ = casted.?, .tag = .Value };
                        }
                    },
                    .Sub => {
                        if (lhs_flags.is_void_pointer) {
                            t.c.report_error(Binary_Op.position, "can't subtract 'void' pointer", .{});
                            exit(1);
                        } else if (lhs_flags.is_pointer and safe_cast_to_integer(t, Binary_Op.rhs, rhs_type, .Unsigned) != null) {
                            Binary_Op.rhs = make_expression_pointer_mul_integer(t, Binary_Op.rhs, lhs_type.data.as.Pointer.data);
                            break :result .{ .typ = lhs_type, .tag = .Value };
                        } else {
                            const casted = safe_cast_two_integers(t, Binary_Op.lhs, lhs_type, Binary_Op.rhs, rhs_type);
                            if (casted == null) {
                                t.c.report_error(Binary_Op.position, "expected integer/pointer: '{}' and '{}'", .{ lhs_type, rhs_type });
                                exit(1);
                            }
                            break :result .{ .typ = casted.?, .tag = .Value };
                        }
                    },
                    .Mul, .Div, .Mod => {
                        const casted = safe_cast_two_integers(t, Binary_Op.lhs, lhs_type, Binary_Op.rhs, rhs_type);
                        if (casted == null) {
                            t.c.report_error(Binary_Op.position, "expected integer/pointer: '{}' and '{}'", .{ lhs_type, rhs_type });
                            exit(1);
                        }
                        break :result .{ .typ = casted.?, .tag = .Value };
                    },
                }
            },
            .Unary_Op => |Unary_Op| {
                const subexpression_type = check_expression_only(t, Unary_Op.subexpression);

                expression.flags.is_const = Unary_Op.subexpression.flags.is_const;

                switch (Unary_Op.tag) {
                    .Pos => {
                        const casted = safe_cast_to_integer(t, Unary_Op.subexpression, subexpression_type, .Both);
                        if (casted == null) {
                            t.c.report_error(Unary_Op.subexpression.position, "expected integer, but got '{}'", .{subexpression_type});
                            exit(1);
                        }
                        break :result .{ .typ = casted.?, .tag = .Value };
                    },
                    .Neg => {
                        const casted = safe_cast_to_integer(t, Unary_Op.subexpression, subexpression_type, .Signed);
                        if (casted == null) {
                            t.c.report_error(Unary_Op.subexpression.position, "expected integer, but got '{}'", .{subexpression_type});
                            exit(1);
                        }
                        break :result .{ .typ = casted.?, .tag = .Value };
                    },
                    .Not => {
                        if (!safe_cast(t, Unary_Op.subexpression, subexpression_type, Ast.bool_type)) {
                            t.c.report_error(Unary_Op.subexpression.position, "expected 'bool', but got '{}'", .{subexpression_type});
                        }

                        break :result .{ .typ = Ast.bool_type, .tag = .Value };
                    },
                }
            },
            .Ref => |subexpression| {
                const subexpression_type = check_expression_only(t, subexpression);

                if (!subexpression.flags.is_lvalue) {
                    t.c.report_error(subexpression.position, "expression is not an lvalue", .{});
                    exit(1);
                } else if (subexpression.flags.is_const) {
                    t.c.report_error(subexpression.position, "can't take raferences to constant expression", .{});
                    exit(1);
                }

                const data = t.ast.create(Ast.Type.SharedData);
                data.* = .{
                    .as = .{ .Pointer = subexpression_type },
                    .byte_size = Ast.pointer_byte_size,
                    .alignment = Ast.pointer_alignment,
                    .stages = Ast.default_stages_done,
                };
                const typ = t.ast.create(Ast.Type);
                typ.* = .{
                    .position = expression.position,
                    .data = data,
                    .symbol = null,
                };

                expression.flags.is_const = subexpression.flags.is_const or subexpression.flags.is_static;

                break :result .{ .typ = typ, .tag = .Value };
            },
            .Deref => |subexpression| {
                const subexpression_result = check_expression(t, subexpression);

                if (subexpression_result.tag == .Type) {
                    const data = t.ast.create(Ast.Type.SharedData);
                    data.* = .{
                        .as = .{ .Pointer = subexpression_result.typ },
                        .byte_size = Ast.pointer_byte_size,
                        .alignment = Ast.pointer_alignment,
                        .stages = Ast.default_stages_none,
                    };

                    expression.as = .{ .Type = .{
                        .position = expression.position,
                        .data = data,
                        .symbol = null,
                    } };

                    expression.flags.is_const = true;

                    break :result .{ .typ = &expression.as.Type, .tag = .Type };
                } else {
                    const subexpression_type = subexpression_result.typ;
                    const can_be_dereferenced = subexpression_type.compare().can_be_dereferenced;

                    if (!can_be_dereferenced) {
                        t.c.report_error(subexpression.position, "can't dereference expression of type '{}'", .{subexpression_type});
                        exit(1);
                    }

                    expression.flags.is_lvalue = true;
                    expression.flags.is_const = subexpression.flags.is_static;

                    break :result .{ .typ = subexpression_type.data.as.Pointer, .tag = .Value };
                }
            },
            .If => |If| {
                const condition_type = check_expression_only(t, If.condition);

                if (!safe_cast(t, If.condition, condition_type, Ast.bool_type)) {
                    t.c.report_error(If.condition.position, "expected 'bool', but got '{}'", .{condition_type});
                    exit(1);
                }

                const true_branch_type = check_expression_only(t, If.true_branch);
                const false_branch_type = check_expression_only(t, If.false_branch);

                if (!symetric_safe_cast(t, If.true_branch, true_branch_type, If.false_branch, false_branch_type)) {
                    t.c.report_error(If.condition.position, "mismatched types: '{}' and '{}'", .{ true_branch_type, false_branch_type });
                    exit(1);
                }

                expression.flags.is_const = If.condition.flags.is_const and
                    If.true_branch.flags.is_const and
                    If.false_branch.flags.is_const;

                break :result .{ .typ = true_branch_type, .tag = .Value };
            },
            .Call => |Call| {
                const subexpression_result = check_expression(t, Call.subexpression);

                if (subexpression_result.tag == .Type) {
                    const typ = subexpression_result.typ;

                    var is_const = false;

                    check_type(t, typ);
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
                                                const rhs_type = check_expression_only(t, Designator.rhs);
                                                if (!safe_cast(t, Designator.rhs, rhs_type, Field.typ)) {
                                                    t.c.report_error(Designator.rhs.position, "expected '{}', but got '{}'", .{ Field.typ, rhs_type });
                                                    exit(1);
                                                }
                                                is_const = is_const and Designator.rhs.flags.is_const;
                                            },
                                            else => {
                                                t.c.report_error(typ.position, "expected a structure or union field", .{});
                                                exit(1);
                                            },
                                        }
                                    },
                                    .Expression => |subexpression| {
                                        t.c.report_error(subexpression.position, "unexpected expression", .{});
                                        exit(1);
                                    },
                                }

                                it = node.next;
                            }
                        },
                        .Array => |Array| {
                            if (Array.computed_size != Call.args.len) {
                                t.c.report_error(expression.position, "expected {} values, but got {}", .{ Array.computed_size, Call.args.len });
                                exit(1);
                            }

                            is_const = true;

                            var it = Call.args.first;
                            while (it) |node| {
                                switch (node.data.*) {
                                    .Designator => |Designator| {
                                        t.c.report_error(Designator.lhs.position, "unexpected designator", .{});
                                        exit(1);
                                    },
                                    .Expression => |arg| {
                                        const arg_type = check_expression_only(t, arg);
                                        if (!safe_cast(t, arg, arg_type, Array.subtype)) {
                                            t.c.report_error(arg.position, "expected '{}', but got '{}'", .{ Array.subtype, arg_type });
                                            exit(1);
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
                                t.c.report_error(expression.position, "expected 1 argument for scalar type, but got {}", .{Call.args.len});
                                exit(1);
                            }

                            switch (Call.args.first.?.data.*) {
                                .Designator => |Designator| {
                                    t.c.report_error(Designator.lhs.position, "unexpected designator", .{});
                                    exit(1);
                                },
                                .Expression => |arg| {
                                    const arg_type = check_expression_only(t, arg);

                                    if (!safe_cast(t, arg, arg_type, typ)) {
                                        t.c.report_error(expression.position, "expected '{}', but got '{}'", .{ typ, arg_type });
                                        exit(1);
                                    }

                                    is_const = arg.flags.is_const;
                                },
                            }
                        },
                        .Void => {
                            t.c.report_error(expression.position, "can't construct expression of type 'void'", .{});
                            exit(1);
                        },
                        .Field,
                        .Identifier,
                        .Type_Of,
                        => unreachable,
                    }

                    const args = Call.args;
                    expression.as = .{ .Constructor = .{
                        .typ = typ,
                        .args = args,
                    } };
                    expression.flags.is_const = is_const;

                    break :result .{ .typ = typ, .tag = .Value };
                } else {
                    const subexpression_type = subexpression_result.typ;

                    if (subexpression_type.data.as != .Proc) {
                        t.c.report_error(Call.subexpression.position, "expected a procedure, but got '{}'", .{subexpression_type});
                        exit(1);
                    }

                    const Proc = &subexpression_type.data.as.Proc;

                    if (Proc.params.len != Call.args.len) {
                        t.c.report_error(Call.subexpression.position, "expected {} arguments, but got {}", .{ Proc.params.len, Call.args.len });
                        exit(1);
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
                                t.c.report_error(Designator.lhs.position, "unexpected designator", .{});
                                exit(1);
                            },
                            .Expression => |arg| {
                                const arg_type = check_expression_only(t, arg);

                                if (!safe_cast(t, arg, arg_type, param_type)) {
                                    t.c.report_error(arg.position, "expected '{}', but got '{}'", .{ param_type, arg_type });
                                    exit(1);
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
                const subexpression_result = check_expression(t, Subscript.subexpression);

                if (subexpression_result.tag == .Type) {
                    const data = t.ast.create(Ast.Type.SharedData);
                    data.* = .{
                        .as = .{ .Array = .{
                            .subtype = subexpression_result.typ,
                            .size = Subscript.index,
                            .computed_size = 0,
                        } },
                        .byte_size = 0,
                        .alignment = .Byte,
                        .stages = Ast.default_stages_none,
                    };

                    expression.as = .{ .Type = .{
                        .position = expression.position,
                        .data = data,
                        .symbol = null,
                    } };

                    expression.flags.is_const = true;

                    break :result .{ .typ = &expression.as.Type, .tag = .Type };
                } else {
                    const subexpression_type = subexpression_result.typ;
                    const index_type = check_expression_only(t, Subscript.index);

                    if (safe_cast_to_integer(t, Subscript.index, index_type, .Both) == null) {
                        t.c.report_error(Subscript.subexpression.position, "expected integer, but got '{}'", .{index_type});
                        exit(1);
                    }

                    expression.flags.is_lvalue = Subscript.subexpression.flags.is_lvalue;
                    expression.flags.is_const = Subscript.subexpression.flags.is_const and Subscript.index.flags.is_const;
                    expression.flags.is_static = Subscript.subexpression.flags.is_static;

                    switch (subexpression_type.data.as) {
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
                            t.c.report_error(Subscript.subexpression.position, "expected array or pointer to array, but got '{}'", .{subexpression_type});
                            exit(1);
                        },
                    }
                }
            },
            .Field => |Field| {
                const subexpression_result = check_expression(t, Field.subexpression);

                if (subexpression_result.tag == .Type) {
                    check_type(t, subexpression_result.typ);

                    const scope: *Ast.Scope = switch (subexpression_result.typ.data.as) {
                        .Struct, .Union => |Struct| Struct.scope,
                        .Enum => |Enum| Enum.scope,
                        .Proc,
                        .Array,
                        .Pointer,
                        .Integer,
                        .Bool,
                        .Void,
                        => {
                            t.c.report_error(subexpression_result.typ.position, "expected struct, union, or enum type, but got '{}'", .{subexpression_result.typ});
                            exit(1);
                        },
                        .Field,
                        .Identifier,
                        .Type_Of,
                        => unreachable,
                    };

                    const symbol = resolve_identifier(t, Field.field, scope);

                    switch (symbol.as) {
                        .Type => |Type| {
                            expression.as = .{ .Type = .{
                                .position = expression.position,
                                .data = Type.data,
                                .symbol = symbol,
                            } };

                            expression.flags.is_const = true;

                            break :result .{ .typ = &expression.as.Type, .tag = .Type };
                        },
                        else => {
                            break :result fns.check_expression_symbol(t, expression, symbol);
                        },
                    }
                } else {
                    expression.flags.is_lvalue = Field.subexpression.flags.is_lvalue;
                    expression.flags.is_const = Field.subexpression.flags.is_const;
                    expression.flags.is_static = Field.subexpression.flags.is_static;

                    const scope: *Ast.Scope = switch (subexpression_result.typ.data.as) {
                        .Struct, .Union => |Struct| Struct.scope,
                        .Pointer => |subtype| switch (subtype.data.as) {
                            .Struct, .Union => |Struct| Struct.scope,
                            else => {
                                t.c.report_error(Field.subexpression.position, "expected pointer to struct/union, but got '{}'", .{subexpression_result.typ});
                                exit(1);
                            },
                        },
                        else => {
                            t.c.report_error(Field.subexpression.position, "expected struct, union, or enum, but got '{}'", .{subexpression_result.typ});
                            exit(1);
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
            .Byte_Size_Of => |subexpression| {
                const subexpression_result = check_expression(t, subexpression);
                if (subexpression_result.tag == .Type) {
                    check_type(t, subexpression_result.typ);
                }
                const byte_size = subexpression_result.typ.data.byte_size;

                expression.as = .{ .Integer = byte_size };
                expression.flags.is_const = true;

                break :result .{ .typ = Ast.integer_type_from_u64(byte_size), .tag = .Value };
            },
            .Alignment_Of => |subexpression| {
                const subexpression_result = check_expression(t, subexpression);
                if (subexpression_result.tag == .Type) {
                    check_type(t, subexpression_result.typ);
                }
                const alignment = subexpression_result.typ.data.alignment.to_byte_size();

                expression.as = .{ .Integer = alignment };
                expression.flags.is_const = true;

                break :result .{ .typ = Ast.integer_type_from_u64(alignment), .tag = .Value };
            },
            .As => |As| {
                check_type(t, As.typ);
                const expression_type = check_expression_only(t, As.expression);
                if (!safe_cast(t, As.expression, expression_type, As.typ)) {
                    t.c.report_error(As.expression.position, "can't reinterpret '{}' as '{}'", .{ As.typ, expression_type });
                    exit(1);
                }
                expression.flags.is_const = As.expression.flags.is_const;
                break :result .{ .typ = As.typ, .tag = .Value };
            },
            .Cast => |Cast| {
                check_type(t, Cast.typ);
                const expression_type = check_expression_only(t, Cast.expression);
                if (!can_unsafe_cast(t, Cast.expression, expression_type, Cast.typ)) {
                    t.c.report_error(Cast.expression.position, "can't cast '{}' to '{}'", .{ expression_type, Cast.typ });
                    exit(1);
                }
                expression.flags.is_const = Cast.expression.flags.is_const;
                break :result .{ .typ = Cast.typ, .tag = .Value };
            },
            .Boolean => {
                expression.flags.is_const = true;
                break :result .{ .typ = expression.typ, .tag = .Value };
            },
            .Integer => {
                expression.flags.is_const = true;
                break :result .{ .typ = expression.typ, .tag = .Value };
            },
            .Null => {
                expression.flags.is_const = true;
                break :result .{ .typ = expression.typ, .tag = .Value };
            },
            .Type => |*Type| {
                expression.flags.is_const = true;
                break :result .{ .typ = Type, .tag = .Type };
            },
            .Symbol => unreachable,
            .Identifier => |Identifier| {
                const has_symbol = t.ast.find_symbol_with_scope_bound(.{
                    .name = Identifier.name,
                    .scope = Identifier.scope,
                }, t.scope_bound, expression.position.offset);

                if (has_symbol) |symbol| {
                    check_symbol_type(t, symbol);

                    break :result fns.check_expression_symbol(t, expression, symbol);
                } else {
                    t.c.report_error(expression.position, "symbol '{s}' is not defined", .{Identifier.name});
                    exit(1);
                }
            },
        }
    };

    expression.typ = result.typ;

    return result;
}

fn resolve_identifier(t: *TypeChecker, expression: *Ast.Expression, scope: *Ast.Scope) *Ast.Symbol {
    switch (expression.as) {
        .Identifier => |*Identifier| {
            Identifier.scope = scope;

            const has_symbol = t.ast.find_symbol_in_scope(.{
                .name = Identifier.name,
                .scope = Identifier.scope,
            }, false, expression.position.offset);

            if (has_symbol) |symbol| {
                expression.as = .{ .Symbol = symbol };
                return symbol;
            } else {
                t.c.report_error(expression.position, "symbol '{s}' is not defined", .{Identifier.name});
                exit(1);
            }
        },
        else => {
            t.c.report_error(expression.position, "expected identifier", .{});
            exit(1);
        },
    }
}

// TODO: line info is not properly reported.
fn reject_void_type(t: *TypeChecker, typ: *Ast.Type) void {
    if (typ.equal(Ast.void_type)) {
        t.c.report_error(typ.position, "unexpected 'void' type", .{});
        exit(1);
    }
}

fn reject_symbol(t: *TypeChecker, statement: *Ast.Statement) void {
    if (statement.as == .Symbol) {
        t.c.report_error(statement.position, "unexpected symbol definition", .{});
        exit(1);
    }
}

fn safe_cast_two_integers(t: *TypeChecker, lhs: *Ast.Expression, lhs_type: *Ast.Type, rhs: *Ast.Expression, rhs_type: *Ast.Type) ?*Ast.Type {
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

    const Case = enum(u8) {
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
        const left_sign: u8 = @intFromBool(lInteger.is_signed);
        const right_sign: u8 = @intFromBool(rInteger.is_signed);
        const order: u8 = if (lInteger.bits < rInteger.bits)
            0
        else if (lInteger.bits > rInteger.bits)
            1
        else
            2;
        break :case @enumFromInt(3 * (2 * left_sign + right_sign) + order);
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
        const subexpression = t.ast.create(Ast.Expression);
        subexpression.* = lhs.*;
        lhs.* = .{
            .position = subexpression.position,
            .as = .{ .Cast = .{
                .typ = typ,
                .expression = subexpression,
            } },
            .typ = typ,
            .flags = .{
                .is_const = subexpression.flags.is_const,
            },
            .typechecking = .Done,
        };
    }

    if (which_side_to_cast == .Right or which_side_to_cast == .Both) {
        const typ = casted.?;
        const subexpression = t.ast.create(Ast.Expression);
        subexpression.* = rhs.*;
        rhs.* = .{
            .position = subexpression.position,
            .as = .{ .Cast = .{
                .typ = typ,
                .expression = subexpression,
            } },
            .typ = typ,
            .flags = .{
                .is_const = subexpression.flags.is_const,
            },
            .typechecking = .Done,
        };
    }

    return casted;
}

fn safe_cast_to_integer(t: *TypeChecker, expression: *Ast.Expression, expression_type: *Ast.Type, what_sign: Sign) ?*Ast.Type {
    var should_cast = false;
    var typ: *Ast.Type = switch (expression_type.data.as) {
        .Struct,
        .Union,
        .Array,
        .Void,
        => return null,
        .Enum => |Enum| Enum.integer_type,
        .Proc, .Pointer => Ast.lookup_integer_type(64, false),
        .Integer => expression_type,
        .Bool => Ast.lookup_integer_type(1, false),
        .Field, .Identifier, .Type_Of => unreachable,
    };

    {
        const Integer = &typ.data.as.Integer;
        switch (what_sign) {
            .Signed => {
                if (Integer.is_signed) {
                    // NOTE[reinterp-expression]: Reinterpret the value, no need to cast it.
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
                // NOTE[reinterp-expression].
            },
            .Both => {
                // NOTE[reinterp-expression].
            },
        }
    }

    if (should_cast) {
        const subexpression = t.ast.create(Ast.Expression);
        subexpression.* = expression.*;
        expression.* = .{
            .position = subexpression.position,
            .as = .{ .Cast = .{
                .typ = typ,
                .expression = subexpression,
            } },
            .typ = typ,
            .flags = .{
                .is_const = subexpression.flags.is_const,
            },
            .typechecking = .Done,
        };
    }

    expression.typ = typ;

    return typ;
}

fn safe_cast(t: *TypeChecker, expression: *Ast.Expression, expression_type: *Ast.Type, cast_to: *Ast.Type) bool {
    if (expression_type.equal(cast_to)) {
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
                const is_void_pointer = expression_type.compare().is_void_pointer;
                if (is_void_pointer) {
                    break :can_cast true;
                }
            },
            .Pointer => |dsubtype| {
                switch (expression_type.data.as) {
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
                const casted = safe_cast_to_integer(t, expression, expression_type, if (dInteger.is_signed) .Signed else .Unsigned);

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
                switch (expression_type.data.as) {
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
        const subexpression = t.ast.create(Ast.Expression);
        subexpression.* = expression.*;
        expression.* = .{
            .position = subexpression.position,
            .as = .{ .Cast = .{
                .typ = cast_to,
                .expression = subexpression,
            } },
            .typ = cast_to,
            .flags = .{
                .is_const = subexpression.flags.is_const,
            },
            .typechecking = .Done,
        };
    }

    return can_cast;
}

fn symetric_safe_cast(t: *TypeChecker, lhs: *Ast.Expression, lhs_type: *Ast.Type, rhs: *Ast.Expression, rhs_type: *Ast.Type) bool {
    return safe_cast(t, lhs, lhs_type, rhs_type) or safe_cast(t, rhs, rhs_type, lhs_type);
}

fn can_unsafe_cast(t: *TypeChecker, expression: *Ast.Expression, expression_type: *Ast.Type, cast_to: *Ast.Type) bool {
    if (safe_cast(t, expression, expression_type, cast_to)) {
        return true;
    }

    var can_cast = false;

    switch (cast_to.data.as) {
        .Struct, .Union, .Proc, .Array, .Void => {},
        .Pointer, .Enum, .Integer, .Bool => {
            switch (expression_type.data.as) {
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

fn make_expression_pointer_mul_integer(t: *TypeChecker, expression: *Ast.Expression, data: *Ast.Type.SharedData) *Ast.Expression {
    const size = nostd.align_up(data.byte_size, data.alignment);
    const rhs = t.ast.create(Ast.Expression);
    rhs.* = .{
        .position = expression.position,
        .as = .{ .Integer = size },
        .typ = Ast.integer_type_from_u64(size),
        .flags = .{
            .is_const = true,
        },
        .typechecking = .Done,
    };
    std.debug.assert(symetric_safe_cast(t, expression, expression.typ, rhs, rhs.typ));
    const new_expression = t.ast.create(Ast.Expression);
    new_expression.* = .{
        .position = expression.position,
        .as = .{ .Binary_Op = .{
            .lhs = expression,
            .rhs = rhs,
            .tag = .Mul,
            .position = undefined,
        } },
        .typ = expression.typ,
        .flags = .{
            .is_const = expression.flags.is_const,
        },
        .typechecking = .Done,
    };
    return new_expression;
}

const exit = nostd.exit;

const std = @import("std");
const nostd = @import("nostd.zig");
const Compiler = @import("Compiler.zig");
const Ast = @import("Ast.zig");
const IR = @import("IR.zig");
const IRGenerator = @import("IRGenerator.zig");
const Interpreter = @import("Interpreter.zig");

const Alignment = nostd.Alignment;
const IRD = IR.Decoded;
const IRE = IR.Encoded;

const TypecheckExpressionResult = struct {
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
