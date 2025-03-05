typed_ast: TypedAst,
ast: *Ast,
c: *Compiler,

const TypeChecker = @This();

const Type = TypedAst.Type;
const TypedExpression = TypedAst.Expression;
const TypedStatement = TypedAst.Statement;
const TypedStatementList = TypedAst.StatementList;
const TypedSymbol = TypedAst.Symbol;
const TypedSymbolList = TypedAst.SymbolList;

const void_pointer_type = &TypedAst.void_pointer_type;
const boolean_type = &TypedAst.boolean_type;
const void_type = &TypedAst.void_type;

const DoubleCastResult = TypedAst.DoubleCastResult;

inline fn perform_double_cast(t: *TypeChecker, result: DoubleCastResult, lhs: *TypedExpression, rhs: *TypedExpression) bool {
    return t.typed_ast.perform_double_cast(result, lhs, rhs);
}

inline fn safe_cast(t: *TypeChecker, expression: *TypedExpression, cast_to: *const Type) bool {
    return t.typed_ast.safe_cast(expression, cast_to);
}

inline fn safe_cast_to_integer(t: *TypeChecker, expression: *TypedExpression, sign: TypedAst.Sign) bool {
    return t.typed_ast.safe_cast_to_integer(expression, sign);
}

inline fn safe_cast_to_two_compatible_integers(t: *TypeChecker, lhs: *TypedExpression, rhs: *TypedExpression) bool {
    return t.typed_ast.safe_cast_to_two_compatible_integers(lhs, rhs);
}

inline fn can_ranked_safe_cast(t: *TypeChecker, lhs: *const Type, rhs: *const Type) DoubleCastResult {
    return t.typed_ast.can_ranked_safe_cast(lhs, rhs);
}

inline fn unsafe_cast(t: *TypeChecker, expression: *TypedExpression, cast_to: *const Type) bool {
    return t.typed_ast.unsafe_cast(expression, cast_to);
}

fn add_or_subtract_pointer_and_integer(t: *TypeChecker, pointer_expression: *TypedExpression, offset_expression: *TypedExpression, operation: enum { Add, Subtract }) *TypedExpression {
    const size = pointer_expression.typ.as.Pointer.size;

    const size_expression = t.typed_ast.create(TypedExpression);
    size_expression.* = .{
        .flags = .{
            .is_lvalue = false,
            .is_const = true,
            .is_static = false,
        },
        .typ = t.typed_ast.lookup_integer_type_from_u64(size),
        .as = .{ .Integer_Literal = size },
    };

    // NOTE: cast to two unsigned integers can never fail.
    std.debug.assert(safe_cast_to_two_compatible_integers(t, offset_expression, size_expression));

    const actual_offset_expression = t.typed_ast.create(TypedExpression);
    actual_offset_expression.* = .{
        .flags = .{
            .is_lvalue = false,
            .is_const = offset_expression.flags.is_const,
            .is_static = false,
        },
        .typ = offset_expression.typ,
        .as = .{ .Binary_Operator = .{
            .lhs = offset_expression,
            .rhs = size_expression,
            .tag = .Mul,
        } },
    };

    const new_expression = t.typed_ast.create(TypedExpression);
    new_expression.* = .{
        .flags = .{
            .is_lvalue = false,
            .is_const = pointer_expression.flags.is_const and actual_offset_expression.flags.is_const,
            .is_static = false,
        },
        .typ = pointer_expression.typ,
        .as = .{ .Binary_Operator = .{
            .lhs = pointer_expression,
            .rhs = actual_offset_expression,
            .tag = switch (operation) {
                .Add => .Add,
                .Subtract => .Sub,
            },
        } },
    };

    return new_expression;
}

fn check_structure(t: *TypeChecker, Structure: Ast.Expression.Structure) *const Type {
    const new_fields = t.typed_ast.alloc(Type.Structure.Field, Structure.fields.len);

    var maximum_alignment: Type.Alignment = .Byte;
    var field_offset: Type.ByteSize = 0;

    {
        var i: usize = 0;
        var it = Structure.fields.first;
        while (it) |node| : (it = node.next) {
            // TODO: put in a seperate function to set typechecking flags.
            const field = &node.data.as.Structure_Field;

            const new_type = check_type(t, field.typ);

            if (new_type.as == .Void) {
                t.c.report_fatal_error(field.typ.position, "field can't be of type '{}'", .{new_type});
            }

            if (field.value) |value| {
                t.c.report_error(value.position, "default values for structure fields are not allowed", .{});

                const new_value = check_expression(t, value);

                if (!new_value.flags.is_const) {
                    t.c.report_error(value.position, "default value must be constant", .{});
                }

                if (!safe_cast(t, new_value, new_type)) {
                    t.c.report_error(value.position, "assigning to incompatible type: '{}'/'{}'", .{ new_type, new_value.typ });
                }
            }

            {
                const d = @intFromEnum(maximum_alignment);
                const s = @intFromEnum(new_type.alignment);
                if (d < s) {
                    maximum_alignment = @enumFromInt(s);
                }
            }

            const offset = nostd.align_up(field_offset, new_type.alignment);
            field_offset = offset + new_type.size;
            new_fields[i] = .{
                .typ = new_type,
                .offset = offset,
            };
            i += 1;
        }
    }

    var new_rest = TypedSymbolList{};

    {
        var it = Structure.rest.first;
        while (it) |node| : (it = node.next) {
            const new_symbol = check_symbol(t, node.data);

            {
                const new_node = t.typed_ast.create(TypedSymbolList.Node);
                new_node.* = .{ .data = new_symbol };
                new_rest.append(new_node);
            }
        }
    }

    const size = nostd.align_up(if (field_offset > 0) field_offset else 1, maximum_alignment);

    const typ = t.typed_ast.create(Type);
    typ.* = .{
        .size = size,
        .alignment = maximum_alignment,
        .as = .{ .Structure = .{
            .fields = new_fields,
            .rest = new_rest,
        } },
    };

    return typ;
}

fn check_union(t: *TypeChecker, Union: Ast.Expression.Union) *const Type {
    const new_fields = t.typed_ast.alloc(Type.Union.Field, Union.fields.len);

    var maximum_size: Type.ByteSize = 1;
    var maximum_alignment: Type.Alignment = .Byte;

    {
        var i: usize = 0;
        var it = Union.fields.first;
        while (it) |node| : (it = node.next) {
            // TODO: put in a seperate function to set typechecking flags.
            const field = &node.data.as.Union_Field;

            const new_type = check_type(t, field.typ);

            if (new_type.as == .Void) {
                t.c.report_fatal_error(field.typ.position, "parameter can't be of type '{}'", .{new_type});
            }

            if (field.value) |value| {
                t.c.report_error(value.position, "default values for structure fields are not allowed", .{});

                const new_value = check_expression(t, value);

                if (!new_value.flags.is_const) {
                    t.c.report_error(value.position, "default value must be constant", .{});
                }

                if (!safe_cast(t, new_value, new_type)) {
                    t.c.report_error(value.position, "assigning to incompatible type: '{}'/'{}'", .{ new_type, new_value.typ });
                }
            }

            {
                const d = @intFromEnum(maximum_alignment);
                const s = @intFromEnum(new_type.alignment);
                if (d < s) {
                    maximum_alignment = @enumFromInt(s);
                }
            }

            if (maximum_size < new_type.size) {
                maximum_size = new_type.size;
            }

            new_fields[i] = .{
                .typ = new_type,
            };
            i += 1;
        }
    }

    var new_rest = TypedSymbolList{};

    {
        var it = Union.rest.first;
        while (it) |node| : (it = node.next) {
            const new_symbol = check_symbol(t, node.data);

            {
                const new_node = t.typed_ast.create(TypedSymbolList.Node);
                new_node.* = .{ .data = new_symbol };
                new_rest.append(new_node);
            }
        }
    }

    const size = nostd.align_up(maximum_size, maximum_alignment);

    const typ = t.typed_ast.create(Type);
    typ.* = .{
        .size = size,
        .alignment = maximum_alignment,
        .as = .{ .Union = .{
            .fields = new_fields,
            .rest = new_rest,
        } },
    };

    return typ;
}

fn check_enumerator(t: *TypeChecker, Enumerator: Ast.Expression.Enumerator) *const Type {
    const new_values = t.typed_ast.alloc(Type.Enumerator.Value, Enumerator.fields.len);

    var maximum_value: Type.Enumerator.Value = 0;
    var is_signed = false;

    // TODO: deal with signed integers.
    {
        var next_value: Type.Enumerator.Value = 0;

        var i: usize = 0;
        var it = Enumerator.fields.first;
        while (it) |node| : (it = node.next) {
            // TODO: put in a seperate function to set typechecking flags.
            const enumerator = &node.data.as.Enumerator_Value;

            if (enumerator.value) |value| {
                // TODO: discard expression.
                const new_value = check_expression(t, value);

                if (!safe_cast_to_integer(t, new_value, .Any)) {
                    t.c.report_error(value.position, "expected integer, but got '{}'", .{new_value.typ});
                    exit(1);
                }

                is_signed = is_signed or new_value.typ.as.Integer.is_signed;

                const v = compute_simple_expression(t, value.position, new_value);
                new_values[i] = v;
                next_value = v;
            } else {
                new_values[i] = next_value;
            }

            {
                const new_value = new_values[i];
                if (maximum_value < new_value) {
                    maximum_value = new_value;
                }
            }

            next_value += 1;
            i += 1;
        }
    }

    var new_rest = TypedSymbolList{};

    {
        var it = Enumerator.rest.first;
        while (it) |node| : (it = node.next) {
            const new_symbol = check_symbol(t, node.data);

            {
                const new_node = t.typed_ast.create(TypedSymbolList.Node);
                new_node.* = .{ .data = new_symbol };
                new_rest.append(new_node);
            }
        }
    }

    // TODO: add one if it is signed.
    const underlying_type = t.typed_ast.lookup_integer_type_from_u64(maximum_value);

    const typ = t.typed_ast.create(Type);
    typ.* = .{
        .size = underlying_type.size,
        .alignment = underlying_type.alignment,
        .as = .{ .Enumerator = .{
            .underlying_type = underlying_type,
            .values = new_values,
            .rest = new_rest,
        } },
    };

    return typ;
}

fn check_procedure(t: *TypeChecker, Procedure: Ast.Expression.Procedure) TypedSymbol.Procedure {
    const new_type = check_type(t, Procedure.typ);
    const new_block = t.typed_ast.alloc(*TypedStatement, Procedure.block.len);

    const env = StatementEnviroment{
        .is_in_loop = false,
        .return_type = new_type.as.Procedure.return_type,
    };

    var i: usize = 0;
    var it = Procedure.block.first;
    while (it) |node| : (it = node.next) {
        const new_statement = check_statement(t, env, node.data);
        new_block[i] = new_statement;
        i += 1;
    }

    return .{
        .typ = new_type,
        .block = new_block,
    };
}

fn check_parameter(t: *TypeChecker, symbol: *Ast.Symbol) Type.Procedure.Parameter {
    const parameter = &symbol.as.Parameter;
    const new_type = check_type(t, parameter.typ);
    if (new_type.as == .Void) {
        t.c.report_fatal_error(parameter.typ.position, "parameter can't be of type '{}'", .{new_type});
    }
    return .{
        .name = symbol.key.name,
        .typ = new_type,
    };
}

const CheckAnyExpressionResult = union(enum) {
    Value: *TypedExpression,
    Type: *const Type,
};

fn check_any_expression(t: *TypeChecker, expression: *Ast.Expression) CheckAnyExpressionResult {
    switch (expression.as) {
        .Structure => |Structure| {
            const typ = check_structure(t, Structure);
            return .{ .Type = typ };
        },
        .Union => |Union| {
            const typ = check_union(t, Union);
            return .{ .Type = typ };
        },
        .Enumerator => |Enumerator| {
            const typ = check_enumerator(t, Enumerator);
            return .{ .Type = typ };
        },
        .Procedure_Type => |Procedure_Type| {
            const new_parameters = t.typed_ast.alloc(Type.Procedure.Parameter, Procedure_Type.parameters.len);

            var i: usize = 0;
            var it = Procedure_Type.parameters.first;
            while (it) |node| : (it = node.next) {
                const new_parameter = check_parameter(t, node.data);
                new_parameters[i] = new_parameter;
                i += 1;
            }

            const new_return_type = check_type(t, Procedure_Type.return_type);

            const new_type = t.typed_ast.create(Type);
            new_type.* = .{
                .size = void_pointer_type.size,
                .alignment = void_pointer_type.alignment,
                .as = .{ .Procedure = .{
                    .parameters = new_parameters,
                    .return_type = new_return_type,
                } },
            };

            return .{ .Type = new_type };
        },
        .Subscript => |Subscript| {
            switch (check_any_expression(t, Subscript.lhs)) {
                .Value => |new_lhs| {
                    const lhs_type = new_lhs.typ;
                    const new_index = check_expression(t, Subscript.index);

                    if (!safe_cast_to_integer(t, new_index, .Unsigned)) {
                        t.c.report_fatal_error(Subscript.lhs.position, "expected integer, but got '{}'", .{new_index.typ});
                    }

                    const typ: *const Type = switch (lhs_type.as) {
                        .Array => |Array| Array.subtype,
                        .Pointer => |subtype| switch (subtype.as) {
                            .Array => |Array| Array.subtype,
                            else => subtype,
                        },
                        else => {
                            t.c.report_fatal_error(Subscript.lhs.position, "expected array or pointer to array, but got '{}'", .{lhs_type});
                        },
                    };

                    const new_expression = t.typed_ast.create(TypedExpression);
                    new_expression.* = .{
                        .flags = .{
                            .is_lvalue = new_lhs.flags.is_lvalue,
                            .is_const = new_lhs.flags.is_const and new_index.flags.is_const,
                            .is_static = new_lhs.flags.is_static and new_index.flags.is_const,
                        },
                        .typ = typ,
                        .as = .{ .Subscript = .{
                            .lhs = new_lhs,
                            .index = new_index,
                        } },
                    };

                    return .{ .Value = new_expression };
                },
                .Type => |new_subtype| {
                    if (new_subtype.as == .Void) {
                        t.c.report_fatal_error(Subscript.lhs.position, "unexpected void type", .{});
                    }

                    const new_index = check_expression(t, Subscript.index);

                    if (!new_index.flags.is_const) {
                        t.c.report_fatal_error(Subscript.index.position, "expected constant expression", .{});
                    }

                    const count = compute_simple_expression(t, Subscript.index.position, new_index);
                    const new_type = t.typed_ast.create(Type);
                    new_type.* = .{
                        .size = new_subtype.size * count,
                        .alignment = new_subtype.alignment,
                        .as = .{ .Array = .{
                            .subtype = new_subtype,
                            .count = count,
                        } },
                    };

                    return .{ .Type = new_type };
                },
            }
        },
        .Dereference => |subexpression| {
            switch (check_any_expression(t, subexpression)) {
                .Value => |new_subexpression| {
                    const subexpression_type = new_subexpression.typ;

                    if (!subexpression_type.compare().can_be_dereferenced) {
                        t.c.report_fatal_error(subexpression.position, "can't dereference expression of type '{}'", .{subexpression_type});
                    }

                    const new_expression = t.typed_ast.create(TypedExpression);
                    new_expression.* = .{
                        .flags = .{
                            .is_lvalue = true,
                            .is_const = false,
                            .is_static = false,
                        },
                        .typ = subexpression_type.as.Pointer,
                        .as = .{ .Dereference = new_subexpression },
                    };

                    return .{ .Value = new_expression };
                },
                .Type => |new_subtype| {
                    const new_type = t.typed_ast.create(Type);
                    new_type.* = .{
                        .size = void_pointer_type.size,
                        .alignment = void_pointer_type.alignment,
                        .as = .{ .Pointer = new_subtype },
                    };
                    return .{ .Type = new_type };
                },
            }
        },
        .Integer_Type => |Integer_Type| {
            const new_type = t.typed_ast.lookup_integer_type(.{ .bits = Integer_Type.bits, .is_signed = Integer_Type.is_signed });
            return .{ .Type = new_type };
        },
        .Boolean_Type => {
            return .{ .Type = boolean_type };
        },
        .Void_Type => {
            return .{ .Type = void_type };
        },
        .Member_Access,
        .Identifier,
        => unreachable,
        .Type_Of => |subexpression| {
            return switch (check_any_expression(t, subexpression)) {
                .Value => |new_subexpression| .{ .Type = new_subexpression.typ },
                .Type => |new_type| .{ .Type = new_type },
            };
        },

        .Procedure => |Procedure| {
            const new_Procedure = check_procedure(t, Procedure);
            const new_expression = t.typed_ast.create(TypedExpression);
            new_expression.* = .{
                .flags = .{
                    .is_lvalue = false,
                    .is_const = true,
                    .is_static = true,
                },
                .typ = new_Procedure.typ,
                .as = .{ .Procedure = new_Procedure },
            };

            return .{ .Value = new_expression };
        },
        .Procedure_Call => |Procedure_Call| {
            switch (check_any_expression(t, Procedure_Call.lhs)) {
                .Value => |new_lhs| {
                    const lhs_type = new_lhs.typ;

                    if (lhs_type.as != .Procedure) {
                        t.c.report_fatal_error(Procedure_Call.lhs.position, "expected a procedure, but got '{}'", .{lhs_type});
                    }

                    const Procedure = &lhs_type.as.Procedure;

                    if (Procedure.parameters.len != Procedure_Call.arguments.len) {
                        t.c.report_fatal_error(Procedure_Call.lhs.position, "expected {} arguments, but got {}", .{ Procedure.parameters.len, Procedure_Call.arguments.len });
                    }

                    const new_arguments = t.typed_ast.alloc(*TypedExpression, Procedure_Call.arguments.len);

                    var ait = Procedure_Call.arguments.first;
                    for (Procedure.parameters, 0..) |parameter, i| {
                        const anode = ait.?;

                        switch (anode.data) {
                            .Field_Designator => |Designator| {
                                t.c.report_fatal_error(Designator.name.position, "unexpected designator", .{});
                            },
                            .Expression => |argument| {
                                const new_argument = check_expression(t, argument);

                                if (!safe_cast(t, new_argument, parameter.typ)) {
                                    t.c.report_fatal_error(argument.position, "expected '{}', but got '{}'", .{ parameter.typ, new_argument.typ });
                                }

                                new_arguments[i] = new_argument;
                            },
                        }

                        ait = anode.next;
                    }

                    const new_expression = t.typed_ast.create(TypedExpression);
                    new_expression.* = .{
                        .flags = .{
                            .is_lvalue = false,
                            .is_const = false,
                            .is_static = false,
                        },
                        .typ = Procedure.return_type,
                        .as = .{ .Procedure_Call = .{
                            .lhs = new_lhs,
                            .arguments = new_arguments,
                        } },
                    };

                    return .{ .Value = new_expression };
                },
                .Type => {
                    t.c.report_fatal_error(Procedure_Call.lhs.position, "TODO: constructors", .{});
                },
            }
        },

        .Reference => |subexpression| {
            const new_subexpression = check_expression(t, subexpression);

            if (!new_subexpression.flags.is_lvalue) {
                t.c.report_fatal_error(subexpression.position, "expression is not an lvalue", .{});
            } else if (new_subexpression.flags.is_const) {
                t.c.report_fatal_error(subexpression.position, "can't take raferences to constant expression", .{});
            }

            const typ = t.typed_ast.create(Type);
            typ.* = .{
                .size = void_pointer_type.size,
                .alignment = void_pointer_type.alignment,
                .as = .{ .Pointer = new_subexpression.typ },
            };
            const new_expression = t.typed_ast.create(TypedExpression);
            new_expression.* = .{
                .flags = .{
                    .is_lvalue = false,
                    .is_const = new_subexpression.flags.is_const or new_subexpression.flags.is_static,
                    .is_static = false,
                },
                .typ = typ,
                .as = .{ .Reference = new_subexpression },
            };

            return .{ .Value = new_expression };
        },
        .Unary_Operator => |Unary_Operator| {
            const subexpression = check_expression(t, Unary_Operator.subexpression);

            switch (Unary_Operator.tag) {
                .Plus => {
                    if (!safe_cast_to_integer(t, subexpression, .Any)) {
                        t.c.report_fatal_error(Unary_Operator.subexpression.position, "expected integer, but got '{}'", .{subexpression.typ});
                    }

                    return .{ .Value = subexpression };
                },
                .Minus => {
                    if (!safe_cast_to_integer(t, subexpression, .Signed)) {
                        t.c.report_fatal_error(Unary_Operator.subexpression.position, "expected signed integer, but got '{}'", .{subexpression.typ});
                    }

                    const new_expression = t.typed_ast.create(TypedExpression);
                    new_expression.* = .{
                        .flags = .{
                            .is_lvalue = false,
                            .is_const = subexpression.flags.is_const,
                            .is_static = false,
                        },
                        .typ = subexpression.typ,
                        .as = .{ .Unary_Operator = .{
                            .subexpression = subexpression,
                            .tag = .Minus,
                        } },
                    };

                    return .{ .Value = new_expression };
                },
                .Not => {
                    if (!safe_cast(t, subexpression, boolean_type)) {
                        t.c.report_error(Unary_Operator.subexpression.position, "expected 'bool', but got '{}'", .{subexpression.typ});
                    }

                    const new_expression = t.typed_ast.create(TypedExpression);
                    new_expression.* = .{
                        .flags = .{
                            .is_lvalue = false,
                            .is_const = subexpression.flags.is_const,
                            .is_static = false,
                        },
                        .typ = subexpression.typ,
                        .as = .{ .Unary_Operator = .{
                            .subexpression = subexpression,
                            .tag = .Not,
                        } },
                    };

                    return .{ .Value = new_expression };
                },
            }
        },
        .Binary_Operator => |Binary_Operator| {
            const new_lhs = check_expression(t, Binary_Operator.lhs);
            const new_rhs = check_expression(t, Binary_Operator.rhs);

            const flags = TypedExpression.Flags{
                .is_lvalue = false,
                .is_const = new_lhs.flags.is_const and new_rhs.flags.is_const,
                .is_static = false,
            };

            switch (Binary_Operator.tag) {
                .Or,
                .And,
                => {
                    if (!safe_cast(t, new_lhs, boolean_type) or !safe_cast(t, new_rhs, boolean_type)) {
                        t.c.report_fatal_error(Binary_Operator.position, "mismatched types: '{}' and '{}'", .{ new_lhs.typ, new_rhs.typ });
                    }

                    // TODO: only short circuit if right hand side is not constant.
                    switch (Binary_Operator.tag) {
                        .Or => {
                            // lhs or rhs == if (lhs) true else rhs
                            const true_literal = t.typed_ast.create(TypedExpression);
                            true_literal.* = .{
                                .flags = .{
                                    .is_lvalue = false,
                                    .is_const = true,
                                    .is_static = false,
                                },
                                .typ = boolean_type,
                                .as = .{ .Integer_Literal = @intFromBool(true) },
                            };

                            const new_expression = t.typed_ast.create(TypedExpression);
                            new_expression.* = .{
                                .flags = flags,
                                .typ = boolean_type,
                                .as = .{ .If = .{
                                    .condition = new_lhs,
                                    .true_branch = true_literal,
                                    .false_branch = new_rhs,
                                } },
                            };

                            return .{ .Value = new_expression };
                        },
                        .And => {
                            // lhs and rhs == if (lhs) rhs else false
                            const false_literal = t.typed_ast.create(TypedExpression);
                            false_literal.* = .{
                                .flags = .{
                                    .is_lvalue = false,
                                    .is_const = true,
                                    .is_static = false,
                                },
                                .typ = boolean_type,
                                .as = .{ .Integer_Literal = @intFromBool(false) },
                            };

                            const new_expression = t.typed_ast.create(TypedExpression);
                            new_expression.* = .{
                                .flags = flags,
                                .typ = boolean_type,
                                .as = .{ .If = .{
                                    .condition = new_lhs,
                                    .true_branch = new_rhs,
                                    .false_branch = false_literal,
                                } },
                            };

                            return .{ .Value = new_expression };
                        },
                        else => unreachable,
                    }
                },
                .Eq,
                .Neq,
                => {
                    const result = can_ranked_safe_cast(t, new_lhs.typ, new_rhs.typ);

                    if (result.lhs == .Cant_Cast or !new_lhs.typ.compare().is_comparable) {
                        t.c.report_error(Binary_Operator.position, "can't compare '{}' and '{}'", .{ new_lhs.typ, new_rhs.typ });
                    }

                    std.debug.assert(perform_double_cast(t, result, new_lhs, new_rhs));

                    const new_expression = t.typed_ast.create(TypedExpression);
                    new_expression.* = .{
                        .flags = flags,
                        .typ = boolean_type,
                        .as = .{ .Binary_Operator = .{
                            .lhs = new_lhs,
                            .rhs = new_rhs,
                            .tag = switch (Binary_Operator.tag) {
                                .Eq => .Eq,
                                .Neq => .Neq,
                                else => unreachable,
                            },
                        } },
                    };

                    return .{ .Value = new_expression };
                },
                .Lt,
                .Leq,
                .Gt,
                .Geq,
                => {
                    const result = can_ranked_safe_cast(t, new_lhs.typ, new_rhs.typ);

                    if (result.lhs == .Cant_Cast or !new_lhs.typ.compare().is_ordered) {
                        t.c.report_error(Binary_Operator.position, "can't compare '{}' and '{}'", .{ new_lhs.typ, new_rhs.typ });
                    }

                    const new_expression = t.typed_ast.create(TypedExpression);
                    new_expression.* = .{
                        .flags = flags,
                        .typ = boolean_type,
                        .as = .{ .Binary_Operator = .{
                            .lhs = new_lhs,
                            .rhs = new_rhs,
                            .tag = switch (Binary_Operator.tag) {
                                .Lt => .Lt,
                                .Leq => .Leq,
                                .Gt => .Gt,
                                .Geq => .Geq,
                                else => unreachable,
                            },
                        } },
                    };

                    return .{ .Value = new_expression };
                },
                .Add => {
                    if (new_lhs.typ.equal(void_pointer_type) or new_rhs.typ.equal(void_pointer_type)) {
                        t.c.report_fatal_error(Binary_Operator.position, "can't add 'void' pointers", .{});
                    } else if (new_lhs.typ.as == .Pointer and safe_cast_to_integer(t, new_rhs, .Unsigned)) {
                        const new_expression = add_or_subtract_pointer_and_integer(t, new_lhs, new_rhs, .Add);
                        return .{ .Value = new_expression };
                    } else if (new_rhs.typ.as == .Pointer and safe_cast_to_integer(t, new_lhs, .Unsigned)) {
                        const new_expression = add_or_subtract_pointer_and_integer(t, new_rhs, new_lhs, .Add);
                        return .{ .Value = new_expression };
                    } else {
                        if (!safe_cast_to_two_compatible_integers(t, new_lhs, new_rhs)) {
                            t.c.report_fatal_error(Binary_Operator.position, "expected integer/pointer: '{}' and '{}'", .{ new_lhs.typ, new_rhs.typ });
                        }

                        const new_expression = t.typed_ast.create(TypedExpression);
                        new_expression.* = .{
                            .flags = flags,
                            .typ = new_lhs.typ,
                            .as = .{ .Binary_Operator = .{
                                .lhs = new_lhs,
                                .rhs = new_rhs,
                                .tag = .Add,
                            } },
                        };

                        return .{ .Value = new_expression };
                    }
                },
                .Sub => {
                    if (new_lhs.typ.equal(void_pointer_type)) {
                        t.c.report_fatal_error(Binary_Operator.position, "can't subtract 'void' pointer", .{});
                    } else if (new_lhs.typ.as == .Pointer and safe_cast_to_integer(t, new_rhs, .Unsigned)) {
                        const new_expression = add_or_subtract_pointer_and_integer(t, new_lhs, new_rhs, .Subtract);
                        return .{ .Value = new_expression };
                    } else {
                        if (!safe_cast_to_two_compatible_integers(t, new_lhs, new_rhs)) {
                            t.c.report_fatal_error(Binary_Operator.position, "expected integer/pointer: '{}' and '{}'", .{ new_lhs.typ, new_rhs.typ });
                        }

                        const new_expression = t.typed_ast.create(TypedExpression);
                        new_expression.* = .{
                            .flags = flags,
                            .typ = new_lhs.typ,
                            .as = .{ .Binary_Operator = .{
                                .lhs = new_lhs,
                                .rhs = new_rhs,
                                .tag = .Sub,
                            } },
                        };

                        return .{ .Value = new_expression };
                    }
                },
                .Mul,
                .Div,
                .Mod,
                => {
                    if (!safe_cast_to_two_compatible_integers(t, new_lhs, new_rhs)) {
                        t.c.report_fatal_error(Binary_Operator.position, "expected integer/pointer: '{}' and '{}'", .{ new_lhs.typ, new_rhs.typ });
                    }

                    const new_expression = t.typed_ast.create(TypedExpression);
                    new_expression.* = .{
                        .flags = flags,
                        .typ = new_lhs.typ,
                        .as = .{ .Binary_Operator = .{
                            .lhs = new_lhs,
                            .rhs = new_rhs,
                            .tag = switch (Binary_Operator.tag) {
                                .Mul => .Mul,
                                .Div => .Div,
                                .Mod => .Mod,
                                else => unreachable,
                            },
                        } },
                    };

                    return .{ .Value = new_expression };
                },
            }
        },

        .Size_Of => |subexpression| {
            const size: Type.ByteSize = switch (check_any_expression(t, subexpression)) {
                .Value => |value| value.typ.size,
                .Type => |new_type| new_type.size,
            };

            const typ = t.typed_ast.lookup_integer_type_from_u64(size);
            const new_expression = t.typed_ast.create(TypedExpression);
            new_expression.* = .{
                .flags = .{
                    .is_lvalue = false,
                    .is_const = true,
                    .is_static = false,
                },
                .typ = typ,
                .as = .{ .Integer_Literal = size },
            };

            return .{ .Value = new_expression };
        },
        .Alignment_Of => |subexpression| {
            const alignment: Type.Alignment = switch (check_any_expression(t, subexpression)) {
                .Value => |value| value.typ.alignment,
                .Type => |new_type| new_type.alignment,
            };

            const value = alignment.to_byte_size();
            const typ = t.typed_ast.lookup_integer_type_from_u64(value);
            const new_expression = t.typed_ast.create(TypedExpression);
            new_expression.* = .{
                .flags = .{
                    .is_lvalue = false,
                    .is_const = true,
                    .is_static = false,
                },
                .typ = typ,
                .as = .{ .Integer_Literal = value },
            };

            return .{ .Value = new_expression };
        },
        .As => |As| {
            const new_type = check_type(t, As.typ);
            const new_expression = check_expression(t, As.expression);

            if (!safe_cast(t, new_expression, new_type)) {
                t.c.report_fatal_error(As.expression.position, "can't reinterpret '{}' as '{}'", .{ new_expression.typ, new_type });
            }

            return .{ .Value = new_expression };
        },
        .Cast => |Cast| {
            const new_type = check_type(t, Cast.typ);
            const new_expression = check_expression(t, Cast.expression);

            if (!unsafe_cast(t, new_expression, new_type)) {
                t.c.report_fatal_error(Cast.expression.position, "can't cast '{}' to '{}'", .{ new_expression.typ, new_type });
            }

            return .{ .Value = new_expression };
        },

        .Integer_Literal => |value| {
            const typ = t.typed_ast.lookup_integer_type_from_u64(value);
            const new_expression = t.typed_ast.create(TypedExpression);
            new_expression.* = .{
                .flags = .{
                    .is_lvalue = false,
                    .is_const = true,
                    .is_static = false,
                },
                .typ = typ,
                .as = .{ .Integer_Literal = value },
            };
            return .{ .Value = new_expression };
        },
        .Boolean_Literal => |value| {
            const new_expression = t.typed_ast.create(TypedExpression);
            new_expression.* = .{
                .flags = .{
                    .is_lvalue = false,
                    .is_const = true,
                    .is_static = false,
                },
                .typ = boolean_type,
                .as = .{ .Integer_Literal = @intFromBool(value) },
            };
            return .{ .Value = new_expression };
        },
        .Null_Literal => {
            const new_expression = t.typed_ast.create(TypedExpression);
            new_expression.* = .{
                .flags = .{
                    .is_lvalue = false,
                    .is_const = true,
                    .is_static = false,
                },
                .typ = void_pointer_type,
                .as = .Null_Literal,
            };
            return .{ .Value = new_expression };
        },
    }
}

fn check_type(t: *TypeChecker, expression: *Ast.Expression) *const Type {
    return switch (check_any_expression(t, expression)) {
        .Value => {
            t.c.report_fatal_error(expression.position, "expected a type, but got an expression", .{});
        },
        .Type => |new_type| new_type,
    };
}

fn check_expression(t: *TypeChecker, expression: *Ast.Expression) *TypedExpression {
    return switch (check_any_expression(t, expression)) {
        .Value => |new_expression| new_expression,
        .Type => {
            t.c.report_fatal_error(expression.position, "expected an expression, but got a type", .{});
        },
    };
}

const StatementEnviroment = struct {
    is_in_loop: bool,
    return_type: *const Type,
};

fn check_statement(t: *TypeChecker, _: StatementEnviroment, statement: *Ast.Statement) *TypedStatement {
    switch (statement.as) {
        .Symbol => |symbol| {
            const new_symbol = check_symbol(t, symbol);
            const new_statement = t.typed_ast.create(TypedStatement);
            new_statement.* = .{
                .as = .{ .Symbol = new_symbol },
            };
            return new_statement;
        },
        .Assign => |Assign| {
            const new_lhs = check_expression(t, Assign.lhs);

            if (!new_lhs.flags.is_lvalue) {
                t.c.report_fatal_error(Assign.lhs.position, "expression is not an lvalue", .{});
            } else if (new_lhs.flags.is_const) {
                t.c.report_fatal_error(Assign.lhs.position, "can't assign to constant expression", .{});
            }

            const new_rhs = check_expression(t, Assign.rhs);

            if (!safe_cast(t, new_rhs, new_lhs.typ)) {
                t.c.report_fatal_error(statement.position, "expected '{}', but got '{}'", .{ new_lhs.typ, new_rhs.typ });
            }

            const new_statement = t.typed_ast.create(TypedStatement);
            new_statement.* = .{
                .as = .{ .Assign = .{
                    .lhs = new_lhs,
                    .rhs = new_rhs,
                } },
            };
            return new_statement;
        },
        .Expression => |expression| {
            const new_expression = check_expression(t, expression);
            const new_statement = t.typed_ast.create(TypedStatement);
            new_statement.* = .{
                .as = .{ .Expression = new_expression },
            };
            return new_statement;
        },
        else => {
            t.c.report_fatal_error(statement.position, "TODO: implement rest of statements", .{});
        },
    }
}

fn compute_simple_expression(t: *TypeChecker, position: FilePosition, expression: *TypedExpression) u64 {
    return switch (expression.as) {
        .Integer_Literal => |value| value,
        .Null_Literal => 0,
        else => {
            t.c.report_fatal_error(position, "TODO: interpreter", .{});
        },
    };
}

fn check_symbol(t: *TypeChecker, symbol: *Ast.Symbol) *TypedSymbol {
    switch (symbol.as) {
        .Alias => |expression| {
            switch (check_any_expression(t, expression)) {
                .Value => |new_expression| {
                    if (!new_expression.flags.is_const) {
                        t.c.report_fatal_error(expression.position, "expected constant expression", .{});
                    }

                    const new_symbol = t.typed_ast.create(TypedSymbol);
                    new_symbol.* = .{
                        .as = .{ .Variable = .{
                            .attributes = .{
                                .is_const = true,
                                .is_static = false,
                            },
                            .typ = new_expression.typ,
                            .value = new_expression,
                        } },
                    };
                    return new_symbol;
                },
                .Type => |new_type| {
                    const new_symbol = t.typed_ast.create(TypedSymbol);
                    new_symbol.* = .{
                        .as = .{ .Type = new_type },
                    };
                    return new_symbol;
                },
            }
        },
        .Structure => |Structure| {
            const typ = check_structure(t, Structure);
            const new_symbol = t.typed_ast.create(TypedSymbol);
            new_symbol.* = .{
                .as = .{ .Type = typ },
            };
            return new_symbol;
        },
        .Union => |Union| {
            const typ = check_union(t, Union);
            const new_symbol = t.typed_ast.create(TypedSymbol);
            new_symbol.* = .{
                .as = .{ .Type = typ },
            };
            return new_symbol;
        },
        .Enumerator => |Enumerator| {
            const typ = check_enumerator(t, Enumerator);
            const new_symbol = t.typed_ast.create(TypedSymbol);
            new_symbol.* = .{
                .as = .{ .Type = typ },
            };
            return new_symbol;
        },
        .Procedure => |Procedure| {
            const new_Procedure = check_procedure(t, Procedure);

            const new_symbol = t.typed_ast.create(TypedSymbol);
            new_symbol.* = .{
                .as = .{ .Procedure = new_Procedure },
            };

            return new_symbol;
        },
        .Variable => |Variable| {
            if (Variable.typ) |typ| {
                const new_type = check_type(t, typ);

                if (new_type.as == .Void) {
                    t.c.report_fatal_error(typ.position, "variable can't be of type '{}'", .{new_type});
                }

                var new_value: ?*TypedExpression = null;

                if (Variable.value) |value| {
                    const _new_value = check_expression(t, value);
                    new_value = _new_value;

                    if (Variable.attributes.is_const or Variable.attributes.is_static and !_new_value.flags.is_const) {
                        t.c.report_fatal_error(value.position, "expected constant expression to initialize constant/static variables", .{});
                    }

                    if (!safe_cast(t, _new_value, new_type)) {
                        t.c.report_error(value.position, "expected '{}', but got '{}'", .{ new_type, _new_value.typ });
                        t.c.report_note(typ.position, "expected type is here", .{});
                        exit(1);
                    }
                } else if (Variable.attributes.is_const or Variable.attributes.is_static) {
                    t.c.report_fatal_error(symbol.position, "expected initializer for constant/static variables", .{});
                }

                const new_symbol = t.typed_ast.create(TypedSymbol);
                new_symbol.* = .{
                    .as = .{ .Variable = .{
                        .attributes = .{
                            .is_const = Variable.attributes.is_const,
                            .is_static = Variable.attributes.is_static,
                        },
                        .typ = new_type,
                        .value = new_value,
                    } },
                };

                return new_symbol;
            } else {
                t.c.report_fatal_error(symbol.position, "type inference is not implemented, yet", .{});
            }
        },
        .Structure_Field,
        .Union_Field,
        .Enumerator_Value,
        .Parameter,
        => unreachable,
    }
}

fn check_top_level(t: *TypeChecker) void {
    var it = t.ast.symbols.first;
    while (it) |node| : (it = node.next) {
        const new_symbol = check_symbol(t, node.data);

        {
            const new_node = t.typed_ast.create(TypedSymbolList.Node);
            new_node.* = .{ .data = new_symbol };
            t.typed_ast.symbols.append(new_node);
        }
    }
}

pub fn check(c: *Compiler, ast: *Ast) TypedAst {
    var typechecker = TypeChecker{
        .typed_ast = .{
            .symbols = .{},
            .arena = TypedAst.ArenaAllocator.init(std.heap.page_allocator),
        },
        .ast = ast,
        .c = c,
    };

    check_top_level(&typechecker);

    if (c.had_error) {
        exit(1);
    }

    return typechecker.typed_ast;
}

const exit = nostd.exit;

const FilePosition = Ast.FilePosition;

const std = @import("std");
const nostd = @import("nostd.zig");
const Compiler = @import("Compiler.zig");
const Ast = @import("Ast.zig");
const TypedAst = @import("TypedAst.zig");
