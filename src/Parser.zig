current_scope: *Ast.Scope,
lexer: Lexer,
ast: Ast,
c: *Compiler,

const Parser = @This();

const TokenIndex = Lexer.TokenIndex;
const Token = Lexer.Token;

inline fn putback(parser: *Parser, token: Token) void {
    parser.lexer.putback(token);
}

inline fn grab(parser: *Parser) Token {
    return parser.lexer.grab();
}

inline fn peek_at(parser: *Parser, index: TokenIndex) Token.Tag {
    return parser.lexer.peek_at(index);
}

inline fn peek(parser: *Parser) Token.Tag {
    return parser.lexer.peek();
}

inline fn advance_many(parser: *Parser, count: TokenIndex) void {
    parser.lexer.advance_many(count);
}

inline fn advance(parser: *Parser) void {
    parser.lexer.advance();
}

inline fn expect(parser: *Parser, expected: Token.Tag) void {
    parser.lexer.expect(expected);
}

fn push_scope(parser: *Parser, tag: Ast.Scope.Tag) void {
    const scope = parser.ast.create(Ast.Scope);
    scope.* = .{
        .parent = parser.current_scope,
        .tag = tag,
    };
    parser.current_scope = scope;
}

fn pop_scope(parser: *Parser) void {
    parser.current_scope = parser.current_scope.parent.?;
}

// Assume 'expression' is heap allocated and can be reused.
pub fn expression_to_type(parser: *Parser, expression: *Ast.Expression) *Ast.Type {
    switch (expression.as) {
        .Deref => |subexpression| {
            const subtype = expression_to_type(parser, subexpression);

            const data = parser.ast.create(Ast.Type.SharedData);
            data.* = .{
                .as = .{ .Pointer = subtype },
                .byte_size = Ast.pointer_byte_size,
                .alignment = Ast.pointer_alignment,
                .stages = Ast.default_stages_none,
            };
            expression.as = .{ .Type = .{
                .position = expression.position,
                .data = data,
                .symbol = null,
            } };

            return &expression.as.Type;
        },
        .Field => |Field| {
            const subtype = expression_to_type(parser, Field.subexpression);

            const data = parser.ast.create(Ast.Type.SharedData);
            data.* = .{
                .as = .{ .Field = .{
                    .subtype = subtype,
                    .field = Field.field,
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

            return &expression.as.Type;
        },
        .Subscript => |Subscript| {
            const subtype = expression_to_type(parser, Subscript.subexpression);

            const data = parser.ast.create(Ast.Type.SharedData);
            data.* = .{
                .as = .{ .Array = .{
                    .subtype = subtype,
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

            return &expression.as.Type;
        },
        .Type => |*typ| {
            return typ;
        },
        .Identifier => |Identifier| {
            const data = parser.ast.create(Ast.Type.SharedData);
            data.* = .{
                .as = .{ .Identifier = .{
                    .name = Identifier.name,
                    .scope = Identifier.scope,
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

            return &expression.as.Type;
        },
        else => {
            parser.c.report_error(expression.position, "expected expressionession, not a type", .{});
            exit(1);
        },
    }
}

fn parse_type(parser: *Parser) *Ast.Type {
    return expression_to_type(parser, parse_expression(parser));
}

fn maybe_eat_semicolon(parser: *Parser, symbol: *Ast.Symbol) void {
    switch (symbol.as) {
        .Variable,
        .Struct_Field,
        .Union_Field,
        .Enum_Field,
        => expect(parser, .Semicolon),
        .Parameter,
        .Procedure,
        .Type,
        => {},
    }
}

fn parse_structure_union_or_enumerator_body(parser: *Parser, tag: Ast.Scope.Tag) Ast.Type.Struct {
    push_scope(parser, tag);

    expect(parser, .Left_Brace);

    var fields = parser.ast.create(Ast.SymbolList);
    var rest = parser.ast.create(Ast.SymbolList);

    fields.* = .{};
    rest.* = .{};

    var tt = peek(parser);
    while (tt != .End_Of_File and tt != .Right_Brace) {
        const symbol = parse_symbol(parser);

        {
            const node = parser.ast.create(Ast.SymbolList.Node);
            node.* = .{ .data = symbol };
            switch (symbol.as) {
                .Struct_Field,
                .Union_Field,
                .Enum_Field,
                => fields.append(node),
                .Variable,
                .Procedure,
                .Type,
                => rest.append(node),
                .Parameter,
                => unreachable,
            }

            maybe_eat_semicolon(parser, symbol);
        }

        tt = peek(parser);
    }

    expect(parser, .Right_Brace);

    const scope = parser.current_scope;

    pop_scope(parser);

    return .{
        .fields = fields,
        .rest = rest,
        .scope = scope,
    };
}

fn parse_structure_union_or_enumerator(parser: *Parser) struct { *Ast.Expression, ?Token } {
    const token = grab(parser);
    advance(parser);

    switch (token.as) {
        .Struct => {
            const has_id = maybe_eat_symbol_declaration(parser);
            const body = parse_structure_union_or_enumerator_body(parser, .Structure);

            const data = parser.ast.create(Ast.Type.SharedData);
            data.* = .{
                .as = .{ .Struct = body },
                .byte_size = 0,
                .alignment = .Byte,
                .stages = Ast.default_stages_none,
            };
            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .{ .Type = .{
                    .position = token.position,
                    .data = data,
                    .symbol = null,
                } },
                .typ = Ast.void_type,
                .flags = .{},
            };

            {
                const node = parser.ast.create(Ast.TypeList.Node);
                node.* = .{ .data = &expression.as.Type };
                parser.ast.namespaces.append(node);
            }

            return .{ expression, has_id };
        },
        .Union => {
            const has_id = maybe_eat_symbol_declaration(parser);
            const body = parse_structure_union_or_enumerator_body(parser, .Union);

            const data = parser.ast.create(Ast.Type.SharedData);
            data.* = .{
                .as = .{ .Union = body },
                .byte_size = 0,
                .alignment = .Byte,
                .stages = Ast.default_stages_none,
            };
            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .{ .Type = .{
                    .position = token.position,
                    .data = data,
                    .symbol = null,
                } },
                .typ = Ast.void_type,
                .flags = .{},
            };

            {
                const node = parser.ast.create(Ast.TypeList.Node);
                node.* = .{ .data = &expression.as.Type };
                parser.ast.namespaces.append(node);
            }

            return .{ expression, has_id };
        },
        .Enum => {
            const has_id = maybe_eat_symbol_declaration(parser);
            const body = parse_structure_union_or_enumerator_body(parser, .Enumerator);

            const data = parser.ast.create(Ast.Type.SharedData);
            data.* = .{
                .as = .{ .Enum = .{
                    .fields = body.fields,
                    .rest = body.rest,
                    .integer_type = Ast.void_type,
                    .scope = body.scope,
                } },
                .byte_size = 0,
                .alignment = .Byte,
                .stages = Ast.default_stages_none,
            };
            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .{ .Type = .{
                    .position = token.position,
                    .data = data,
                    .symbol = null,
                } },
                .typ = Ast.void_type,
                .flags = .{},
            };

            {
                const node = parser.ast.create(Ast.TypeList.Node);
                node.* = .{ .data = &expression.as.Type };
                parser.ast.namespaces.append(node);
            }

            return .{ expression, has_id };
        },
        else => unreachable,
    }
}

const ProcedureOrItsType = union(enum) {
    Procedure_Type: Ast.Type,
    Procedure: Ast.Symbol.Procedure,
};

fn parse_procedure_or_its_type(parser: *Parser, position: FilePosition) ProcedureOrItsType {
    push_scope(parser, .Procedure);
    defer pop_scope(parser);

    expect(parser, .Left_Parenthesis);

    var parameters = Ast.SymbolList{};

    var tt = peek(parser);
    while (tt != .End_Of_File and tt != .Right_Parenthesis) {
        const symbol = parse_variable(parser);

        switch (symbol.as) {
            .Variable => |Variable| {
                if (Variable.attributes.is_static or Variable.attributes.is_const) {
                    parser.c.report_error(symbol.position, "parameter can't be constant or static", .{});
                }

                if (Variable.value) |value| {
                    parser.c.report_error(value.position, "default value for parameters is not supported", .{});
                }

                if (Variable.typ == null) {
                    parser.c.report_fatal_error(symbol.position, "expected type for a parameter", .{});
                }

                const tmp = Variable;
                symbol.as = .{ .Parameter = .{
                    .typ = tmp.typ.?,
                    .value = tmp.value,
                    .storage = null,
                } };
            },
            else => unreachable,
        }

        {
            const node = parser.ast.create(Ast.SymbolList.Node);
            node.* = .{ .data = symbol };
            parameters.append(node);
        }

        tt = peek(parser);
        if (tt != .End_Of_File and tt != .Right_Parenthesis) {
            expect(parser, .Comma);
            tt = peek(parser);
        }
    }

    expect(parser, .Right_Parenthesis);

    const return_type = parse_type(parser);

    const data = parser.ast.create(Ast.Type.SharedData);
    data.* = .{
        .as = .{ .Proc = .{
            .params = parameters,
            .return_type = return_type,
            .scope = parser.current_scope,
        } },
        .byte_size = Ast.pointer_byte_size,
        .alignment = Ast.pointer_alignment,
        .stages = Ast.default_stages_none,
    };

    if (peek(parser) == .Left_Brace) {
        const block = parse_statement_list(parser);

        const typ = parser.ast.create(Ast.Type);
        typ.* = .{
            .position = position,
            .data = data,
            .symbol = null,
        };

        return .{ .Procedure = .{
            .typ = typ,
            .block = block,
            .labels = null,
        } };
    }

    return .{ .Procedure_Type = .{
        .position = position,
        .data = data,
        .symbol = null,
    } };
}

fn parse_base_expression(parser: *Parser) *Ast.Expression {
    const token = grab(parser);
    advance(parser);

    switch (token.as) {
        .Struct,
        .Union,
        .Enum,
        => {
            putback(parser, token);
            const typ, const has_id = parse_structure_union_or_enumerator(parser);

            if (has_id) |id| {
                parser.c.report_error(id.position, "unexpected identifier declaration for an unnamed type", .{});
            }

            return typ;
        },
        .Proc => {
            switch (parse_procedure_or_its_type(parser, token.position)) {
                .Procedure_Type => |Type| {
                    const expression = parser.ast.create(Ast.Expression);
                    expression.* = .{
                        .position = token.position,
                        .as = .{ .Type = Type },
                        .typ = Ast.void_type,
                        .flags = .{},
                    };
                    return expression;
                },
                .Procedure => {
                    parser.c.report_fatal_error(token.position, "unnamed procedures are not supported", .{});
                },
            }
        },
        .Integer_Type => |Integer_Type| {
            const typ = parser.ast.lookup_integer_type(.{ .bits = Integer_Type.bits, .is_signed = Integer_Type.is_signed });
            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .{ .Type = .{
                    .position = token.position,
                    .data = typ.data,
                    .symbol = null,
                } },
                .typ = Ast.void_type,
                .flags = .{},
            };
            return expression;
        },
        .Boolean_Type => {
            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .{ .Type = .{
                    .position = token.position,
                    .data = Ast.bool_type.data,
                    .symbol = null,
                } },
                .typ = Ast.void_type,
                .flags = .{},
            };
            return expression;
        },
        .Void_Type => {
            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .{ .Type = .{
                    .position = token.position,
                    .data = Ast.void_type.data,
                    .symbol = null,
                } },
                .typ = Ast.void_type,
                .flags = .{},
            };
            return expression;
        },
        .Type_Of => {
            var arguments: [1]*Ast.Expression = undefined;
            parse_fixed_size_expression_list(parser, &arguments);

            const data = parser.ast.create(Ast.Type.SharedData);
            data.* = .{
                .as = .{ .Type_Of = arguments[0] },
                .byte_size = 0,
                .alignment = .Byte,
                .stages = Ast.default_stages_none,
            };
            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .{ .Type = .{
                    .position = token.position,
                    .data = data,
                    .symbol = null,
                } },
                .typ = Ast.void_type,
                .flags = .{},
            };
            return expression;
        },

        .Byte_Size_Of => {
            var arguments: [1]*Ast.Expression = undefined;
            parse_fixed_size_expression_list(parser, &arguments);

            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .{ .Byte_Size_Of = arguments[0] },
                .typ = Ast.void_type,
                .flags = .{},
            };

            return expression;
        },
        .Alignment_Of => {
            var arguments: [1]*Ast.Expression = undefined;
            parse_fixed_size_expression_list(parser, &arguments);

            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .{ .Alignment_Of = arguments[0] },
                .typ = Ast.void_type,
                .flags = .{},
            };

            return expression;
        },
        .As => {
            var arguments: [2]*Ast.Expression = undefined;
            parse_fixed_size_expression_list(parser, &arguments);

            const typ = expression_to_type(parser, arguments[0]);
            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .{ .As = .{
                    .typ = typ,
                    .expression = arguments[1],
                } },
                .typ = typ,
                .flags = .{},
            };
            return expression;
        },
        .Cast => {
            var arguments: [2]*Ast.Expression = undefined;
            parse_fixed_size_expression_list(parser, &arguments);

            const typ = expression_to_type(parser, arguments[0]);
            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .{ .Cast = .{
                    .typ = typ,
                    .expression = arguments[1],
                } },
                .typ = typ,
                .flags = .{},
            };
            return expression;
        },
        .Integer_Literal => |value| {
            const typ = parser.ast.integer_type_from_u64(value);
            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .{ .Integer = value },
                .typ = typ,
                .flags = .{},
            };
            return expression;
        },
        .Boolean_Literal => |value| {
            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .{ .Boolean = value },
                .typ = Ast.bool_type,
                .flags = .{},
            };
            return expression;
        },
        .Null_Literal => {
            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .Null,
                .typ = Ast.void_pointer_type,
                .flags = .{},
            };
            return expression;
        },
        .Identifier => |name| {
            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .{ .Identifier = .{
                    .name = name,
                    .scope = parser.current_scope,
                } },
                .typ = Ast.void_type,
                .flags = .{},
            };
            return expression;
        },

        .Left_Parenthesis => {
            const expression = parse_expression(parser);
            expression.position = token.position;
            expect(parser, .Right_Parenthesis);
            return expression;
        },

        .Reference => {
            const subexpression = parse_highest_precedence_expression(parser);
            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = subexpression.position,
                .as = .{ .Ref = subexpression },
                .typ = Ast.void_type,
                .flags = .{},
            };
            return expression;
        },

        .Not,
        .Plus,
        .Minus,
        => {
            const subexpression = parse_highest_precedence_expression(parser);
            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = subexpression.position,
                .as = .{ .Unary_Op = .{
                    .subexpression = subexpression,
                    .tag = switch (token.as) {
                        .Not => .Not,
                        .Plus => .Pos,
                        .Minus => .Neg,
                        else => unreachable,
                    },
                } },
                .typ = Ast.void_type,
                .flags = .{},
            };
            return expression;
        },

        .And => {
            const subsubexpression = parse_highest_precedence_expression(parser);
            const subexpression = parser.ast.create(Ast.Expression);
            subexpression.* = .{
                .position = token.position,
                .as = .{ .Ref = subsubexpression },
                .typ = Ast.void_type,
                .flags = .{},
            };
            subexpression.position.column += 1;
            subexpression.position.offset += 1;
            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .{ .Ref = subexpression },
                .typ = Ast.void_type,
                .flags = .{},
            };
            return expression;
        },

        .If => {
            const condition = parse_expression(parser);
            if (peek(parser) == .Then) {
                advance(parser);
            }
            const true_branch = parse_expression(parser);
            expect(parser, .Else);
            const false_branch = parse_expression(parser);

            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .{ .If = .{
                    .condition = condition,
                    .true_branch = true_branch,
                    .false_branch = false_branch,
                } },
                .typ = Ast.void_type,
                .flags = .{},
            };

            return expression;
        },
        else => {
            parser.c.report_error(token.position, "token doesn't start an expression or a type", .{});
            exit(1);
        },
    }
}

fn parse_highest_precedence_expression(parser: *Parser) *Ast.Expression {
    var base = parse_base_expression(parser);

    while (true) {
        switch (peek(parser)) {
            .Left_Parenthesis => {
                const args = parse_expression_list(parser);

                const new_base = parser.ast.create(Ast.Expression);
                new_base.* = .{
                    .position = base.position,
                    .as = .{ .Call = .{
                        .subexpression = base,
                        .args = args,
                    } },
                    .typ = Ast.void_type,
                    .flags = .{},
                };
                base = new_base;
            },
            .Dereference => {
                advance(parser);

                const new_base = parser.ast.create(Ast.Expression);
                new_base.* = .{
                    .position = base.position,
                    .as = .{ .Deref = base },
                    .typ = Ast.void_type,
                    .flags = .{},
                };
                base = new_base;
            },
            .Dot => {
                advance(parser);

                const id = grab(parser);
                expect(parser, .Identifier);

                const field = parser.ast.create(Ast.Expression);
                field.* = .{
                    .position = id.position,
                    .as = .{ .Identifier = .{
                        .name = id.as.Identifier,
                        .scope = parser.current_scope,
                    } },
                    .typ = Ast.void_type,
                    .flags = .{},
                };
                const new_base = parser.ast.create(Ast.Expression);
                new_base.* = .{
                    .position = base.position,
                    .as = .{ .Field = .{
                        .subexpression = base,
                        .field = field,
                    } },
                    .typ = Ast.void_type,
                    .flags = .{},
                };
                base = new_base;
            },
            .Left_Bracket => {
                advance(parser);
                const index = parse_expression(parser);
                expect(parser, .Right_Bracket);

                const new_base = parser.ast.create(Ast.Expression);
                new_base.* = .{
                    .position = base.position,
                    .as = .{ .Subscript = .{
                        .subexpression = base,
                        .index = index,
                    } },
                    .typ = Ast.void_type,
                    .flags = .{},
                };
                base = new_base;
            },
            else => break,
        }
    }

    return base;
}

const PrecedenceType = i32;
const lowest_precedence: PrecedenceType = std.math.minInt(PrecedenceType) + 1;
const highest_precedence: PrecedenceType = std.math.maxInt(PrecedenceType) - 1;
const invalid_precedence = lowest_precedence - 1;

fn precedence_of_binary_operator(operator: Token.Tag) PrecedenceType {
    return switch (operator) {
        .Or => 0,
        .And => 1,
        .Double_Equal,
        .Not_Equal,
        => 2,
        .Less_Than,
        .Less_Equal,
        .Greater_Than,
        .Greater_Equal,
        => 3,
        .Plus,
        .Minus,
        => 4,
        .Asterisk,
        .Slash,
        .Percent_Sign,
        => 5,
        else => invalid_precedence,
    };
}

fn parse_binary_expression(parser: *Parser, min_precedence: PrecedenceType) *Ast.Expression {
    var lhs = parse_highest_precedence_expression(parser);
    var operator = peek(parser);
    var previous_precedence = highest_precedence;
    var current_precedence = precedence_of_binary_operator(operator);

    while (current_precedence < previous_precedence and current_precedence >= min_precedence) {
        const new_precedence = while (true) {
            const position = grab(parser).position;
            advance(parser);

            const rhs = parse_binary_expression(parser, current_precedence + 1);
            const new_lhs = parser.ast.create(Ast.Expression);
            new_lhs.* = .{
                .position = lhs.position,
                .as = .{ .Binary_Op = .{
                    .position = position,
                    .lhs = lhs,
                    .rhs = rhs,
                    .tag = switch (operator) {
                        .Or => .Or,
                        .And => .And,
                        .Double_Equal => .Eq,
                        .Not_Equal => .Neq,
                        .Less_Than => .Lt,
                        .Less_Equal => .Leq,
                        .Greater_Than => .Gt,
                        .Greater_Equal => .Geq,
                        .Plus => .Add,
                        .Minus => .Sub,
                        .Asterisk => .Mul,
                        .Slash => .Div,
                        .Percent_Sign => .Mod,
                        else => unreachable,
                    },
                } },
                .typ = Ast.void_type,
                .flags = .{},
            };
            lhs = new_lhs;

            operator = peek(parser);
            const new_precedence = precedence_of_binary_operator(operator);

            if (current_precedence != new_precedence) break new_precedence;
        };

        previous_precedence = current_precedence;
        current_precedence = new_precedence;
    }

    return lhs;
}

fn parse_expression(parser: *Parser) *Ast.Expression {
    return parse_binary_expression(parser, lowest_precedence);
}

fn parse_fixed_size_expression_list(parser: *Parser, dst: []*Ast.Expression) void {
    const position = grab(parser).position;

    expect(parser, .Left_Parenthesis);

    var count: usize = 0;

    var tt = peek(parser);
    while (tt != .End_Of_File and tt != .Right_Parenthesis) {
        const expression = parse_expression(parser);

        if (count < dst.len) {
            dst[count] = expression;
        }

        count += 1;

        tt = peek(parser);
        if (tt != .End_Of_File and tt != .Right_Parenthesis) {
            expect(parser, .Comma);
            tt = peek(parser);
        }
    }

    expect(parser, .Right_Parenthesis);

    if (count != dst.len) {
        parser.c.report_fatal_error(position, "expected {} arguments, but got {}", .{ dst.len, count });
    }
}

fn parse_expression_list(parser: *Parser) Ast.ExpressionList {
    expect(parser, .Left_Parenthesis);

    var list = Ast.ExpressionList{};

    var tt = peek(parser);
    while (tt != .End_Of_File and tt != .Right_Parenthesis) {
        const expression_node = parser.ast.create(Ast.ExpressionListNode);
        expression_node.* = expression_node: {
            const lhs = parse_expression(parser);
            if (peek(parser) == .Equal) {
                advance(parser);
                const rhs = parse_expression(parser);

                break :expression_node .{ .Designator = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } };
            }
            break :expression_node .{ .Expression = lhs };
        };

        {
            const node = parser.ast.create(Ast.ExpressionList.Node);
            node.* = .{
                .data = expression_node,
            };
            list.append(node);
        }

        tt = peek(parser);
        if (tt != .End_Of_File and tt != .Right_Parenthesis) {
            expect(parser, .Comma);
            tt = peek(parser);
        }
    }

    expect(parser, .Right_Parenthesis);

    return list;
}

fn parse_statement_switch(parser: *Parser) Ast.Statement.Switch.CaseList {
    expect(parser, .Left_Brace);
    push_scope(parser, .Statement_Block);

    var list = Ast.Statement.Switch.CaseList{};

    var tt = peek(parser);
    while (tt != .End_Of_File and tt != .Right_Brace) {
        const case = parse_switch_case(parser);

        {
            const node = parser.ast.create(Ast.Statement.Switch.CaseList.Node);
            node.* = .{
                .data = case,
            };
            list.append(node);
        }

        tt = peek(parser);
    }

    expect(parser, .Right_Brace);
    pop_scope(parser);

    return list;
}

fn parse_switch_case(parser: *Parser) *Ast.Statement.Switch.Case {
    switch (peek(parser)) {
        .Case => {
            advance(parser);
            const value = parse_expression(parser);
            if (peek(parser) == .Then) {
                advance(parser);
            }
            const subcase = parse_switch_case(parser);

            const case = parser.ast.create(Ast.Statement.Switch.Case);
            case.* = .{ .Case = .{
                .value = value,
                .subcase = subcase,
            } };

            return case;
        },
        else => {
            const statement = parse_statement(parser);

            const case = parser.ast.create(Ast.Statement.Switch.Case);
            case.* = .{ .Statement = statement };

            return case;
        },
    }
}

fn parse_statement(parser: *Parser) *Ast.Statement {
    const tok = grab(parser);
    advance(parser);

    switch (tok.as) {
        .Print => {
            const expression = parse_expression(parser);
            expect(parser, .Semicolon);

            const statement = parser.ast.create(Ast.Statement);
            statement.* = .{
                .position = tok.position,
                .as = .{ .Print = expression },
            };

            return statement;
        },
        .If => {
            const condition = parse_expression(parser);
            if (peek(parser) == .Then) {
                advance(parser);
            }
            const true_branch = parse_statement(parser);
            var false_branch: ?*Ast.Statement = null;
            if (peek(parser) == .Else) {
                advance(parser);
                false_branch = parse_statement(parser);
            }

            const statement = parser.ast.create(Ast.Statement);
            statement.* = .{
                .position = tok.position,
                .as = .{ .If = .{
                    .condition = condition,
                    .true_branch = true_branch,
                    .false_branch = false_branch,
                } },
            };

            return statement;
        },
        .While => {
            const condition = parse_expression(parser);
            if (peek(parser) == .Do) {
                advance(parser);
            }
            const body = parse_statement(parser);

            const statement = parser.ast.create(Ast.Statement);
            statement.* = .{
                .position = tok.position,
                .as = .{ .While = .{
                    .condition = condition,
                    .body = body,
                } },
            };

            return statement;
        },
        .Do => {
            const body = parse_statement(parser);
            expect(parser, .While);
            const condition = parse_expression(parser);
            expect(parser, .Semicolon);

            const statement = parser.ast.create(Ast.Statement);
            statement.* = .{
                .position = tok.position,
                .as = .{ .Do_While = .{
                    .condition = condition,
                    .body = body,
                } },
            };

            return statement;
        },
        .Break => {
            expect(parser, .Semicolon);

            const statement = parser.ast.create(Ast.Statement);
            statement.* = .{
                .position = tok.position,
                .as = .Break,
            };

            return statement;
        },
        .Continue => {
            expect(parser, .Semicolon);

            const statement = parser.ast.create(Ast.Statement);
            statement.* = .{
                .position = tok.position,
                .as = .Continue,
            };

            return statement;
        },
        .Switch => {
            const condition = parse_expression(parser);
            const cases = parse_statement_switch(parser);
            var default_case: ?*Ast.Statement = null;
            if (peek(parser) == .Else) {
                advance(parser);
                default_case = parse_statement(parser);
            }

            const statement = parser.ast.create(Ast.Statement);
            statement.* = .{
                .position = tok.position,
                .as = .{ .Switch = .{
                    .condition = condition,
                    .cases = cases,
                    .default_case = default_case,
                } },
            };

            return statement;
        },
        .Return => {
            if (peek(parser) == .Semicolon) {
                advance(parser);

                const statement = parser.ast.create(Ast.Statement);
                statement.* = .{
                    .position = tok.position,
                    .as = .{ .Return = null },
                };

                return statement;
            }

            const expression = parse_expression(parser);
            expect(parser, .Semicolon);

            const statement = parser.ast.create(Ast.Statement);
            statement.* = .{
                .position = tok.position,
                .as = .{ .Return = expression },
            };

            return statement;
        },
        .Left_Brace => {
            putback(parser, tok);

            push_scope(parser, .Statement_Block);
            const block = parse_statement_list(parser);
            pop_scope(parser);

            const statement = parser.ast.create(Ast.Statement);
            statement.* = .{
                .position = tok.position,
                .as = .{ .Block = block },
            };

            return statement;
        },
        else => {
            putback(parser, tok);

            if (try_parse_symbol(parser)) |symbol| {
                maybe_eat_semicolon(parser, symbol);

                const statement = parser.ast.create(Ast.Statement);
                statement.* = .{
                    .position = tok.position,
                    .as = .{ .Symbol = symbol },
                };
                return statement;
            } else {
                const expression = parse_expression(parser);

                switch (peek(parser)) {
                    .Equal => {
                        advance(parser);
                        const rhs = parse_expression(parser);
                        expect(parser, .Semicolon);

                        const statement = parser.ast.create(Ast.Statement);
                        statement.* = .{
                            .position = tok.position,
                            .as = .{ .Assign = .{
                                .lhs = expression,
                                .rhs = rhs,
                            } },
                        };

                        return statement;
                    },
                    else => {
                        expect(parser, .Semicolon);
                        const statement = parser.ast.create(Ast.Statement);
                        statement.* = .{
                            .position = tok.position,
                            .as = .{ .Expression = expression },
                        };
                        return statement;
                    },
                }
            }
        },
    }
}

fn parse_statement_list(parser: *Parser) Ast.StatementList {
    expect(parser, .Left_Brace);

    var list = Ast.StatementList{};

    var tt = peek(parser);
    while (tt != .End_Of_File and tt != .Right_Brace) {
        const statement = parse_statement(parser);

        {
            const node = parser.ast.create(Ast.StatementList.Node);
            node.* = .{
                .data = statement,
            };
            list.append(node);
        }

        tt = peek(parser);
    }

    expect(parser, .Right_Brace);

    return list;
}

fn insert_symbol(parser: *Parser, id: Token) *Ast.Symbol {
    const key = Ast.Symbol.Key{
        .name = id.as.Identifier,
        .scope = parser.current_scope,
    };
    const insert_result = parser.ast.symbol_table.insert(key);

    if (insert_result.found_existing) {
        parser.c.report_error(id.position, "symbol '{s}' is defined already", .{key.name});
        parser.c.report_note(insert_result.value_ptr.*.position, "first defined here", .{});
        exit(1);
    }

    const symbol = parser.ast.create(Ast.Symbol);
    symbol.* = .{
        .position = id.position,
        .as = undefined,
        .key = key,
        .typechecking = .None,
    };
    insert_result.value_ptr.* = symbol;

    return symbol;
}

fn maybe_eat_symbol_declaration(parser: *Parser) ?Token {
    if (peek(parser) == .Identifier and peek_at(parser, 1) == .Colon) {
        const id = grab(parser);
        advance_many(parser, 2);
        return id;
    }

    return null;
}

fn eat_symbol_declaration(parser: *Parser) Token {
    if (maybe_eat_symbol_declaration(parser)) |id| {
        return id;
    }

    parser.c.report_fatal_error(grab(parser).position, "expected identifier declaration", .{});
}

fn try_parse_variable(parser: *Parser) ?*Ast.Symbol {
    var lookahead: TokenIndex = 0;
    var has_attributes = false;

    if (peek_at(parser, lookahead) == .Attributes) {
        lookahead += 1;
        has_attributes = true;
    }

    switch (peek_at(parser, lookahead)) {
        .Identifier => switch (peek_at(parser, lookahead + 1)) {
            .Colon => {
                var attributes = Token.Attributes{
                    .is_const = false,
                    .is_static = false,
                };

                if (has_attributes) {
                    attributes = grab(parser).as.Attributes;
                    advance(parser);
                }

                const id = eat_symbol_declaration(parser);

                var has_type: ?*Ast.Type = null;
                var has_value: ?*Ast.Expression = null;

                if (peek(parser) == .Equal) {
                    advance(parser);
                    has_value = parse_expression(parser);
                } else {
                    has_type = parse_type(parser);

                    if (peek(parser) == .Equal) {
                        advance(parser);
                        has_value = parse_expression(parser);
                    }
                }

                if (attributes.is_const and attributes.is_static) {
                    parser.c.report_error(id.position, "variable can't be static and constant at the same time", .{});
                }

                const symbol = insert_symbol(parser, id);
                symbol.as = if (attributes.is_const or attributes.is_static) as: {
                    break :as .{ .Variable = .{
                        .attributes = .{
                            .is_const = attributes.is_const,
                            .is_static = attributes.is_static,
                        },
                        .typ = has_type,
                        .value = has_value,
                        .storage = null,
                    } };
                } else as: {
                    switch (symbol.key.scope.tag) {
                        .Global => {
                            attributes.is_static = !attributes.is_const;

                            if (has_value == null) {
                                parser.c.report_error(symbol.position, "expected initializer for a global variable", .{});
                            }

                            break :as .{ .Variable = .{
                                .attributes = .{
                                    .is_const = attributes.is_const,
                                    .is_static = attributes.is_static,
                                },
                                .typ = has_type,
                                .value = has_value,
                                .storage = null,
                            } };
                        },
                        .Structure => {
                            if (has_type == null) {
                                parser.c.report_error(symbol.position, "expected type for a structure field", .{});
                            }

                            break :as .{ .Struct_Field = .{
                                .typ = has_type.?,
                                .value = has_value,
                                .offset = 0,
                            } };
                        },
                        .Union => {
                            if (has_type == null) {
                                parser.c.report_error(symbol.position, "expected type for a union field", .{});
                            }

                            break :as .{ .Union_Field = .{
                                .typ = has_type.?,
                                .value = has_value,
                                .offset = 0,
                            } };
                        },
                        .Enumerator => {
                            if (has_type) |typ| {
                                parser.c.report_error(typ.position, "unexpected type for an enumerator value", .{});
                            }

                            break :as .{ .Enum_Field = .{
                                .value = has_value,
                                .computed_value = 0,
                            } };
                        },
                        .Procedure,
                        .Statement_Block,
                        => {
                            break :as .{ .Variable = .{
                                .attributes = .{
                                    .is_const = attributes.is_const,
                                    .is_static = attributes.is_static,
                                },
                                .typ = has_type,
                                .value = has_value,
                                .storage = null,
                            } };
                        },
                    }
                };

                return symbol;
            },
            else => if (parser.current_scope.tag == .Enumerator) {
                var maybe_id = grab(parser);

                if (has_attributes) {
                    parser.c.report_error(maybe_id.position, "unexpected attributes for an enumerator value", .{});
                    advance(parser);
                }

                maybe_id = grab(parser);
                advance(parser);

                const symbol = insert_symbol(parser, maybe_id);
                symbol.as = .{ .Enum_Field = .{
                    .value = null,
                    .computed_value = 0,
                } };

                return symbol;
            } else return null,
        },
        else => return null,
    }
}

fn parse_variable(parser: *Parser) *Ast.Symbol {
    const has_symbol = try_parse_variable(parser);

    if (has_symbol) |symbol| return symbol;

    parser.c.report_fatal_error(grab(parser).position, "token doesn't start a variable", .{});
}

fn try_parse_symbol(parser: *Parser) ?*Ast.Symbol {
    switch (peek(parser)) {
        .Alias => {
            advance(parser);

            const id = eat_symbol_declaration(parser);
            const typ = parse_type(parser);

            const symbol = insert_symbol(parser, id);
            symbol.as = .{ .Type = typ };

            return symbol;
        },
        .Struct,
        .Union,
        .Enum,
        => {
            const typ, const has_id = parse_structure_union_or_enumerator(parser);

            if (has_id == null) {
                parser.c.report_fatal_error(typ.position, "expected identifier declaration", .{});
            }

            const symbol = insert_symbol(parser, has_id.?);
            symbol.as = .{ .Type = &typ.as.Type };

            return symbol;
        },
        .Proc => {
            advance(parser);

            const id = eat_symbol_declaration(parser);

            switch (parse_procedure_or_its_type(parser, id.position)) {
                .Procedure => |Procedure| {
                    const symbol = insert_symbol(parser, id);
                    symbol.as = .{ .Procedure = Procedure };

                    {
                        const node = parser.ast.create(Ast.SymbolList.Node);
                        node.* = .{ .data = symbol };
                        parser.ast.local_procedures.append(node);
                    }

                    return symbol;
                },
                .Procedure_Type => |typ| {
                    parser.c.report_fatal_error(typ.position, "expected body for a procedure", .{});
                },
            }
        },
        else => return try_parse_variable(parser),
    }
}

fn parse_symbol(parser: *Parser) *Ast.Symbol {
    const has_symbol = try_parse_symbol(parser);

    if (has_symbol) |symbol| return symbol;

    parser.c.report_fatal_error(grab(parser).position, "token doesn't start a symbol", .{});
}

fn parse_symbol_list(parser: *Parser, fields: *Ast.SymbolList, rest: ?*Ast.SymbolList) void {
    var tt = peek(parser);
    while (tt != .End_Of_File and tt != .Right_Parenthesis and tt != .Right_Brace) {
        const symbol = parse_symbol(parser);
        var expect_comma = false;

        {
            const node = parser.ast.create(Ast.SymbolList.Node);
            node.* = .{
                .data = symbol,
            };

            switch (symbol.as) {
                .Struct_Field,
                .Union_Field,
                .Enum_Field,
                .Parameter,
                => {
                    expect_comma = true;
                    fields.append(node);
                },
                .Variable,
                .Procedure,
                .Type,
                => {
                    rest.?.append(node);
                },
            }
        }

        tt = peek(parser);
        if (expect_comma and tt != .End_Of_File and tt != .Right_Parenthesis and tt != .Right_Brace) {
            expect(parser, .Comma);
            tt = peek(parser);
        }
    }
}

fn parse_top_level(parser: *Parser) void {
    while (peek(parser) != .End_Of_File) {
        const statement = parse_statement(parser);
        switch (statement.as) {
            .Symbol => |symbol| {
                const node = parser.ast.create(Ast.SymbolList.Node);
                node.* = .{
                    .data = symbol,
                };
                parser.ast.globals.append(node);
            },
            else => {
                parser.c.report_error(statement.position, "expected symbol definition", .{});
            },
        }
    }

    std.debug.assert(parser.current_scope == &Ast.global_scope);

    if (parser.c.had_error) {
        exit(1);
    }
}

pub fn parse(c: *Compiler) Ast {
    var parser = Parser{
        .current_scope = &Ast.global_scope,
        .lexer = .{
            .c = c,
        },
        .ast = Ast.init(),
        .c = c,
    };

    parse_top_level(&parser);

    return parser.ast;
}

const exit = nostd.exit;

const FilePosition = Lexer.FilePosition;

const std = @import("std");
const nostd = @import("nostd.zig");
const Compiler = @import("Compiler.zig");
const Lexer = @import("Lexer.zig");
const Ast = @import("Ast.zig");
