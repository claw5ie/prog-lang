current_scope: *const Ast.Scope,
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

fn maybe_eat_semicolon(parser: *Parser, symbol: *Ast.Symbol) void {
    switch (symbol.as) {
        .Alias,
        .Structure_Field,
        .Union_Field,
        .Enumerator_Value,
        .Variable,
        => expect(parser, .Semicolon),
        .Structure,
        .Union,
        .Enumerator,
        .Procedure,
        .Parameter,
        => {},
    }
}

fn parse_structure_union_or_enumerator_body(parser: *Parser, tag: Ast.Scope.Tag) Ast.Expression.Structure {
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
                .Structure_Field,
                .Union_Field,
                .Enumerator_Value,
                => fields.append(node),

                .Alias,
                .Structure,
                .Union,
                .Enumerator,
                .Procedure,
                .Variable,
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

const StructureOrUnionOrEnumerator = union(enum) {
    Structure: Ast.Expression.Structure,
    Union: Ast.Expression.Union,
    Enumerator: Ast.Expression.Enumerator,
};

fn parse_structure_union_or_enumerator(parser: *Parser) struct { StructureOrUnionOrEnumerator, ?Token } {
    const token = grab(parser);
    advance(parser);

    switch (token.as) {
        .Struct => {
            const has_id = maybe_eat_symbol_declaration(parser);
            const body = parse_structure_union_or_enumerator_body(parser, .Structure);

            return .{ .{ .Structure = body }, has_id };
        },
        .Union => {
            const has_id = maybe_eat_symbol_declaration(parser);
            const body = parse_structure_union_or_enumerator_body(parser, .Union);

            return .{ .{ .Union = body }, has_id };
        },
        .Enum => {
            const has_id = maybe_eat_symbol_declaration(parser);
            const body = parse_structure_union_or_enumerator_body(parser, .Enumerator);

            return .{ .{ .Enumerator = body }, has_id };
        },
        else => unreachable,
    }
}

const ProcedureOrItsType = union(enum) {
    Procedure_Type: Ast.Expression.ProcedureType,
    Procedure: Ast.Expression.Procedure,
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

                const _Variable = Variable;
                symbol.as = .{ .Parameter = .{
                    .typ = _Variable.typ.?,
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

    const return_type = parse_expression(parser);

    const procedure_type = Ast.Expression.ProcedureType{
        .parameters = parameters,
        .return_type = return_type,
        .scope = parser.current_scope,
    };

    if (peek(parser) == .Left_Brace) {
        const block = parse_statement_list(parser);

        const typ = parser.ast.create(Ast.Expression);
        typ.* = .{
            .position = position,
            .as = .{ .Procedure_Type = procedure_type },
        };

        return .{ .Procedure = .{
            .typ = typ,
            .block = block,
        } };
    }

    return .{ .Procedure_Type = procedure_type };
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
            const result, const has_id = parse_structure_union_or_enumerator(parser);

            if (has_id) |id| {
                parser.c.report_error(id.position, "unexpected identifier declaration for an unnamed type", .{});
            }

            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = switch (result) {
                    .Structure => |Structure| .{ .Structure = Structure },
                    .Union => |Union| .{ .Union = Union },
                    .Enumerator => |Enumerator| .{ .Enumerator = Enumerator },
                },
            };

            return expression;
        },
        .Proc => {
            const result = parse_procedure_or_its_type(parser, token.position);

            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = switch (result) {
                    .Procedure_Type => |Procedure_Type| .{ .Procedure_Type = Procedure_Type },
                    .Procedure => |Procedure| .{ .Procedure = Procedure },
                },
            };

            return expression;
        },
        .Integer_Type => |Integer_Type| {
            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .{ .Integer_Type = Integer_Type },
            };
            return expression;
        },
        .Boolean_Type => {
            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .Boolean_Type,
            };
            return expression;
        },
        .Void_Type => {
            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .Void_Type,
            };
            return expression;
        },
        .Type_Of => {
            var arguments: [1]*Ast.Expression = undefined;
            parse_fixed_size_expression_list(parser, &arguments);

            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .{ .Type_Of = arguments[0] },
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
            };

            return expression;
        },
        .As => {
            var arguments: [2]*Ast.Expression = undefined;
            parse_fixed_size_expression_list(parser, &arguments);

            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .{ .As = .{
                    .typ = arguments[0],
                    .expression = arguments[1],
                } },
            };

            return expression;
        },
        .Cast => {
            var arguments: [2]*Ast.Expression = undefined;
            parse_fixed_size_expression_list(parser, &arguments);

            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .{ .Cast = .{
                    .typ = arguments[0],
                    .expression = arguments[1],
                } },
            };

            return expression;
        },
        .Integer_Literal => |value| {
            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .{ .Integer_Literal = value },
            };
            return expression;
        },
        .Boolean_Literal => |value| {
            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .{ .Boolean_Literal = value },
            };
            return expression;
        },
        .Null_Literal => {
            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .Null_Literal,
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
                .as = .{ .Reference = subexpression },
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
                .as = .{ .Unary_Operator = .{
                    .subexpression = subexpression,
                    .tag = switch (token.as) {
                        .Not => .Not,
                        .Plus => .Plus,
                        .Minus => .Minus,
                        else => unreachable,
                    },
                } },
            };
            return expression;
        },

        .And => {
            const subsubexpression = parse_highest_precedence_expression(parser);
            const subexpression = parser.ast.create(Ast.Expression);
            subexpression.* = .{
                .position = token.position,
                .as = .{ .Reference = subsubexpression },
            };
            subexpression.position.column += 1;
            subexpression.position.offset += 1;
            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .{ .Reference = subexpression },
            };
            return expression;
        },
        else => {
            parser.c.report_error(token.position, "token doesn't start an expression or a type", .{});
            exit(1);
        },
    }
}

const Argument = Ast.Expression.ProcedureCall.Argument;
const ArgumentList = Ast.Expression.ProcedureCall.ArgumentList;

fn parse_argument_list(parser: *Parser) ArgumentList {
    expect(parser, .Left_Parenthesis);

    var arguments = ArgumentList{};

    var tt = peek(parser);
    while (tt != .End_Of_File and tt != .Right_Parenthesis) {
        const argument: Argument = argument: {
            switch (tt) {
                .Dot => {
                    advance(parser);
                    const name = parser.ast.create(Token);
                    name.* = grab(parser);
                    expect(parser, .Identifier);
                    expect(parser, .Equal);
                    const value = parse_expression(parser);

                    break :argument .{ .Field_Designator = .{
                        .name = name,
                        .value = value,
                    } };
                },
                else => {
                    const expression = parse_expression(parser);
                    break :argument .{ .Expression = expression };
                },
            }
        };

        {
            const node = parser.ast.create(ArgumentList.Node);
            node.* = .{ .data = argument };
            arguments.append(node);
        }

        tt = peek(parser);
        if (tt != .End_Of_File and tt != .Right_Parenthesis) {
            expect(parser, .Comma);
            tt = peek(parser);
        }
    }

    expect(parser, .Right_Parenthesis);

    return arguments;
}

fn parse_highest_precedence_expression(parser: *Parser) *Ast.Expression {
    var base = parse_base_expression(parser);

    while (true) {
        switch (peek(parser)) {
            .Left_Parenthesis => {
                const arguments = parse_argument_list(parser);

                const new_base = parser.ast.create(Ast.Expression);
                new_base.* = .{
                    .position = base.position,
                    .as = .{ .Procedure_Call = .{
                        .lhs = base,
                        .arguments = arguments,
                    } },
                };
                base = new_base;
            },
            .Dereference => {
                advance(parser);

                const new_base = parser.ast.create(Ast.Expression);
                new_base.* = .{
                    .position = base.position,
                    .as = .{ .Dereference = base },
                };
                base = new_base;
            },
            .Dot => {
                advance(parser);

                const name = parser.ast.create(Token);
                name.* = grab(parser);
                expect(parser, .Identifier);

                const new_base = parser.ast.create(Ast.Expression);
                new_base.* = .{
                    .position = base.position,
                    .as = .{ .Member_Access = .{
                        .lhs = base,
                        .name = name,
                    } },
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
                        .lhs = base,
                        .index = index,
                    } },
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
                .as = .{ .Binary_Operator = .{
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
        .key = key,
        .as = undefined,
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

                var has_type: ?*Ast.Expression = null;
                var has_value: ?*Ast.Expression = null;

                if (peek(parser) == .Equal) {
                    advance(parser);
                    has_value = parse_expression(parser);
                } else {
                    has_type = parse_expression(parser);

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
                            } };
                        },
                        .Structure => {
                            if (has_type == null) {
                                parser.c.report_error(symbol.position, "expected type for a structure field", .{});
                            }

                            break :as .{ .Structure_Field = .{
                                .typ = has_type.?,
                                .value = has_value,
                            } };
                        },
                        .Union => {
                            if (has_type == null) {
                                parser.c.report_error(symbol.position, "expected type for a union field", .{});
                            }

                            break :as .{ .Union_Field = .{
                                .typ = has_type.?,
                                .value = has_value,
                            } };
                        },
                        .Enumerator => {
                            if (has_type) |typ| {
                                parser.c.report_error(typ.position, "unexpected type for an enumerator value", .{});
                            }

                            break :as .{ .Enumerator_Value = .{
                                .value = has_value,
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
                            } };
                        },
                    }
                };

                return symbol;
            },
            else => if (!has_attributes and parser.current_scope.tag == .Enumerator) {
                const id = grab(parser);
                advance(parser);

                const symbol = insert_symbol(parser, id);
                symbol.as = .{ .Enumerator_Value = .{
                    .value = null,
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
    const token = grab(parser);

    switch (token.as) {
        .Alias => {
            advance(parser);

            const id = eat_symbol_declaration(parser);
            const expression = parse_expression(parser);

            const symbol = insert_symbol(parser, id);
            symbol.as = .{ .Alias = expression };

            return symbol;
        },
        .Struct,
        .Union,
        .Enum,
        => {
            const result, const has_id = parse_structure_union_or_enumerator(parser);

            if (has_id == null) {
                parser.c.report_fatal_error(token.position, "expected identifier declaration", .{});
            }

            const symbol = insert_symbol(parser, has_id.?);
            symbol.as = switch (result) {
                .Structure => |Structure| .{ .Structure = Structure },
                .Union => |Union| .{ .Union = Union },
                .Enumerator => |Enumerator| .{ .Enumerator = Enumerator },
            };

            return symbol;
        },
        .Proc => {
            advance(parser);

            const id = eat_symbol_declaration(parser);

            switch (parse_procedure_or_its_type(parser, id.position)) {
                .Procedure => |Procedure| {
                    const symbol = insert_symbol(parser, id);
                    symbol.as = .{ .Procedure = Procedure };
                    return symbol;
                },
                .Procedure_Type => {
                    parser.c.report_fatal_error(token.position, "expected body for a procedure", .{});
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
                parser.ast.symbols.append(node);
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
        .ast = .{
            .symbols = .{},
            .symbol_table = Ast.SymbolTable.init(nostd.general_allocator),
            .arena = Ast.ArenaAllocator.init(std.heap.page_allocator),
        },
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
