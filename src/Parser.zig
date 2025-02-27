current_scope: *Ast.Scope,
lexer: Lexer,
ast: *Ast,
c: *Compiler,

const Parser = @This();

pub fn init(c: *Compiler, ast: *Ast) Parser {
    return .{
        .current_scope = &Ast.global_scope,
        .lexer = .{
            .c = c,
        },
        .ast = ast,
        .c = c,
    };
}

pub fn deinit(_: *Parser) void {}

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

fn push_scope(parser: *Parser) void {
    const scope = parser.ast.create(Ast.Scope);
    scope.* = .{
        .parent = parser.current_scope,
    };
    parser.current_scope = scope;
}

fn pop_scope(parser: *Parser) void {
    parser.current_scope = parser.current_scope.parent.?;
}

fn parse_type(parser: *Parser) *Ast.Type {
    return parser.ast.expression_to_type(parse_expression(parser));
}

fn parse_base_expression(parser: *Parser) *Ast.Expression {
    const token = grab(parser);
    advance(parser);

    switch (token.as) {
        .And => {
            const subsubexpression = parse_expression_highest_prec(parser);
            const subexpression = parser.ast.create(Ast.Expression);
            subexpression.* = .{
                .position = token.position,
                .as = .{ .Ref = subsubexpression },
                .typ = Ast.void_type,
                .flags = .{},
                .typechecking = .None,
            };
            subexpression.position.column += 1;
            subexpression.position.offset += 1;
            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .{ .Ref = subexpression },
                .typ = Ast.void_type,
                .flags = .{},
                .typechecking = .None,
            };
            return expression;
        },
        .Plus,
        .Minus,
        .Not,
        => {
            const subexpression = parse_expression_highest_prec(parser);
            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = subexpression.position,
                .as = .{ .Unary_Op = .{
                    .subexpression = subexpression,
                    .tag = switch (token.as) {
                        .Plus => .Pos,
                        .Minus => .Neg,
                        .Not => .Not,
                        else => unreachable,
                    },
                } },
                .typ = Ast.void_type,
                .flags = .{},
                .typechecking = .None,
            };
            return expression;
        },
        .Reference => {
            const subexpression = parse_expression_highest_prec(parser);
            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = subexpression.position,
                .as = .{ .Ref = subexpression },
                .typ = Ast.void_type,
                .flags = .{},
                .typechecking = .None,
            };
            return expression;
        },
        .Left_Parenthesis => {
            const expression = parse_expression(parser);
            expression.position = token.position;
            expect(parser, .Right_Parenthesis);
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
                .typechecking = .None,
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
                .typechecking = .None,
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
                .typechecking = .None,
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
                .typechecking = .None,
            };
            return expression;
        },
        .Integer_Literal => |value| {
            const typ = Ast.integer_type_from_u64(value);
            const expression = parser.ast.create(Ast.Expression);
            expression.* = .{
                .position = token.position,
                .as = .{ .Integer = value },
                .typ = typ,
                .flags = .{},
                .typechecking = .None,
            };
            return expression;
        },
        .Byte_Size_Of => {
            var expressions: [1]*Ast.Expression = undefined;
            const count = parse_fixed_size_expression_list(parser, &expressions);

            switch (count) {
                1 => {
                    const expression = parser.ast.create(Ast.Expression);
                    expression.* = .{
                        .position = token.position,
                        .as = .{ .Byte_Size_Of = expressions[0] },
                        .typ = Ast.void_type,
                        .flags = .{},
                        .typechecking = .None,
                    };
                    return expression;
                },
                else => {
                    parser.c.report_error(token.position, "expected 1 argument, but got {}", .{count});
                    exit(1);
                },
            }
        },
        .Alignment_Of => {
            var expressions: [1]*Ast.Expression = undefined;
            const count = parse_fixed_size_expression_list(parser, &expressions);

            switch (count) {
                1 => {
                    const expression = parser.ast.create(Ast.Expression);
                    expression.* = .{
                        .position = token.position,
                        .as = .{ .Alignment_Of = expressions[0] },
                        .typ = Ast.void_type,
                        .flags = .{},
                        .typechecking = .None,
                    };
                    return expression;
                },
                else => {
                    parser.c.report_error(token.position, "expected 1 argument, but got {}", .{count});
                    exit(1);
                },
            }
        },
        .As => {
            var expressions: [2]*Ast.Expression = undefined;
            const count = parse_fixed_size_expression_list(parser, &expressions);

            switch (count) {
                2 => {
                    const typ = parser.ast.expression_to_type(expressions[0]);
                    const expression = parser.ast.create(Ast.Expression);
                    expression.* = .{
                        .position = token.position,
                        .as = .{ .As = .{
                            .typ = typ,
                            .expression = expressions[1],
                        } },
                        .typ = typ,
                        .flags = .{},
                        .typechecking = .None,
                    };
                    return expression;
                },
                else => {
                    parser.c.report_error(token.position, "expected 2 arguments, but got {}", .{count});
                    exit(1);
                },
            }
        },
        .Cast => {
            var expressions: [2]*Ast.Expression = undefined;
            const count = parse_fixed_size_expression_list(parser, &expressions);

            switch (count) {
                2 => {
                    const typ = parser.ast.expression_to_type(expressions[0]);
                    const expression = parser.ast.create(Ast.Expression);
                    expression.* = .{
                        .position = token.position,
                        .as = .{ .Cast = .{
                            .typ = typ,
                            .expression = expressions[1],
                        } },
                        .typ = typ,
                        .flags = .{},
                        .typechecking = .None,
                    };
                    return expression;
                },
                else => {
                    parser.c.report_error(token.position, "expected 2 arguments, but got {}", .{count});
                    exit(1);
                },
            }
        },
        .Struct => {
            expect(parser, .Left_Brace);
            push_scope(parser);
            const scope = parser.current_scope;
            const fields = parser.ast.create(Ast.SymbolList);
            const rest = parser.ast.create(Ast.SymbolList);
            fields.* = .{};
            rest.* = .{};
            parse_symbol_list(parser, fields, rest, .{ .Parsing_Container = .Struct });
            pop_scope(parser);
            expect(parser, .Right_Brace);
            const data = parser.ast.create(Ast.Type.SharedData);
            data.* = .{
                .as = .{ .Struct = .{
                    .fields = fields,
                    .rest = rest,
                    .scope = scope,
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
                .typechecking = .None,
            };
            {
                const node = parser.ast.create(Ast.TypeList.Node);
                node.* = .{
                    .data = &expression.as.Type,
                };
                parser.ast.namespaces.append(node);
            }
            return expression;
        },
        .Union => {
            expect(parser, .Left_Brace);
            push_scope(parser);
            const scope = parser.current_scope;
            const fields = parser.ast.create(Ast.SymbolList);
            const rest = parser.ast.create(Ast.SymbolList);
            fields.* = .{};
            rest.* = .{};
            parse_symbol_list(parser, fields, rest, .{ .Parsing_Container = .Union });
            pop_scope(parser);
            expect(parser, .Right_Brace);
            const data = parser.ast.create(Ast.Type.SharedData);
            data.* = .{
                .as = .{ .Union = .{
                    .fields = fields,
                    .rest = rest,
                    .scope = scope,
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
                .typechecking = .None,
            };
            {
                const node = parser.ast.create(Ast.TypeList.Node);
                node.* = .{
                    .data = &expression.as.Type,
                };
                parser.ast.namespaces.append(node);
            }
            return expression;
        },
        .Enum => {
            expect(parser, .Left_Brace);
            push_scope(parser);
            const scope = parser.current_scope;
            const fields = parser.ast.create(Ast.SymbolList);
            const rest = parser.ast.create(Ast.SymbolList);
            fields.* = .{};
            rest.* = .{};
            parse_symbol_list(parser, fields, rest, .{ .Parsing_Container = .Enum });
            pop_scope(parser);
            expect(parser, .Right_Brace);
            const data = parser.ast.create(Ast.Type.SharedData);
            data.* = .{
                .as = .{ .Enum = .{
                    .fields = fields,
                    .rest = rest,
                    .integer_type = Ast.lookup_integer_type(0, false),
                    .scope = scope,
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
                .typechecking = .None,
            };
            {
                const node = parser.ast.create(Ast.TypeList.Node);
                node.* = .{
                    .data = &expression.as.Type,
                };
                parser.ast.namespaces.append(node);
            }
            return expression;
        },
        .Proc => {
            expect(parser, .Left_Parenthesis);
            push_scope(parser);
            const scope = parser.current_scope;
            var params = Ast.SymbolList{};
            parse_symbol_list(parser, &params, null, .Parsing_Procedure);
            pop_scope(parser);
            expect(parser, .Right_Parenthesis);
            const return_type = parse_type(parser);
            const data = parser.ast.create(Ast.Type.SharedData);
            data.* = .{
                .as = .{ .Proc = .{
                    .params = params,
                    .return_type = return_type,
                    .scope = scope,
                } },
                .byte_size = Ast.pointer_byte_size,
                .alignment = Ast.pointer_alignment,
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
                .typechecking = .None,
            };
            return expression;
        },
        .Integer_Type => |Integer_Type| {
            const typ = Ast.lookup_integer_type(Integer_Type.bits, Integer_Type.is_signed);
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
                .typechecking = .None,
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
                .typechecking = .None,
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
                .typechecking = .None,
            };
            return expression;
        },
        .Type_Of => {
            var expressions: [1]*Ast.Expression = undefined;
            const count = parse_fixed_size_expression_list(parser, &expressions);
            switch (count) {
                1 => {
                    const data = parser.ast.create(Ast.Type.SharedData);
                    data.* = .{
                        .as = .{ .Type_Of = expressions[0] },
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
                        .typechecking = .None,
                    };
                    return expression;
                },
                else => {
                    parser.c.report_error(token.position, "expected 1 arguments, but got {}", .{count});
                    exit(1);
                },
            }
        },
        else => {
            parser.c.report_error(token.position, "token doesn't start an expression or a type", .{});
            exit(1);
        },
    }
}

fn parse_expression_highest_prec(parser: *Parser) *Ast.Expression {
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
                    .typechecking = .None,
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
                    .typechecking = .None,
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
                    .typechecking = .None,
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
                    .typechecking = .None,
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
                    .typechecking = .None,
                };
                base = new_base;
            },
            else => break,
        }
    }

    return base;
}

const LOWEST_PREC: i32 = std.math.minInt(i32) + 1;

fn prec_of_op(op: Token.Tag) i32 {
    return switch (op) {
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
        else => LOWEST_PREC - 1,
    };
}

fn parse_expression_prec(parser: *Parser, min_prec: i32) *Ast.Expression {
    var lhs = parse_expression_highest_prec(parser);
    var op = peek(parser);
    var prev_prec: i32 = std.math.maxInt(i32);
    var curr_prec: i32 = prec_of_op(op);

    while (curr_prec < prev_prec and curr_prec >= min_prec) {
        while (true) {
            const position = grab(parser).position;
            advance(parser);

            const rhs = parse_expression_prec(parser, curr_prec + 1);
            const new_lhs = parser.ast.create(Ast.Expression);
            new_lhs.* = .{
                .position = lhs.position,
                .as = .{ .Binary_Op = .{
                    .position = position,
                    .lhs = lhs,
                    .rhs = rhs,
                    .tag = switch (op) {
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
                .typechecking = .None,
            };
            lhs = new_lhs;

            op = peek(parser);
            if (curr_prec != prec_of_op(op)) break;
        }

        prev_prec = curr_prec;
        curr_prec = prec_of_op(op);
    }

    return lhs;
}

fn parse_expression(parser: *Parser) *Ast.Expression {
    return parse_expression_prec(parser, LOWEST_PREC);
}

fn parse_fixed_size_expression_list(parser: *Parser, dst: []*Ast.Expression) usize {
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

    return count;
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
    push_scope(parser);

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
        .Left_Brace => {
            putback(parser, tok);

            push_scope(parser);
            const block = parse_statement_list(parser);
            pop_scope(parser);

            const statement = parser.ast.create(Ast.Statement);
            statement.* = .{
                .position = tok.position,
                .as = .{ .Block = block },
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
        else => {
            putback(parser, tok);

            const result = try_parse_symbol(parser);
            switch (result.tag) {
                .Type,
                .Symbol_Without_Type,
                .Symbol_With_Type,
                .Symbol_With_Type_And_Value,
                .Procedure,
                => {
                    const symbol = insert_symbol(parser, result, .Parsing_Statement);

                    if (symbol.as == .Procedure) {
                        const node = parser.ast.create(Ast.SymbolList.Node);
                        node.* = .{
                            .data = symbol,
                        };
                        parser.ast.local_procedures.append(node);
                    }

                    const statement = parser.ast.create(Ast.Statement);
                    statement.* = .{
                        .position = tok.position,
                        .as = .{ .Symbol = symbol },
                    };
                    return statement;
                },
                .Expression => {
                    if (!result.attributes.is_empty()) {
                        parser.c.report_error(tok.position, "unexpected attributes", .{});
                    }

                    const expression = result.pattern;
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
                },
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

const HowToParseSymbol = union(enum) {
    Parsing_Container: enum {
        Struct,
        Union,
        Enum,
    },
    Parsing_Procedure: void,
    Parsing_Statement: void,
};

fn insert_symbol(parser: *Parser, result: ParseSymbolResult, how_to_parse: HowToParseSymbol) *Ast.Symbol {
    const symbol: *Ast.Symbol = symbol: {
        switch (result.pattern.as) {
            .Identifier => |Identifier| {
                const key = Ast.Symbol.Key{
                    .name = Identifier.name,
                    .scope = Identifier.scope,
                };
                const insert_result = parser.ast.symbol_table.insert(key);

                if (insert_result.found_existing) {
                    parser.c.report_error(result.pattern.position, "symbol '{s}' is defined already", .{key.name});
                    parser.c.report_note(insert_result.value_ptr.*.position, "first defined here", .{});
                    exit(1);
                }

                const symbol = parser.ast.create(Ast.Symbol);
                symbol.* = .{
                    .position = result.pattern.position,
                    .as = undefined,
                    .key = key,
                    .typechecking = .None,
                    .attributes = result.attributes,
                };
                insert_result.value_ptr.* = symbol;

                break :symbol symbol;
            },
            else => {
                parser.c.report_error(result.pattern.position, "expected identifier", .{});
                exit(1);
            },
        }
    };

    const symbol_tag: Ast.Symbol.Tag = symbol_tag: {
        switch (how_to_parse) {
            .Parsing_Container => |container_type| {
                switch (result.tag) {
                    .Type => break :symbol_tag .Type,
                    .Symbol_Without_Type => {
                        if (!result.attributes.is_empty()) {
                            break :symbol_tag .Variable;
                        }

                        switch (container_type) {
                            .Struct, .Union => {
                                parser.c.report_error(result.pattern.position, "expected type after pattern", .{});
                                exit(1);
                            },
                            .Enum => break :symbol_tag .Enum_Field,
                        }
                    },
                    .Symbol_With_Type,
                    .Symbol_With_Type_And_Value,
                    => {
                        if (!result.attributes.is_empty()) {
                            break :symbol_tag .Variable;
                        }

                        switch (container_type) {
                            .Struct => break :symbol_tag .Struct_Field,
                            .Union => break :symbol_tag .Union_Field,
                            .Enum => {
                                parser.c.report_error(result.typ.?.position, "unexpected type", .{});
                                exit(1);
                            },
                        }
                    },
                    .Procedure => break :symbol_tag .Procedure,
                    .Expression => {
                        switch (container_type) {
                            .Struct, .Union => {
                                parser.c.report_error(result.pattern.position, "expected type after pattern", .{});
                                exit(1);
                            },
                            .Enum => break :symbol_tag .Enum_Field,
                        }
                    },
                }
            },
            .Parsing_Procedure => {
                switch (result.tag) {
                    .Symbol_With_Type,
                    .Symbol_With_Type_And_Value,
                    => break :symbol_tag .Parameter,
                    .Type => {
                        parser.c.report_error(result.pattern.position, "unexpected type", .{});
                        exit(1);
                    },
                    .Symbol_Without_Type => {
                        parser.c.report_error(result.pattern.position, "expected type after pattern", .{});
                        exit(1);
                    },
                    .Procedure => {
                        parser.c.report_error(result.pattern.position, "unexpected procedure", .{});
                        exit(1);
                    },
                    .Expression => {
                        parser.c.report_error(result.pattern.position, "unexpected expression", .{});
                        exit(1);
                    },
                }
            },
            .Parsing_Statement => {
                switch (result.tag) {
                    .Type => break :symbol_tag .Type,
                    .Symbol_Without_Type,
                    .Symbol_With_Type,
                    .Symbol_With_Type_And_Value,
                    => break :symbol_tag .Variable,
                    .Procedure => break :symbol_tag .Procedure,
                    .Expression => unreachable,
                }
            },
        }
    };

    symbol.as = as: {
        switch (symbol_tag) {
            .Variable => {
                expect(parser, .Semicolon);

                const is_global = symbol.key.scope == &Ast.global_scope or how_to_parse == .Parsing_Container;
                const is_static = is_global and !symbol.attributes.is_const;

                symbol.attributes.is_static = symbol.attributes.is_static or is_static;
                symbol.attributes.is_global = is_global;
                break :as .{ .Variable = .{
                    .typ = result.typ,
                    .value = result.value,
                    .storage = null,
                } };
            },
            .Parameter => {
                if (!symbol.attributes.is_empty()) {
                    parser.c.report_error(result.pattern.position, "unexpected attributes for parameter", .{});
                    exit(1);
                }

                break :as .{ .Parameter = .{
                    .typ = result.typ.?,
                    .value = result.value,
                    .storage = null,
                } };
            },
            .Procedure => {
                if (symbol.attributes.is_static) {
                    parser.c.report_error(result.pattern.position, "procedures can't be static", .{});
                    exit(1);
                }

                symbol.attributes.is_const = true;
                break :as .{ .Procedure = .{
                    .typ = result.typ.?,
                    .block = result.block,
                    .labels = null,
                } };
            },
            .Struct_Field => {
                std.debug.assert(symbol.attributes.is_empty());
                break :as .{ .Struct_Field = .{
                    .typ = result.typ.?,
                    .value = result.value,
                    .offset = 0,
                } };
            },
            .Union_Field => {
                std.debug.assert(symbol.attributes.is_empty());
                break :as .{ .Union_Field = .{
                    .typ = result.typ.?,
                    .value = result.value,
                    .offset = 0,
                } };
            },
            .Enum_Field => {
                symbol.attributes.is_const = true;
                break :as .{ .Enum_Field = .{
                    .value = result.value,
                    .computed_value = 0,
                } };
            },
            .Type => {
                expect(parser, .Semicolon);

                if (!symbol.attributes.is_empty()) {
                    parser.c.report_error(result.pattern.position, "unexpected attributes for a type", .{});
                    exit(1);
                }

                break :as .{ .Type = result.typ.? };
            },
        }
    };

    return symbol;
}

const ParseSymbolResult = struct {
    attributes: Token.Attributes,
    pattern: *Ast.Expression,
    typ: ?*Ast.Type,
    value: ?*Ast.Expression,
    block: Ast.StatementList,
    tag: Tag,

    pub const Tag = enum {
        Type,
        Symbol_Without_Type,
        Symbol_With_Type,
        Symbol_With_Type_And_Value,
        Procedure,
        Expression,
    };
};

fn try_parse_symbol(parser: *Parser) ParseSymbolResult {
    const position = grab(parser).position;
    var attributes = Token.Attributes{};

    if (peek(parser) == .Attributes) {
        attributes = grab(parser).as.Attributes;
        advance(parser);
    }

    if (attributes.is_const and attributes.is_static) {
        parser.c.report_error(position, "'#const' and '#static' are mutually exclusive", .{});
        exit(1);
    }

    var is_type = false;

    if (peek(parser) == .Alias) {
        advance(parser);
        is_type = true;
    }

    const pattern = parse_expression(parser);
    var typ: ?*Ast.Type = null;
    var value: ?*Ast.Expression = null;
    var block: Ast.StatementList = .{};
    var tag: ParseSymbolResult.Tag = .Expression;

    switch (peek(parser)) {
        .Colon => {
            if (peek_at(parser, 1) == .Equal) {
                advance(parser);
                tag = .Symbol_Without_Type;
                advance(parser);
                value = parse_expression(parser);
            } else {
                tag = .Symbol_With_Type;
                advance(parser);
                typ = parse_type(parser);
                switch (peek(parser)) {
                    .Left_Brace => {
                        switch (typ.?.data.as) {
                            .Proc => |Proc| {
                                tag = .Procedure;
                                const old_current_scope = parser.current_scope;

                                parser.current_scope = Proc.scope;
                                block = parse_statement_list(parser);
                                parser.current_scope = old_current_scope;
                            },
                            else => {
                                parser.c.report_error(grab(parser).position, "unexpected procedure body", .{});
                                exit(1);
                            },
                        }
                    },
                    .Equal => {
                        tag = .Symbol_With_Type_And_Value;
                        advance(parser);
                        value = parse_expression(parser);
                    },
                    else => {},
                }
            }
        },
        else => {},
    }

    if (is_type) {
        switch (tag) {
            .Symbol_With_Type => {
                tag = .Type;
            },
            .Symbol_Without_Type => {
                parser.c.report_error(pattern.position, "missing type after expression", .{});
                exit(1);
            },
            .Symbol_With_Type_And_Value => {
                parser.c.report_error(value.?.position, "value expression", .{});
                exit(1);
            },
            .Procedure => {
                parser.c.report_error(value.?.position, "expected type definition, not procedure", .{});
                exit(1);
            },
            .Expression => {
                parser.c.report_error(pattern.position, "missing type after expression", .{});
                exit(1);
            },
            .Type => unreachable,
        }
    }

    return .{
        .attributes = attributes,
        .pattern = pattern,
        .typ = typ,
        .value = value,
        .block = block,
        .tag = tag,
    };
}

fn parse_symbol(parser: *Parser, how_to_parse: HowToParseSymbol) *Ast.Symbol {
    const result = try_parse_symbol(parser);
    const symbol = insert_symbol(parser, result, how_to_parse);
    return symbol;
}

fn parse_symbol_list(parser: *Parser, fields: *Ast.SymbolList, rest: ?*Ast.SymbolList, how_to_parse: HowToParseSymbol) void {
    var tt = peek(parser);
    while (tt != .End_Of_File and tt != .Right_Parenthesis and tt != .Right_Brace) {
        const symbol = parse_symbol(parser, how_to_parse);
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

pub fn parse(parser: *Parser) void {
    parse_top_level(parser);
}

const exit = nostd.exit;

const std = @import("std");
const nostd = @import("nostd.zig");
const Compiler = @import("Compiler.zig");
const Lexer = @import("Lexer.zig");
const Ast = @import("Ast.zig");
