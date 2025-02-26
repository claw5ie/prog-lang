current_scope: *Ast.Scope,
lexer: Lexer,
ast: *Ast,
c: *Compiler,

const Parser = @This();

const LOWEST_PREC: i32 = std.math.minInt(i32) + 1;

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

pub fn parse(p: *Parser) void {
    parse_top_level(p);
}

fn parse_top_level(p: *Parser) void {
    while (p.lexer.peek() != .End_Of_File) {
        const stmt = parse_stmt(p);
        switch (stmt.as) {
            .Symbol => |symbol| {
                const node = p.ast.create(Ast.SymbolList.Node);
                node.* = .{
                    .data = symbol,
                };
                p.ast.globals.append(node);
            },
            else => {
                p.c.report_error(stmt.position, "expected symbol definition", .{});
            },
        }
    }

    std.debug.assert(p.current_scope == &Ast.global_scope);

    if (p.c.had_error) {
        exit(1);
    }
}

fn parse_stmt(p: *Parser) *Ast.Stmt {
    const tok = p.lexer.grab();
    p.lexer.advance();

    switch (tok.as) {
        .Print => {
            const expr = parse_expr(p);
            p.lexer.expect(.Semicolon);

            const stmt = p.ast.create(Ast.Stmt);
            stmt.* = .{
                .position = tok.position,
                .as = .{ .Print = expr },
            };

            return stmt;
        },
        .Left_Brace => {
            p.lexer.putback(tok);

            push_scope(p);
            const block = parse_stmt_list(p);
            pop_scope(p);

            const stmt = p.ast.create(Ast.Stmt);
            stmt.* = .{
                .position = tok.position,
                .as = .{ .Block = block },
            };

            return stmt;
        },
        .If => {
            const condition = parse_expr(p);
            if (p.lexer.peek() == .Then) {
                p.lexer.advance();
            }
            const true_branch = parse_stmt(p);
            var false_branch: ?*Ast.Stmt = null;
            if (p.lexer.peek() == .Else) {
                p.lexer.advance();
                false_branch = parse_stmt(p);
            }

            const stmt = p.ast.create(Ast.Stmt);
            stmt.* = .{
                .position = tok.position,
                .as = .{ .If = .{
                    .condition = condition,
                    .true_branch = true_branch,
                    .false_branch = false_branch,
                } },
            };

            return stmt;
        },
        .While => {
            const condition = parse_expr(p);
            if (p.lexer.peek() == .Do) {
                p.lexer.advance();
            }
            const body = parse_stmt(p);

            const stmt = p.ast.create(Ast.Stmt);
            stmt.* = .{
                .position = tok.position,
                .as = .{ .While = .{
                    .condition = condition,
                    .body = body,
                } },
            };

            return stmt;
        },
        .Do => {
            const body = parse_stmt(p);
            p.lexer.expect(.While);
            const condition = parse_expr(p);
            p.lexer.expect(.Semicolon);

            const stmt = p.ast.create(Ast.Stmt);
            stmt.* = .{
                .position = tok.position,
                .as = .{ .Do_While = .{
                    .condition = condition,
                    .body = body,
                } },
            };

            return stmt;
        },
        .Break => {
            p.lexer.expect(.Semicolon);

            const stmt = p.ast.create(Ast.Stmt);
            stmt.* = .{
                .position = tok.position,
                .as = .Break,
            };

            return stmt;
        },
        .Continue => {
            p.lexer.expect(.Semicolon);

            const stmt = p.ast.create(Ast.Stmt);
            stmt.* = .{
                .position = tok.position,
                .as = .Continue,
            };

            return stmt;
        },
        .Switch => {
            const condition = parse_expr(p);
            const cases = parse_stmt_switch(p);
            var default_case: ?*Ast.Stmt = null;
            if (p.lexer.peek() == .Else) {
                p.lexer.advance();
                default_case = parse_stmt(p);
            }

            const stmt = p.ast.create(Ast.Stmt);
            stmt.* = .{
                .position = tok.position,
                .as = .{ .Switch = .{
                    .condition = condition,
                    .cases = cases,
                    .default_case = default_case,
                } },
            };

            return stmt;
        },
        .Return => {
            if (p.lexer.peek() == .Semicolon) {
                p.lexer.advance();

                const stmt = p.ast.create(Ast.Stmt);
                stmt.* = .{
                    .position = tok.position,
                    .as = .{ .Return = null },
                };

                return stmt;
            }

            const expr = parse_expr(p);
            p.lexer.expect(.Semicolon);

            const stmt = p.ast.create(Ast.Stmt);
            stmt.* = .{
                .position = tok.position,
                .as = .{ .Return = expr },
            };

            return stmt;
        },
        else => {
            p.lexer.putback(tok);

            const result = try_parse_symbol(p);
            switch (result.tag) {
                .Type,
                .Symbol_Without_Type,
                .Symbol_With_Type,
                .Symbol_With_Type_And_Value,
                .Procedure,
                => {
                    const symbol = insert_symbol(p, result, .Parsing_Statement);

                    if (symbol.as == .Procedure) {
                        const node = p.ast.create(Ast.SymbolList.Node);
                        node.* = .{
                            .data = symbol,
                        };
                        p.ast.local_procedures.append(node);
                    }

                    const stmt = p.ast.create(Ast.Stmt);
                    stmt.* = .{
                        .position = tok.position,
                        .as = .{ .Symbol = symbol },
                    };
                    return stmt;
                },
                .Expr => {
                    if (!result.attributes.is_empty()) {
                        p.c.report_error(tok.position, "unexpected attributes", .{});
                    }

                    const expr = result.pattern;
                    switch (p.lexer.peek()) {
                        .Equal => {
                            p.lexer.advance();
                            const rhs = parse_expr(p);
                            p.lexer.expect(.Semicolon);

                            const stmt = p.ast.create(Ast.Stmt);
                            stmt.* = .{
                                .position = tok.position,
                                .as = .{ .Assign = .{
                                    .lhs = expr,
                                    .rhs = rhs,
                                } },
                            };

                            return stmt;
                        },
                        else => {
                            p.lexer.expect(.Semicolon);
                            const stmt = p.ast.create(Ast.Stmt);
                            stmt.* = .{
                                .position = tok.position,
                                .as = .{ .Expr = expr },
                            };
                            return stmt;
                        },
                    }
                },
            }
        },
    }
}

fn parse_stmt_switch(p: *Parser) Ast.Stmt.Switch.CaseList {
    p.lexer.expect(.Left_Brace);
    push_scope(p);

    var list = Ast.Stmt.Switch.CaseList{};

    var tt = p.lexer.peek();
    while (tt != .End_Of_File and tt != .Right_Brace) {
        const case = parse_switch_case(p);

        {
            const node = p.ast.create(Ast.Stmt.Switch.CaseList.Node);
            node.* = .{
                .data = case,
            };
            list.append(node);
        }

        tt = p.lexer.peek();
    }

    p.lexer.expect(.Right_Brace);
    pop_scope(p);

    return list;
}

fn parse_switch_case(p: *Parser) *Ast.Stmt.Switch.Case {
    switch (p.lexer.peek()) {
        .Case => {
            p.lexer.advance();
            const value = parse_expr(p);
            if (p.lexer.peek() == .Then) {
                p.lexer.advance();
            }
            const subcase = parse_switch_case(p);

            const case = p.ast.create(Ast.Stmt.Switch.Case);
            case.* = .{ .Case = .{
                .value = value,
                .subcase = subcase,
            } };

            return case;
        },
        else => {
            const stmt = parse_stmt(p);

            const case = p.ast.create(Ast.Stmt.Switch.Case);
            case.* = .{ .Stmt = stmt };

            return case;
        },
    }
}

fn parse_stmt_list(p: *Parser) Ast.StmtList {
    p.lexer.expect(.Left_Brace);

    var list = Ast.StmtList{};

    var tt = p.lexer.peek();
    while (tt != .End_Of_File and tt != .Right_Brace) {
        const stmt = parse_stmt(p);

        {
            const node = p.ast.create(Ast.StmtList.Node);
            node.* = .{
                .data = stmt,
            };
            list.append(node);
        }

        tt = p.lexer.peek();
    }

    p.lexer.expect(.Right_Brace);

    return list;
}

fn try_parse_symbol(p: *Parser) ParseSymbolResult {
    const position = p.lexer.grab().position;
    var attributes = Token.Attributes{};

    if (p.lexer.peek() == .Attributes) {
        attributes = p.lexer.grab().as.Attributes;
        p.lexer.advance();
    }

    if (attributes.is_const and attributes.is_static) {
        p.c.report_error(position, "'#const' and '#static' are mutually exclusive", .{});
        exit(1);
    }

    var is_type = false;

    if (p.lexer.peek() == .Alias) {
        p.lexer.advance();
        is_type = true;
    }

    const pattern = parse_expr(p);
    var typ: ?*Ast.Type = null;
    var value: ?*Ast.Expr = null;
    var block: Ast.StmtList = .{};
    var tag: ParseSymbolResult.Tag = .Expr;

    switch (p.lexer.peek()) {
        .Colon => {
            if (p.lexer.peek_at(1) == .Equal) {
                p.lexer.advance();
                tag = .Symbol_Without_Type;
                p.lexer.advance();
                value = parse_expr(p);
            } else {
                tag = .Symbol_With_Type;
                p.lexer.advance();
                typ = parse_type(p);
                switch (p.lexer.peek()) {
                    .Left_Brace => {
                        switch (typ.?.data.as) {
                            .Proc => |Proc| {
                                tag = .Procedure;
                                const old_current_scope = p.current_scope;

                                p.current_scope = Proc.scope;
                                block = parse_stmt_list(p);
                                p.current_scope = old_current_scope;
                            },
                            else => {
                                p.c.report_error(p.lexer.grab().position, "unexpected procedure body", .{});
                                exit(1);
                            },
                        }
                    },
                    .Equal => {
                        tag = .Symbol_With_Type_And_Value;
                        p.lexer.advance();
                        value = parse_expr(p);
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
                p.c.report_error(pattern.position, "missing type after expression", .{});
                exit(1);
            },
            .Symbol_With_Type_And_Value => {
                p.c.report_error(value.?.position, "value expression", .{});
                exit(1);
            },
            .Procedure => {
                p.c.report_error(value.?.position, "expected type definition, not procedure", .{});
                exit(1);
            },
            .Expr => {
                p.c.report_error(pattern.position, "missing type after expression", .{});
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

fn parse_symbol(p: *Parser, how_to_parse: HowToParseSymbol) *Ast.Symbol {
    const result = try_parse_symbol(p);
    const symbol = insert_symbol(p, result, how_to_parse);
    return symbol;
}

fn parse_symbol_list(p: *Parser, fields: *Ast.SymbolList, rest: ?*Ast.SymbolList, how_to_parse: HowToParseSymbol) void {
    var tt = p.lexer.peek();
    while (tt != .End_Of_File and tt != .Right_Parenthesis and tt != .Right_Brace) {
        const symbol = parse_symbol(p, how_to_parse);
        var expect_comma = false;

        {
            const node = p.ast.create(Ast.SymbolList.Node);
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

        tt = p.lexer.peek();
        if (expect_comma and tt != .End_Of_File and tt != .Right_Parenthesis and tt != .Right_Brace) {
            p.lexer.expect(.Comma);
            tt = p.lexer.peek();
        }
    }
}

fn insert_symbol(p: *Parser, result: ParseSymbolResult, how_to_parse: HowToParseSymbol) *Ast.Symbol {
    const symbol: *Ast.Symbol = symbol: {
        switch (result.pattern.as) {
            .Identifier => |Identifier| {
                const key = Ast.Symbol.Key{
                    .name = Identifier.name,
                    .scope = Identifier.scope,
                };
                const insert_result = p.ast.symbol_table.insert(key);

                if (insert_result.found_existing) {
                    p.c.report_error(result.pattern.position, "symbol '{s}' is defined already", .{key.name});
                    p.c.report_note(insert_result.value_ptr.*.position, "first defined here", .{});
                    exit(1);
                }

                const symbol = p.ast.create(Ast.Symbol);
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
                p.c.report_error(result.pattern.position, "expected identifier", .{});
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
                                p.c.report_error(result.pattern.position, "expected type after pattern", .{});
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
                                p.c.report_error(result.typ.?.position, "unexpected type", .{});
                                exit(1);
                            },
                        }
                    },
                    .Procedure => break :symbol_tag .Procedure,
                    .Expr => {
                        switch (container_type) {
                            .Struct, .Union => {
                                p.c.report_error(result.pattern.position, "expected type after pattern", .{});
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
                        p.c.report_error(result.pattern.position, "unexpected type", .{});
                        exit(1);
                    },
                    .Symbol_Without_Type => {
                        p.c.report_error(result.pattern.position, "expected type after pattern", .{});
                        exit(1);
                    },
                    .Procedure => {
                        p.c.report_error(result.pattern.position, "unexpected procedure", .{});
                        exit(1);
                    },
                    .Expr => {
                        p.c.report_error(result.pattern.position, "unexpected expression", .{});
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
                    .Expr => unreachable,
                }
            },
        }
    };

    symbol.as = as: {
        switch (symbol_tag) {
            .Variable => {
                p.lexer.expect(.Semicolon);

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
                    p.c.report_error(result.pattern.position, "unexpected attributes for parameter", .{});
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
                    p.c.report_error(result.pattern.position, "procedures can't be static", .{});
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
                p.lexer.expect(.Semicolon);

                if (!symbol.attributes.is_empty()) {
                    p.c.report_error(result.pattern.position, "unexpected attributes for a type", .{});
                    exit(1);
                }

                break :as .{ .Type = result.typ.? };
            },
        }
    };

    return symbol;
}

fn parse_type(p: *Parser) *Ast.Type {
    return p.ast.expr_to_type(parse_expr(p));
}

fn parse_expr(p: *Parser) *Ast.Expr {
    return parse_expr_prec(p, LOWEST_PREC);
}

fn parse_expr_prec(p: *Parser, min_prec: i32) *Ast.Expr {
    var lhs = parse_expr_highest_prec(p);
    var op = p.lexer.peek();
    var prev_prec: i32 = std.math.maxInt(i32);
    var curr_prec: i32 = prec_of_op(op);

    while (curr_prec < prev_prec and curr_prec >= min_prec) {
        while (true) {
            const position = p.lexer.grab().position;
            p.lexer.advance();

            const rhs = parse_expr_prec(p, curr_prec + 1);
            const new_lhs = p.ast.create(Ast.Expr);
            new_lhs.* = .{
                .position = lhs.position,
                .as = .{ .Binary_Op = .{
                    .position = position,
                    .lhs = lhs,
                    .rhs = rhs,
                    .tag = token_tag_to_binary_op_tag(op),
                } },
                .typ = Ast.void_type,
                .flags = .{},
                .typechecking = .None,
            };
            lhs = new_lhs;

            op = p.lexer.peek();
            if (curr_prec != prec_of_op(op)) break;
        }

        prev_prec = curr_prec;
        curr_prec = prec_of_op(op);
    }

    return lhs;
}

fn parse_expr_highest_prec(p: *Parser) *Ast.Expr {
    var base: *Ast.Expr = base: {
        const tok = p.lexer.grab();
        p.lexer.advance();

        switch (tok.as) {
            .And => {
                const subsubexpr = parse_expr_highest_prec(p);
                const subexpr = p.ast.create(Ast.Expr);
                subexpr.* = .{
                    .position = tok.position,
                    .as = .{ .Ref = subsubexpr },
                    .typ = Ast.void_type,
                    .flags = .{},
                    .typechecking = .None,
                };
                subexpr.position.column += 1;
                subexpr.position.offset += 1;
                const expr = p.ast.create(Ast.Expr);
                expr.* = .{
                    .position = tok.position,
                    .as = .{ .Ref = subexpr },
                    .typ = Ast.void_type,
                    .flags = .{},
                    .typechecking = .None,
                };
                break :base expr;
            },
            .Plus,
            .Minus,
            .Not,
            => {
                const subexpr = parse_expr_highest_prec(p);
                const expr = p.ast.create(Ast.Expr);
                expr.* = .{
                    .position = subexpr.position,
                    .as = .{ .Unary_Op = .{
                        .subexpr = subexpr,
                        .tag = token_tag_to_unary_op_tag(tok.as),
                    } },
                    .typ = Ast.void_type,
                    .flags = .{},
                    .typechecking = .None,
                };
                break :base expr;
            },
            .Reference => {
                const subexpr = parse_expr_highest_prec(p);
                const expr = p.ast.create(Ast.Expr);
                expr.* = .{
                    .position = subexpr.position,
                    .as = .{ .Ref = subexpr },
                    .typ = Ast.void_type,
                    .flags = .{},
                    .typechecking = .None,
                };
                break :base expr;
            },
            .Left_Parenthesis => {
                const expr = parse_expr(p);
                expr.position = tok.position;
                p.lexer.expect(.Right_Parenthesis);
                break :base expr;
            },
            .If => {
                const condition = parse_expr(p);
                if (p.lexer.peek() == .Then) {
                    p.lexer.advance();
                }
                const true_branch = parse_expr(p);
                p.lexer.expect(.Else);
                const false_branch = parse_expr(p);

                const expr = p.ast.create(Ast.Expr);
                expr.* = .{
                    .position = tok.position,
                    .as = .{ .If = .{
                        .condition = condition,
                        .true_branch = true_branch,
                        .false_branch = false_branch,
                    } },
                    .typ = Ast.void_type,
                    .flags = .{},
                    .typechecking = .None,
                };

                break :base expr;
            },
            .Boolean => |value| {
                const expr = p.ast.create(Ast.Expr);
                expr.* = .{
                    .position = tok.position,
                    .as = .{ .Boolean = value },
                    .typ = Ast.bool_type,
                    .flags = .{},
                    .typechecking = .None,
                };
                break :base expr;
            },
            .Null => {
                const expr = p.ast.create(Ast.Expr);
                expr.* = .{
                    .position = tok.position,
                    .as = .Null,
                    .typ = Ast.void_pointer_type,
                    .flags = .{},
                    .typechecking = .None,
                };
                break :base expr;
            },
            .Identifier => |name| {
                const expr = p.ast.create(Ast.Expr);
                expr.* = .{
                    .position = tok.position,
                    .as = .{ .Identifier = .{
                        .name = name,
                        .scope = p.current_scope,
                    } },
                    .typ = Ast.void_type,
                    .flags = .{},
                    .typechecking = .None,
                };
                break :base expr;
            },
            .Integer => |value| {
                const typ = Ast.integer_type_from_u64(value);
                const expr = p.ast.create(Ast.Expr);
                expr.* = .{
                    .position = tok.position,
                    .as = .{ .Integer = value },
                    .typ = typ,
                    .flags = .{},
                    .typechecking = .None,
                };
                break :base expr;
            },
            .Byte_Size_Of => {
                var exprs: [1]*Ast.Expr = undefined;
                const count = parse_fixed_size_expr_list(p, &exprs);

                switch (count) {
                    1 => {
                        const expr = p.ast.create(Ast.Expr);
                        expr.* = .{
                            .position = tok.position,
                            .as = .{ .Byte_Size_Of = exprs[0] },
                            .typ = Ast.void_type,
                            .flags = .{},
                            .typechecking = .None,
                        };
                        break :base expr;
                    },
                    else => {
                        p.c.report_error(tok.position, "expected 1 argument, but got {}", .{count});
                        exit(1);
                    },
                }
            },
            .Alignment_Of => {
                var exprs: [1]*Ast.Expr = undefined;
                const count = parse_fixed_size_expr_list(p, &exprs);

                switch (count) {
                    1 => {
                        const expr = p.ast.create(Ast.Expr);
                        expr.* = .{
                            .position = tok.position,
                            .as = .{ .Alignment_Of = exprs[0] },
                            .typ = Ast.void_type,
                            .flags = .{},
                            .typechecking = .None,
                        };
                        break :base expr;
                    },
                    else => {
                        p.c.report_error(tok.position, "expected 1 argument, but got {}", .{count});
                        exit(1);
                    },
                }
            },
            .As => {
                var exprs: [2]*Ast.Expr = undefined;
                const count = parse_fixed_size_expr_list(p, &exprs);

                switch (count) {
                    2 => {
                        const typ = p.ast.expr_to_type(exprs[0]);
                        const expr = p.ast.create(Ast.Expr);
                        expr.* = .{
                            .position = tok.position,
                            .as = .{ .As = .{
                                .typ = typ,
                                .expr = exprs[1],
                            } },
                            .typ = typ,
                            .flags = .{},
                            .typechecking = .None,
                        };
                        break :base expr;
                    },
                    else => {
                        p.c.report_error(tok.position, "expected 2 arguments, but got {}", .{count});
                        exit(1);
                    },
                }
            },
            .Cast => {
                var exprs: [2]*Ast.Expr = undefined;
                const count = parse_fixed_size_expr_list(p, &exprs);

                switch (count) {
                    2 => {
                        const typ = p.ast.expr_to_type(exprs[0]);
                        const expr = p.ast.create(Ast.Expr);
                        expr.* = .{
                            .position = tok.position,
                            .as = .{ .Cast = .{
                                .typ = typ,
                                .expr = exprs[1],
                            } },
                            .typ = typ,
                            .flags = .{},
                            .typechecking = .None,
                        };
                        break :base expr;
                    },
                    else => {
                        p.c.report_error(tok.position, "expected 2 arguments, but got {}", .{count});
                        exit(1);
                    },
                }
            },
            .Struct => {
                p.lexer.expect(.Left_Brace);
                push_scope(p);
                const scope = p.current_scope;
                const fields = p.ast.create(Ast.SymbolList);
                const rest = p.ast.create(Ast.SymbolList);
                fields.* = .{};
                rest.* = .{};
                parse_symbol_list(p, fields, rest, .{ .Parsing_Container = .Struct });
                pop_scope(p);
                p.lexer.expect(.Right_Brace);
                const data = p.ast.create(Ast.Type.SharedData);
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
                const expr = p.ast.create(Ast.Expr);
                expr.* = .{
                    .position = tok.position,
                    .as = .{ .Type = .{
                        .position = tok.position,
                        .data = data,
                        .symbol = null,
                    } },
                    .typ = Ast.void_type,
                    .flags = .{},
                    .typechecking = .None,
                };
                {
                    const node = p.ast.create(Ast.TypeList.Node);
                    node.* = .{
                        .data = &expr.as.Type,
                    };
                    p.ast.namespaces.append(node);
                }
                break :base expr;
            },
            .Union => {
                p.lexer.expect(.Left_Brace);
                push_scope(p);
                const scope = p.current_scope;
                const fields = p.ast.create(Ast.SymbolList);
                const rest = p.ast.create(Ast.SymbolList);
                fields.* = .{};
                rest.* = .{};
                parse_symbol_list(p, fields, rest, .{ .Parsing_Container = .Union });
                pop_scope(p);
                p.lexer.expect(.Right_Brace);
                const data = p.ast.create(Ast.Type.SharedData);
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
                const expr = p.ast.create(Ast.Expr);
                expr.* = .{
                    .position = tok.position,
                    .as = .{ .Type = .{
                        .position = tok.position,
                        .data = data,
                        .symbol = null,
                    } },
                    .typ = Ast.void_type,
                    .flags = .{},
                    .typechecking = .None,
                };
                {
                    const node = p.ast.create(Ast.TypeList.Node);
                    node.* = .{
                        .data = &expr.as.Type,
                    };
                    p.ast.namespaces.append(node);
                }
                break :base expr;
            },
            .Enum => {
                p.lexer.expect(.Left_Brace);
                push_scope(p);
                const scope = p.current_scope;
                const fields = p.ast.create(Ast.SymbolList);
                const rest = p.ast.create(Ast.SymbolList);
                fields.* = .{};
                rest.* = .{};
                parse_symbol_list(p, fields, rest, .{ .Parsing_Container = .Enum });
                pop_scope(p);
                p.lexer.expect(.Right_Brace);
                const data = p.ast.create(Ast.Type.SharedData);
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
                const expr = p.ast.create(Ast.Expr);
                expr.* = .{
                    .position = tok.position,
                    .as = .{ .Type = .{
                        .position = tok.position,
                        .data = data,
                        .symbol = null,
                    } },
                    .typ = Ast.void_type,
                    .flags = .{},
                    .typechecking = .None,
                };
                {
                    const node = p.ast.create(Ast.TypeList.Node);
                    node.* = .{
                        .data = &expr.as.Type,
                    };
                    p.ast.namespaces.append(node);
                }
                break :base expr;
            },
            .Proc => {
                p.lexer.expect(.Left_Parenthesis);
                push_scope(p);
                const scope = p.current_scope;
                var params = Ast.SymbolList{};
                parse_symbol_list(p, &params, null, .Parsing_Procedure);
                pop_scope(p);
                p.lexer.expect(.Right_Parenthesis);
                const return_type = parse_type(p);
                const data = p.ast.create(Ast.Type.SharedData);
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
                const expr = p.ast.create(Ast.Expr);
                expr.* = .{
                    .position = tok.position,
                    .as = .{ .Type = .{
                        .position = tok.position,
                        .data = data,
                        .symbol = null,
                    } },
                    .typ = Ast.void_type,
                    .flags = .{},
                    .typechecking = .None,
                };
                break :base expr;
            },
            .Integer_Type => |Integer_Type| {
                const typ = Ast.lookup_integer_type(Integer_Type.bits, Integer_Type.is_signed);
                const expr = p.ast.create(Ast.Expr);
                expr.* = .{
                    .position = tok.position,
                    .as = .{ .Type = .{
                        .position = tok.position,
                        .data = typ.data,
                        .symbol = null,
                    } },
                    .typ = Ast.void_type,
                    .flags = .{},
                    .typechecking = .None,
                };
                break :base expr;
            },
            .Boolean_Type => {
                const expr = p.ast.create(Ast.Expr);
                expr.* = .{
                    .position = tok.position,
                    .as = .{ .Type = .{
                        .position = tok.position,
                        .data = Ast.bool_type.data,
                        .symbol = null,
                    } },
                    .typ = Ast.void_type,
                    .flags = .{},
                    .typechecking = .None,
                };
                break :base expr;
            },
            .Void_Type => {
                const expr = p.ast.create(Ast.Expr);
                expr.* = .{
                    .position = tok.position,
                    .as = .{ .Type = .{
                        .position = tok.position,
                        .data = Ast.void_type.data,
                        .symbol = null,
                    } },
                    .typ = Ast.void_type,
                    .flags = .{},
                    .typechecking = .None,
                };
                break :base expr;
            },
            .Type_Of => {
                var exprs: [1]*Ast.Expr = undefined;
                const count = parse_fixed_size_expr_list(p, &exprs);
                switch (count) {
                    1 => {
                        const data = p.ast.create(Ast.Type.SharedData);
                        data.* = .{
                            .as = .{ .Type_Of = exprs[0] },
                            .byte_size = 0,
                            .alignment = .Byte,
                            .stages = Ast.default_stages_none,
                        };
                        const expr = p.ast.create(Ast.Expr);
                        expr.* = .{
                            .position = tok.position,
                            .as = .{ .Type = .{
                                .position = tok.position,
                                .data = data,
                                .symbol = null,
                            } },
                            .typ = Ast.void_type,
                            .flags = .{},
                            .typechecking = .None,
                        };
                        break :base expr;
                    },
                    else => {
                        p.c.report_error(tok.position, "expected 1 arguments, but got {}", .{count});
                        exit(1);
                    },
                }
            },
            else => {
                p.c.report_error(tok.position, "token doesn't start an expression or a type", .{});
                exit(1);
            },
        }
    };

    while (true) {
        switch (p.lexer.peek()) {
            .Left_Parenthesis => {
                const args = parse_expr_list(p);

                const new_base = p.ast.create(Ast.Expr);
                new_base.* = .{
                    .position = base.position,
                    .as = .{ .Call = .{
                        .subexpr = base,
                        .args = args,
                    } },
                    .typ = Ast.void_type,
                    .flags = .{},
                    .typechecking = .None,
                };
                base = new_base;
            },
            .Left_Bracket => {
                p.lexer.advance();
                const index = parse_expr(p);
                p.lexer.expect(.Right_Bracket);

                const new_base = p.ast.create(Ast.Expr);
                new_base.* = .{
                    .position = base.position,
                    .as = .{ .Subscript = .{
                        .subexpr = base,
                        .index = index,
                    } },
                    .typ = Ast.void_type,
                    .flags = .{},
                    .typechecking = .None,
                };
                base = new_base;
            },
            .Dereference => {
                p.lexer.advance();

                const new_base = p.ast.create(Ast.Expr);
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
                p.lexer.advance();

                const id = p.lexer.grab();
                p.lexer.expect(.Identifier);

                const field = p.ast.create(Ast.Expr);
                field.* = .{
                    .position = id.position,
                    .as = .{ .Identifier = .{
                        .name = id.as.Identifier,
                        .scope = p.current_scope,
                    } },
                    .typ = Ast.void_type,
                    .flags = .{},
                    .typechecking = .None,
                };
                const new_base = p.ast.create(Ast.Expr);
                new_base.* = .{
                    .position = base.position,
                    .as = .{ .Field = .{
                        .subexpr = base,
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

fn parse_fixed_size_expr_list(p: *Parser, dst: []*Ast.Expr) usize {
    p.lexer.expect(.Left_Parenthesis);

    var count: usize = 0;

    var tt = p.lexer.peek();
    while (tt != .End_Of_File and tt != .Right_Parenthesis) {
        const expr = parse_expr(p);

        if (count < dst.len) {
            dst[count] = expr;
        }

        count += 1;

        tt = p.lexer.peek();
        if (tt != .End_Of_File and tt != .Right_Parenthesis) {
            p.lexer.expect(.Comma);
            tt = p.lexer.peek();
        }
    }

    p.lexer.expect(.Right_Parenthesis);

    return count;
}

fn parse_expr_list(p: *Parser) Ast.ExprList {
    p.lexer.expect(.Left_Parenthesis);

    var list = Ast.ExprList{};

    var tt = p.lexer.peek();
    while (tt != .End_Of_File and tt != .Right_Parenthesis) {
        const expr_node = p.ast.create(Ast.ExprListNode);
        expr_node.* = expr_node: {
            const lhs = parse_expr(p);
            if (p.lexer.peek() == .Equal) {
                p.lexer.advance();
                const rhs = parse_expr(p);

                break :expr_node .{ .Designator = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } };
            }
            break :expr_node .{ .Expr = lhs };
        };

        {
            const node = p.ast.create(Ast.ExprList.Node);
            node.* = .{
                .data = expr_node,
            };
            list.append(node);
        }

        tt = p.lexer.peek();
        if (tt != .End_Of_File and tt != .Right_Parenthesis) {
            p.lexer.expect(.Comma);
            tt = p.lexer.peek();
        }
    }

    p.lexer.expect(.Right_Parenthesis);

    return list;
}

fn push_scope(p: *Parser) void {
    const scope = p.ast.create(Ast.Scope);
    scope.* = .{
        .parent = p.current_scope,
    };
    p.current_scope = scope;
}

fn pop_scope(p: *Parser) void {
    p.current_scope = p.current_scope.parent.?;
}

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

fn token_tag_to_binary_op_tag(op: Token.Tag) Ast.Expr.BinaryOp.Tag {
    return switch (op) {
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
    };
}

fn token_tag_to_unary_op_tag(op: Token.Tag) Ast.Expr.UnaryOp.Tag {
    return switch (op) {
        .Plus => .Pos,
        .Minus => .Neg,
        .Not => .Not,
        else => unreachable,
    };
}

const exit = nostd.exit;

const std = @import("std");
const nostd = @import("nostd.zig");
const Compiler = @import("Compiler.zig");
const Lexer = @import("Lexer.zig");
const Ast = @import("Ast.zig");

const Token = Lexer.Token;

const ParseSymbolResult = struct {
    attributes: Token.Attributes,
    pattern: *Ast.Expr,
    typ: ?*Ast.Type,
    value: ?*Ast.Expr,
    block: Ast.StmtList,
    tag: Tag,

    pub const Tag = enum {
        Type,
        Symbol_Without_Type,
        Symbol_With_Type,
        Symbol_With_Type_And_Value,
        Procedure,
        Expr,
    };
};

const HowToParseSymbol = union(enum) {
    Parsing_Container: enum {
        Struct,
        Union,
        Enum,
    },
    Parsing_Procedure: void,
    Parsing_Statement: void,
};
