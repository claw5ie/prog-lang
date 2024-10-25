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
                p.c.report_error(stmt.line_info, "expected symbol definition", .{});
            },
        }
    }

    std.debug.assert(p.current_scope == &Ast.global_scope);

    if (p.c.had_error) {
        Compiler.exit(1);
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
                .line_info = tok.line_info,
                .as = .{ .Print = expr },
            };

            return stmt;
        },
        .Open_Curly => {
            p.lexer.putback(tok);

            push_scope(p);
            const block = parse_stmt_list(p);
            pop_scope(p);

            const stmt = p.ast.create(Ast.Stmt);
            stmt.* = .{
                .line_info = tok.line_info,
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
                .line_info = tok.line_info,
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
                .line_info = tok.line_info,
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
                .line_info = tok.line_info,
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
                .line_info = tok.line_info,
                .as = .Break,
            };

            return stmt;
        },
        .Continue => {
            p.lexer.expect(.Semicolon);

            const stmt = p.ast.create(Ast.Stmt);
            stmt.* = .{
                .line_info = tok.line_info,
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
                .line_info = tok.line_info,
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
                    .line_info = tok.line_info,
                    .as = .{ .Return = null },
                };

                return stmt;
            }

            const expr = parse_expr(p);
            p.lexer.expect(.Semicolon);

            const stmt = p.ast.create(Ast.Stmt);
            stmt.* = .{
                .line_info = tok.line_info,
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

                    const stmt = p.ast.create(Ast.Stmt);
                    stmt.* = .{
                        .line_info = tok.line_info,
                        .as = .{ .Symbol = symbol },
                    };
                    return stmt;
                },
                .Expr => {
                    if (!result.attributes.is_empty()) {
                        p.c.report_error(tok.line_info, "unexpected attributes", .{});
                    }

                    const expr = result.pattern;
                    switch (p.lexer.peek()) {
                        .Equal => {
                            p.lexer.advance();
                            const rhs = parse_expr(p);
                            p.lexer.expect(.Semicolon);

                            const stmt = p.ast.create(Ast.Stmt);
                            stmt.* = .{
                                .line_info = tok.line_info,
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
                                .line_info = tok.line_info,
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
    p.lexer.expect(.Open_Curly);
    push_scope(p);

    var list = Ast.Stmt.Switch.CaseList{};

    var tt = p.lexer.peek();
    while (tt != .End_Of_File and tt != .Close_Curly) {
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

    p.lexer.expect(.Close_Curly);
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
    p.lexer.expect(.Open_Curly);

    var list = Ast.StmtList{};

    var tt = p.lexer.peek();
    while (tt != .End_Of_File and tt != .Close_Curly) {
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

    p.lexer.expect(.Close_Curly);

    return list;
}

fn try_parse_symbol(p: *Parser) ParseSymbolResult {
    const line_info = p.lexer.grab_line_info();
    var attributes = Compiler.Attributes{};

    if (p.lexer.peek() == .Attribute) {
        attributes = p.lexer.grab().as.Attribute;
        p.lexer.advance();
    }

    if (attributes.is_const and attributes.is_static) {
        p.c.report_error(line_info, "'#const' and '#static' are mutually exclusive", .{});
        Compiler.exit(1);
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
        .Colon_Equal => {
            tag = .Symbol_Without_Type;
            p.lexer.advance();
            value = parse_expr(p);
        },
        .Colon => {
            tag = .Symbol_With_Type;
            p.lexer.advance();
            typ = parse_type(p);
            switch (p.lexer.peek()) {
                .Open_Curly => {
                    switch (typ.?.data.as) {
                        .Proc => |Proc| {
                            tag = .Procedure;
                            const old_current_scope = p.current_scope;

                            p.current_scope = Proc.scope;
                            block = parse_stmt_list(p);
                            p.current_scope = old_current_scope;
                        },
                        else => {
                            p.c.report_error(p.lexer.grab_line_info(), "unexpected procedure body", .{});
                            Compiler.exit(1);
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
        },
        else => {},
    }

    if (is_type) {
        switch (tag) {
            .Symbol_With_Type => {
                tag = .Type;
            },
            .Symbol_Without_Type => {
                p.c.report_error(pattern.line_info, "missing type after expression", .{});
                Compiler.exit(1);
            },
            .Symbol_With_Type_And_Value => {
                p.c.report_error(value.?.line_info, "value expression", .{});
                Compiler.exit(1);
            },
            .Procedure => {
                p.c.report_error(value.?.line_info, "expected type definition, not procedure", .{});
                Compiler.exit(1);
            },
            .Expr => {
                p.c.report_error(pattern.line_info, "missing type after expression", .{});
                Compiler.exit(1);
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
    while (tt != .End_Of_File and tt != .Close_Paren and tt != .Close_Curly) {
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
        if (expect_comma and tt != .End_Of_File and tt != .Close_Paren and tt != .Close_Curly) {
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
                const insert_result = p.c.symbol_table.insert(key);

                if (insert_result.found_existing) {
                    p.c.report_error(result.pattern.line_info, "symbol '{s}' is defined already", .{key.name});
                    p.c.report_note(insert_result.value_ptr.*.line_info, "first defined here", .{});
                    Compiler.exit(1);
                }

                const symbol = p.ast.create(Ast.Symbol);
                symbol.* = .{
                    .line_info = result.pattern.line_info,
                    .as = undefined,
                    .key = key,
                    .typechecking = .None,
                    .attributes = result.attributes,
                };
                insert_result.value_ptr.* = symbol;

                break :symbol symbol;
            },
            else => {
                p.c.report_error(result.pattern.line_info, "expected identifier", .{});
                Compiler.exit(1);
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
                                p.c.report_error(result.pattern.line_info, "expected type after pattern", .{});
                                Compiler.exit(1);
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
                                p.c.report_error(result.typ.?.line_info, "unexpected type", .{});
                                Compiler.exit(1);
                            },
                        }
                    },
                    .Procedure => break :symbol_tag .Procedure,
                    .Expr => {
                        switch (container_type) {
                            .Struct, .Union => {
                                p.c.report_error(result.pattern.line_info, "expected type after pattern", .{});
                                Compiler.exit(1);
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
                        p.c.report_error(result.pattern.line_info, "unexpected type", .{});
                        Compiler.exit(1);
                    },
                    .Symbol_Without_Type => {
                        p.c.report_error(result.pattern.line_info, "expected type after pattern", .{});
                        Compiler.exit(1);
                    },
                    .Procedure => {
                        p.c.report_error(result.pattern.line_info, "unexpected procedure", .{});
                        Compiler.exit(1);
                    },
                    .Expr => {
                        p.c.report_error(result.pattern.line_info, "unexpected expression", .{});
                        Compiler.exit(1);
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
                    p.c.report_error(result.pattern.line_info, "unexpected attributes for parameter", .{});
                    Compiler.exit(1);
                }

                break :as .{ .Parameter = .{
                    .typ = result.typ.?,
                    .value = result.value,
                    .storage = null,
                } };
            },
            .Procedure => {
                if (symbol.attributes.is_static) {
                    p.c.report_error(result.pattern.line_info, "procedures can't be static", .{});
                    Compiler.exit(1);
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
                    p.c.report_error(result.pattern.line_info, "unexpected attributes for a type", .{});
                    Compiler.exit(1);
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
            const line_info = p.lexer.grab_line_info();
            p.lexer.advance();

            const rhs = parse_expr_prec(p, curr_prec + 1);
            const new_lhs = p.ast.create(Ast.Expr);
            new_lhs.* = .{
                .line_info = lhs.line_info,
                .as = .{ .Binary_Op = .{
                    .line_info = line_info,
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
                    .line_info = tok.line_info,
                    .as = .{ .Ref = subsubexpr },
                    .typ = Ast.void_type,
                    .flags = .{},
                    .typechecking = .None,
                };
                subexpr.line_info.column += 1;
                subexpr.line_info.offset += 1;
                const expr = p.ast.create(Ast.Expr);
                expr.* = .{
                    .line_info = tok.line_info,
                    .as = .{ .Ref = subexpr },
                    .typ = Ast.void_type,
                    .flags = .{},
                    .typechecking = .None,
                };
                break :base expr;
            },
            .Add, .Sub, .Not => {
                const subexpr = parse_expr_highest_prec(p);
                const expr = p.ast.create(Ast.Expr);
                expr.* = .{
                    .line_info = subexpr.line_info,
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
            .Ref => {
                const subexpr = parse_expr_highest_prec(p);
                const expr = p.ast.create(Ast.Expr);
                expr.* = .{
                    .line_info = subexpr.line_info,
                    .as = .{ .Ref = subexpr },
                    .typ = Ast.void_type,
                    .flags = .{},
                    .typechecking = .None,
                };
                break :base expr;
            },
            .Open_Paren => {
                const expr = parse_expr(p);
                expr.line_info = tok.line_info;
                p.lexer.expect(.Close_Paren);
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
                    .line_info = tok.line_info,
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
                    .line_info = tok.line_info,
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
                    .line_info = tok.line_info,
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
                    .line_info = tok.line_info,
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
                    .line_info = tok.line_info,
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
                            .line_info = tok.line_info,
                            .as = .{ .Byte_Size_Of = exprs[0] },
                            .typ = Ast.void_type,
                            .flags = .{},
                            .typechecking = .None,
                        };
                        break :base expr;
                    },
                    else => {
                        p.c.report_error(tok.line_info, "expected 1 argument, but got {}", .{count});
                        Compiler.exit(1);
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
                            .line_info = tok.line_info,
                            .as = .{ .Alignment_Of = exprs[0] },
                            .typ = Ast.void_type,
                            .flags = .{},
                            .typechecking = .None,
                        };
                        break :base expr;
                    },
                    else => {
                        p.c.report_error(tok.line_info, "expected 1 argument, but got {}", .{count});
                        Compiler.exit(1);
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
                            .line_info = tok.line_info,
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
                        p.c.report_error(tok.line_info, "expected 2 arguments, but got {}", .{count});
                        Compiler.exit(1);
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
                            .line_info = tok.line_info,
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
                        p.c.report_error(tok.line_info, "expected 2 arguments, but got {}", .{count});
                        Compiler.exit(1);
                    },
                }
            },
            .Struct => {
                p.lexer.expect(.Open_Curly);
                push_scope(p);
                const scope = p.current_scope;
                const fields = p.ast.create(Ast.SymbolList);
                const rest = p.ast.create(Ast.SymbolList);
                fields.* = .{};
                rest.* = .{};
                parse_symbol_list(p, fields, rest, .{ .Parsing_Container = .Struct });
                pop_scope(p);
                p.lexer.expect(.Close_Curly);
                const data = p.ast.create(Ast.Type.SharedData);
                data.* = .{
                    .as = .{ .Struct = .{
                        .fields = fields,
                        .rest = rest,
                        .scope = scope,
                    } },
                    .byte_size = 0,
                    .alignment = .BYTE,
                    .stages = Ast.default_stages_none,
                };
                const expr = p.ast.create(Ast.Expr);
                expr.* = .{
                    .line_info = tok.line_info,
                    .as = .{ .Type = .{
                        .line_info = tok.line_info,
                        .data = data,
                        .symbol = null,
                    } },
                    .typ = Ast.void_type,
                    .flags = .{},
                    .typechecking = .None,
                };
                break :base expr;
            },
            .Union => {
                p.lexer.expect(.Open_Curly);
                push_scope(p);
                const scope = p.current_scope;
                const fields = p.ast.create(Ast.SymbolList);
                const rest = p.ast.create(Ast.SymbolList);
                fields.* = .{};
                rest.* = .{};
                parse_symbol_list(p, fields, rest, .{ .Parsing_Container = .Union });
                pop_scope(p);
                p.lexer.expect(.Close_Curly);
                const data = p.ast.create(Ast.Type.SharedData);
                data.* = .{
                    .as = .{ .Union = .{
                        .fields = fields,
                        .rest = rest,
                        .scope = scope,
                    } },
                    .byte_size = 0,
                    .alignment = .BYTE,
                    .stages = Ast.default_stages_none,
                };
                const expr = p.ast.create(Ast.Expr);
                expr.* = .{
                    .line_info = tok.line_info,
                    .as = .{ .Type = .{
                        .line_info = tok.line_info,
                        .data = data,
                        .symbol = null,
                    } },
                    .typ = Ast.void_type,
                    .flags = .{},
                    .typechecking = .None,
                };
                break :base expr;
            },
            .Enum => {
                p.lexer.expect(.Open_Curly);
                push_scope(p);
                const scope = p.current_scope;
                const fields = p.ast.create(Ast.SymbolList);
                const rest = p.ast.create(Ast.SymbolList);
                fields.* = .{};
                rest.* = .{};
                parse_symbol_list(p, fields, rest, .{ .Parsing_Container = .Enum });
                pop_scope(p);
                p.lexer.expect(.Close_Curly);
                const data = p.ast.create(Ast.Type.SharedData);
                data.* = .{
                    .as = .{ .Enum = .{
                        .fields = fields,
                        .rest = rest,
                        .integer_type = Ast.lookup_integer_type(0, false),
                        .scope = scope,
                    } },
                    .byte_size = 0,
                    .alignment = .BYTE,
                    .stages = Ast.default_stages_none,
                };
                const expr = p.ast.create(Ast.Expr);
                expr.* = .{
                    .line_info = tok.line_info,
                    .as = .{ .Type = .{
                        .line_info = tok.line_info,
                        .data = data,
                        .symbol = null,
                    } },
                    .typ = Ast.void_type,
                    .flags = .{},
                    .typechecking = .None,
                };
                break :base expr;
            },
            .Proc => {
                p.lexer.expect(.Open_Paren);
                push_scope(p);
                const scope = p.current_scope;
                var params = Ast.SymbolList{};
                parse_symbol_list(p, &params, null, .Parsing_Procedure);
                pop_scope(p);
                p.lexer.expect(.Close_Paren);
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
                    .line_info = tok.line_info,
                    .as = .{ .Type = .{
                        .line_info = tok.line_info,
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
                    .line_info = tok.line_info,
                    .as = .{ .Type = .{
                        .line_info = tok.line_info,
                        .data = typ.data,
                        .symbol = null,
                    } },
                    .typ = Ast.void_type,
                    .flags = .{},
                    .typechecking = .None,
                };
                break :base expr;
            },
            .Bool_Type => {
                const expr = p.ast.create(Ast.Expr);
                expr.* = .{
                    .line_info = tok.line_info,
                    .as = .{ .Type = .{
                        .line_info = tok.line_info,
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
                    .line_info = tok.line_info,
                    .as = .{ .Type = .{
                        .line_info = tok.line_info,
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
                            .alignment = .BYTE,
                            .stages = Ast.default_stages_none,
                        };
                        const expr = p.ast.create(Ast.Expr);
                        expr.* = .{
                            .line_info = tok.line_info,
                            .as = .{ .Type = .{
                                .line_info = tok.line_info,
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
                        p.c.report_error(tok.line_info, "expected 1 arguments, but got {}", .{count});
                        Compiler.exit(1);
                    },
                }
            },
            else => {
                p.c.report_error(tok.line_info, "token doesn't start an expression or a type", .{});
                Compiler.exit(1);
            },
        }
    };

    while (true) {
        switch (p.lexer.peek()) {
            .Open_Paren => {
                const args = parse_expr_list(p);

                const new_base = p.ast.create(Ast.Expr);
                new_base.* = .{
                    .line_info = base.line_info,
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
            .Open_Bracket => {
                p.lexer.advance();
                const index = parse_expr(p);
                p.lexer.expect(.Close_Bracket);

                const new_base = p.ast.create(Ast.Expr);
                new_base.* = .{
                    .line_info = base.line_info,
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
            .Deref => {
                p.lexer.advance();

                const new_base = p.ast.create(Ast.Expr);
                new_base.* = .{
                    .line_info = base.line_info,
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
                    .line_info = id.line_info,
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
                    .line_info = base.line_info,
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
    p.lexer.expect(.Open_Paren);

    var count: usize = 0;

    var tt = p.lexer.peek();
    while (tt != .End_Of_File and tt != .Close_Paren) {
        const expr = parse_expr(p);

        if (count < dst.len) {
            dst[count] = expr;
        }

        count += 1;

        tt = p.lexer.peek();
        if (tt != .End_Of_File and tt != .Close_Paren) {
            p.lexer.expect(.Comma);
            tt = p.lexer.peek();
        }
    }

    p.lexer.expect(.Close_Paren);

    return count;
}

fn parse_expr_list(p: *Parser) Ast.ExprList {
    p.lexer.expect(.Open_Paren);

    var list = Ast.ExprList{};

    var tt = p.lexer.peek();
    while (tt != .End_Of_File and tt != .Close_Paren) {
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
        if (tt != .End_Of_File and tt != .Close_Paren) {
            p.lexer.expect(.Comma);
            tt = p.lexer.peek();
        }
    }

    p.lexer.expect(.Close_Paren);

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
        .Eq, .Neq => 2,
        .Lt, .Leq, .Gt, .Geq => 3,
        .Add, .Sub => 4,
        .Mul, .Div, .Mod => 5,
        else => LOWEST_PREC - 1,
    };
}

fn token_tag_to_binary_op_tag(op: Token.Tag) Ast.Expr.BinaryOp.Tag {
    return switch (op) {
        .Or => .Or,
        .And => .And,
        .Eq => .Eq,
        .Neq => .Neq,
        .Lt => .Lt,
        .Leq => .Leq,
        .Gt => .Gt,
        .Geq => .Geq,
        .Add => .Add,
        .Sub => .Sub,
        .Mul => .Mul,
        .Div => .Div,
        .Mod => .Mod,
        else => unreachable,
    };
}

fn token_tag_to_unary_op_tag(op: Token.Tag) Ast.Expr.UnaryOp.Tag {
    return switch (op) {
        .Add => .Pos,
        .Sub => .Neg,
        .Not => .Not,
        else => unreachable,
    };
}

const std = @import("std");
const utils = @import("utils.zig");
const Compiler = @import("compiler.zig");
const Lexer = @import("lexer.zig");
const Ast = @import("ast.zig");

const Token = Lexer.Token;

const ParseSymbolResult = struct {
    attributes: Compiler.Attributes,
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
