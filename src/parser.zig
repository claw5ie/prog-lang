lexer: Lexer,
global_symbols: Ast.SymbolList,
symbol_table: Ast.SymbolTable,
arena: common.ArenaAllocator,

current_scope: *Ast.Scope,
had_error: bool,

const Parser = @This();

const LOWEST_PREC: i32 = std.math.minInt(i32) + 1;

pub fn parse(filepath: [:0]const u8) Ast {
    var parser = Parser{
        .lexer = Lexer.init(filepath),
        .global_symbols = .{},
        .symbol_table = Ast.SymbolTable.init(common.gpa),
        .arena = common.ArenaAllocator.init(std.heap.page_allocator),
        .current_scope = &Ast.global_scope,
        .had_error = false,
    };
    defer parser.lexer.allocator.free(parser.lexer.source_code);

    parse_top_level(&parser);

    return .{
        .global_symbols = parser.global_symbols,
        .main = null,
        .symbol_table = parser.symbol_table,
        .string_pool = parser.lexer.string_pool,
        .arena = parser.arena,
        .filepath = filepath,
    };
}

fn parse_top_level(p: *Parser) void {
    while (p.lexer.peek() != .End_Of_File) {
        const stmt = parse_stmt(p);
        switch (stmt.as) {
            .Symbol => |symbol| {
                const node = create(p, Ast.SymbolList.Node);
                node.* = .{
                    .data = symbol,
                };
                p.global_symbols.append(node);
            },
            else => {
                report_error(p, stmt.line_info, "expected symbol definition", .{});
            },
        }
    }

    std.debug.assert(p.current_scope == &Ast.global_scope);

    if (p.had_error) {
        common.exit(1);
    }
}

fn parse_stmt(p: *Parser) *Ast.Stmt {
    const tok = p.lexer.grab();
    p.lexer.advance();

    switch (tok.as) {
        .Print => {
            const expr = parse_expr(p);
            p.lexer.expect(.Semicolon);

            const stmt = create(p, Ast.Stmt);
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

            const stmt = create(p, Ast.Stmt);
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

            const stmt = create(p, Ast.Stmt);
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

            const stmt = create(p, Ast.Stmt);
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

            const stmt = create(p, Ast.Stmt);
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

            const stmt = create(p, Ast.Stmt);
            stmt.* = .{
                .line_info = tok.line_info,
                .as = .Break,
            };

            return stmt;
        },
        .Continue => {
            p.lexer.expect(.Semicolon);

            const stmt = create(p, Ast.Stmt);
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

            const stmt = create(p, Ast.Stmt);
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

                const stmt = create(p, Ast.Stmt);
                stmt.* = .{
                    .line_info = tok.line_info,
                    .as = .{ .Return = null },
                };

                return stmt;
            }

            const expr = parse_expr(p);
            p.lexer.expect(.Semicolon);

            const stmt = create(p, Ast.Stmt);
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
                    const symbol = insert_symbol(p, result, .{
                        .reinterpret_variable_as = .Variable,
                        .accept_type = true,
                        .accept_proc = true,
                    });

                    if (symbol.as == .Variable or symbol.as == .Type) {
                        p.lexer.expect(.Semicolon);
                    }

                    const stmt = create(p, Ast.Stmt);
                    stmt.* = .{
                        .line_info = tok.line_info,
                        .as = .{ .Symbol = symbol },
                    };
                    return stmt;
                },
                .Expr => {
                    if (!result.attributes.is_empty()) {
                        report_error(p, tok.line_info, "unexpected attributes", .{});
                    }

                    const expr = result.pattern;
                    switch (p.lexer.peek()) {
                        .Equal => {
                            p.lexer.advance();
                            const rhs = parse_expr(p);
                            p.lexer.expect(.Semicolon);

                            const stmt = create(p, Ast.Stmt);
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
                            const stmt = create(p, Ast.Stmt);
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
            const node = create(p, Ast.Stmt.Switch.CaseList.Node);
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

            const case = create(p, Ast.Stmt.Switch.Case);
            case.* = .{ .Case = .{
                .value = value,
                .subcase = subcase,
            } };

            return case;
        },
        else => {
            const stmt = parse_stmt(p);

            const case = create(p, Ast.Stmt.Switch.Case);
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
            const node = create(p, Ast.StmtList.Node);
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
    var attributes = Ast.Attributes{};

    while (p.lexer.peek() == .Attribute) {
        const attribute = p.lexer.grab().as.Attribute;
        attributes = attributes.combine(attribute);
        p.lexer.advance();
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
                            report_error(p, p.lexer.grab_line_info(), "unexpected procedure body", .{});
                            common.exit(1);
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
                report_error(p, pattern.line_info, "missing type after expression", .{});
                common.exit(1);
            },
            .Symbol_With_Type_And_Value => {
                report_error(p, value.?.line_info, "value expression", .{});
                common.exit(1);
            },
            .Procedure => {
                report_error(p, value.?.line_info, "expected type definition, not procedure", .{});
                common.exit(1);
            },
            .Expr => {
                report_error(p, pattern.line_info, "missing type after expression", .{});
                common.exit(1);
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

fn parse_symbol_list(p: *Parser, how_to_parse: HowToParseSymbol) Ast.SymbolList {
    var list = Ast.SymbolList{};

    var tt = p.lexer.peek();
    while (tt != .End_Of_File and tt != .Close_Paren and tt != .Close_Curly) {
        const symbol = parse_symbol(p, how_to_parse);

        {
            const node = create(p, Ast.SymbolList.Node);
            node.* = .{
                .data = symbol,
            };
            list.append(node);
        }

        tt = p.lexer.peek();
        if (tt != .End_Of_File and tt != .Close_Paren and tt != .Close_Curly) {
            p.lexer.expect(.Comma);
            tt = p.lexer.peek();
        }
    }

    return list;
}

fn insert_symbol(p: *Parser, result: ParseSymbolResult, how_to_parse: HowToParseSymbol) *Ast.Symbol {
    const symbol: *Ast.Symbol = symbol: {
        switch (result.pattern.as) {
            .Identifier => |Identifier| {
                const key = Ast.Symbol.Key{
                    .name = Identifier.name,
                    .scope = Identifier.scope,
                };
                const insert_result = p.symbol_table.insert(key);

                if (insert_result.found_existing) {
                    report_error(p, result.pattern.line_info, "symbol '{s}' is defined already", .{key.name});
                    report_note(p, insert_result.value_ptr.*.line_info, "first defined here", .{});
                    common.exit(1);
                }

                if (!result.attributes.is_empty()) {
                    report_error(p, result.pattern.line_info, "attributes are not supported yet", .{});
                    common.exit(1);
                }

                const symbol = create(p, Ast.Symbol);
                symbol.* = .{
                    .line_info = result.pattern.line_info,
                    .as = undefined,
                    .key = key,
                    .typechecking = .None,
                };
                insert_result.value_ptr.* = symbol;

                break :symbol symbol;
            },
            else => {
                report_error(p, result.pattern.line_info, "expected identifier", .{});
                common.exit(1);
            },
        }
    };

    symbol.as = as: {
        switch (result.tag) {
            .Type => {
                if (!how_to_parse.accept_type) {
                    report_error(p, result.pattern.line_info, "unexpected type", .{});
                    common.exit(1);
                }

                break :as .{ .Type = result.typ.? };
            },
            .Symbol_Without_Type => {
                switch (how_to_parse.reinterpret_variable_as) {
                    .Variable => break :as .{ .Variable = .{
                        .typ = result.typ,
                        .value = result.value,
                        .storage = undefined,
                    } },
                    .Enum_Field => break :as .{ .Enum_Field = .{
                        .value = result.value,
                        .computed_value = 0,
                    } },
                    .Parameter,
                    .Struct_Field,
                    .Union_Field,
                    => {
                        report_error(p, result.pattern.line_info, "missing type after expression", .{});
                        common.exit(1);
                    },
                }
            },
            .Symbol_With_Type => {
                switch (how_to_parse.reinterpret_variable_as) {
                    .Variable => break :as .{ .Variable = .{
                        .typ = result.typ,
                        .value = result.value,
                        .storage = undefined,
                    } },
                    .Parameter => break :as .{ .Parameter = .{
                        .typ = result.typ.?,
                        .value = result.value,
                        .storage = undefined,
                    } },
                    .Struct_Field => break :as .{ .Struct_Field = .{
                        .typ = result.typ.?,
                        .value = result.value,
                        .offset = 0,
                    } },
                    .Union_Field => break :as .{ .Union_Field = .{
                        .typ = result.typ.?,
                        .value = result.value,
                        .offset = 0,
                    } },
                    .Enum_Field => {
                        report_error(p, result.typ.?.line_info, "unexpected type", .{});
                        common.exit(1);
                    },
                }
            },
            .Symbol_With_Type_And_Value => {
                switch (how_to_parse.reinterpret_variable_as) {
                    .Variable => break :as .{ .Variable = .{
                        .typ = result.typ,
                        .value = result.value,
                        .storage = undefined,
                    } },
                    .Parameter => break :as .{ .Parameter = .{
                        .typ = result.typ.?,
                        .value = result.value,
                        .storage = undefined,
                    } },
                    .Struct_Field => break :as .{ .Struct_Field = .{
                        .typ = result.typ.?,
                        .value = result.value,
                        .offset = 0,
                    } },
                    .Union_Field => break :as .{ .Union_Field = .{
                        .typ = result.typ.?,
                        .value = result.value,
                        .offset = 0,
                    } },
                    .Enum_Field => {
                        report_error(p, result.typ.?.line_info, "unexpected type", .{});
                        common.exit(1);
                    },
                }
            },
            .Procedure => {
                if (!how_to_parse.accept_proc) {
                    report_error(p, result.typ.?.line_info, "unexpected procedure body after non-procedure type", .{});
                    common.exit(1);
                }

                break :as .{ .Procedure = .{
                    .typ = result.typ.?,
                    .block = result.block,
                    .label = 0,
                } };
            },
            .Expr => {
                switch (how_to_parse.reinterpret_variable_as) {
                    .Enum_Field => break :as .{ .Enum_Field = .{
                        .value = result.value,
                        .computed_value = 0,
                    } },
                    else => {
                        report_error(p, result.pattern.line_info, "expected ':' or ':=' after expression", .{});
                        common.exit(1);
                    },
                }
            },
        }
    };

    return symbol;
}

fn parse_type(p: *Parser) *Ast.Type {
    const typ = create(p, Ast.Type);
    typ.* = parse_type_by_value(p);
    return typ;
}

fn parse_type_by_value(p: *Parser) Ast.Type {
    var typ: Ast.Type = undefined;
    if (!try_parse_type_by_value(p, &typ)) {
        report_error(p, p.lexer.grab_line_info(), "token doesn't start a type", .{});
        common.exit(1);
    }
    return typ;
}

fn try_parse_type_by_value(p: *Parser, dst: *Ast.Type) bool {
    const tok = p.lexer.grab();
    p.lexer.advance();

    dst.* = dst: {
        switch (tok.as) {
            .Open_Paren => {
                const typ = parse_type_by_value(p);
                p.lexer.expect(.Close_Paren);
                break :dst typ;
            },
            .Struct => {
                p.lexer.expect(.Open_Curly);
                push_scope(p);
                const scope = p.current_scope;
                const fields = parse_symbol_list(p, .{
                    .reinterpret_variable_as = .Struct_Field,
                });
                pop_scope(p);
                p.lexer.expect(.Close_Curly);
                const data = create(p, Ast.Type.SharedData);
                data.* = .{
                    .as = .{ .Struct = .{
                        .fields = fields,
                        .scope = scope,
                    } },
                    .byte_size = 0,
                    .alignment = .BYTE,
                    .stages = Ast.default_stages_none,
                };
                break :dst .{
                    .line_info = tok.line_info,
                    .data = data,
                    .symbol = null,
                };
            },
            .Union => {
                p.lexer.expect(.Open_Curly);
                push_scope(p);
                const scope = p.current_scope;
                const fields = parse_symbol_list(p, .{
                    .reinterpret_variable_as = .Union_Field,
                });
                pop_scope(p);
                p.lexer.expect(.Close_Curly);
                const data = create(p, Ast.Type.SharedData);
                data.* = .{
                    .as = .{ .Union = .{
                        .fields = fields,
                        .scope = scope,
                    } },
                    .byte_size = 0,
                    .alignment = .BYTE,
                    .stages = Ast.default_stages_none,
                };
                break :dst .{
                    .line_info = tok.line_info,
                    .data = data,
                    .symbol = null,
                };
            },
            .Enum => {
                p.lexer.expect(.Open_Curly);
                push_scope(p);
                const scope = p.current_scope;
                const fields = parse_symbol_list(p, .{
                    .reinterpret_variable_as = .Enum_Field,
                });
                pop_scope(p);
                p.lexer.expect(.Close_Curly);
                const data = create(p, Ast.Type.SharedData);
                data.* = .{
                    .as = .{ .Enum = .{
                        .fields = fields,
                        .integer_type = Ast.lookup_integer_type(0, false),
                        .scope = scope,
                    } },
                    .byte_size = 0,
                    .alignment = .BYTE,
                    .stages = Ast.default_stages_none,
                };
                break :dst .{
                    .line_info = tok.line_info,
                    .data = data,
                    .symbol = null,
                };
            },
            .Proc => {
                p.lexer.expect(.Open_Paren);
                push_scope(p);
                const scope = p.current_scope;
                const params = parse_symbol_list(p, .{
                    .reinterpret_variable_as = .Parameter,
                });
                pop_scope(p);
                p.lexer.expect(.Close_Paren);
                const return_type = parse_type(p);
                const data = create(p, Ast.Type.SharedData);
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
                break :dst .{
                    .line_info = tok.line_info,
                    .data = data,
                    .symbol = null,
                };
            },
            .Integer_Type => |Integer_Type| {
                const typ = Ast.lookup_integer_type(Integer_Type.bits, Integer_Type.is_signed);
                break :dst .{
                    .line_info = tok.line_info,
                    .data = typ.data,
                    .symbol = null,
                };
            },
            .Bool_Type => {
                break :dst .{
                    .line_info = tok.line_info,
                    .data = Ast.bool_type.data,
                    .symbol = null,
                };
            },
            .Void_Type => {
                break :dst .{
                    .line_info = tok.line_info,
                    .data = Ast.void_type.data,
                    .symbol = null,
                };
            },
            .Type_Of => {
                var exprs: [1]*Ast.Expr = undefined;
                const count = parse_fixed_size_expr_list(p, &exprs);
                switch (count) {
                    1 => {
                        const data = create(p, Ast.Type.SharedData);
                        data.* = .{
                            .as = .{ .Type_Of = exprs[0] },
                            .byte_size = 0,
                            .alignment = .BYTE,
                            .stages = Ast.default_stages_none,
                        };
                        break :dst .{
                            .line_info = tok.line_info,
                            .data = data,
                            .symbol = null,
                        };
                    },
                    else => {
                        report_error(p, tok.line_info, "expected 1 arguments, but got {}", .{count});
                        common.exit(1);
                    },
                }
            },
            .Identifier => |name| {
                const data = create(p, Ast.Type.SharedData);
                data.* = .{
                    .as = .{ .Identifier = .{
                        .name = name,
                        .scope = p.current_scope,
                    } },
                    .byte_size = 0,
                    .alignment = .BYTE,
                    .stages = Ast.default_stages_none,
                };
                break :dst .{
                    .line_info = tok.line_info,
                    .data = data,
                    .symbol = null,
                };
            },
            else => {
                p.lexer.putback(tok);
                return false;
            },
        }
    };

    while (true) {
        switch (p.lexer.peek()) {
            .Deref => {
                const line_info = p.lexer.grab_line_info();
                p.lexer.advance();

                const subtype = create(p, Ast.Type);
                subtype.* = dst.*;
                const data = create(p, Ast.Type.SharedData);
                data.* = .{
                    .as = .{ .Pointer = subtype },
                    .byte_size = Ast.pointer_byte_size,
                    .alignment = Ast.pointer_alignment,
                    .stages = Ast.default_stages_none,
                };
                dst.* = .{
                    .line_info = line_info,
                    .data = data,
                    .symbol = null,
                };
            },
            .Open_Bracket => {
                const line_info = p.lexer.grab_line_info();

                p.lexer.advance();
                const size = parse_expr(p);
                p.lexer.expect(.Close_Bracket);

                const subtype = create(p, Ast.Type);
                subtype.* = dst.*;
                const data = create(p, Ast.Type.SharedData);
                data.* = .{
                    .as = .{ .Array = .{
                        .subtype = subtype,
                        .size = size,
                        .computed_size = 0,
                    } },
                    .byte_size = 0,
                    .alignment = .BYTE,
                    .stages = Ast.default_stages_none,
                };
                dst.* = .{
                    .line_info = line_info,
                    .data = data,
                    .symbol = null,
                };
            },
            else => break,
        }
    }

    return true;
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
            const new_lhs = create(p, Ast.Expr);
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
    var base = parse_expr_base(p);

    while (true) {
        switch (p.lexer.peek()) {
            .Open_Paren => {
                const args = parse_expr_list(p);

                const new_base = create(p, Ast.Expr);
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

                const new_base = create(p, Ast.Expr);
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

                const new_base = create(p, Ast.Expr);
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

                const field = parse_expr_base(p);
                const new_base = create(p, Ast.Expr);
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

fn parse_expr_base(p: *Parser) *Ast.Expr {
    const tok = p.lexer.grab();
    p.lexer.advance();

    switch (tok.as) {
        .And => {
            const subsubexpr = parse_expr_base(p);
            const subexpr = create(p, Ast.Expr);
            subexpr.* = .{
                .line_info = tok.line_info,
                .as = .{ .Ref = subsubexpr },
                .typ = Ast.void_type,
                .flags = .{},
                .typechecking = .None,
            };
            subexpr.line_info.column += 1;
            subexpr.line_info.offset += 1;
            const expr = create(p, Ast.Expr);
            expr.* = .{
                .line_info = tok.line_info,
                .as = .{ .Ref = subexpr },
                .typ = Ast.void_type,
                .flags = .{},
                .typechecking = .None,
            };
            return expr;
        },
        .Add, .Sub, .Not => {
            const subexpr = parse_expr_base(p);
            const expr = create(p, Ast.Expr);
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
            return expr;
        },
        .Ref => {
            const subexpr = parse_expr_base(p);
            const expr = create(p, Ast.Expr);
            expr.* = .{
                .line_info = subexpr.line_info,
                .as = .{ .Ref = subexpr },
                .typ = Ast.void_type,
                .flags = .{},
                .typechecking = .None,
            };
            return expr;
        },
        .Open_Paren => {
            const expr = parse_expr(p);
            expr.line_info = tok.line_info;
            p.lexer.expect(.Close_Paren);
            return expr;
        },
        .If => {
            const condition = parse_expr(p);
            if (p.lexer.peek() == .Then) {
                p.lexer.advance();
            }
            const true_branch = parse_expr(p);
            p.lexer.expect(.Else);
            const false_branch = parse_expr(p);

            const expr = create(p, Ast.Expr);
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

            return expr;
        },
        .Boolean => |value| {
            const expr = create(p, Ast.Expr);
            expr.* = .{
                .line_info = tok.line_info,
                .as = .{ .Boolean = value },
                .typ = Ast.bool_type,
                .flags = .{},
                .typechecking = .None,
            };
            return expr;
        },
        .Null => {
            const expr = create(p, Ast.Expr);
            expr.* = .{
                .line_info = tok.line_info,
                .as = .Null,
                .typ = Ast.void_pointer_type,
                .flags = .{},
                .typechecking = .None,
            };
            return expr;
        },
        .Identifier => |name| {
            const expr = create(p, Ast.Expr);
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
            return expr;
        },
        .Integer => |value| {
            const typ = Ast.integer_type_from_u64(value);
            const expr = create(p, Ast.Expr);
            expr.* = .{
                .line_info = tok.line_info,
                .as = .{ .Integer = value },
                .typ = typ,
                .flags = .{},
                .typechecking = .None,
            };
            return expr;
        },
        .Byte_Size_Of => {
            var exprs: [1]*Ast.Expr = undefined;
            const count = parse_fixed_size_expr_list(p, &exprs);

            switch (count) {
                1 => {
                    const expr = create(p, Ast.Expr);
                    expr.* = .{
                        .line_info = tok.line_info,
                        .as = .{ .Byte_Size_Of = exprs[0] },
                        .typ = Ast.void_type,
                        .flags = .{},
                        .typechecking = .None,
                    };
                    return expr;
                },
                else => {
                    report_error(p, tok.line_info, "expected 1 argument, but got {}", .{count});
                    common.exit(1);
                },
            }
        },
        .Alignment_Of => {
            var exprs: [1]*Ast.Expr = undefined;
            const count = parse_fixed_size_expr_list(p, &exprs);

            switch (count) {
                1 => {
                    const expr = create(p, Ast.Expr);
                    expr.* = .{
                        .line_info = tok.line_info,
                        .as = .{ .Alignment_Of = exprs[0] },
                        .typ = Ast.void_type,
                        .flags = .{},
                        .typechecking = .None,
                    };
                    return expr;
                },
                else => {
                    report_error(p, tok.line_info, "expected 1 argument, but got {}", .{count});
                    common.exit(1);
                },
            }
        },
        .As => {
            var exprs: [2]*Ast.Expr = undefined;
            const count = parse_fixed_size_expr_list(p, &exprs);

            switch (count) {
                2 => {
                    const typ = expr_to_type(p, exprs[0]);
                    const expr = create(p, Ast.Expr);
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
                    return expr;
                },
                else => {
                    report_error(p, tok.line_info, "expected 2 arguments, but got {}", .{count});
                    common.exit(1);
                },
            }
        },
        .Cast => {
            var exprs: [2]*Ast.Expr = undefined;
            const count = parse_fixed_size_expr_list(p, &exprs);

            switch (count) {
                2 => {
                    const typ = expr_to_type(p, exprs[0]);
                    const expr = create(p, Ast.Expr);
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
                    return expr;
                },
                else => {
                    report_error(p, tok.line_info, "expected 2 arguments, but got {}", .{count});
                    common.exit(1);
                },
            }
        },
        else => {
            p.lexer.putback(tok);

            var typ: Ast.Type = undefined;
            if (!try_parse_type_by_value(p, &typ)) {
                common.print_error(p.lexer.filepath, tok.line_info, "token doesn't start an expression.", .{});
                common.exit(1);
            }

            const expr = create(p, Ast.Expr);
            expr.* = .{
                .line_info = tok.line_info,
                .as = .{ .Type = typ },
                .typ = Ast.void_type,
                .flags = .{},
                .typechecking = .None,
            };
            expr.typ = &expr.as.Type;

            return expr;
        },
    }
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
        const expr_node = create(p, Ast.ExprListNode);
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
            const node = create(p, Ast.ExprList.Node);
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

// Assume 'expr' is heap allocated and can be reused.
fn expr_to_type(p: *Parser, expr: *Ast.Expr) *Ast.Type {
    switch (expr.as) {
        .Deref => |subexpr| {
            const subtype = expr_to_type(p, subexpr);

            const data = create(p, Ast.Type.SharedData);
            data.* = .{
                .as = .{ .Pointer = subtype },
                .byte_size = Ast.pointer_byte_size,
                .alignment = Ast.pointer_alignment,
                .stages = Ast.default_stages_none,
            };
            expr.as = .{ .Type = .{
                .line_info = expr.line_info,
                .data = data,
                .symbol = null,
            } };

            return &expr.as.Type;
        },
        .Subscript => |Subscript| {
            const subtype = expr_to_type(p, Subscript.subexpr);

            const data = create(p, Ast.Type.SharedData);
            data.* = .{
                .as = .{ .Array = .{
                    .subtype = subtype,
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

            return &expr.as.Type;
        },
        .Type => |*typ| {
            return typ;
        },
        .Identifier => |Identifier| {
            const data = create(p, Ast.Type.SharedData);
            data.* = .{
                .as = .{ .Identifier = .{
                    .name = Identifier.name,
                    .scope = Identifier.scope,
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

            return &expr.as.Type;
        },
        else => {
            report_error(p, expr.line_info, "expected expression, not a type", .{});
            common.exit(1);
        },
    }
}

fn create(p: *Parser, comptime T: type) *T {
    return p.arena.allocator().create(T) catch {
        common.exit(1);
    };
}

fn push_scope(p: *Parser) void {
    const scope = create(p, Ast.Scope);
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

fn report_error(p: *Parser, line_info: LineInfo, comptime format: []const u8, args: anytype) void {
    p.had_error = true;
    common.print_error(p.lexer.filepath, line_info, format, args);
}

fn report_note(p: *Parser, line_info: LineInfo, comptime format: []const u8, args: anytype) void {
    common.print_note(p.lexer.filepath, line_info, format, args);
}

const std = @import("std");
const common = @import("common.zig");
const utils = @import("utils.zig");
const Lexer = @import("lexer.zig");
const Ast = @import("ast.zig");

const ArenaAllocator = std.heap.ArenaAllocator;
const Allocator = std.mem.Allocator;
const Token = Lexer.Token;
const LineInfo = common.LineInfo;

const ParseSymbolResult = struct {
    attributes: Ast.Attributes,
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

const HowToParseSymbol = packed struct {
    reinterpret_variable_as: enum(u3) {
        Variable,
        Parameter,
        Struct_Field,
        Union_Field,
        Enum_Field,
    },
    accept_type: bool = false,
    accept_proc: bool = false,
};
