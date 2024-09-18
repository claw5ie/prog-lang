const std = @import("std");
const utils = @import("utils.zig");
const Compiler = @import("compiler.zig");
const Lexer = @import("lexer.zig");

const Token = Compiler.Lexer.Token;
const Ast = Compiler.Ast;

const LOWEST_PREC: i32 = std.math.minInt(i32) + 1;

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

pub fn parse(c: *Compiler, filepath: [:0]const u8) void {
    Compiler.gpa.free(c.source_code);
    c.filepath = filepath;
    c.source_code = utils.read_entire_file(Compiler.gpa, filepath) catch {
        Compiler.eprint("error: failed to read from a file '{s}'\n", .{filepath});
        Compiler.exit(1);
    };

    parse_top_level(c);
}

fn parse_top_level(c: *Compiler) void {
    while (Lexer.peek(c) != .End_Of_File) {
        const stmt = parse_stmt(c);
        switch (stmt.as) {
            .Symbol => |symbol| {
                const node = c.ast.create(Ast.SymbolList.Node);
                node.* = .{
                    .data = symbol,
                };
                c.ast.globals.append(node);
            },
            else => {
                c.report_error(stmt.line_info, "expected symbol definition", .{});
            },
        }
    }

    std.debug.assert(c.parser.current_scope == &Compiler.global_scope);

    if (c.had_error) {
        Compiler.exit(1);
    }
}

fn parse_stmt(c: *Compiler) *Ast.Stmt {
    const tok = Lexer.grab(c);
    Lexer.advance(c);

    switch (tok.as) {
        .Print => {
            const expr = parse_expr(c);
            Lexer.expect(c, .Semicolon);

            const stmt = c.ast.create(Ast.Stmt);
            stmt.* = .{
                .line_info = tok.line_info,
                .as = .{ .Print = expr },
            };

            return stmt;
        },
        .Open_Curly => {
            Lexer.putback(c, tok);

            push_scope(c);
            const block = parse_stmt_list(c);
            pop_scope(c);

            const stmt = c.ast.create(Ast.Stmt);
            stmt.* = .{
                .line_info = tok.line_info,
                .as = .{ .Block = block },
            };

            return stmt;
        },
        .If => {
            const condition = parse_expr(c);
            if (Lexer.peek(c) == .Then) {
                Lexer.advance(c);
            }
            const true_branch = parse_stmt(c);
            var false_branch: ?*Ast.Stmt = null;
            if (Lexer.peek(c) == .Else) {
                Lexer.advance(c);
                false_branch = parse_stmt(c);
            }

            const stmt = c.ast.create(Ast.Stmt);
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
            const condition = parse_expr(c);
            if (Lexer.peek(c) == .Do) {
                Lexer.advance(c);
            }
            const body = parse_stmt(c);

            const stmt = c.ast.create(Ast.Stmt);
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
            const body = parse_stmt(c);
            Lexer.expect(c, .While);
            const condition = parse_expr(c);
            Lexer.expect(c, .Semicolon);

            const stmt = c.ast.create(Ast.Stmt);
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
            Lexer.expect(c, .Semicolon);

            const stmt = c.ast.create(Ast.Stmt);
            stmt.* = .{
                .line_info = tok.line_info,
                .as = .Break,
            };

            return stmt;
        },
        .Continue => {
            Lexer.expect(c, .Semicolon);

            const stmt = c.ast.create(Ast.Stmt);
            stmt.* = .{
                .line_info = tok.line_info,
                .as = .Continue,
            };

            return stmt;
        },
        .Switch => {
            const condition = parse_expr(c);
            const cases = parse_stmt_switch(c);
            var default_case: ?*Ast.Stmt = null;
            if (Lexer.peek(c) == .Else) {
                Lexer.advance(c);
                default_case = parse_stmt(c);
            }

            const stmt = c.ast.create(Ast.Stmt);
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
            if (Lexer.peek(c) == .Semicolon) {
                Lexer.advance(c);

                const stmt = c.ast.create(Ast.Stmt);
                stmt.* = .{
                    .line_info = tok.line_info,
                    .as = .{ .Return = null },
                };

                return stmt;
            }

            const expr = parse_expr(c);
            Lexer.expect(c, .Semicolon);

            const stmt = c.ast.create(Ast.Stmt);
            stmt.* = .{
                .line_info = tok.line_info,
                .as = .{ .Return = expr },
            };

            return stmt;
        },
        else => {
            Lexer.putback(c, tok);

            const result = try_parse_symbol(c);
            switch (result.tag) {
                .Type,
                .Symbol_Without_Type,
                .Symbol_With_Type,
                .Symbol_With_Type_And_Value,
                .Procedure,
                => {
                    const symbol = insert_symbol(c, result, .Parsing_Statement);

                    const stmt = c.ast.create(Ast.Stmt);
                    stmt.* = .{
                        .line_info = tok.line_info,
                        .as = .{ .Symbol = symbol },
                    };
                    return stmt;
                },
                .Expr => {
                    if (!result.attributes.is_empty()) {
                        c.report_error(tok.line_info, "unexpected attributes", .{});
                    }

                    const expr = result.pattern;
                    switch (Lexer.peek(c)) {
                        .Equal => {
                            Lexer.advance(c);
                            const rhs = parse_expr(c);
                            Lexer.expect(c, .Semicolon);

                            const stmt = c.ast.create(Ast.Stmt);
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
                            Lexer.expect(c, .Semicolon);
                            const stmt = c.ast.create(Ast.Stmt);
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

fn parse_stmt_switch(c: *Compiler) Ast.Stmt.Switch.CaseList {
    Lexer.expect(c, .Open_Curly);
    push_scope(c);

    var list = Ast.Stmt.Switch.CaseList{};

    var tt = Lexer.peek(c);
    while (tt != .End_Of_File and tt != .Close_Curly) {
        const case = parse_switch_case(c);

        {
            const node = c.ast.create(Ast.Stmt.Switch.CaseList.Node);
            node.* = .{
                .data = case,
            };
            list.append(node);
        }

        tt = Lexer.peek(c);
    }

    Lexer.expect(c, .Close_Curly);
    pop_scope(c);

    return list;
}

fn parse_switch_case(c: *Compiler) *Ast.Stmt.Switch.Case {
    switch (Lexer.peek(c)) {
        .Case => {
            Lexer.advance(c);
            const value = parse_expr(c);
            if (Lexer.peek(c) == .Then) {
                Lexer.advance(c);
            }
            const subcase = parse_switch_case(c);

            const case = c.ast.create(Ast.Stmt.Switch.Case);
            case.* = .{ .Case = .{
                .value = value,
                .subcase = subcase,
            } };

            return case;
        },
        else => {
            const stmt = parse_stmt(c);

            const case = c.ast.create(Ast.Stmt.Switch.Case);
            case.* = .{ .Stmt = stmt };

            return case;
        },
    }
}

fn parse_stmt_list(c: *Compiler) Ast.StmtList {
    Lexer.expect(c, .Open_Curly);

    var list = Ast.StmtList{};

    var tt = Lexer.peek(c);
    while (tt != .End_Of_File and tt != .Close_Curly) {
        const stmt = parse_stmt(c);

        {
            const node = c.ast.create(Ast.StmtList.Node);
            node.* = .{
                .data = stmt,
            };
            list.append(node);
        }

        tt = Lexer.peek(c);
    }

    Lexer.expect(c, .Close_Curly);

    return list;
}

fn try_parse_symbol(c: *Compiler) ParseSymbolResult {
    var attributes = Compiler.Attributes{};

    while (Lexer.peek(c) == .Attribute) {
        const attribute = Lexer.grab(c).as.Attribute;
        attributes = attributes.combine(attribute);
        Lexer.advance(c);
    }

    var is_type = false;

    if (Lexer.peek(c) == .Alias) {
        Lexer.advance(c);
        is_type = true;
    }

    const pattern = parse_expr(c);
    var typ: ?*Ast.Type = null;
    var value: ?*Ast.Expr = null;
    var block: Ast.StmtList = .{};
    var tag: ParseSymbolResult.Tag = .Expr;

    switch (Lexer.peek(c)) {
        .Colon_Equal => {
            tag = .Symbol_Without_Type;
            Lexer.advance(c);
            value = parse_expr(c);
        },
        .Colon => {
            tag = .Symbol_With_Type;
            Lexer.advance(c);
            typ = parse_type(c);
            switch (Lexer.peek(c)) {
                .Open_Curly => {
                    switch (typ.?.data.as) {
                        .Proc => |Proc| {
                            tag = .Procedure;
                            const old_current_scope = c.parser.current_scope;

                            c.parser.current_scope = Proc.scope;
                            block = parse_stmt_list(c);
                            c.parser.current_scope = old_current_scope;
                        },
                        else => {
                            c.report_error(Lexer.grab_line_info(c), "unexpected procedure body", .{});
                            Compiler.exit(1);
                        },
                    }
                },
                .Equal => {
                    tag = .Symbol_With_Type_And_Value;
                    Lexer.advance(c);
                    value = parse_expr(c);
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
                c.report_error(pattern.line_info, "missing type after expression", .{});
                Compiler.exit(1);
            },
            .Symbol_With_Type_And_Value => {
                c.report_error(value.?.line_info, "value expression", .{});
                Compiler.exit(1);
            },
            .Procedure => {
                c.report_error(value.?.line_info, "expected type definition, not procedure", .{});
                Compiler.exit(1);
            },
            .Expr => {
                c.report_error(pattern.line_info, "missing type after expression", .{});
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

fn parse_symbol(c: *Compiler, how_to_parse: HowToParseSymbol) *Ast.Symbol {
    const result = try_parse_symbol(c);
    const symbol = insert_symbol(c, result, how_to_parse);
    return symbol;
}

fn parse_symbol_list(c: *Compiler, fields: *Ast.SymbolList, rest: ?*Ast.SymbolList, how_to_parse: HowToParseSymbol) void {
    var tt = Lexer.peek(c);
    while (tt != .End_Of_File and tt != .Close_Paren and tt != .Close_Curly) {
        const symbol = parse_symbol(c, how_to_parse);

        {
            const node = c.ast.create(Ast.SymbolList.Node);
            node.* = .{
                .data = symbol,
            };

            switch (symbol.as) {
                .Struct_Field,
                .Union_Field,
                .Enum_Field,
                .Parameter,
                => {
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

        tt = Lexer.peek(c);
        if (tt != .End_Of_File and tt != .Close_Paren and tt != .Close_Curly) {
            switch (symbol.as) {
                .Struct_Field,
                .Union_Field,
                .Enum_Field,
                .Parameter,
                => {
                    Lexer.expect(c, .Comma);
                    tt = Lexer.peek(c);
                },
                .Variable,
                .Procedure,
                .Type,
                => {},
            }
        }
    }
}

fn insert_symbol(c: *Compiler, result: ParseSymbolResult, how_to_parse: HowToParseSymbol) *Ast.Symbol {
    const symbol: *Ast.Symbol = symbol: {
        switch (result.pattern.as) {
            .Identifier => |Identifier| {
                const key = Ast.Symbol.Key{
                    .name = Identifier.name,
                    .scope = Identifier.scope,
                };
                const insert_result = c.symbol_table.insert(key);

                if (insert_result.found_existing) {
                    c.report_error(result.pattern.line_info, "symbol '{s}' is defined already", .{key.name});
                    c.report_note(insert_result.value_ptr.*.line_info, "first defined here", .{});
                    Compiler.exit(1);
                }

                const symbol = c.ast.create(Ast.Symbol);
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
                c.report_error(result.pattern.line_info, "expected identifier", .{});
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
                                c.report_error(result.pattern.line_info, "expected type after pattern", .{});
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
                                c.report_error(result.typ.?.line_info, "unexpected type", .{});
                                Compiler.exit(1);
                            },
                        }
                    },
                    .Procedure => break :symbol_tag .Procedure,
                    .Expr => {
                        switch (container_type) {
                            .Struct, .Union => {
                                c.report_error(result.pattern.line_info, "expected type after pattern", .{});
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
                        c.report_error(result.pattern.line_info, "unexpected type", .{});
                        Compiler.exit(1);
                    },
                    .Symbol_Without_Type => {
                        c.report_error(result.pattern.line_info, "expected type after pattern", .{});
                        Compiler.exit(1);
                    },
                    .Procedure => {
                        c.report_error(result.pattern.line_info, "unexpected procedure", .{});
                        Compiler.exit(1);
                    },
                    .Expr => {
                        c.report_error(result.pattern.line_info, "unexpected expression", .{});
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

    if (!result.attributes.is_empty() and symbol_tag != .Variable) {
        c.report_error(result.pattern.line_info, "unexpected attributes", .{});
        Compiler.exit(1);
    }

    symbol.as = as: {
        switch (symbol_tag) {
            .Variable => {
                Lexer.expect(c, .Semicolon);

                symbol.attributes.is_static = symbol.key.scope == &Compiler.global_scope;
                break :as .{ .Variable = .{
                    .typ = result.typ,
                    .value = result.value,
                    .storage = undefined,
                } };
            },
            .Parameter => break :as .{ .Parameter = .{
                .typ = result.typ.?,
                .value = result.value,
                .storage = undefined,
            } },
            .Procedure => break :as .{ .Procedure = .{
                .typ = result.typ.?,
                .block = result.block,
                .start_label = null,
                .end_label = null,
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
                symbol.attributes.is_const = true;
                break :as .{ .Enum_Field = .{
                    .value = result.value,
                    .computed_value = 0,
                } };
            },
            .Type => {
                Lexer.expect(c, .Semicolon);
                break :as .{ .Type = result.typ.? };
            },
        }
    };

    if (!result.attributes.is_empty()) {
        c.report_error(result.pattern.line_info, "unexpected attributes", .{});
        Compiler.exit(1);
    }

    return symbol;
}

fn parse_type(c: *Compiler) *Ast.Type {
    const typ = c.ast.create(Ast.Type);
    typ.* = parse_type_by_value(c);
    return typ;
}

fn parse_type_by_value(c: *Compiler) Ast.Type {
    var typ: Ast.Type = undefined;
    if (!try_parse_type_by_value(c, &typ)) {
        c.report_error(Lexer.grab_line_info(c), "token doesn't start a type", .{});
        Compiler.exit(1);
    }
    return typ;
}

fn try_parse_type_by_value(c: *Compiler, dst: *Ast.Type) bool {
    const tok = Lexer.grab(c);
    Lexer.advance(c);

    dst.* = dst: {
        switch (tok.as) {
            .Open_Paren => {
                const typ = parse_type_by_value(c);
                Lexer.expect(c, .Close_Paren);
                break :dst typ;
            },
            .Struct => {
                Lexer.expect(c, .Open_Curly);
                push_scope(c);
                const scope = c.parser.current_scope;
                const fields = c.ast.create(Ast.SymbolList);
                const rest = c.ast.create(Ast.SymbolList);
                fields.* = .{};
                rest.* = .{};
                parse_symbol_list(c, fields, rest, .{ .Parsing_Container = .Struct });
                pop_scope(c);
                Lexer.expect(c, .Close_Curly);
                const data = c.ast.create(Ast.Type.SharedData);
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
                break :dst .{
                    .line_info = tok.line_info,
                    .data = data,
                    .symbol = null,
                };
            },
            .Union => {
                Lexer.expect(c, .Open_Curly);
                push_scope(c);
                const scope = c.parser.current_scope;
                const fields = c.ast.create(Ast.SymbolList);
                const rest = c.ast.create(Ast.SymbolList);
                fields.* = .{};
                rest.* = .{};
                parse_symbol_list(c, fields, rest, .{ .Parsing_Container = .Union });
                pop_scope(c);
                Lexer.expect(c, .Close_Curly);
                const data = c.ast.create(Ast.Type.SharedData);
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
                break :dst .{
                    .line_info = tok.line_info,
                    .data = data,
                    .symbol = null,
                };
            },
            .Enum => {
                Lexer.expect(c, .Open_Curly);
                push_scope(c);
                const scope = c.parser.current_scope;
                const fields = c.ast.create(Ast.SymbolList);
                const rest = c.ast.create(Ast.SymbolList);
                fields.* = .{};
                rest.* = .{};
                parse_symbol_list(c, fields, rest, .{ .Parsing_Container = .Enum });
                pop_scope(c);
                Lexer.expect(c, .Close_Curly);
                const data = c.ast.create(Ast.Type.SharedData);
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
                break :dst .{
                    .line_info = tok.line_info,
                    .data = data,
                    .symbol = null,
                };
            },
            .Proc => {
                Lexer.expect(c, .Open_Paren);
                push_scope(c);
                const scope = c.parser.current_scope;
                var params = Ast.SymbolList{};
                parse_symbol_list(c, &params, null, .Parsing_Procedure);
                pop_scope(c);
                Lexer.expect(c, .Close_Paren);
                const return_type = parse_type(c);
                const data = c.ast.create(Ast.Type.SharedData);
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
                const count = parse_fixed_size_expr_list(c, &exprs);
                switch (count) {
                    1 => {
                        const data = c.ast.create(Ast.Type.SharedData);
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
                        c.report_error(tok.line_info, "expected 1 arguments, but got {}", .{count});
                        Compiler.exit(1);
                    },
                }
            },
            .Identifier => |name| {
                const data = c.ast.create(Ast.Type.SharedData);
                data.* = .{
                    .as = .{ .Identifier = .{
                        .name = name,
                        .scope = c.parser.current_scope,
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
                Lexer.putback(c, tok);
                return false;
            },
        }
    };

    while (true) {
        switch (Lexer.peek(c)) {
            .Deref => {
                const line_info = Lexer.grab_line_info(c);
                Lexer.advance(c);

                const subtype = c.ast.create(Ast.Type);
                subtype.* = dst.*;
                const data = c.ast.create(Ast.Type.SharedData);
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
                const line_info = Lexer.grab_line_info(c);

                Lexer.advance(c);
                const size = parse_expr(c);
                Lexer.expect(c, .Close_Bracket);

                const subtype = c.ast.create(Ast.Type);
                subtype.* = dst.*;
                const data = c.ast.create(Ast.Type.SharedData);
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

fn parse_expr(c: *Compiler) *Ast.Expr {
    return parse_expr_prec(c, LOWEST_PREC);
}

fn parse_expr_prec(c: *Compiler, min_prec: i32) *Ast.Expr {
    var lhs = parse_expr_highest_prec(c);
    var op = Lexer.peek(c);
    var prev_prec: i32 = std.math.maxInt(i32);
    var curr_prec: i32 = prec_of_op(op);

    while (curr_prec < prev_prec and curr_prec >= min_prec) {
        while (true) {
            const line_info = Lexer.grab_line_info(c);
            Lexer.advance(c);

            const rhs = parse_expr_prec(c, curr_prec + 1);
            const new_lhs = c.ast.create(Ast.Expr);
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

            op = Lexer.peek(c);
            if (curr_prec != prec_of_op(op)) break;
        }

        prev_prec = curr_prec;
        curr_prec = prec_of_op(op);
    }

    return lhs;
}

fn parse_expr_highest_prec(c: *Compiler) *Ast.Expr {
    var base = parse_expr_base(c);

    while (true) {
        switch (Lexer.peek(c)) {
            .Open_Paren => {
                const args = parse_expr_list(c);

                const new_base = c.ast.create(Ast.Expr);
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
                Lexer.advance(c);
                const index = parse_expr(c);
                Lexer.expect(c, .Close_Bracket);

                const new_base = c.ast.create(Ast.Expr);
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
                Lexer.advance(c);

                const new_base = c.ast.create(Ast.Expr);
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
                Lexer.advance(c);

                const field = parse_expr_base(c);
                const new_base = c.ast.create(Ast.Expr);
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

fn parse_expr_base(c: *Compiler) *Ast.Expr {
    const tok = Lexer.grab(c);
    Lexer.advance(c);

    switch (tok.as) {
        .And => {
            const subsubexpr = parse_expr_base(c);
            const subexpr = c.ast.create(Ast.Expr);
            subexpr.* = .{
                .line_info = tok.line_info,
                .as = .{ .Ref = subsubexpr },
                .typ = Ast.void_type,
                .flags = .{},
                .typechecking = .None,
            };
            subexpr.line_info.column += 1;
            subexpr.line_info.offset += 1;
            const expr = c.ast.create(Ast.Expr);
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
            const subexpr = parse_expr_base(c);
            const expr = c.ast.create(Ast.Expr);
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
            const subexpr = parse_expr_base(c);
            const expr = c.ast.create(Ast.Expr);
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
            const expr = parse_expr(c);
            expr.line_info = tok.line_info;
            Lexer.expect(c, .Close_Paren);
            return expr;
        },
        .If => {
            const condition = parse_expr(c);
            if (Lexer.peek(c) == .Then) {
                Lexer.advance(c);
            }
            const true_branch = parse_expr(c);
            Lexer.expect(c, .Else);
            const false_branch = parse_expr(c);

            const expr = c.ast.create(Ast.Expr);
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
            const expr = c.ast.create(Ast.Expr);
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
            const expr = c.ast.create(Ast.Expr);
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
            const expr = c.ast.create(Ast.Expr);
            expr.* = .{
                .line_info = tok.line_info,
                .as = .{ .Identifier = .{
                    .name = name,
                    .scope = c.parser.current_scope,
                } },
                .typ = Ast.void_type,
                .flags = .{},
                .typechecking = .None,
            };
            return expr;
        },
        .Integer => |value| {
            const typ = Ast.integer_type_from_u64(value);
            const expr = c.ast.create(Ast.Expr);
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
            const count = parse_fixed_size_expr_list(c, &exprs);

            switch (count) {
                1 => {
                    const expr = c.ast.create(Ast.Expr);
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
                    c.report_error(tok.line_info, "expected 1 argument, but got {}", .{count});
                    Compiler.exit(1);
                },
            }
        },
        .Alignment_Of => {
            var exprs: [1]*Ast.Expr = undefined;
            const count = parse_fixed_size_expr_list(c, &exprs);

            switch (count) {
                1 => {
                    const expr = c.ast.create(Ast.Expr);
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
                    c.report_error(tok.line_info, "expected 1 argument, but got {}", .{count});
                    Compiler.exit(1);
                },
            }
        },
        .As => {
            var exprs: [2]*Ast.Expr = undefined;
            const count = parse_fixed_size_expr_list(c, &exprs);

            switch (count) {
                2 => {
                    const typ = expr_to_type(c, exprs[0]);
                    const expr = c.ast.create(Ast.Expr);
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
                    c.report_error(tok.line_info, "expected 2 arguments, but got {}", .{count});
                    Compiler.exit(1);
                },
            }
        },
        .Cast => {
            var exprs: [2]*Ast.Expr = undefined;
            const count = parse_fixed_size_expr_list(c, &exprs);

            switch (count) {
                2 => {
                    const typ = expr_to_type(c, exprs[0]);
                    const expr = c.ast.create(Ast.Expr);
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
                    c.report_error(tok.line_info, "expected 2 arguments, but got {}", .{count});
                    Compiler.exit(1);
                },
            }
        },
        else => {
            Lexer.putback(c, tok);

            var typ: Ast.Type = undefined;
            if (!try_parse_type_by_value(c, &typ)) {
                c.report_error(tok.line_info, "token doesn't start an expression.", .{});
                Compiler.exit(1);
            }

            const expr = c.ast.create(Ast.Expr);
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

fn parse_fixed_size_expr_list(c: *Compiler, dst: []*Ast.Expr) usize {
    Lexer.expect(c, .Open_Paren);

    var count: usize = 0;

    var tt = Lexer.peek(c);
    while (tt != .End_Of_File and tt != .Close_Paren) {
        const expr = parse_expr(c);

        if (count < dst.len) {
            dst[count] = expr;
        }

        count += 1;

        tt = Lexer.peek(c);
        if (tt != .End_Of_File and tt != .Close_Paren) {
            Lexer.expect(c, .Comma);
            tt = Lexer.peek(c);
        }
    }

    Lexer.expect(c, .Close_Paren);

    return count;
}

fn parse_expr_list(c: *Compiler) Ast.ExprList {
    Lexer.expect(c, .Open_Paren);

    var list = Ast.ExprList{};

    var tt = Lexer.peek(c);
    while (tt != .End_Of_File and tt != .Close_Paren) {
        const expr_node = c.ast.create(Ast.ExprListNode);
        expr_node.* = expr_node: {
            const lhs = parse_expr(c);
            if (Lexer.peek(c) == .Equal) {
                Lexer.advance(c);
                const rhs = parse_expr(c);

                break :expr_node .{ .Designator = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } };
            }
            break :expr_node .{ .Expr = lhs };
        };

        {
            const node = c.ast.create(Ast.ExprList.Node);
            node.* = .{
                .data = expr_node,
            };
            list.append(node);
        }

        tt = Lexer.peek(c);
        if (tt != .End_Of_File and tt != .Close_Paren) {
            Lexer.expect(c, .Comma);
            tt = Lexer.peek(c);
        }
    }

    Lexer.expect(c, .Close_Paren);

    return list;
}

// Assume 'expr' is heap allocated and can be reused.
fn expr_to_type(c: *Compiler, expr: *Ast.Expr) *Ast.Type {
    switch (expr.as) {
        .Deref => |subexpr| {
            const subtype = expr_to_type(c, subexpr);

            const data = c.ast.create(Ast.Type.SharedData);
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
            const subtype = expr_to_type(c, Subscript.subexpr);

            const data = c.ast.create(Ast.Type.SharedData);
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
            const data = c.ast.create(Ast.Type.SharedData);
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
            c.report_error(expr.line_info, "expected expression, not a type", .{});
            Compiler.exit(1);
        },
    }
}

fn push_scope(c: *Compiler) void {
    const scope = c.ast.create(Ast.Scope);
    scope.* = .{
        .parent = c.parser.current_scope,
    };
    c.parser.current_scope = scope;
}

fn pop_scope(c: *Compiler) void {
    c.parser.current_scope = c.parser.current_scope.parent.?;
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
