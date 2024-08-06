const std = @import("std");
const common = @import("common.zig");
const Lexer = @import("lexer.zig");
const Ast = @import("ast.zig");
const Ir = @import("ircode.zig");

const ArenaAllocator = std.heap.ArenaAllocator;
const Allocator = std.mem.Allocator;
const Token = Lexer.Token;

lexer: Lexer,
ast_arena: ArenaAllocator,
ast_arena_allocator: Allocator,
symbols: Ast.SymbolTable,

current_scope: *Ast.Scope,
function_depth: Ast.FunctionDepth = 0,
next_label: Ir.Label = 0,

const This = @This();

const LOWEST_PREC = -127;

fn grab_label(p: *This) Ir.Label {
    const result = p.next_label;
    p.next_label += 1;
    return result;
}

pub fn parse(filepath: [:0]const u8) Ast {
    var ast_arena = ArenaAllocator.init(std.heap.page_allocator);
    const ast_arena_allocator = ast_arena.allocator();
    const global_scope = ast_arena_allocator.create(Ast.Scope) catch {
        common.exit(1);
    };
    global_scope.* = .{
        .parent = null,
    };

    var parser: This = .{
        .lexer = Lexer.init(filepath),
        .ast_arena = ast_arena,
        .ast_arena_allocator = ast_arena_allocator,
        .symbols = Ast.SymbolTable.init(common.gpa),
        .current_scope = global_scope,
    };
    var p = &parser;
    defer parser.lexer.deinit();

    var globals: Ast.SymbolList = .{};

    while (p.lexer.peek() != .End_Of_File) {
        const has_symbol = parse_symbol(p);
        if (has_symbol) |symbol| {
            const node = create(p, Ast.SymbolList.Node);
            node.* = .{
                .payload = symbol,
            };
            globals.insert_last(node);
        } else {
            const line_info = p.lexer.grab().line_info;
            common.print_error(p.lexer.filepath, line_info, "expected ':' or ':=' to start definition.", .{});
            common.exit(1);
        }
    }

    std.debug.assert(p.current_scope == global_scope);

    return .{
        .globals = globals,
        .locals = .{},
        .main = undefined,
        .symbols = p.symbols,
        .ast_arena = ast_arena,
        .global_scope = global_scope,
        .next_label = p.next_label,
        .filepath = filepath,
    };
}

fn create(p: *This, comptime T: type) *T {
    return p.ast_arena_allocator.create(T) catch {
        common.exit(1);
    };
}

fn create_scope(p: *This) *Ast.Scope {
    const result = create(p, Ast.Scope);
    result.* = .{
        .parent = p.current_scope,
    };
    return result;
}

fn push_scope(p: *This) void {
    const scope = create_scope(p);
    p.current_scope = scope;
}

fn pop_scope(p: *This) void {
    p.current_scope = p.current_scope.parent.?;
}

fn create_symbol(p: *This, token: Token) *Ast.Symbol {
    const symbol = create(p, Ast.Symbol);
    symbol.* = .{
        .payload = undefined,
        .key = .{
            .text = token.as.Identifier,
            .scope = p.current_scope,
        },
        .depth = p.function_depth,
        .line_info = token.line_info,
    };
    return symbol;
}

fn insert_symbol(p: *This, id: Token) *Ast.Symbol {
    std.debug.assert(id.as == .Identifier);

    // Should create arena with identifier strings?
    const symbol = create_symbol(p, id);
    insert_existing_symbol(p, symbol);

    return symbol;
}

fn insert_existing_symbol(p: *This, symbol: *Ast.Symbol) void {
    const get_or_put_result = p.symbols.getOrPut(symbol.key) catch {
        common.exit(1);
    };

    if (get_or_put_result.found_existing) {
        common.print_error(p.lexer.filepath, symbol.line_info, "symbol '{s}' is already defined.", .{symbol.key.text});
        common.print_note(p.lexer.filepath, get_or_put_result.value_ptr.*.line_info, "first defined here.", .{});
        common.exit(1);
    }

    get_or_put_result.value_ptr.* = symbol;
}

fn extract_type(p: *This, expr: Ast.Expr) *Ast.Type {
    const ok = Ast.extract_type(p.ast_arena_allocator, expr);
    if (ok) |_type| {
        return _type;
    } else {
        common.print_error(p.lexer.filepath, expr.line_info, "expression doesn't look like a type.", .{});
        common.exit(1);
    }
}

fn parse_type(p: *This) *Ast.Type {
    return extract_type(p, parse_expr(p));
}

fn parse_type_function(p: *This) Ast.TypePayload {
    p.lexer.expect(.Open_Paren);

    var params = Ast.SymbolList{};

    var tt = p.lexer.peek();
    while (tt != .End_Of_File and tt != .Close_Paren) {
        const id = p.lexer.grab();
        var has_id = false;

        if (p.lexer.peek() == .Identifier) {
            p.lexer.advance();
            p.lexer.expect(.Colon);
            has_id = true;
        }

        const _type = parse_type(p);
        const symbol = create_symbol(p, id);
        symbol.payload = .{ .Parameter = .{
            ._type = _type,
            .has_id = has_id,
            .tmp = undefined,
        } };

        const node = create(p, Ast.SymbolList.Node);
        node.* = .{
            .payload = symbol,
        };
        params.insert_last(node);

        tt = p.lexer.peek();
        if (tt != .End_Of_File and tt != .Close_Paren) {
            p.lexer.expect(.Comma);
            tt = p.lexer.peek();
        }
    }

    p.lexer.expect(.Close_Paren);

    var return_type: *Ast.Type = undefined;

    if (p.lexer.peek() == .Arrow) {
        p.lexer.advance();
        return_type = parse_type(p);
    } else {
        return_type = create(p, Ast.Type);
        return_type.* = .{
            .payload = .Void,
            .size = undefined,
            .line_info = p.lexer.grab().line_info,
        };
    }

    return .{ .Function = .{
        .params = params,
        .return_type = return_type,
        .scope = p.current_scope,
    } };
}

fn parse_struct_fields(p: *This) Ast.TypeStruct {
    p.lexer.expect(.Open_Curly);

    push_scope(p);

    var _struct = Ast.TypeStruct{
        .fields = .{},
        .scope = p.current_scope,
    };

    var tt = p.lexer.peek();
    while (tt != .End_Of_File and tt != .Close_Curly) {
        const id = p.lexer.grab();
        p.lexer.expect(.Identifier);
        p.lexer.expect(.Colon);

        const _type = parse_type(p);
        const symbol = insert_symbol(p, id);
        symbol.payload = .{ .Struct_Field = .{
            ._type = _type,
        } };

        const node = create(p, Ast.SymbolList.Node);
        node.* = .{
            .payload = symbol,
        };
        _struct.fields.insert_last(node);

        tt = p.lexer.peek();
        if (tt != .End_Of_File and tt != .Close_Curly) {
            p.lexer.expect(.Comma);
            tt = p.lexer.peek();
        }
    }

    pop_scope(p);

    p.lexer.expect(.Close_Curly);

    return _struct;
}

fn parse_symbol(p: *This) ?*Ast.Symbol {
    if (p.lexer.peek() != .Identifier) return null;

    switch (p.lexer.peek_ahead(1)) {
        .Colon_Equal => {
            const id = p.lexer.grab();
            const symbol = insert_symbol(p, id);
            p.lexer.advance_many(2);

            if (p.lexer.peek() == .Proc) {
                p.function_depth += 1;

                const _type = parse_type(p);

                if (p.lexer.peek() == .Open_Curly) {
                    push_scope(p);

                    var it = _type.payload.Function.params.iterator();
                    while (it.next()) |param_ptr| {
                        const param = &param_ptr.*.payload.Parameter;
                        if (param.has_id) {
                            param_ptr.*.key.scope = p.current_scope;
                            insert_existing_symbol(p, param_ptr.*);
                        }
                    }

                    const block = parse_stmt_block(p);

                    symbol.payload = .{ .Function = .{
                        ._type = _type,
                        .block = block,
                        .label = p.grab_label(),
                        .depth = p.function_depth,
                    } };

                    pop_scope(p);
                } else {
                    p.lexer.expect(.Semicolon);
                    symbol.payload = .{ .Type = _type };
                }

                p.function_depth -= 1;

                return symbol;
            } else {
                const expr = create(p, Ast.Expr);
                expr.* = parse_expr(p);
                p.lexer.expect(.Semicolon);

                symbol.payload = .{ .Definition = .{
                    .expr = expr,
                } };

                return symbol;
            }
        },
        .Colon => {
            const id = p.lexer.grab();
            const symbol = insert_symbol(p, id);
            p.lexer.advance_many(2);

            var expr: ?*Ast.Expr = null;
            const _type = parse_type(p);

            if (p.lexer.peek() == .Equal) {
                p.lexer.advance();
                expr = create(p, Ast.Expr);
                expr.?.* = parse_expr(p);
            }

            p.lexer.expect(.Semicolon);

            symbol.payload = .{ .Variable = .{
                ._type = _type,
                .expr = expr,
                .tmp = undefined,
            } };

            return symbol;
        },
        else => {
            return null;
        },
    }
}

fn parse_expr_list(p: *This) Ast.ExprList {
    p.lexer.expect(.Open_Curly);

    var result = Ast.ExprList{};

    var tt = p.lexer.peek();
    while (tt != .End_Of_File and tt != .Close_Curly) {
        const expr: Ast.Expr = expr: {
            if (p.lexer.peek() == .Identifier and
                p.lexer.peek_ahead(1) == .Equal)
            {
                const id = p.lexer.grab();
                p.lexer.advance_many(2);

                const value = create(p, Ast.Expr);
                value.* = parse_expr(p);

                break :expr .{
                    .payload = .{ .Designator = .{
                        .id = id,
                        .expr = value,
                    } },
                    ._type = undefined,
                    .line_info = id.line_info,
                };
            } else {
                break :expr parse_expr(p);
            }
        };

        const node = create(p, Ast.ExprList.Node);
        node.* = .{
            .payload = expr,
        };
        result.insert_last(node);

        tt = p.lexer.peek();
        if (tt != .End_Of_File and tt != .Close_Curly) {
            p.lexer.expect(.Comma);
            tt = p.lexer.peek();
        }
    }

    p.lexer.expect(.Close_Curly);

    return result;
}

fn parse_expr(p: *This) Ast.Expr {
    return parse_prec(p, LOWEST_PREC);
}

fn parse_prec(p: *This, min_prec: i32) Ast.Expr {
    var lhs = parse_highest_prec(p);
    var op = p.lexer.peek();
    var prev_prec: i32 = 0x7FFF_FFFF;
    var curr_prec: i32 = prec_of_op(op);

    while (curr_prec < prev_prec and curr_prec >= min_prec) {
        while (curr_prec == prec_of_op(op)) {
            p.lexer.advance();

            const lhs_ptr = create(p, Ast.Expr);
            const rhs_ptr = create(p, Ast.Expr);
            lhs_ptr.* = lhs;
            rhs_ptr.* = parse_prec(p, curr_prec + 1);
            lhs = .{
                .payload = .{ .Binary_Op = .{
                    .tag = token_tag_to_expr_binary_op_tag(op),
                    .lhs = lhs_ptr,
                    .rhs = rhs_ptr,
                } },
                ._type = undefined,
                .line_info = lhs_ptr.line_info,
            };

            op = p.lexer.peek();
        }

        prev_prec = curr_prec;
        curr_prec = prec_of_op(op);
    }

    return lhs;
}

fn parse_highest_prec_base(p: *This) Ast.Expr {
    const tok = p.lexer.grab();
    p.lexer.advance();

    switch (tok.as) {
        .Sub, .Ref, .Not => {
            const subexpr = create(p, Ast.Expr);
            subexpr.* = parse_highest_prec_base(p);

            return .{
                .payload = .{ .Unary_Op = .{
                    .tag = token_tag_to_expr_unary_op_tag(tok.as),
                    .subexpr = subexpr,
                } },
                ._type = undefined,
                .line_info = tok.line_info,
            };
        },
        .And => {
            common.print_error(p.lexer.filepath, tok.line_info, "can't take a reference of rvalue.", .{});
            common.exit(1);
        },
        .Open_Paren => {
            const expr = parse_expr(p);
            p.lexer.expect(.Close_Paren);

            return .{
                .payload = expr.payload,
                ._type = undefined,
                .line_info = tok.line_info,
            };
        },
        .Dot => {
            switch (p.lexer.peek()) {
                .Open_Curly => return .{
                    .payload = .{ .Expr_List = parse_expr_list(p) },
                    ._type = undefined,
                    .line_info = tok.line_info,
                },
                .Identifier => {
                    const id = p.lexer.grab();
                    p.lexer.advance();

                    return .{
                        .payload = .{ .Enum_Field = id },
                        ._type = undefined,
                        .line_info = tok.line_info,
                    };
                },
                else => {
                    const line_info = p.lexer.grab().line_info;
                    common.print_error(p.lexer.filepath, line_info, "expected identifier or '{{' after '.' operator.", .{});
                    common.print_note(p.lexer.filepath, line_info, "expected designator list or identifier.", .{});
                    common.exit(1);
                },
            }
        },
        .If => {
            const cond = create(p, Ast.Expr);
            const if_true = create(p, Ast.Expr);
            const if_false = create(p, Ast.Expr);

            cond.* = parse_expr(p);
            if (p.lexer.peek() == .Then) {
                p.lexer.advance();
            }
            if_true.* = parse_expr(p);
            p.lexer.expect(.Else);
            if_false.* = parse_expr(p);

            return .{
                .payload = .{ .If = .{
                    .cond = cond,
                    .if_true = if_true,
                    .if_false = if_false,
                } },
                ._type = undefined,
                .line_info = tok.line_info,
            };
        },
        .Boolean => |value| {
            return .{
                .payload = .{ .Bool = value },
                ._type = undefined,
                .line_info = tok.line_info,
            };
        },
        .Null => {
            return .{
                .payload = .Null,
                ._type = undefined,
                .line_info = tok.line_info,
            };
        },
        .Identifier => {
            return .{
                .payload = .{ .Identifier = .{
                    .token = tok,
                    .scope = p.current_scope,
                } },
                ._type = undefined,
                .line_info = tok.line_info,
            };
        },
        .Integer => |value| {
            return .{
                .payload = .{ .Int64 = @bitCast(value) },
                ._type = undefined,
                .line_info = tok.line_info,
            };
        },
        .Mul => {
            const subtype = parse_type(p);
            const _type = create(p, Ast.Type);
            _type.* = .{
                .payload = .{ .Pointer = subtype },
                .size = undefined,
                .line_info = tok.line_info,
            };

            return .{
                .payload = .{ .Type = _type },
                ._type = undefined,
                .line_info = tok.line_info,
            };
        },
        .Open_Bracket => {
            const expr = create(p, Ast.Expr);
            expr.* = parse_expr(p);
            p.lexer.expect(.Close_Bracket);

            const subtype = parse_type(p);
            const _type = create(p, Ast.Type);
            _type.* = .{
                .payload = .{ .Array = .{
                    .expr = expr,
                    .count = undefined,
                    .subtype = subtype,
                } },
                .size = undefined,
                .line_info = tok.line_info,
            };

            return .{
                .payload = .{ .Type = _type },
                ._type = undefined,
                .line_info = tok.line_info,
            };
        },
        .Struct => {
            const _struct = parse_struct_fields(p);
            const _type = create(p, Ast.Type);
            _type.* = .{
                .payload = .{ .Struct = _struct },
                .size = undefined,
                .line_info = tok.line_info,
            };

            return .{
                .payload = .{ .Type = _type },
                ._type = undefined,
                .line_info = tok.line_info,
            };
        },
        .Union => {
            const _union = parse_struct_fields(p);
            const _type = create(p, Ast.Type);
            _type.* = .{
                .payload = .{ .Union = _union },
                .size = undefined,
                .line_info = tok.line_info,
            };

            return .{
                .payload = .{ .Type = _type },
                ._type = undefined,
                .line_info = tok.line_info,
            };
        },
        .Enum => {
            p.lexer.expect(.Open_Curly);

            push_scope(p);

            var _enum = Ast.TypeStruct{
                .fields = .{},
                .scope = p.current_scope,
            };

            var tt = p.lexer.peek();
            while (tt != .End_Of_File and tt != .Close_Curly) {
                const id = p.lexer.grab();
                p.lexer.expect(.Identifier);

                const symbol = insert_symbol(p, id);
                symbol.payload = .{ .Enum_Field = .{
                    ._type = undefined,
                    .value = undefined,
                } };

                const node = create(p, Ast.SymbolList.Node);
                node.* = .{
                    .payload = symbol,
                };
                _enum.fields.insert_last(node);

                tt = p.lexer.peek();
                if (tt != .End_Of_File and tt != .Close_Curly) {
                    p.lexer.expect(.Comma);
                    tt = p.lexer.peek();
                }
            }

            pop_scope(p);

            p.lexer.expect(.Close_Curly);

            const _type = create(p, Ast.Type);
            _type.* = .{
                .payload = .{ .Enum = _enum },
                .size = undefined,
                .line_info = tok.line_info,
            };

            return .{
                .payload = .{ .Type = _type },
                ._type = undefined,
                .line_info = tok.line_info,
            };
        },
        .Proc => {
            const _type = create(p, Ast.Type);
            _type.* = .{
                .payload = parse_type_function(p),
                .size = undefined,
                .line_info = tok.line_info,
            };

            return .{
                .payload = .{ .Type = _type },
                ._type = undefined,
                .line_info = tok.line_info,
            };
        },
        .Void_Type => {
            const _type = create(p, Ast.Type);
            _type.* = .{
                .payload = .Void,
                .size = undefined,
                .line_info = tok.line_info,
            };
            return .{
                .payload = .{ .Type = _type },
                ._type = undefined,
                .line_info = tok.line_info,
            };
        },
        .Bool_Type => {
            const _type = create(p, Ast.Type);
            _type.* = .{
                .payload = .Bool,
                .size = undefined,
                .line_info = tok.line_info,
            };
            return .{
                .payload = .{ .Type = _type },
                ._type = undefined,
                .line_info = tok.line_info,
            };
        },
        .Integer_Type => {
            const _type = create(p, Ast.Type);
            _type.* = .{
                .payload = .Int64,
                .size = undefined,
                .line_info = tok.line_info,
            };
            return .{
                .payload = .{ .Type = _type },
                ._type = undefined,
                .line_info = tok.line_info,
            };
        },
        .Cast => {
            p.lexer.expect(.Open_Paren);

            const lhs = parse_expr(p);

            if (p.lexer.peek() == .Comma) {
                p.lexer.advance();
            }

            if (p.lexer.peek() == .Close_Paren) {
                p.lexer.advance();

                const expr = create(p, Ast.Expr);
                expr.* = lhs;
                return .{
                    .payload = .{ .Cast1 = expr },
                    ._type = undefined,
                    .line_info = tok.line_info,
                };
            } else {
                const _type = extract_type(p, lhs);
                const rhs = create(p, Ast.Expr);
                rhs.* = parse_expr(p);

                if (p.lexer.peek() == .Comma) {
                    p.lexer.advance();
                }

                p.lexer.expect(.Close_Paren);

                return .{
                    .payload = .{ .Cast2 = .{
                        ._type = _type,
                        .expr = rhs,
                    } },
                    ._type = undefined,
                    .line_info = tok.line_info,
                };
            }
        },
        else => {
            common.print_error(p.lexer.filepath, tok.line_info, "token doesn't start an expression.", .{});
            common.exit(1);
        },
    }
}

fn parse_highest_prec(p: *This) Ast.Expr {
    var lhs = parse_highest_prec_base(p);
    parse_postfix_unary_ops(p, &lhs);
    return lhs;
}

fn parse_postfix_unary_ops(p: *This, inner: *Ast.Expr) void {
    while (true) {
        switch (p.lexer.peek()) {
            .Open_Paren => {
                const line_info = p.lexer.grab().line_info;
                p.lexer.advance();

                var args = Ast.ExprList{};

                var tt = p.lexer.peek();
                while (tt != .End_Of_File and tt != .Close_Paren) {
                    const expr = parse_expr(p);
                    const node = create(p, Ast.ExprList.Node);
                    node.* = .{
                        .payload = expr,
                    };
                    args.insert_last(node);

                    tt = p.lexer.peek();
                    if (tt != .End_Of_File and tt != .Close_Paren) {
                        p.lexer.expect(.Comma);
                        tt = p.lexer.peek();
                    }
                }

                p.lexer.expect(.Close_Paren);

                const lhs = create(p, Ast.Expr);
                lhs.* = inner.*;
                inner.* = .{
                    .payload = .{ .Call = .{
                        .lhs = lhs,
                        .args = args,
                    } },
                    ._type = undefined,
                    .line_info = line_info,
                };
            },
            .Open_Bracket => {
                const line_info = p.lexer.grab().line_info;
                p.lexer.advance();

                const expr = create(p, Ast.Expr);
                expr.* = parse_expr(p);
                p.lexer.expect(.Close_Bracket);

                const lhs = create(p, Ast.Expr);
                lhs.* = inner.*;
                inner.* = .{
                    .payload = .{ .Index = .{
                        .lhs = lhs,
                        .index = expr,
                    } },
                    ._type = undefined,
                    .line_info = line_info,
                };
            },
            .Dot => {
                p.lexer.advance();

                if (p.lexer.peek() == .Mul) {
                    p.lexer.advance();

                    const lhs = create(p, Ast.Expr);
                    lhs.* = inner.*;
                    inner.* = .{
                        .payload = .{ .Unary_Op = .{
                            .tag = .Deref,
                            .subexpr = lhs,
                        } },
                        ._type = undefined,
                        .line_info = lhs.line_info,
                    };
                } else if (p.lexer.peek() == .Open_Curly) {
                    const expr_list = parse_expr_list(p);
                    const _type = extract_type(p, inner.*);
                    inner.* = .{
                        .payload = .{ .Initializer = .{
                            ._type = _type,
                            .expr_list = expr_list,
                        } },
                        ._type = undefined,
                        .line_info = _type.line_info,
                    };
                } else {
                    const id = p.lexer.grab();
                    p.lexer.expect(.Identifier);

                    const lhs = create(p, Ast.Expr);
                    lhs.* = inner.*;
                    inner.* = .{
                        .payload = .{ .Field = .{
                            .lhs = lhs,
                            .id = id,
                        } },
                        ._type = undefined,
                        .line_info = id.line_info,
                    };
                }
            },
            else => {
                break;
            },
        }
    }
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

fn token_tag_to_expr_binary_op_tag(op: Token.Tag) Ast.ExprBinaryOpTag {
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

fn token_tag_to_expr_unary_op_tag(op: Token.Tag) Ast.ExprUnaryOpTag {
    return switch (op) {
        .Sub => .Neg,
        .Ref => .Ref,
        .Not => .Not,
        else => unreachable,
    };
}

fn parse_stmt(p: *This) Ast.Stmt {
    const line_info = p.lexer.grab().line_info;

    switch (p.lexer.peek()) {
        .Print => {
            p.lexer.advance();

            const expr = create(p, Ast.Expr);
            expr.* = parse_expr(p);
            p.lexer.expect(.Semicolon);

            return .{
                .payload = .{ .Print = expr },
                .line_info = line_info,
            };
        },
        .Open_Curly => {
            push_scope(p);
            const block = parse_stmt_block(p);
            pop_scope(p);

            return .{
                .payload = .{ .Block = block },
                .line_info = line_info,
            };
        },
        .If => {
            p.lexer.advance();

            const cond = create(p, Ast.Expr);
            cond.* = parse_expr(p);

            if (p.lexer.peek() == .Then) {
                p.lexer.advance();
            }

            const if_true = create(p, Ast.Stmt);
            var if_false: ?*Ast.Stmt = null;

            if_true.* = parse_stmt(p);
            if (p.lexer.peek() == .Else) {
                p.lexer.advance();
                if_false = create(p, Ast.Stmt);
                if_false.?.* = parse_stmt(p);
            }

            return .{
                .payload = .{ .If = .{
                    .cond = cond,
                    .if_true = if_true,
                    .if_false = if_false,
                } },
                .line_info = line_info,
            };
        },
        .While => {
            p.lexer.advance();

            const cond = create(p, Ast.Expr);
            cond.* = parse_expr(p);

            if (p.lexer.peek() == .Do) {
                p.lexer.advance();
            }

            const block = create(p, Ast.Stmt);
            block.* = parse_stmt(p);

            return .{
                .payload = .{ .While = .{
                    .cond = cond,
                    .block = block,
                } },
                .line_info = line_info,
            };
        },
        .Do => {
            p.lexer.advance();

            const block = create(p, Ast.Stmt);
            block.* = parse_stmt(p);

            p.lexer.expect(.While);

            const cond = create(p, Ast.Expr);
            cond.* = parse_expr(p);

            p.lexer.expect(.Semicolon);

            return .{
                .payload = .{ .While = .{
                    .block = block,
                    .cond = cond,
                    .is_do_while = true,
                } },
                .line_info = line_info,
            };
        },
        .Break => {
            p.lexer.advance();
            p.lexer.expect(.Semicolon);
            return .{
                .payload = .Break,
                .line_info = line_info,
            };
        },
        .Continue => {
            p.lexer.advance();
            p.lexer.expect(.Semicolon);
            return .{
                .payload = .Continue,
                .line_info = line_info,
            };
        },
        .Switch => {
            p.lexer.advance();

            const cond = create(p, Ast.Expr);
            cond.* = parse_expr(p);

            push_scope(p);
            const block = parse_stmt_block(p);
            pop_scope(p);

            return .{
                .payload = .{ .Switch = .{
                    .cond = cond,
                    .cases = block,
                } },
                .line_info = line_info,
            };
        },
        .Return => {
            p.lexer.advance();

            if (p.lexer.peek() == .Semicolon) {
                p.lexer.advance();

                return .{
                    .payload = .Return,
                    .line_info = line_info,
                };
            }

            const expr = create(p, Ast.Expr);
            expr.* = parse_expr(p);
            p.lexer.expect(.Semicolon);

            return .{
                .payload = .{ .Return_Expr = expr },
                .line_info = line_info,
            };
        },
        else => {
            const has_symbol = parse_symbol(p);

            if (has_symbol) |symbol| {
                return .{
                    .payload = .{ .Symbol = symbol },
                    .line_info = line_info,
                };
            } else {
                const lhs = create(p, Ast.Expr);
                lhs.* = parse_expr(p);

                switch (p.lexer.peek()) {
                    .Equal => {
                        p.lexer.advance();

                        const rhs = create(p, Ast.Expr);
                        rhs.* = parse_expr(p);
                        p.lexer.expect(.Semicolon);

                        return .{
                            .payload = .{ .Assign = .{
                                .lhs = lhs,
                                .rhs = rhs,
                            } },
                            .line_info = line_info,
                        };
                    },
                    .Colon => {
                        p.lexer.advance();

                        const stmt = create(p, Ast.Stmt);
                        stmt.* = parse_stmt(p);

                        return .{
                            .payload = .{ .Case = .{
                                .expr = lhs,
                                .stmt = stmt,
                            } },
                            .line_info = line_info,
                        };
                    },
                    else => {
                        p.lexer.expect(.Semicolon);

                        return .{
                            .payload = .{ .Expr = lhs },
                            .line_info = line_info,
                        };
                    },
                }
            }
        },
    }
}

fn parse_stmt_block(p: *This) Ast.StmtBlock {
    p.lexer.expect(.Open_Curly);

    var block = Ast.StmtBlock{};

    var tt = p.lexer.peek();
    while (tt != .End_Of_File and tt != .Close_Curly) {
        const stmt = parse_stmt(p);
        const node = create(p, Ast.StmtBlock.Node);
        node.* = .{
            .payload = stmt,
        };
        block.insert_last(node);

        tt = p.lexer.peek();
    }

    p.lexer.expect(.Close_Curly);

    return block;
}
