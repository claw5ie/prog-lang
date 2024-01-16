const std = @import("std");
const common = @import("common.zig");
const Lexer = @import("lexer.zig");
const Ast = @import("ast.zig");
const Ir = @import("ircode.zig");

const ArenaAllocator = std.heap.ArenaAllocator;
const Allocator = std.mem.Allocator;

lexer: Lexer,
ast_arena: ArenaAllocator,
ast_arena_allocator: Allocator,
symbols: Ast.SymbolTable,

current_scope: *Ast.Scope,
function_depth: Ast.FunctionDepth = 0,
next_label: Ir.Label = 0,

const This = @This();

const LOWEST_PREC = -127;

pub fn grab_label(p: *This) Ir.Label {
    var result = p.next_label;
    p.next_label += 1;
    return result;
}

inline fn grab(p: *This) Lexer.Token {
    return p.lexer.grab();
}

inline fn expect(p: *This, expected: Lexer.TokenTag) void {
    return p.lexer.expect(expected);
}

inline fn peek(p: *This) Lexer.TokenTag {
    return p.lexer.peek();
}

inline fn peek_ahead(p: *This, index: u8) Lexer.TokenTag {
    return p.lexer.peek_ahead(index);
}

inline fn advance(p: *This) void {
    return p.lexer.advance();
}

inline fn advance_many(p: *This, count: u8) void {
    return p.lexer.advance_many(count);
}

pub fn parse(filepath: []const u8) Ast {
    var ast_arena = ArenaAllocator.init(std.heap.page_allocator);
    var ast_arena_allocator = ast_arena.allocator();
    var global_scope = ast_arena_allocator.create(Ast.Scope) catch {
        std.os.exit(1);
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

    var globals: Ast.SymbolList = .{};

    while (p.peek() != .End_Of_File) {
        var symbol = parse_symbol(p);
        var node = parser_create(p, Ast.SymbolList.Node);
        node.* = .{
            .payload = symbol,
        };
        globals.insert_last(node);
    }

    std.debug.assert(p.current_scope == global_scope);

    return .{
        .globals = globals,
        .locals = .{},
        .ast_arena = ast_arena,
        .symbols = p.symbols,
        .global_scope = global_scope,
        .next_label = p.next_label,
        .filepath = filepath,
    };
}

fn parser_create(p: *This, comptime T: type) *T {
    return p.ast_arena_allocator.create(T) catch {
        std.os.exit(1);
    };
}

fn create_scope(p: *This) *Ast.Scope {
    var result = parser_create(p, Ast.Scope);
    result.* = .{
        .parent = p.current_scope,
    };
    return result;
}

fn push_scope(p: *This) void {
    var scope = create_scope(p);
    p.current_scope = scope;
}

fn pop_scope(p: *This) void {
    p.current_scope = p.current_scope.parent.?;
}

fn create_symbol(p: *This, token: Lexer.Token) *Ast.Symbol {
    var symbol = parser_create(p, Ast.Symbol);
    symbol.* = .{
        .payload = undefined,
        .key = .{
            .text = token.text,
            .scope = p.current_scope,
        },
        .depth = p.function_depth,
        .line_info = token.line_info,
    };
    return symbol;
}

fn insert_symbol(p: *This, id: Lexer.Token) *Ast.Symbol {
    std.debug.assert(id.tag == .Identifier);

    // Should create arena with identifier strings?
    var symbol = create_symbol(p, id);
    insert_existing_symbol(p, symbol);

    return symbol;
}

fn insert_existing_symbol(p: *This, symbol: *Ast.Symbol) void {
    var get_or_put_result = p.symbols.getOrPut(symbol.key) catch {
        std.os.exit(1);
    };

    if (get_or_put_result.found_existing) {
        common.print_error(p.lexer.filepath, symbol.line_info, "symbol '{s}' is already defined.", .{symbol.key.text});
        common.print_note(p.lexer.filepath, get_or_put_result.value_ptr.*.line_info, "first defined here.", .{});
        std.os.exit(1);
    }

    get_or_put_result.value_ptr.* = symbol;
}

fn extract_type(p: *This, expr: Ast.Expr) *Ast.Type {
    var ok = Ast.extract_type(p.ast_arena_allocator, expr);
    if (ok) |_type| {
        return _type;
    } else {
        common.print_error(p.lexer.filepath, expr.line_info, "expression doesn't look like a type.", .{});
        std.os.exit(1);
    }
}

fn parse_type(p: *This) *Ast.Type {
    return extract_type(p, parse_expr(p));
}

fn parse_type_function(p: *This) Ast.TypePayload {
    p.expect(.Open_Paren);

    push_scope(p);

    var params = Ast.SymbolList{};

    var tt = p.peek();
    while (tt != .End_Of_File and tt != .Close_Paren) {
        var id = p.grab();
        var has_id = false;

        if (p.peek() == .Identifier) {
            p.advance();
            p.expect(.Colon);
            has_id = true;
        }

        var _type = parse_type(p);
        var symbol = create_symbol(p, id);
        symbol.payload = .{ .Parameter = .{
            ._type = _type,
            .has_id = has_id,
            .tmp = undefined,
        } };

        var node = parser_create(p, Ast.SymbolList.Node);
        node.* = .{
            .payload = symbol,
        };
        params.insert_last(node);

        tt = p.peek();
        if (tt != .End_Of_File and tt != .Close_Paren) {
            p.expect(.Comma);
            tt = p.peek();
        }
    }

    p.expect(.Close_Paren);

    var return_type: *Ast.Type = undefined;

    if (p.peek() == .Arrow) {
        p.advance();
        return_type = parse_type(p);
    } else {
        return_type = parser_create(p, Ast.Type);
        return_type.* = .{
            .payload = .Void,
            .size = undefined,
            .line_info = p.grab().line_info,
        };
    }

    var scope = p.current_scope;

    pop_scope(p);

    return .{ .Function = .{
        .params = params,
        .return_type = return_type,
        .scope = scope,
    } };
}

fn parse_struct_fields(p: *This, is_struct: bool) Ast.TypeStruct {
    p.expect(.Open_Curly);

    push_scope(p);

    var _struct = Ast.TypeStruct{
        .fields = .{},
        .scope = p.current_scope,
    };

    var tt = p.peek();
    while (tt != .End_Of_File and tt != .Close_Curly) {
        var id = p.grab();
        p.expect(.Identifier);
        p.expect(.Colon);

        var _type = parse_type(p);
        var symbol = insert_symbol(p, id);

        if (is_struct) {
            symbol.payload = .{ .Struct_Field = .{
                ._type = _type,
            } };
        } else {
            symbol.payload = .{ .Union_Field = .{
                ._type = _type,
            } };
        }

        var node = parser_create(p, Ast.SymbolList.Node);
        node.* = .{
            .payload = symbol,
        };
        _struct.fields.insert_last(node);

        tt = p.peek();
        if (tt != .End_Of_File and tt != .Close_Curly) {
            p.expect(.Comma);
            tt = p.peek();
        }
    }

    pop_scope(p);

    p.expect(.Close_Curly);

    return _struct;
}

fn is_def(p: *This) bool {
    var fst = p.peek();
    var snd = p.peek_ahead(1);
    return fst == .Identifier and (snd == .Colon_Equal or snd == .Colon);
}

fn parse_symbol(p: *This) *Ast.Symbol {
    var id = p.grab();
    p.expect(.Identifier);

    var symbol = insert_symbol(p, id);

    switch (p.peek()) {
        .Colon_Equal => {
            p.advance();

            if (p.peek() == .Proc) {
                p.function_depth += 1;

                var _type = parse_type(p);

                if (p.peek() == .Open_Curly) {
                    var previous_scope = p.current_scope;
                    p.current_scope = _type.payload.Function.scope;

                    var it = _type.payload.Function.params.iterator();
                    while (it.next()) |param_ptr| {
                        var param = &param_ptr.*.payload.Parameter;
                        if (param.has_id) {
                            param_ptr.*.key.scope = p.current_scope;
                            insert_existing_symbol(p, param_ptr.*);
                        }
                    }

                    var block = parse_block_given_scope(p, p.current_scope);

                    symbol.payload = .{ .Function = .{
                        ._type = _type,
                        .block = block,
                        .label = p.grab_label(),
                        .depth = p.function_depth,
                    } };
                    p.current_scope = previous_scope;
                } else {
                    p.expect(.Semicolon);
                    symbol.payload = .{ .Type = _type };
                }

                p.function_depth -= 1;

                return symbol;
            } else {
                var expr = parser_create(p, Ast.Expr);
                expr.* = parse_expr(p);
                p.expect(.Semicolon);

                symbol.payload = .{ .Definition = .{
                    .expr = expr,
                } };

                return symbol;
            }
        },
        .Colon => {
            p.advance();

            var expr: ?*Ast.Expr = null;
            var _type = parse_type(p);

            if (p.peek() == .Equal) {
                p.advance();
                expr = parser_create(p, Ast.Expr);
                expr.?.* = parse_expr(p);
            }

            p.expect(.Semicolon);

            symbol.payload = .{ .Variable = .{
                ._type = _type,
                .expr = expr,
                .tmp = undefined,
            } };

            return symbol;
        },
        else => {
            var token = p.grab();
            common.print_error(p.lexer.filepath, token.line_info, "expected ':' or ':=' to define symbol.", .{});
            std.os.exit(1);
        },
    }
}

fn parse_expr_list(p: *This) Ast.ExprList {
    p.expect(.Open_Curly);

    var result = Ast.ExprList{};

    var tt = p.peek();
    while (tt != .End_Of_File and tt != .Close_Curly) {
        var expr: Ast.Expr = expr: {
            if (p.peek() == .Identifier and
                p.peek_ahead(1) == .Equal)
            {
                var id = p.grab();
                p.advance_many(2);

                var value = parser_create(p, Ast.Expr);
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

        var node = parser_create(p, Ast.ExprList.Node);
        node.* = .{
            .payload = expr,
        };
        result.insert_last(node);

        tt = p.peek();
        if (tt != .End_Of_File and tt != .Close_Curly) {
            p.expect(.Comma);
            tt = p.peek();
        }
    }

    p.expect(.Close_Curly);

    return result;
}

fn parse_expr(p: *This) Ast.Expr {
    return parse_prec(p, LOWEST_PREC);
}

fn parse_prec(p: *This, min_prec: i32) Ast.Expr {
    var lhs = parse_highest_prec(p);
    var op = p.peek();
    var prev_prec: i32 = 0x7FFF_FFFF;
    var curr_prec: i32 = prec_of_op(op);

    while (curr_prec < prev_prec and curr_prec >= min_prec) {
        while (curr_prec == prec_of_op(op)) {
            p.advance();

            var lhs_ptr = parser_create(p, Ast.Expr);
            var rhs_ptr = parser_create(p, Ast.Expr);
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

            op = p.peek();
        }

        prev_prec = curr_prec;
        curr_prec = prec_of_op(op);
    }

    return lhs;
}

fn parse_highest_prec(p: *This) Ast.Expr {
    var token = p.grab();
    p.advance();

    var result = Ast.Expr{
        .payload = undefined,
        ._type = undefined,
        .line_info = token.line_info,
    };
    result.payload = payload: {
        switch (token.tag) {
            .Sub,
            .Ref,
            .Not,
            => {
                var subexpr = parser_create(p, Ast.Expr);
                subexpr.* = parse_highest_prec(p);

                break :payload .{ .Unary_Op = .{
                    .tag = token_tag_to_expr_unary_op_tag(token.tag),
                    .subexpr = subexpr,
                } };
            },
            .And => {
                common.print_error(p.lexer.filepath, token.line_info, "can't take a reference of rvalue.", .{});
                std.os.exit(1);
            },
            .Open_Paren => {
                var expr = parse_expr(p);
                p.expect(.Close_Paren);

                break :payload expr.payload;
            },
            .Dot => {
                switch (p.peek()) {
                    .Open_Curly => break :payload .{ .Expr_List = parse_expr_list(p) },
                    .Identifier => {
                        var id = p.grab();
                        p.advance();

                        break :payload .{ .Enum_Field = id };
                    },
                    else => {
                        var _token = p.grab();
                        common.print_error(p.lexer.filepath, _token.line_info, "unexpected '{s}' after '.' operator.", .{_token.text});
                        common.print_note(p.lexer.filepath, _token.line_info, "expected designator list or identifier.", .{});
                        std.os.exit(1);
                    },
                }
            },
            .If => {
                var cond = parser_create(p, Ast.Expr);
                var if_true = parser_create(p, Ast.Expr);
                var if_false = parser_create(p, Ast.Expr);

                cond.* = parse_expr(p);
                if (p.peek() == .Then) {
                    p.advance();
                }
                if_true.* = parse_expr(p);
                p.expect(.Else);
                if_false.* = parse_expr(p);

                break :payload .{ .If = .{
                    .cond = cond,
                    .if_true = if_true,
                    .if_false = if_false,
                } };
            },
            .False,
            .True,
            => {
                break :payload .{ .Bool = token.tag == .True };
            },
            .Null => {
                break :payload .Null;
            },
            .Identifier => {
                break :payload .{ .Identifier = .{
                    .token = token,
                    .scope = p.current_scope,
                } };
            },
            .Integer => {
                var value: i64 = 0;
                for (token.text) |ch| {
                    value = 10 * value + (ch - '0');
                }

                break :payload .{ .Int64 = value };
            },
            .Mul => {
                var subtype = parse_type(p);
                var _type = parser_create(p, Ast.Type);
                _type.* = .{
                    .payload = .{ .Pointer = subtype },
                    .size = undefined,
                    .line_info = token.line_info,
                };

                break :payload .{ .Type = _type };
            },
            .Open_Bracket => {
                var expr = parser_create(p, Ast.Expr);
                expr.* = parse_expr(p);
                p.expect(.Close_Bracket);

                var subtype = parse_type(p);
                var _type = parser_create(p, Ast.Type);
                _type.* = .{
                    .payload = .{ .Array = .{
                        .expr = expr,
                        .count = undefined,
                        .subtype = subtype,
                    } },
                    .size = undefined,
                    .line_info = token.line_info,
                };

                break :payload .{ .Type = _type };
            },
            .Struct => {
                var _struct = parse_struct_fields(p, true);
                var _type = parser_create(p, Ast.Type);
                _type.* = .{
                    .payload = .{ .Struct = _struct },
                    .size = undefined,
                    .line_info = token.line_info,
                };

                break :payload .{ .Type = _type };
            },
            .Union => {
                var _union = parse_struct_fields(p, false);
                var _type = parser_create(p, Ast.Type);
                _type.* = .{
                    .payload = .{ .Union = _union },
                    .size = undefined,
                    .line_info = token.line_info,
                };

                break :payload .{ .Type = _type };
            },
            .Enum => {
                p.expect(.Open_Curly);

                push_scope(p);

                var _enum = Ast.TypeStruct{
                    .fields = .{},
                    .scope = p.current_scope,
                };

                var tt = p.peek();
                while (tt != .End_Of_File and tt != .Close_Curly) {
                    var id = p.grab();
                    p.expect(.Identifier);

                    var symbol = insert_symbol(p, id);
                    symbol.payload = .{ .Enum_Field = .{
                        ._type = undefined,
                        .value = undefined,
                    } };

                    var node = parser_create(p, Ast.SymbolList.Node);
                    node.* = .{
                        .payload = symbol,
                    };
                    _enum.fields.insert_last(node);

                    tt = p.peek();
                    if (tt != .End_Of_File and tt != .Close_Curly) {
                        p.expect(.Comma);
                        tt = p.peek();
                    }
                }

                pop_scope(p);

                p.expect(.Close_Curly);

                var _type = parser_create(p, Ast.Type);
                _type.* = .{
                    .payload = .{ .Enum = _enum },
                    .size = undefined,
                    .line_info = token.line_info,
                };

                break :payload .{ .Type = _type };
            },
            .Proc => {
                var _type = parser_create(p, Ast.Type);
                _type.* = .{
                    .payload = parse_type_function(p),
                    .size = undefined,
                    .line_info = token.line_info,
                };

                break :payload .{ .Type = _type };
            },
            .Void => {
                var _type = parser_create(p, Ast.Type);
                _type.* = .{
                    .payload = .Void,
                    .size = undefined,
                    .line_info = token.line_info,
                };
                break :payload .{ .Type = _type };
            },
            .Bool => {
                var _type = parser_create(p, Ast.Type);
                _type.* = .{
                    .payload = .Bool,
                    .size = undefined,
                    .line_info = token.line_info,
                };
                break :payload .{ .Type = _type };
            },
            .Int => {
                var _type = parser_create(p, Ast.Type);
                _type.* = .{
                    .payload = .Int64,
                    .size = undefined,
                    .line_info = token.line_info,
                };
                break :payload .{ .Type = _type };
            },
            .Cast => {
                p.expect(.Open_Paren);

                var lhs = parse_expr(p);

                if (p.peek() == .Comma) {
                    p.advance();
                }

                if (p.peek() == .Close_Paren) {
                    p.advance();

                    var expr = parser_create(p, Ast.Expr);
                    expr.* = lhs;
                    break :payload .{ .Cast1 = expr };
                } else {
                    var _type = extract_type(p, lhs);
                    var rhs = parser_create(p, Ast.Expr);
                    rhs.* = parse_expr(p);

                    if (p.peek() == .Comma) {
                        p.advance();
                    }

                    p.expect(.Close_Paren);

                    break :payload .{ .Cast2 = .{
                        ._type = _type,
                        .expr = rhs,
                    } };
                }
            },
            else => {
                common.print_error(p.lexer.filepath, token.line_info, "'{s}' doesn't start expression.", .{token.text});
                std.os.exit(1);
            },
        }
    };

    parse_postfix_unary_ops(p, &result);

    return result;
}

fn parse_postfix_unary_ops(p: *This, inner: *Ast.Expr) void {
    while (true) {
        switch (p.peek()) {
            .Open_Paren => {
                var line_info = p.grab().line_info;
                p.advance();

                var args = Ast.ExprList{};

                var tt = p.peek();
                while (tt != .End_Of_File and tt != .Close_Paren) {
                    var expr = parse_expr(p);
                    var node = parser_create(p, Ast.ExprList.Node);
                    node.* = .{
                        .payload = expr,
                    };
                    args.insert_last(node);

                    tt = p.peek();
                    if (tt != .End_Of_File and tt != .Close_Paren) {
                        p.expect(.Comma);
                        tt = p.peek();
                    }
                }

                p.expect(.Close_Paren);

                var lhs = parser_create(p, Ast.Expr);
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
                var line_info = p.grab().line_info;
                p.advance();

                var expr = parser_create(p, Ast.Expr);
                expr.* = parse_expr(p);
                p.expect(.Close_Bracket);

                var lhs = parser_create(p, Ast.Expr);
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
                p.advance();

                if (p.peek() == .Mul) {
                    p.advance();

                    var lhs = parser_create(p, Ast.Expr);
                    lhs.* = inner.*;
                    inner.* = .{
                        .payload = .{ .Unary_Op = .{
                            .tag = .Deref,
                            .subexpr = lhs,
                        } },
                        ._type = undefined,
                        .line_info = lhs.line_info,
                    };
                } else if (p.peek() == .Open_Curly) {
                    var expr_list = parse_expr_list(p);
                    var _type = extract_type(p, inner.*);
                    inner.* = .{
                        .payload = .{ .Initializer = .{
                            ._type = _type,
                            .expr_list = expr_list,
                        } },
                        ._type = undefined,
                        .line_info = _type.line_info,
                    };
                } else {
                    var id = p.grab();
                    p.expect(.Identifier);

                    var lhs = parser_create(p, Ast.Expr);
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

fn prec_of_op(op: Lexer.TokenTag) i32 {
    return switch (op) {
        .Or => 0,
        .And => 1,
        .Eq,
        .Neq,
        => 2,
        .Lt,
        .Leq,
        .Gt,
        .Geq,
        => 3,
        .Add,
        .Sub,
        => 4,
        .Mul,
        .Div,
        .Mod,
        => 5,
        else => LOWEST_PREC - 1,
    };
}

fn token_tag_to_expr_binary_op_tag(op: Lexer.TokenTag) Ast.ExprBinaryOpTag {
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

fn token_tag_to_expr_unary_op_tag(op: Lexer.TokenTag) Ast.ExprUnaryOpTag {
    return switch (op) {
        .Sub => .Neg,
        .Ref => .Ref,
        .Not => .Not,
        else => unreachable,
    };
}

fn parse_stmt(p: *This) Ast.Stmt {
    var result: Ast.Stmt = .{
        .payload = undefined,
        .line_info = p.grab().line_info,
    };

    result.payload = payload: {
        switch (p.peek()) {
            .Print => {
                p.advance();

                var expr = parser_create(p, Ast.Expr);
                expr.* = parse_expr(p);
                p.expect(.Semicolon);

                break :payload .{ .Print = expr };
            },
            .Open_Curly => {
                break :payload .{ .Block = parse_scoped_block(p) };
            },
            .If => {
                p.advance();

                var cond = parser_create(p, Ast.Expr);
                cond.* = parse_expr(p);

                if (p.peek() == .Then) {
                    p.advance();
                }

                var if_true = parse_stmt_or_scoped_block(p);
                var if_false = Ast.StmtBlock{
                    .stmts = .{},
                    .scope = null,
                };

                if (p.peek() == .Else) {
                    p.advance();
                    if_false = parse_stmt_or_scoped_block(p);
                }

                break :payload .{ .If = .{
                    .cond = cond,
                    .if_true = if_true,
                    .if_false = if_false,
                } };
            },
            .While => {
                p.advance();

                var cond = parser_create(p, Ast.Expr);
                cond.* = parse_expr(p);

                if (p.peek() == .Do) {
                    p.advance();
                }

                var block = parse_stmt_or_scoped_block(p);

                break :payload .{ .While = .{
                    .cond = cond,
                    .block = block,
                } };
            },
            .Do => {
                p.advance();

                var block = parse_stmt_or_scoped_block(p);

                p.expect(.While);

                var cond = parser_create(p, Ast.Expr);
                cond.* = parse_expr(p);

                p.expect(.Semicolon);

                break :payload .{ .While = .{
                    .block = block,
                    .cond = cond,
                    .is_do_while = true,
                } };
            },
            .Break => {
                p.advance();
                p.expect(.Semicolon);
                break :payload .Break;
            },
            .Continue => {
                p.advance();
                p.expect(.Semicolon);
                break :payload .Continue;
            },
            .Switch => {
                p.advance();

                var cond = parser_create(p, Ast.Expr);
                cond.* = parse_expr(p);

                p.expect(.Open_Curly);

                var cases = Ast.SwitchCaseList{};

                var tt = p.peek();
                while (tt != .End_Of_File and tt != .Close_Curly) {
                    while (true) {
                        var value = parser_create(p, Ast.Expr);
                        value.* = parse_expr(p);

                        push_scope(p);

                        var case = Ast.SwitchCase{
                            .value = value,
                            .block = .{
                                .stmts = .{},
                                .scope = p.current_scope,
                            },
                            .should_fallthrough = true,
                        };

                        pop_scope(p);

                        var node = parser_create(p, Ast.SwitchCaseList.Node);
                        node.* = .{
                            .payload = case,
                        };
                        cases.insert_last(node);

                        tt = p.peek();
                        if (tt != .End_Of_File and tt != .Colon) {
                            p.expect(.Comma);
                            tt = p.peek();
                        }

                        if (tt == .End_Of_File or tt == .Colon) {
                            break;
                        }
                    }

                    p.expect(.Colon);

                    var case = cases.grab_last();
                    case.should_fallthrough = false;
                    case.block.stmts = parse_block_given_scope(p, case.block.scope.?);

                    tt = p.peek();
                }

                p.expect(.Close_Curly);

                break :payload .{ .Switch = .{
                    .cond = cond,
                    .cases = cases,
                } };
            },
            .Return => {
                p.advance();

                if (p.peek() == .Semicolon) {
                    p.advance();

                    break :payload .Return;
                }

                var expr = parser_create(p, Ast.Expr);
                expr.* = parse_expr(p);
                p.expect(.Semicolon);

                break :payload .{ .Return_Expr = expr };
            },
            else => {
                if (is_def(p)) {
                    var symbol = parse_symbol(p);
                    break :payload .{ .Symbol = symbol };
                } else {
                    var lhs = parser_create(p, Ast.Expr);
                    lhs.* = parse_expr(p);

                    if (p.peek() == .Equal) {
                        p.advance();

                        var rhs = parser_create(p, Ast.Expr);
                        rhs.* = parse_expr(p);
                        p.expect(.Semicolon);

                        break :payload .{ .Assign = .{
                            .lhs = lhs,
                            .rhs = rhs,
                        } };
                    }

                    p.expect(.Semicolon);

                    break :payload .{ .Expr = lhs };
                }
            },
        }
    };

    return result;
}

fn parse_scoped_block(p: *This) Ast.StmtBlock {
    var scope = create_scope(p);
    var stmts = parse_block_given_scope(p, scope);
    return .{
        .stmts = stmts,
        .scope = scope,
    };
}

fn parse_block_given_scope(p: *This, scope: *Ast.Scope) Ast.StmtList {
    // Previous scope may not be its parent.
    var previous_scope = p.current_scope;
    p.current_scope = scope;

    var block = Ast.StmtList{};

    p.expect(.Open_Curly);

    var tt = p.peek();
    while (tt != .End_Of_File and tt != .Close_Curly) {
        var stmt = parse_stmt(p);
        var node = parser_create(p, Ast.StmtList.Node);
        node.* = .{
            .payload = stmt,
        };
        block.insert_last(node);

        tt = p.peek();
    }

    p.expect(.Close_Curly);

    p.current_scope = previous_scope;

    return block;
}

fn parse_stmt_or_scoped_block(p: *This) Ast.StmtBlock {
    var result = Ast.StmtBlock{
        .stmts = .{},
        .scope = null,
    };

    switch (p.peek()) {
        .Open_Curly => {
            result = parse_scoped_block(p);
        },
        .Semicolon => {
            p.advance();
        },
        else => {
            var stmt = parse_stmt(p);
            var node = parser_create(p, Ast.StmtList.Node);
            node.* = .{
                .payload = stmt,
            };
            result.stmts.insert_last(node);

            if (stmt.payload == .Symbol) {
                common.print_error(p.lexer.filepath, stmt.line_info, "definitions are not allowed inside a single statement block.", .{});
                std.os.exit(1);
            }
        },
    }

    return result;
}
