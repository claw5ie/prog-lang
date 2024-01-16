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
        var has_symbol = parse_symbol(p);
        if (has_symbol) |symbol| {
            var node = create(p, Ast.SymbolList.Node);
            node.* = .{
                .payload = symbol,
            };
            globals.insert_last(node);
        } else {
            var line_info = p.grab().line_info;
            common.print_error(p.lexer.filepath, line_info, "expected ':' or ':=' to start definition.", .{});
            std.os.exit(1);
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
        std.os.exit(1);
    };
}

fn create_scope(p: *This) *Ast.Scope {
    var result = create(p, Ast.Scope);
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
    var symbol = create(p, Ast.Symbol);
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

        var node = create(p, Ast.SymbolList.Node);
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
        return_type = create(p, Ast.Type);
        return_type.* = .{
            .payload = .Void,
            .size = undefined,
            .line_info = p.grab().line_info,
        };
    }

    return .{ .Function = .{
        .params = params,
        .return_type = return_type,
        .scope = p.current_scope,
    } };
}

fn parse_struct_fields(p: *This) Ast.TypeStruct {
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
        symbol.payload = .{ .Struct_Field = .{
            ._type = _type,
        } };

        var node = create(p, Ast.SymbolList.Node);
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

fn parse_symbol(p: *This) ?*Ast.Symbol {
    if (p.peek() != .Identifier) return null;

    switch (p.peek_ahead(1)) {
        .Colon_Equal => {
            var id = p.grab();
            var symbol = insert_symbol(p, id);
            p.advance_many(2);

            if (p.peek() == .Proc) {
                p.function_depth += 1;

                var _type = parse_type(p);

                if (p.peek() == .Open_Curly) {
                    push_scope(p);

                    var it = _type.payload.Function.params.iterator();
                    while (it.next()) |param_ptr| {
                        var param = &param_ptr.*.payload.Parameter;
                        if (param.has_id) {
                            param_ptr.*.key.scope = p.current_scope;
                            insert_existing_symbol(p, param_ptr.*);
                        }
                    }

                    var block = parse_stmt_block(p);

                    symbol.payload = .{ .Function = .{
                        ._type = _type,
                        .block = block,
                        .label = p.grab_label(),
                        .depth = p.function_depth,
                    } };

                    pop_scope(p);
                } else {
                    p.expect(.Semicolon);
                    symbol.payload = .{ .Type = _type };
                }

                p.function_depth -= 1;

                return symbol;
            } else {
                var expr = create(p, Ast.Expr);
                expr.* = parse_expr(p);
                p.expect(.Semicolon);

                symbol.payload = .{ .Definition = .{
                    .expr = expr,
                } };

                return symbol;
            }
        },
        .Colon => {
            var id = p.grab();
            var symbol = insert_symbol(p, id);
            p.advance_many(2);

            var expr: ?*Ast.Expr = null;
            var _type = parse_type(p);

            if (p.peek() == .Equal) {
                p.advance();
                expr = create(p, Ast.Expr);
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
            return null;
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

                var value = create(p, Ast.Expr);
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

        var node = create(p, Ast.ExprList.Node);
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

            var lhs_ptr = create(p, Ast.Expr);
            var rhs_ptr = create(p, Ast.Expr);
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

fn parse_highest_prec_base(p: *This) Ast.Expr {
    var token = p.grab();
    p.advance();

    switch (token.tag) {
        .Sub,
        .Ref,
        .Not,
        => {
            var subexpr = create(p, Ast.Expr);
            subexpr.* = parse_highest_prec_base(p);

            return .{
                .payload = .{ .Unary_Op = .{
                    .tag = token_tag_to_expr_unary_op_tag(token.tag),
                    .subexpr = subexpr,
                } },
                ._type = undefined,
                .line_info = token.line_info,
            };
        },
        .And => {
            common.print_error(p.lexer.filepath, token.line_info, "can't take a reference of rvalue.", .{});
            std.os.exit(1);
        },
        .Open_Paren => {
            var expr = parse_expr(p);
            p.expect(.Close_Paren);

            return .{
                .payload = expr.payload,
                ._type = undefined,
                .line_info = token.line_info,
            };
        },
        .Dot => {
            switch (p.peek()) {
                .Open_Curly => return .{
                    .payload = .{ .Expr_List = parse_expr_list(p) },
                    ._type = undefined,
                    .line_info = token.line_info,
                },
                .Identifier => {
                    var id = p.grab();
                    p.advance();

                    return .{
                        .payload = .{ .Enum_Field = id },
                        ._type = undefined,
                        .line_info = token.line_info,
                    };
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
            var cond = create(p, Ast.Expr);
            var if_true = create(p, Ast.Expr);
            var if_false = create(p, Ast.Expr);

            cond.* = parse_expr(p);
            if (p.peek() == .Then) {
                p.advance();
            }
            if_true.* = parse_expr(p);
            p.expect(.Else);
            if_false.* = parse_expr(p);

            return .{
                .payload = .{ .If = .{
                    .cond = cond,
                    .if_true = if_true,
                    .if_false = if_false,
                } },
                ._type = undefined,
                .line_info = token.line_info,
            };
        },
        .False,
        .True,
        => {
            return .{
                .payload = .{ .Bool = token.tag == .True },
                ._type = undefined,
                .line_info = token.line_info,
            };
        },
        .Null => {
            return .{
                .payload = .Null,
                ._type = undefined,
                .line_info = token.line_info,
            };
        },
        .Identifier => {
            return .{
                .payload = .{ .Identifier = .{
                    .token = token,
                    .scope = p.current_scope,
                } },
                ._type = undefined,
                .line_info = token.line_info,
            };
        },
        .Integer => {
            var value: i64 = 0;
            for (token.text) |ch| {
                value = 10 * value + (ch - '0');
            }

            return .{
                .payload = .{ .Int64 = value },
                ._type = undefined,
                .line_info = token.line_info,
            };
        },
        .Mul => {
            var subtype = parse_type(p);
            var _type = create(p, Ast.Type);
            _type.* = .{
                .payload = .{ .Pointer = subtype },
                .size = undefined,
                .line_info = token.line_info,
            };

            return .{
                .payload = .{ .Type = _type },
                ._type = undefined,
                .line_info = token.line_info,
            };
        },
        .Open_Bracket => {
            var expr = create(p, Ast.Expr);
            expr.* = parse_expr(p);
            p.expect(.Close_Bracket);

            var subtype = parse_type(p);
            var _type = create(p, Ast.Type);
            _type.* = .{
                .payload = .{ .Array = .{
                    .expr = expr,
                    .count = undefined,
                    .subtype = subtype,
                } },
                .size = undefined,
                .line_info = token.line_info,
            };

            return .{
                .payload = .{ .Type = _type },
                ._type = undefined,
                .line_info = token.line_info,
            };
        },
        .Struct => {
            var _struct = parse_struct_fields(p);
            var _type = create(p, Ast.Type);
            _type.* = .{
                .payload = .{ .Struct = _struct },
                .size = undefined,
                .line_info = token.line_info,
            };

            return .{
                .payload = .{ .Type = _type },
                ._type = undefined,
                .line_info = token.line_info,
            };
        },
        .Union => {
            var _union = parse_struct_fields(p);
            var _type = create(p, Ast.Type);
            _type.* = .{
                .payload = .{ .Union = _union },
                .size = undefined,
                .line_info = token.line_info,
            };

            return .{
                .payload = .{ .Type = _type },
                ._type = undefined,
                .line_info = token.line_info,
            };
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

                var node = create(p, Ast.SymbolList.Node);
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

            var _type = create(p, Ast.Type);
            _type.* = .{
                .payload = .{ .Enum = _enum },
                .size = undefined,
                .line_info = token.line_info,
            };

            return .{
                .payload = .{ .Type = _type },
                ._type = undefined,
                .line_info = token.line_info,
            };
        },
        .Proc => {
            var _type = create(p, Ast.Type);
            _type.* = .{
                .payload = parse_type_function(p),
                .size = undefined,
                .line_info = token.line_info,
            };

            return .{
                .payload = .{ .Type = _type },
                ._type = undefined,
                .line_info = token.line_info,
            };
        },
        .Void => {
            var _type = create(p, Ast.Type);
            _type.* = .{
                .payload = .Void,
                .size = undefined,
                .line_info = token.line_info,
            };
            return .{
                .payload = .{ .Type = _type },
                ._type = undefined,
                .line_info = token.line_info,
            };
        },
        .Bool => {
            var _type = create(p, Ast.Type);
            _type.* = .{
                .payload = .Bool,
                .size = undefined,
                .line_info = token.line_info,
            };
            return .{
                .payload = .{ .Type = _type },
                ._type = undefined,
                .line_info = token.line_info,
            };
        },
        .Int => {
            var _type = create(p, Ast.Type);
            _type.* = .{
                .payload = .Int64,
                .size = undefined,
                .line_info = token.line_info,
            };
            return .{
                .payload = .{ .Type = _type },
                ._type = undefined,
                .line_info = token.line_info,
            };
        },
        .Cast => {
            p.expect(.Open_Paren);

            var lhs = parse_expr(p);

            if (p.peek() == .Comma) {
                p.advance();
            }

            if (p.peek() == .Close_Paren) {
                p.advance();

                var expr = create(p, Ast.Expr);
                expr.* = lhs;
                return .{
                    .payload = .{ .Cast1 = expr },
                    ._type = undefined,
                    .line_info = token.line_info,
                };
            } else {
                var _type = extract_type(p, lhs);
                var rhs = create(p, Ast.Expr);
                rhs.* = parse_expr(p);

                if (p.peek() == .Comma) {
                    p.advance();
                }

                p.expect(.Close_Paren);

                return .{
                    .payload = .{ .Cast2 = .{
                        ._type = _type,
                        .expr = rhs,
                    } },
                    ._type = undefined,
                    .line_info = token.line_info,
                };
            }
        },
        else => {
            common.print_error(p.lexer.filepath, token.line_info, "'{s}' doesn't start expression.", .{token.text});
            std.os.exit(1);
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
        switch (p.peek()) {
            .Open_Paren => {
                var line_info = p.grab().line_info;
                p.advance();

                var args = Ast.ExprList{};

                var tt = p.peek();
                while (tt != .End_Of_File and tt != .Close_Paren) {
                    var expr = parse_expr(p);
                    var node = create(p, Ast.ExprList.Node);
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

                var lhs = create(p, Ast.Expr);
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

                var expr = create(p, Ast.Expr);
                expr.* = parse_expr(p);
                p.expect(.Close_Bracket);

                var lhs = create(p, Ast.Expr);
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

                    var lhs = create(p, Ast.Expr);
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

                    var lhs = create(p, Ast.Expr);
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
    var line_info = p.grab().line_info;

    switch (p.peek()) {
        .Print => {
            p.advance();

            var expr = create(p, Ast.Expr);
            expr.* = parse_expr(p);
            p.expect(.Semicolon);

            return .{
                .payload = .{ .Print = expr },
                .line_info = line_info,
            };
        },
        .Open_Curly => {
            push_scope(p);
            var block = parse_stmt_block(p);
            pop_scope(p);

            return .{
                .payload = .{ .Block = block },
                .line_info = line_info,
            };
        },
        .If => {
            p.advance();

            var cond = create(p, Ast.Expr);
            cond.* = parse_expr(p);

            if (p.peek() == .Then) {
                p.advance();
            }

            var if_true = create(p, Ast.Stmt);
            var if_false: ?*Ast.Stmt = null;

            if_true.* = parse_stmt(p);
            if (p.peek() == .Else) {
                p.advance();
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
            p.advance();

            var cond = create(p, Ast.Expr);
            cond.* = parse_expr(p);

            if (p.peek() == .Do) {
                p.advance();
            }

            var block = create(p, Ast.Stmt);
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
            p.advance();

            var block = create(p, Ast.Stmt);
            block.* = parse_stmt(p);

            p.expect(.While);

            var cond = create(p, Ast.Expr);
            cond.* = parse_expr(p);

            p.expect(.Semicolon);

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
            p.advance();
            p.expect(.Semicolon);
            return .{
                .payload = .Break,
                .line_info = line_info,
            };
        },
        .Continue => {
            p.advance();
            p.expect(.Semicolon);
            return .{
                .payload = .Continue,
                .line_info = line_info,
            };
        },
        .Switch => {
            p.advance();

            var cond = create(p, Ast.Expr);
            cond.* = parse_expr(p);

            push_scope(p);
            var block = parse_stmt_block(p);
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
            p.advance();

            if (p.peek() == .Semicolon) {
                p.advance();

                return .{
                    .payload = .Return,
                    .line_info = line_info,
                };
            }

            var expr = create(p, Ast.Expr);
            expr.* = parse_expr(p);
            p.expect(.Semicolon);

            return .{
                .payload = .{ .Return_Expr = expr },
                .line_info = line_info,
            };
        },
        else => {
            var has_symbol = parse_symbol(p);

            if (has_symbol) |symbol| {
                return .{
                    .payload = .{ .Symbol = symbol },
                    .line_info = line_info,
                };
            } else {
                var lhs = create(p, Ast.Expr);
                lhs.* = parse_expr(p);

                switch (p.peek()) {
                    .Equal => {
                        p.advance();

                        var rhs = create(p, Ast.Expr);
                        rhs.* = parse_expr(p);
                        p.expect(.Semicolon);

                        return .{
                            .payload = .{ .Assign = .{
                                .lhs = lhs,
                                .rhs = rhs,
                            } },
                            .line_info = line_info,
                        };
                    },
                    .Colon => {
                        p.advance();

                        var stmt = create(p, Ast.Stmt);
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
                        p.expect(.Semicolon);

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
    p.expect(.Open_Curly);

    var block = Ast.StmtBlock{};

    var tt = p.peek();
    while (tt != .End_Of_File and tt != .Close_Curly) {
        var stmt = parse_stmt(p);
        var node = create(p, Ast.StmtBlock.Node);
        node.* = .{
            .payload = stmt,
        };
        block.insert_last(node);

        tt = p.peek();
    }

    p.expect(.Close_Curly);

    return block;
}
