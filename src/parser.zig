lexer: Lexer,
stmt_list: Ast.StmtList,
arena: ArenaAllocator,

const std = @import("std");
const utils = @import("utils.zig");
const Lexer = @import("lexer.zig");
const Ast = @import("ast.zig");

const ArenaAllocator = std.heap.ArenaAllocator;
const LineInfo = Lexer.LineInfo;
const Token = Lexer.Token;
const Parser = @This();

const LOWEST_PREC = -2147483646;

pub fn parse(filepath: [:0]const u8) Ast {
    var parser = Parser{
        .lexer = Lexer.init(utils.gpa, filepath),
        .stmt_list = .{},
        .arena = ArenaAllocator.init(std.heap.page_allocator),
    };

    parse_top_level(&parser);

    parser.lexer.allocator.free(parser.lexer.source_code);

    return .{
        .stmt_list = parser.stmt_list,
        .arena = parser.arena,
        .filepath = parser.lexer.filepath,
    };
}

fn parse_top_level(p: *Parser) void {
    var list = Ast.StmtList{};

    while (p.lexer.peek() != .End_Of_File) {
        const stmt = parse_stmt(p);
        {
            const node = create(p, Ast.StmtList.Node);
            node.* = .{
                .data = stmt,
            };
            list.append(node);
        }
    }

    p.stmt_list = list;
}

fn create(p: *Parser, T: type) *T {
    return p.arena.allocator().create(T) catch {
        std.posix.exit(1);
    };
}

fn parse_stmt(p: *Parser) *Ast.Stmt {
    switch (p.lexer.peek()) {
        .Print => {
            p.lexer.advance();
            const expr = parse_expr(p);
            p.lexer.expect(.Semicolon);

            const stmt = create(p, Ast.Stmt);
            stmt.* = .{
                .Print = expr,
            };

            return stmt;
        },
        else => {
            const expr = parse_expr(p);
            p.lexer.expect(.Semicolon);

            const stmt = create(p, Ast.Stmt);
            stmt.* = .{
                .Expr = expr,
            };

            return stmt;
        },
    }
}

fn parse_expr(p: *Parser) *Ast.Expr {
    return parse_prec(p, LOWEST_PREC);
}

fn parse_expr_base(p: *Parser) *Ast.Expr {
    const token = p.lexer.take();
    switch (token.as) {
        .Add => {
            const subexpr = parse_expr_base(p);
            const expr = create(p, Ast.Expr);
            expr.* = .{
                .line_info = subexpr.line_info,
                .as = .{ .Unary_Op = .{
                    .subexpr = subexpr,
                    .tag = .Plus,
                } },
                .typ = null,
            };
            return expr;
        },
        .Sub => {
            const subexpr = parse_expr_base(p);
            const expr = create(p, Ast.Expr);
            expr.* = .{
                .line_info = token.line_info,
                .as = .{ .Unary_Op = .{
                    .subexpr = subexpr,
                    .tag = .Minus,
                } },
                .typ = null,
            };
            return expr;
        },
        .Not => {
            const subexpr = parse_expr_base(p);
            const expr = create(p, Ast.Expr);
            expr.* = .{
                .line_info = token.line_info,
                .as = .{ .Unary_Op = .{
                    .subexpr = subexpr,
                    .tag = .Not,
                } },
                .typ = null,
            };
            return expr;
        },
        .Open_Paren => {
            const expr = parse_expr(p);
            expr.line_info = token.line_info;
            p.lexer.expect(.Close_Paren);
            return expr;
        },
        .Bit_Size_Of => {
            var exprs: [1]*Ast.Expr = undefined;
            p.lexer.expect(.Open_Paren);
            const count = parse_fixed_size_expr_list(p, &exprs);
            p.lexer.expect(.Close_Paren);

            switch (count) {
                1 => {
                    const expr = create(p, Ast.Expr);
                    expr.* = .{
                        .line_info = token.line_info,
                        .as = .{ .Bit_Size_Of = exprs[0] },
                        .typ = null,
                    };
                    return expr;
                },
                else => {
                    report_error(p, token.line_info, "expected 1 argument, but got {}", .{count});
                    std.posix.exit(1);
                },
            }
        },
        .Byte_Size_Of => {
            var exprs: [1]*Ast.Expr = undefined;
            p.lexer.expect(.Open_Paren);
            const count = parse_fixed_size_expr_list(p, &exprs);
            p.lexer.expect(.Close_Paren);

            switch (count) {
                1 => {
                    const expr = create(p, Ast.Expr);
                    expr.* = .{
                        .line_info = token.line_info,
                        .as = .{ .Byte_Size_Of = exprs[0] },
                        .typ = null,
                    };
                    return expr;
                },
                else => {
                    report_error(p, token.line_info, "expected 1 argument, but got {}", .{count});
                    std.posix.exit(1);
                },
            }
        },
        .Type_Of => {
            var exprs: [1]*Ast.Expr = undefined;
            p.lexer.expect(.Open_Paren);
            const count = parse_fixed_size_expr_list(p, &exprs);
            p.lexer.expect(.Close_Paren);

            switch (count) {
                1 => {
                    const expr = create(p, Ast.Expr);
                    expr.* = .{
                        .line_info = token.line_info,
                        .as = .{ .Type_Of = exprs[0] },
                        .typ = null,
                    };
                    return expr;
                },
                else => {
                    report_error(p, token.line_info, "expected 1 argument, but got {}", .{count});
                    std.posix.exit(1);
                },
            }
        },
        .As => {
            var exprs: [2]*Ast.Expr = undefined;
            p.lexer.expect(.Open_Paren);
            const count = parse_fixed_size_expr_list(p, &exprs);
            p.lexer.expect(.Close_Paren);

            switch (count) {
                2 => {
                    const typ = expr_to_type(p, exprs[0]);
                    const expr = create(p, Ast.Expr);
                    expr.* = .{
                        .line_info = token.line_info,
                        .as = .{ .As = .{
                            .typ = typ,
                            .expr = exprs[1],
                        } },
                        .typ = null,
                    };
                    return expr;
                },
                else => {
                    report_error(p, token.line_info, "expected 2 arguments, but got {}", .{count});
                    std.posix.exit(1);
                },
            }
        },
        .Cast => {
            var exprs: [2]*Ast.Expr = undefined;
            p.lexer.expect(.Open_Paren);
            const count = parse_fixed_size_expr_list(p, &exprs);
            p.lexer.expect(.Close_Paren);

            switch (count) {
                2 => {
                    const typ = expr_to_type(p, exprs[0]);
                    const expr = create(p, Ast.Expr);
                    expr.* = .{
                        .line_info = token.line_info,
                        .as = .{ .Cast = .{
                            .typ = typ,
                            .expr = exprs[1],
                        } },
                        .typ = null,
                    };
                    return expr;
                },
                else => {
                    report_error(p, token.line_info, "expected 2 arguments, but got {}", .{count});
                    std.posix.exit(1);
                },
            }
        },
        .Bool_Type, .Integer_Type => { // NOTE[sync-enums]: every case here must also appear in 'parse_type'.
            p.lexer.putback(token);
            const typ = parse_type(p);
            const expr = create(p, Ast.Expr);
            expr.* = .{
                .line_info = token.line_info,
                .as = .{ .Type = typ },
                .typ = null,
            };
            return expr;
        },
        .Bool => |value| {
            const expr = create(p, Ast.Expr);
            expr.* = .{
                .line_info = token.line_info,
                .as = .{ .Bool = value },
                .typ = null,
            };
            return expr;
        },
        .Integer => |value| {
            const expr = create(p, Ast.Expr);
            expr.* = .{
                .line_info = token.line_info,
                .as = .{ .Integer = value },
                .typ = null,
            };
            return expr;
        },
        else => {
            report_error(p, token.line_info, "token doesn't start an expression", .{});
            std.posix.exit(1);
        },
    }
}

fn parse_highest_prec(p: *Parser) *Ast.Expr {
    var base = parse_expr_base(p);

    while (true) {
        switch (p.lexer.peek()) {
            .Open_Paren => {
                p.lexer.advance();
                const args = parse_expr_list(p);
                p.lexer.expect(.Close_Paren);

                const new_base = create(p, Ast.Expr);
                new_base.* = .{
                    .line_info = base.line_info,
                    .as = .{ .Call = .{
                        .subexpr = base,
                        .args = args,
                    } },
                    .typ = null,
                };
                base = new_base;
            },
            else => break,
        }
    }

    return base;
}

fn parse_prec(p: *Parser, lowest_prec: i32) *Ast.Expr {
    var lhs = parse_highest_prec(p);
    var op = p.lexer.peek();
    var prev_prec: i32 = 0x7FFFFFFF;
    var curr_prec: i32 = op_prec(op);

    while (curr_prec < prev_prec and curr_prec >= lowest_prec) {
        while (true) {
            const line_info = p.lexer.take().line_info;

            const rhs = parse_prec(p, curr_prec + 1);
            const new_lhs = create(p, Ast.Expr);
            new_lhs.* = .{
                .line_info = lhs.line_info,
                .as = .{ .Binary_Op = .{
                    .line_info = line_info,
                    .lhs = lhs,
                    .rhs = rhs,
                    .tag = token_tag_to_binary_op_tag(op),
                } },
                .typ = null,
            };
            lhs = new_lhs;

            op = p.lexer.peek();

            if (op_prec(op) != curr_prec) {
                break;
            }
        }

        prev_prec = curr_prec;
        curr_prec = op_prec(op);
    }

    return lhs;
}

fn parse_expr_list(p: *Parser) Ast.ExprList {
    var list = Ast.ExprList{};

    var tt = p.lexer.peek();
    while (tt != .End_Of_File and tt != .Close_Paren) {
        const expr = parse_expr(p);

        {
            const node = create(p, Ast.ExprList.Node);
            node.* = .{
                .data = expr,
            };
            list.append(node);
        }

        tt = p.lexer.peek();
        if (tt != .End_Of_File and tt != .Close_Paren) {
            p.lexer.expect(.Comma);
            tt = p.lexer.peek();
        }
    }

    return list;
}

fn parse_fixed_size_expr_list(p: *Parser, dst: []*Ast.Expr) usize {
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

    return count;
}

fn parse_type(p: *Parser) Ast.Type {
    const token = p.lexer.take();
    switch (token.as) { // look at NOTE[sync-enums].
        .Bool_Type => {
            return .{
                .line_info = token.line_info,
                .as = .Bool,
                .size = 1,
            };
        },
        .Integer_Type => |Integer_Type| {
            return .{
                .line_info = token.line_info,
                .as = .{ .Integer = .{
                    .bits = Integer_Type.bits,
                    .is_signed = Integer_Type.is_signed,
                } },
                .size = Integer_Type.bits,
            };
        },
        else => {
            report_error(p, token.line_info, "token doesn't start a type", .{});
            std.posix.exit(1);
        },
    }
}

// NOTE: Assume expression is heap allocated and can be reused.
fn expr_to_type(p: *Parser, expr: *Ast.Expr) *Ast.Type {
    switch (expr.as) {
        .Type => |*typ| {
            return typ;
        },
        else => {
            report_error(p, expr.line_info, "expression is not a type", .{});
            std.posix.exit(1);
        },
    }
}

fn op_prec(tag: Token.Tag) i32 {
    return switch (tag) {
        .Or => 0,
        .And => 1,
        .Eq, .Neq => 2,
        .Lt, .Leq, .Gt, .Geq => 3,
        .Add, .Sub => 4,
        .Mul, .Div, .Mod => 5,
        else => LOWEST_PREC - 1,
    };
}

fn token_tag_to_binary_op_tag(tag: Token.Tag) Ast.Expr.BinaryOp.Tag {
    return switch (tag) {
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

fn report_error(p: *Parser, line_info: LineInfo, comptime format: []const u8, args: anytype) void {
    utils.eprint("{s}:{}:{}: error: " ++ format ++ "\n", .{ p.lexer.filepath, line_info.line, line_info.column } ++ args);
}
