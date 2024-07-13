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
    const expr = parse_expr(p);
    p.lexer.expect(.Semicolon);

    const stmt = create(p, Ast.Stmt);
    stmt.* = .{
        .Expr = expr,
    };

    return stmt;
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
                .Unary_Op = .{
                    .line_info = token.line_info,
                    .subexpr = subexpr,
                    .tag = .Plus,
                },
            };
            return expr;
        },
        .Sub => {
            const subexpr = parse_expr_base(p);
            const expr = create(p, Ast.Expr);
            expr.* = .{
                .Unary_Op = .{
                    .line_info = token.line_info,
                    .subexpr = subexpr,
                    .tag = .Minus,
                },
            };
            return expr;
        },
        .Not => {
            const subexpr = parse_expr_base(p);
            const expr = create(p, Ast.Expr);
            expr.* = .{
                .Unary_Op = .{
                    .line_info = token.line_info,
                    .subexpr = subexpr,
                    .tag = .Not,
                },
            };
            return expr;
        },
        .Open_Paren => {
            const expr = parse_expr(p);
            p.lexer.expect(.Close_Paren);
            return expr;
        },
        .Bool => |value| {
            const expr = create(p, Ast.Expr);
            expr.* = .{
                .Bool = .{
                    .line_info = token.line_info,
                    .value = value,
                },
            };
            return expr;
        },
        .Integer => |value| {
            const expr = create(p, Ast.Expr);
            expr.* = .{
                .Integer = .{
                    .line_info = token.line_info,
                    .value = value,
                },
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
    return parse_expr_base(p);
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
                .Binary_Op = .{
                    .line_info = line_info,
                    .lhs = lhs,
                    .rhs = rhs,
                    .tag = token_tag_to_binary_op_tag(op),
                },
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
