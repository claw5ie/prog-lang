lexer: Lexer,
stmt_list: Ast.StmtList,
arena: ArenaAllocator,

const std = @import("std");
const utils = @import("utils.zig");
const Lexer = @import("lexer.zig");
const Ast = @import("ast.zig");

const ArenaAllocator = std.heap.ArenaAllocator;
const LineInfo = Lexer.LineInfo;
const Parser = @This();

pub fn parse(filepath: [:0]const u8) Ast {
    var parser = Parser{
        .lexer = Lexer.init(utils.gpa, filepath),
        .stmt_list = .{},
        .arena = ArenaAllocator.init(std.heap.page_allocator),
    };

    parse_top_level(&parser);

    return .{
        .stmt_list = parser.stmt_list,
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
    return parse_expr_base(p);
}

fn parse_expr_base(p: *Parser) *Ast.Expr {
    const token = p.lexer.take();
    switch (token.as) {
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
            report_error(p, token.line_info, "", .{});
            std.posix.exit(1);
        },
    }
}

fn report_error(p: *Parser, line_info: LineInfo, comptime format: []const u8, args: anytype) void {
    utils.eprint("{s}:{}:{}: error: " ++ format ++ "\n", .{ p.lexer.filepath, line_info.line, line_info.column } ++ args);
}
