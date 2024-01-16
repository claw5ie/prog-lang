const std = @import("std");
const common = @import("common.zig");
const Ast = @import("ast.zig");

inline fn extract_type(ast: *Ast, expr: Ast.Expr) *Ast.Type {
    var ok = Ast.extract_type(ast.ast_arena.allocator(), expr);
    if (ok) |_type| {
        return _type;
    } else {
        common.print_error(ast.filepath, expr.line_info, "expression doesn't look like a type.", .{});
        std.os.exit(1);
    }
}

fn find_symbol(ast: *Ast, id: Ast.Identifier, is_type: *bool) *Ast.Symbol {
    std.debug.assert(id.token.tag == .Identifier);

    var key = Ast.SymbolKey{
        .text = id.token.text,
        .scope = id.scope,
    };

    while (true) {
        var found_symbol = ast.symbols.get(key);

        if (found_symbol) |symbol| {
            if (is_symbol_a_type(ast, symbol)) {
                is_type.* = true;
                return symbol;
            } else if (symbol.payload == .Function) {
                return symbol;
            } else if (symbol.line_info.offset < id.token.line_info.offset) {
                is_type.* = false;
                return symbol;
            }
        }

        if (key.scope.parent) |parent| {
            key.scope = parent;
        } else {
            break;
        }
    }

    common.print_error(ast.filepath, id.token.line_info, "'{s}' is not defined.", .{id.token.text});
    std.os.exit(1);
}

fn is_expr_a_type(ast: *Ast, expr: *Ast.Expr) bool {
    switch (expr.payload) {
        .Type => {
            return true;
        },
        .Identifier => |ident| {
            var is_type = false;
            var symbol = find_symbol(ast, ident, &is_type);
            if (is_type) {
                var _type = ast.create(Ast.Type);
                _type.* = .{
                    .payload = .{ .Symbol = symbol },
                    .size = undefined,
                    .line_info = ident.token.line_info,
                };
                expr.payload = .{ .Type = _type };
            } else {
                expr.payload = .{ .Symbol = symbol };
            }

            return is_type;
        },
        else => {
            return false;
        },
    }
}

fn is_symbol_a_type(ast: *Ast, symbol: *Ast.Symbol) bool {
    switch (symbol.payload) {
        .Definition => |*definition| {
            if (definition.was_visited) {
                common.print_error(ast.filepath, symbol.line_info, "cyclic reference detected.", .{});
                std.os.exit(1);
            }

            definition.was_visited = true;

            if (is_expr_a_type(ast, definition.expr)) {
                var subtype = definition.expr.payload.Type;
                if (subtype.payload == .Symbol) {
                    symbol.payload = .{ .Type = subtype.payload.Symbol.payload.Type };
                } else {
                    var _type = extract_type(ast, definition.expr.*);

                    symbol.payload = .{ .Type = _type };
                }

                return true;
            } else {
                var expr = definition.expr;
                symbol.payload = .{ .Variable = .{
                    ._type = null,
                    .expr = expr,
                    .tmp = undefined,
                } };

                return false;
            }
        },
        .Type => {
            return true;
        },
        else => {
            return false;
        },
    }
}

pub fn resolve(ast: *Ast) void {
    var it = ast.globals.iterator();
    while (it.next()) |symbol| {
        resolve_symbol(ast, symbol.*);
    }

    var is_type = false;
    var symbol = find_symbol(ast, .{
        .token = .{
            .tag = .Identifier,
            .text = "main",
            .line_info = .{},
        },
        .scope = ast.global_scope,
    }, &is_type);

    ast.main = symbol;
}

fn resolve_symbol(ast: *Ast, symbol: *Ast.Symbol) void {
    _ = is_symbol_a_type(ast, symbol);
    switch (symbol.payload) {
        .Variable => |*variable| {
            if (variable._type) |_type| {
                resolve_type(ast, _type);
            }

            if (variable.expr) |expr| {
                resolve_expr(ast, expr);
            }
        },
        .Parameter => |parameter| {
            resolve_type(ast, parameter._type);
        },
        .Function => |function| {
            resolve_type(ast, function._type);

            var it = function.block.iterator();
            while (it.next()) |stmt| {
                resolve_stmt(ast, stmt);
            }
        },
        .Type => |_type| {
            resolve_type(ast, _type);
        },
        .Struct_Field,
        .Union_Field,
        .Enum_Field,
        .Definition,
        => unreachable,
    }
}

fn resolve_type(ast: *Ast, _type: *Ast.Type) void {
    if (_type.is_resolved) {
        return;
    }

    _type.is_resolved = true;

    switch (_type.payload) {
        .Struct => |_struct| {
            var it = _struct.fields.iterator();
            while (it.next()) |field| {
                resolve_type(ast, field.*.payload.Struct_Field._type);
            }
        },
        .Union => |_union| {
            var it = _union.fields.iterator();
            while (it.next()) |field| {
                resolve_type(ast, field.*.payload.Union_Field._type);
            }
        },
        .Enum => |_enum| {
            var it = _enum.fields.iterator();
            while (it.next()) |field| {
                field.*.payload.Enum_Field._type = _type;
            }
        },
        .Function => |function| {
            var it = function.params.iterator();
            while (it.next()) |param| {
                resolve_type(ast, param.*.payload.Parameter._type);
            }

            resolve_type(ast, function.return_type);
        },
        .Array => |array| {
            resolve_expr(ast, array.expr);
            resolve_type(ast, array.subtype);
        },
        .Pointer => |subtype| {
            resolve_type(ast, subtype);
        },
        .Void,
        .Bool,
        .Int64,
        .Symbol,
        => {},
        .Identifier => |ident| {
            var is_type = false;
            var symbol = find_symbol(ast, ident, &is_type);

            if (is_type) {
                _type.payload = .{ .Symbol = symbol };
            } else {
                common.print_error(ast.filepath, ident.token.line_info, "'{s}' is not a type.", .{ident.token.text});
                std.os.exit(1);
            }
        },
    }
}

fn resolve_block(ast: *Ast, block: Ast.StmtBlock) void {
    var it = block.stmts.iterator();
    while (it.next()) |stmt| {
        resolve_stmt(ast, stmt);
    }
}

fn resolve_stmt(ast: *Ast, stmt: *Ast.Stmt) void {
    switch (stmt.payload) {
        .Print => |expr| {
            resolve_expr(ast, expr);
        },
        .Block => |block| {
            resolve_block(ast, block);
        },
        .If => |_if| {
            resolve_expr(ast, _if.cond);
            resolve_block(ast, _if.if_true);
            resolve_block(ast, _if.if_false);
        },
        .While => |_while| {
            resolve_expr(ast, _while.cond);
            resolve_block(ast, _while.block);
        },
        .Break => {},
        .Continue => {},
        .Switch => |_switch| {
            resolve_expr(ast, _switch.cond);

            var it = _switch.cases.iterator();
            while (it.next()) |case| {
                resolve_expr(ast, case.value);
                resolve_block(ast, case.block);
            }
        },
        .Return => {},
        .Return_Expr => |expr| {
            resolve_expr(ast, expr);
        },
        .Symbol => |symbol| {
            resolve_symbol(ast, symbol);

            if (symbol.payload == .Function) {
                var node = ast.create(Ast.SymbolList.Node);
                node.* = .{
                    .payload = symbol,
                };
                ast.locals.insert_first(node);
            }
        },
        .Assign => |assign| {
            resolve_expr(ast, assign.lhs);
            resolve_expr(ast, assign.rhs);
        },
        .Expr => |expr| {
            resolve_expr(ast, expr);
        },
    }
}

fn resolve_expr(ast: *Ast, expr: *Ast.Expr) void {
    switch (expr.payload) {
        .Binary_Op => |op| {
            resolve_expr(ast, op.lhs);
            resolve_expr(ast, op.rhs);
        },
        .Unary_Op => |op| {
            resolve_expr(ast, op.subexpr);
        },
        .If => |_if| {
            resolve_expr(ast, _if.cond);
            resolve_expr(ast, _if.if_true);
            resolve_expr(ast, _if.if_false);
        },
        .Call => |call| {
            resolve_expr(ast, call.lhs);

            var it = call.args.iterator();
            while (it.next()) |arg| {
                resolve_expr(ast, arg);
            }
        },
        .Index => |index| {
            resolve_expr(ast, index.lhs);
            resolve_expr(ast, index.index);
        },
        .Field => |field| {
            resolve_expr(ast, field.lhs);

            if (field.lhs.payload == .Type) {
                var _type = field.lhs.payload.Type;
                expr.payload = .{ .Enum_Field_From_Type = .{
                    ._type = _type,
                    .id = field.id,
                } };
            }
        },
        .Initializer => |init| {
            resolve_type(ast, init._type);

            var it = init.expr_list.iterator();
            while (it.next()) |subexpr| {
                resolve_expr(ast, subexpr);
            }
        },
        .Expr_List => |list| {
            var it = list.iterator();
            while (it.next()) |subexpr| {
                resolve_expr(ast, subexpr);
            }
        },
        .Designator => |designator| {
            resolve_expr(ast, designator.expr);
        },
        .Enum_Field_From_Type => unreachable,
        .Enum_Field => {},
        .Cast1 => |subexpr| {
            if (is_expr_a_type(ast, subexpr)) {
                common.print_error(ast.filepath, subexpr.line_info, "expected expression, not a type.", .{});
                std.os.exit(1);
            }

            resolve_expr(ast, subexpr);
        },
        .Cast2 => |cast| {
            resolve_type(ast, cast._type);
            resolve_expr(ast, cast.expr);
        },
        .Bool => {},
        .Int64 => {},
        .Null => {},
        .Type => |_type| {
            resolve_type(ast, _type);
        },
        .Symbol => {},
        .Identifier => |ident| {
            var is_type = false;
            var symbol = find_symbol(ast, ident, &is_type);

            if (is_type) {
                var _type = ast.create(Ast.Type);
                _type.* = .{
                    .payload = .{ .Symbol = symbol },
                    .size = undefined,
                    .line_info = ident.token.line_info,
                };
                expr.payload = .{ .Type = _type };
            } else {
                expr.payload = .{ .Symbol = symbol };
            }
        },
    }
}
