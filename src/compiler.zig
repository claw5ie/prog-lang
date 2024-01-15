const std = @import("std");
const utils = @import("utils.zig");
const notstd = @import("notstd.zig");

const ArenaAllocator = std.heap.ArenaAllocator;
const Allocator = std.mem.Allocator;

var VOID_TYPE_HINT = Type{
    .payload = .Void,
    .size = 0,
    .typechecking_stage = .Fully_Typechecked,
    .line_info = .{},
};
var VOID_PTR_TYPE_HINT = Type{
    .payload = .{ .Pointer = &VOID_TYPE_HINT },
    .size = 8,
    .typechecking_stage = .Fully_Typechecked,
    .line_info = .{},
};
var BOOL_TYPE_HINT = Type{
    .payload = .Bool,
    .size = 1,
    .typechecking_stage = .Fully_Typechecked,
    .line_info = .{},
};
var INT64_TYPE_HINT = Type{
    .payload = .Int64,
    .size = 8,
    .typechecking_stage = .Fully_Typechecked,
    .line_info = .{},
};

const Compiler = struct {
    local_functions: Ast.SymbolList,

    main_function: *Symbol,

    stack: []u8,
    rsp: u64 = 0,
    rbp: u64 = 0,

    pub fn grab_global(c: *Compiler) IrTmp {
        var offset = c.next_global;
        c.next_global += 8;
        return .{
            .offset = offset,
            .tag = .Global,
            .height = 0,
        };
    }

    pub fn grab_local(c: *Compiler) IrTmp {
        var offset = c.next_local;
        c.next_local += 8;

        if (c.biggest_local_so_far < c.next_local) {
            c.biggest_local_so_far = c.next_local;
        }

        return .{
            .offset = offset,
            .tag = .Local,
            .height = 0,
        };
    }

    pub fn maybe_grab_local(c: *Compiler, has_lvalue: ?*IrLvalue) IrLvalue {
        return if (has_lvalue) |lvalue|
            lvalue.*
        else
            c.grab_local().as_lvalue();
    }

    pub fn return_local(c: *Compiler, tmp: IrTmp) void {
        std.debug.assert(tmp.tag == .Local);
        c.next_local -= 8;
        std.debug.assert(c.next_local == tmp.offset);
    }

    pub fn grab_label(c: *Compiler) IrLabel {
        var result = c.next_label;
        c.next_label += 1;
        return result;
    }

    pub fn grab_buncha_labels(c: *Compiler, count: u32) IrLabel {
        var result = c.next_label;
        c.next_label += count;
        return result;
    }
};

const TypecheckerStmtContext = struct {
    return_type: *Type,
    is_in_loop: bool,
};

inline fn min_enum(x: anytype, y: @TypeOf(x)) @TypeOf(x) {
    return @enumFromInt(@min(@intFromEnum(x), @intFromEnum(y)));
}

inline fn check_flags(actual: anytype, expected: @TypeOf(actual)) bool {
    return actual & expected == expected;
}

fn find_symbol(c: *Compiler, id: Identifier, is_type: *bool) *Symbol {
    std.debug.assert(id.token.tag == .Identifier);

    var key = SymbolKey{
        .text = id.token.text,
        .scope = id.scope,
    };

    while (true) {
        var found_symbol = c.symbols.get(key);

        if (found_symbol) |symbol| {
            if (is_symbol_a_type(c, symbol)) {
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

    print_error(c, id.token.line_info, "'{s}' is not defined.", .{id.token.text});
    std.os.exit(1);
}

fn is_expr_a_type(c: *Compiler, expr: *Expr) bool {
    switch (expr.payload) {
        .Type => {
            return true;
        },
        .Identifier => |ident| {
            var is_type = false;
            var symbol = find_symbol(c, ident, &is_type);
            if (is_type) {
                var _type = ast_create(c, Type);
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

fn is_symbol_a_type(c: *Compiler, symbol: *Symbol) bool {
    switch (symbol.payload) {
        .Definition => |*definition| {
            if (definition.was_visited) {
                print_error(c, symbol.line_info, "cyclic reference detected.", .{});
                std.os.exit(1);
            }

            definition.was_visited = true;

            if (is_expr_a_type(c, definition.expr)) {
                var subtype = definition.expr.payload.Type;
                if (subtype.payload == .Symbol) {
                    symbol.payload = .{ .Type = subtype.payload.Symbol.payload.Type };
                } else {
                    var _type = extract_type(c, definition.expr.*);

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

fn resolve_identifiers(c: *Compiler) void {
    var it = c.globals.iterator();
    while (it.next()) |symbol| {
        resolve_identifiers_symbol(c, symbol.*);
    }
}

fn resolve_identifiers_symbol(c: *Compiler, symbol: *Symbol) void {
    _ = is_symbol_a_type(c, symbol);
    switch (symbol.payload) {
        .Variable => |*variable| {
            if (variable._type) |_type| {
                resolve_identifiers_type(c, _type);
            }

            if (variable.expr) |expr| {
                resolve_identifiers_expr(c, expr);
            }
        },
        .Parameter => |parameter| {
            resolve_identifiers_type(c, parameter._type);
        },
        .Function => |function| {
            resolve_identifiers_type(c, function._type);

            var it = function.block.iterator();
            while (it.next()) |stmt| {
                resolve_identifiers_stmt(c, stmt);
            }
        },
        .Type => |_type| {
            resolve_identifiers_type(c, _type);
        },
        .Struct_Field,
        .Union_Field,
        .Enum_Field,
        .Definition,
        => unreachable,
    }
}

fn resolve_identifiers_type(c: *Compiler, _type: *Type) void {
    if (_type.is_resolved) {
        return;
    }

    _type.is_resolved = true;

    switch (_type.payload) {
        .Struct => |_struct| {
            var it = _struct.fields.iterator();
            while (it.next()) |field| {
                resolve_identifiers_type(c, field.*.payload.Struct_Field._type);
            }
        },
        .Union => |_union| {
            var it = _union.fields.iterator();
            while (it.next()) |field| {
                resolve_identifiers_type(c, field.*.payload.Union_Field._type);
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
                resolve_identifiers_type(c, param.*.payload.Parameter._type);
            }

            resolve_identifiers_type(c, function.return_type);
        },
        .Array => |array| {
            resolve_identifiers_expr(c, array.expr);
            resolve_identifiers_type(c, array.subtype);
        },
        .Pointer => |subtype| {
            resolve_identifiers_type(c, subtype);
        },
        .Void,
        .Bool,
        .Int64,
        .Symbol,
        => {},
        .Identifier => |ident| {
            var is_type = false;
            var symbol = find_symbol(c, ident, &is_type);

            if (is_type) {
                _type.payload = .{ .Symbol = symbol };
            } else {
                print_error(c, ident.token.line_info, "'{s}' is not a type.", .{ident.token.text});
                std.os.exit(1);
            }
        },
    }
}

fn resolve_identifiers_block(c: *Compiler, block: StmtBlock) void {
    var it = block.stmts.iterator();
    while (it.next()) |stmt| {
        resolve_identifiers_stmt(c, stmt);
    }
}

fn resolve_identifiers_stmt(c: *Compiler, stmt: *Stmt) void {
    switch (stmt.payload) {
        .Print => |expr| {
            resolve_identifiers_expr(c, expr);
        },
        .Block => |block| {
            resolve_identifiers_block(c, block);
        },
        .If => |_if| {
            resolve_identifiers_expr(c, _if.cond);
            resolve_identifiers_block(c, _if.if_true);
            resolve_identifiers_block(c, _if.if_false);
        },
        .While => |_while| {
            resolve_identifiers_expr(c, _while.cond);
            resolve_identifiers_block(c, _while.block);
        },
        .Break => {},
        .Continue => {},
        .Switch => |_switch| {
            resolve_identifiers_expr(c, _switch.cond);

            var it = _switch.cases.iterator();
            while (it.next()) |case| {
                resolve_identifiers_expr(c, case.value);
                resolve_identifiers_block(c, case.block);
            }
        },
        .Return => {},
        .Return_Expr => |expr| {
            resolve_identifiers_expr(c, expr);
        },
        .Symbol => |symbol| {
            resolve_identifiers_symbol(c, symbol);

            if (symbol.payload == .Function) {
                var node = ast_create(c, SymbolList.Node);
                node.* = .{
                    .payload = symbol,
                };
                c.local_functions.insert_first(node);
            }
        },
        .Assign => |assign| {
            resolve_identifiers_expr(c, assign.lhs);
            resolve_identifiers_expr(c, assign.rhs);
        },
        .Expr => |expr| {
            resolve_identifiers_expr(c, expr);
        },
    }
}

fn resolve_identifiers_expr(c: *Compiler, expr: *Expr) void {
    switch (expr.payload) {
        .Binary_Op => |op| {
            resolve_identifiers_expr(c, op.lhs);
            resolve_identifiers_expr(c, op.rhs);
        },
        .Unary_Op => |op| {
            resolve_identifiers_expr(c, op.subexpr);
        },
        .If => |_if| {
            resolve_identifiers_expr(c, _if.cond);
            resolve_identifiers_expr(c, _if.if_true);
            resolve_identifiers_expr(c, _if.if_false);
        },
        .Call => |call| {
            resolve_identifiers_expr(c, call.lhs);

            var it = call.args.iterator();
            while (it.next()) |arg| {
                resolve_identifiers_expr(c, arg);
            }
        },
        .Index => |index| {
            resolve_identifiers_expr(c, index.lhs);
            resolve_identifiers_expr(c, index.index);
        },
        .Field => |field| {
            resolve_identifiers_expr(c, field.lhs);

            if (field.lhs.payload == .Type) {
                var _type = field.lhs.payload.Type;
                expr.payload = .{ .Enum_Field_From_Type = .{
                    ._type = _type,
                    .id = field.id,
                } };
            }
        },
        .Initializer => |init| {
            resolve_identifiers_type(c, init._type);

            var it = init.expr_list.iterator();
            while (it.next()) |subexpr| {
                resolve_identifiers_expr(c, subexpr);
            }
        },
        .Expr_List => |list| {
            var it = list.iterator();
            while (it.next()) |subexpr| {
                resolve_identifiers_expr(c, subexpr);
            }
        },
        .Designator => |designator| {
            resolve_identifiers_expr(c, designator.expr);
        },
        .Enum_Field_From_Type => unreachable,
        .Enum_Field => {},
        .Cast1 => |subexpr| {
            if (is_expr_a_type(c, subexpr)) {
                print_error(c, subexpr.line_info, "expected expression, not a type.", .{});
                std.os.exit(1);
            }

            resolve_identifiers_expr(c, subexpr);
        },
        .Cast2 => |cast| {
            resolve_identifiers_type(c, cast._type);
            resolve_identifiers_expr(c, cast.expr);
        },
        .Bool => {},
        .Int64 => {},
        .Null => {},
        .Type => |_type| {
            resolve_identifiers_type(c, _type);
        },
        .Symbol => {},
        .Identifier => |ident| {
            var is_type = false;
            var symbol = find_symbol(c, ident, &is_type);

            if (is_type) {
                var _type = ast_create(c, Type);
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

pub fn reduce_expr(c: *Compiler, expr: *Expr) void {
    if (expr.payload != .Int64) {
        print_error(c, expr.line_info, "TODO: evaluate expression in compile-time.", .{});
        std.os.exit(1);
    }
}

pub fn typecheck(c: *Compiler) void {
    var it = c.globals.iterator();
    while (it.next()) |symbol| {
        typecheck_symbol(c, symbol.*);
    }

    var is_type = false;
    var symbol = find_symbol(c, .{
        .token = .{
            .tag = .Identifier,
            .text = "main",
            .line_info = .{},
        },
        .scope = c.global_scope,
    }, &is_type);
    switch (symbol.payload) {
        .Function => |function| {
            var _type = &function._type.payload.Function;

            if (_type.params.count != 0) {
                print_error(c, function._type.line_info, "expected 0 arguments, but got {}.", .{_type.params.count});
                std.os.exit(1);
            }

            var return_type = _type.return_type.extract_ptr();

            if (!return_type.is(.Void)) {
                print_error(c, _type.return_type.line_info, "expected 'void', but got '{}'.", .{_type.return_type});
                std.os.exit(1);
            }
        },
        else => {
            print_error(c, symbol.line_info, "'main' should be a function.", .{});
            std.os.exit(1);
        },
    }

    c.main_function = symbol;
}

fn typecheck_symbol(c: *Compiler, symbol: *Symbol) void {
    switch (symbol.payload) {
        .Variable => |*variable| {
            if (variable._type) |_type| {
                typecheck_type(c, _type);

                if (variable.expr) |expr| {
                    var expected_type = _type.extract_ptr();
                    var actual_type = typecheck_expr(c, expected_type, expr);
                    if (!actual_type.eql(expected_type)) {
                        print_error(c, expr.line_info, "expected '{}', but got '{}'.", .{ expected_type, actual_type });
                        print_note(c, _type.line_info, "expected type is here", .{});
                        print_note(c, expr.line_info, "expression is here", .{});
                        std.os.exit(1);
                    }
                }
            } else {
                if (variable.expr) |expr| {
                    var _type = typecheck_expr(c, null, expr);
                    variable._type = _type;

                    if (_type.payload == .Function) {
                        if (_type.payload.Function.scope != c.global_scope) {
                            print_error(c, symbol.line_info, "can't use function pointers to local functions.", .{});
                            std.os.exit(1);
                        }
                    }
                } else {
                    unreachable;
                }
            }

            variable.was_visited = true;
        },
        .Parameter => |parameter| {
            typecheck_type(c, parameter._type);
        },
        .Function => |function| {
            typecheck_type(c, function._type);

            var ctx: TypecheckerStmtContext = .{
                .return_type = function._type.payload.Function.return_type.extract_ptr(),
                .is_in_loop = false,
            };
            var it = function.block.iterator();
            while (it.next()) |stmt| {
                typecheck_stmt(c, &ctx, stmt);
            }
        },
        .Type => |_type| {
            typecheck_type_flags(c, _type, 0);
        },
        .Struct_Field,
        .Union_Field,
        .Enum_Field,
        .Definition,
        => unreachable,
    }
}

const REJECT_VOID_TYPE: u8 = 0x1;
const DO_SHALLOW_TYPECHECK: u8 = 0x2;

inline fn typecheck_type(c: *Compiler, _type: *Type) void {
    typecheck_type_flags(c, _type, REJECT_VOID_TYPE);
}

fn typecheck_type_flags(c: *Compiler, _type: *Type, flags: u8) void {
    _ = typecheck_type_aux(c, _type, flags | DO_SHALLOW_TYPECHECK);
    _ = typecheck_type_aux(c, _type, flags & ~DO_SHALLOW_TYPECHECK);
}

fn typecheck_type_aux(c: *Compiler, _type: *Type, flags: u8) TypecheckingStage {
    var _flags = flags;
    var result: TypecheckingStage = .Fully_Typechecked;

    switch (_type.typechecking_stage) {
        .Not_Typechecked => {
            _type.typechecking_stage = .Being_Typechecked;
        },
        .Being_Typechecked => {
            print_error(c, _type.line_info, "cyclic reference detected.", .{});
            std.os.exit(1);
        },
        .Shallow_Typechecked => {
            if (check_flags(_flags, DO_SHALLOW_TYPECHECK)) {
                return .Shallow_Typechecked;
            }
        },
        .Fully_Typechecked => {
            return .Fully_Typechecked;
        },
    }

    switch (_type.payload) {
        .Struct => |_struct| {
            var size: TypeSize = 0;

            _flags |= REJECT_VOID_TYPE;

            var it = _struct.fields.iterator();
            while (it.next()) |field_ptr| {
                var field = &field_ptr.*.payload.Struct_Field;
                var stage = typecheck_type_aux(c, field._type, _flags);
                result = min_enum(result, stage);
                size += field._type.size;
            }

            _type.size = size;
        },
        .Union => |_union| {
            var size: TypeSize = 0;

            _flags |= REJECT_VOID_TYPE;

            var it = _union.fields.iterator();
            while (it.next()) |field_ptr| {
                var field = &field_ptr.*.payload.Union_Field;
                var stage = typecheck_type_aux(c, field._type, _flags);
                result = min_enum(result, stage);
                size = @max(size, field._type.size);
            }

            _type.size = size;
        },
        .Enum => |_enum| {
            var enum_value: u64 = 0;

            var it = _enum.fields.iterator();
            while (it.next()) |field_ptr| {
                var field = &field_ptr.*.payload.Enum_Field;
                field.value = enum_value;
                enum_value += 1;
            }

            _type.size = 8;
        },
        .Function => |function| {
            _type.size = 8;

            if (check_flags(_flags, DO_SHALLOW_TYPECHECK)) {
                result = .Shallow_Typechecked;
            } else {
                _flags |= REJECT_VOID_TYPE;

                var it = function.params.iterator();
                while (it.next()) |param| {
                    var stage = typecheck_type_aux(c, param.*.payload.Parameter._type, _flags);
                    result = min_enum(result, stage);
                }

                _flags &= ~REJECT_VOID_TYPE;
                var stage = typecheck_type_aux(c, function.return_type, _flags);
                result = min_enum(result, stage);
            }
        },
        .Array => |*array| {
            var size_type = typecheck_expr(c, &INT64_TYPE_HINT, array.expr);
            var size_flags = size_type.compare();

            if (!check_flags(size_flags, Type.Is_Integer)) {
                print_error(c, array.expr.line_info, "expected integer, but got '{}'.", .{size_type});
                std.os.exit(1);
            }

            _flags |= REJECT_VOID_TYPE;
            var stage = typecheck_type_aux(c, array.subtype, _flags);
            result = min_enum(result, stage);

            reduce_expr(c, array.expr);
            var size = array.expr.payload.Int64;
            if (size <= 0) {
                print_error(c, array.expr.line_info, "array size can't be negative ({}).", .{size});
                std.os.exit(1);
            }

            array.count = @intCast(size);
            _type.size = array.count * array.subtype.size;
        },
        .Pointer => |subtype| {
            _type.size = 8;

            if (check_flags(_flags, DO_SHALLOW_TYPECHECK)) {
                result = .Shallow_Typechecked;
            } else {
                _flags &= ~REJECT_VOID_TYPE;
                var stage = typecheck_type_aux(c, subtype, _flags);
                result = min_enum(result, stage);
            }
        },
        .Void => {
            if (check_flags(_flags, REJECT_VOID_TYPE)) {
                print_error(c, _type.line_info, "unexpected 'void' type.", .{});
                std.os.exit(1);
            }

            _type.size = 0;
        },
        .Bool => {
            _type.size = 1;
        },
        .Int64 => {
            _type.size = 8;
        },
        .Symbol => |symbol| {
            var subtype = symbol.payload.Type;
            switch (subtype.typechecking_stage) {
                .Not_Typechecked => {
                    result = typecheck_type_aux(c, subtype, _flags);
                },
                .Being_Typechecked => {
                    print_error(c, symbol.line_info, "cyclic reference detected.", .{});
                    std.os.exit(1);
                },
                .Shallow_Typechecked,
                .Fully_Typechecked,
                => {},
            }
            _type.size = subtype.size;

            if (check_flags(_flags, REJECT_VOID_TYPE) and subtype.is(.Void)) {
                print_error(c, symbol.line_info, "unexpected 'void' type.", .{});
                std.os.exit(1);
            }
        },
        .Identifier => unreachable,
    }

    _type.typechecking_stage = result;

    return result;
}

fn typecheck_block(c: *Compiler, ctx: *const TypecheckerStmtContext, block: StmtBlock) void {
    var it = block.stmts.iterator();
    while (it.next()) |stmt| {
        typecheck_stmt(c, ctx, stmt);
    }
}

fn typecheck_stmt(c: *Compiler, ctx: *const TypecheckerStmtContext, stmt: *Stmt) void {
    switch (stmt.payload) {
        .Print => |expr| {
            _ = typecheck_expr(c, null, expr);
        },
        .Block => |block| {
            typecheck_block(c, ctx, block);
        },
        .If => |_if| {
            var cond_type = typecheck_expr(c, &BOOL_TYPE_HINT, _if.cond);
            if (!cond_type.is(.Bool)) {
                print_error(c, _if.cond.line_info, "expected 'bool', but got '{}'.", .{cond_type});
                std.os.exit(1);
            }

            typecheck_block(c, ctx, _if.if_true);
            typecheck_block(c, ctx, _if.if_false);
        },
        .While => |_while| {
            var cond_type = typecheck_expr(c, &BOOL_TYPE_HINT, _while.cond);
            if (!cond_type.is(.Bool)) {
                print_error(c, _while.cond.line_info, "expected 'bool', but got '{}'.", .{cond_type});
                std.os.exit(1);
            }

            var new_ctx = ctx.*;
            new_ctx.is_in_loop = true;
            typecheck_block(c, &new_ctx, _while.block);
        },
        .Break => {
            if (!ctx.is_in_loop) {
                print_error(c, stmt.line_info, "'break' outside of loop.", .{});
                std.os.exit(1);
            }
        },
        .Continue => {
            if (!ctx.is_in_loop) {
                print_error(c, stmt.line_info, "'continue' outside of loop.", .{});
                std.os.exit(1);
            }
        },
        .Switch => |_switch| {
            var cond_type = typecheck_expr(c, null, _switch.cond);
            var cond_flags = cond_type.compare();

            if (!check_flags(cond_flags, Type.Is_Comparable)) {
                print_error(c, _switch.cond.line_info, "condition isn't comparable: '{}'.", .{cond_type});
                std.os.exit(1);
            }

            var it = _switch.cases.iterator();
            while (it.next()) |case| {
                var case_type = typecheck_expr(c, cond_type, case.value);
                if (!cond_type.eql(case_type)) {
                    print_error(c, case.value.line_info, "mismatched types: '{}' and '{}'.", .{ cond_type, case_type });
                    print_note(c, case.value.line_info, "switch case is here.", .{});
                    print_note(c, _switch.cond.line_info, "switch condition is here.", .{});
                    std.os.exit(1);
                }

                typecheck_block(c, ctx, case.block);
            }
        },
        .Return => {
            if (!ctx.return_type.is(.Void)) {
                print_error(c, stmt.line_info, "expected expression of type '{}'.", .{ctx.return_type});
                std.os.exit(1);
            }
        },
        .Return_Expr => |expr| {
            var expr_type = typecheck_expr(c, ctx.return_type, expr);
            if (ctx.return_type.is(.Void)) {
                print_error(c, expr.line_info, "unexpected expression here.", .{});
                std.os.exit(1);
            } else if (!ctx.return_type.eql(expr_type)) {
                print_error(c, expr.line_info, "mismatched types: '{}' and '{}'.", .{ ctx.return_type, expr_type });
                print_note(c, ctx.return_type.line_info, "return type is here.", .{});
                print_note(c, expr.line_info, "expression is here.", .{});
                std.os.exit(1);
            }
        },
        .Symbol => |symbol| {
            typecheck_symbol(c, symbol);
        },
        .Assign => |assign| {
            var lhs_type = typecheck_expr(c, null, assign.lhs);
            var rhs_type = typecheck_expr(c, lhs_type, assign.rhs);

            if (!assign.lhs.is_lvalue) {
                print_error(c, assign.lhs.line_info, "expression is not an lvalue.", .{});
                std.os.exit(1);
            }

            if (rhs_type.payload == .Function) {
                if (rhs_type.payload.Function.scope != c.global_scope) {
                    print_error(c, assign.rhs.line_info, "can't use function pointers to local functions.", .{});
                    std.os.exit(1);
                }
            }

            if (!lhs_type.eql(rhs_type)) {
                print_error(c, stmt.line_info, "expected '{}', but got '{}'.", .{ lhs_type, rhs_type });
                std.os.exit(1);
            }
        },
        .Expr => |expr| {
            _ = typecheck_expr_allow_void(c, null, expr);
        },
    }
}

fn typecheck_expr(c: *Compiler, type_hint: ?*Type, expr: *Expr) *Type {
    var _type = typecheck_expr_allow_void(c, type_hint, expr);
    if (_type.is(.Void)) {
        print_error(c, expr.line_info, "unexpected 'void' type.", .{});
        std.os.exit(1);
    }
    return _type;
}

fn typecheck_expr_allow_void(c: *Compiler, type_hint: ?*Type, expr: *Expr) *Type {
    switch (expr.payload) {
        .Binary_Op => |op| {
            switch (op.tag) {
                .Or,
                .And,
                => {
                    var lhs_type = typecheck_expr(c, &BOOL_TYPE_HINT, op.lhs);
                    var rhs_type = typecheck_expr(c, &BOOL_TYPE_HINT, op.rhs);

                    if (!lhs_type.is(.Bool) or !rhs_type.is(.Bool)) {
                        print_error(c, op.lhs.line_info, "mismatched types: '{}' and '{}'.", .{ lhs_type, rhs_type });
                        std.os.exit(1);
                    }

                    expr._type = &BOOL_TYPE_HINT;
                },
                .Eq,
                .Neq,
                => {
                    var lhs_type = typecheck_expr(c, null, op.lhs);
                    var rhs_type = typecheck_expr(c, lhs_type, op.rhs);
                    var lhs_flags = lhs_type.compare();

                    if (!check_flags(lhs_flags, Type.Is_Comparable)) {
                        print_error(c, op.lhs.line_info, "expression is not comparable: '{}'.", .{lhs_type});
                        std.os.exit(1);
                    } else if (!lhs_type.eql(rhs_type)) {
                        print_error(c, op.lhs.line_info, "mismatched types: '{}' and '{}'.", .{ lhs_type, rhs_type });
                        std.os.exit(1);
                    }

                    expr._type = &BOOL_TYPE_HINT;
                },
                .Lt,
                .Leq,
                .Gt,
                .Geq,
                => {
                    var lhs_type = typecheck_expr(c, null, op.lhs);
                    var rhs_type = typecheck_expr(c, lhs_type, op.rhs);
                    var lhs_flags = lhs_type.compare();

                    if (!check_flags(lhs_flags, Type.Is_Integral)) {
                        print_error(c, op.lhs.line_info, "expected integral type, but got '{}'.", .{lhs_type});
                        std.os.exit(1);
                    } else if (!lhs_type.eql(rhs_type)) {
                        print_error(c, op.lhs.line_info, "mismatched types: '{}' and '{}'.", .{ lhs_type, rhs_type });
                        std.os.exit(1);
                    }

                    expr._type = &BOOL_TYPE_HINT;
                },
                .Add => {
                    var lhs_type = typecheck_expr(c, null, op.lhs);
                    var rhs_type = typecheck_expr(c, lhs_type, op.rhs);
                    var lhs_flags = lhs_type.compare();
                    var rhs_flags = rhs_type.compare();

                    if (check_flags(lhs_flags, Type.Is_Void_Ptr) or check_flags(rhs_flags, Type.Is_Void_Ptr)) {
                        print_error(c, op.lhs.line_info, "can't add 'void' pointers.", .{});
                        std.os.exit(1);
                    } else if (check_flags(lhs_flags, Type.Is_Ptr) and check_flags(rhs_flags, Type.Is_Integer)) {
                        expr._type = lhs_type;
                    } else if (check_flags(lhs_flags, Type.Is_Integer) and check_flags(rhs_flags, Type.Is_Ptr)) {
                        expr._type = rhs_type;
                    } else if (check_flags(lhs_flags, Type.Is_Integer) and lhs_type.eql(rhs_type)) {
                        expr._type = lhs_type;
                    } else {
                        print_error(c, op.lhs.line_info, "expected integer/pointer: '{}' and '{}'", .{ lhs_type, rhs_type });
                        std.os.exit(1);
                    }
                },
                .Sub => {
                    var lhs_type = typecheck_expr(c, null, op.lhs);
                    var rhs_type = typecheck_expr(c, lhs_type, op.rhs);
                    var lhs_flags = lhs_type.compare();
                    var rhs_flags = rhs_type.compare();

                    if (check_flags(lhs_flags, Type.Is_Void_Ptr)) {
                        print_error(c, op.lhs.line_info, "can't subtract 'void' pointer.", .{});
                        std.os.exit(1);
                    } else if (check_flags(lhs_flags, Type.Is_Ptr) and check_flags(rhs_flags, Type.Is_Integer)) {
                        expr._type = lhs_type;
                    } else if (check_flags(lhs_flags, Type.Is_Integer) and lhs_type.eql(rhs_type)) {
                        expr._type = lhs_type;
                    } else {
                        print_error(c, op.lhs.line_info, "expected integer/pointer: '{}' and '{}'", .{ lhs_type, rhs_type });
                        std.os.exit(1);
                    }
                },
                .Mul,
                .Div,
                .Mod,
                => {
                    var lhs_type = typecheck_expr(c, null, op.lhs);
                    var rhs_type = typecheck_expr(c, lhs_type, op.rhs);
                    var lhs_flags = lhs_type.compare();

                    if (!check_flags(lhs_flags, Type.Is_Integer) or !lhs_type.eql(rhs_type)) {
                        print_error(c, op.lhs.line_info, "expected integer: '{}' and '{}'.", .{ lhs_type, rhs_type });
                        std.os.exit(1);
                    }

                    expr._type = lhs_type;
                },
            }
        },
        .Unary_Op => |op| {
            switch (op.tag) {
                .Not => {
                    var subexpr_type = typecheck_expr(c, &BOOL_TYPE_HINT, op.subexpr);

                    if (!subexpr_type.is(.Bool)) {
                        print_error(c, op.subexpr.line_info, "expected 'bool', but got '{}'.", .{subexpr_type});
                        std.os.exit(1);
                    }

                    expr._type = &BOOL_TYPE_HINT;
                },
                .Neg => {
                    var subexpr_type = typecheck_expr(c, null, op.subexpr);
                    var subexpr_flags = subexpr_type.compare();

                    if (!check_flags(subexpr_flags, Type.Is_Integer)) {
                        print_error(c, op.subexpr.line_info, "expected integer, but got '{}'.", .{subexpr_type});
                        std.os.exit(1);
                    }

                    expr._type = subexpr_type;
                },
                .Ref => {
                    var subexpr_type = typecheck_expr(c, null, op.subexpr);

                    if (!op.subexpr.is_lvalue) {
                        print_error(c, op.subexpr.line_info, "expression is not an lvalue.", .{});
                        std.os.exit(1);
                    }

                    expr._type = ast_create(c, Type);
                    expr._type.* = .{
                        .payload = .{ .Pointer = subexpr_type },
                        .size = 8,
                        .typechecking_stage = .Fully_Typechecked,
                        .line_info = expr.line_info,
                    };
                },
                .Deref => {
                    var subexpr_type = typecheck_expr(c, null, op.subexpr);
                    var subexpr_flags = subexpr_type.compare();

                    if (!check_flags(subexpr_flags, Type.Can_Be_Dereferenced)) {
                        print_error(c, op.subexpr.line_info, "can't dereference value of type '{}'.", .{subexpr_type});
                        std.os.exit(1);
                    }

                    expr.is_lvalue = true;

                    expr._type = subexpr_type.payload.Pointer.extract_ptr();
                },
            }
        },
        .If => |_if| {
            var cond_type = typecheck_expr(c, &BOOL_TYPE_HINT, _if.cond);
            if (!cond_type.is(.Bool)) {
                print_error(c, _if.cond.line_info, "expected 'bool', but got '{}'.", .{cond_type});
                std.os.exit(1);
            }

            var if_true_type = typecheck_expr(c, type_hint, _if.if_true);
            var if_false_type = typecheck_expr(c, type_hint, _if.if_false);

            if (!if_true_type.eql(if_false_type)) {
                print_error(c, _if.cond.line_info, "mismatched types: '{}' and '{}'.", .{ if_true_type, if_false_type });
                std.os.exit(1);
            }

            expr._type = if_true_type;
        },
        .Call => |call| {
            var lhs_type = typecheck_expr(c, null, call.lhs);
            if (!lhs_type.is(.Function)) {
                print_error(c, call.lhs.line_info, "expected a function, but got '{}'.", .{lhs_type});
                std.os.exit(1);
            }

            var function = &lhs_type.payload.Function;
            var params = function.params;

            if (params.count != call.args.count) {
                print_error(c, call.lhs.line_info, "expected {} arguments, but got {}.", .{ params.count, call.args.count });
                std.os.exit(1);
            }

            var pit = params.iterator();
            var ait = call.args.iterator();
            while (ait.next()) |arg| {
                var param = pit.next().?;
                var param_type = param.*.payload.Parameter._type.extract_ptr();
                var arg_type = typecheck_expr(c, param_type, arg);

                if (!param_type.eql(arg_type)) {
                    print_error(c, arg.line_info, "expected '{}', but got '{}'.", .{ param_type, arg_type });
                    std.os.exit(1);
                }
            }

            expr._type = function.return_type.extract_ptr();
        },
        .Index => |index| {
            var lhs_type = typecheck_expr(c, null, index.lhs);
            var index_type = typecheck_expr(c, &INT64_TYPE_HINT, index.index);
            var index_flags = index_type.compare();

            if (!check_flags(index_flags, Type.Is_Integer)) {
                print_error(c, index.lhs.line_info, "expected integer, but got '{}'.", .{index_type});
                std.os.exit(1);
            }

            expr.is_lvalue = true;

            switch (lhs_type.payload) {
                .Array => |array| {
                    expr._type = array.subtype.extract_ptr();
                },
                .Pointer => {
                    var subtype = lhs_type.payload.Pointer.extract_ptr();
                    if (subtype.payload == .Array) {
                        expr._type = subtype.payload.Array.subtype.extract_ptr();
                    } else {
                        expr._type = subtype;
                    }
                },
                else => {
                    print_error(c, index.lhs.line_info, "expected array or pointer to array, but got '{}'.", .{lhs_type});
                    std.os.exit(1);
                },
            }
        },
        .Field => |field| {
            var lhs_type = typecheck_expr(c, null, field.lhs);

            expr.is_lvalue = true;

            var _struct: *TypeStruct = undefined;

            switch (lhs_type.payload) {
                .Struct, .Union => |*struct_ptr| {
                    _struct = struct_ptr;
                },
                .Pointer => {
                    var subtype = lhs_type.payload.Pointer.extract_ptr();
                    switch (subtype.payload) {
                        .Struct,
                        .Union,
                        => |*struct_ptr| {
                            _struct = struct_ptr;
                        },
                        else => {
                            print_error(c, field.lhs.line_info, "expected pointer to struct/union, but got '{}'.", .{lhs_type});
                            std.os.exit(1);
                        },
                    }
                },
                else => {
                    print_error(c, field.lhs.line_info, "expected struct/union, but got '{}'.", .{lhs_type});
                    std.os.exit(1);
                },
            }

            var key = SymbolKey{
                .text = field.id.text,
                .scope = _struct.scope,
            };
            var found_symbol = c.symbols.get(key);

            if (found_symbol) |symbol| {
                switch (symbol.payload) {
                    .Struct_Field => |struct_field| {
                        expr._type = struct_field._type.extract_ptr();
                    },
                    .Union_Field => |struct_field| {
                        expr._type = struct_field._type.extract_ptr();
                    },
                    else => unreachable,
                }
            } else {
                print_error(c, field.lhs.line_info, "field '{s}' is not a member of a struct/union.", .{key.text});
                std.os.exit(1);
            }
        },
        .Initializer => |init| {
            typecheck_type(c, init._type);

            var _type = init._type.extract_ptr();
            var subexpr = Expr{
                .payload = .{ .Expr_List = init.expr_list },
                ._type = undefined,
                .line_info = expr.line_info,
            };
            _ = typecheck_expr_allow_void(c, _type, &subexpr);

            expr._type = _type;
        },
        .Expr_List => |list| {
            if (type_hint == null) {
                print_error(c, expr.line_info, "can't infere the type of expression list.", .{});
                std.os.exit(1);
            }

            var hint = type_hint.?;
            switch (hint.payload) {
                .Array => |array| {
                    if (list.count != array.count) {
                        print_error(c, expr.line_info, "expected {} elements, but got {}.", .{ array.count, list.count });
                        std.os.exit(1);
                    }

                    var subhint = array.subtype.extract_ptr();
                    var it = list.iterator();
                    while (it.next()) |subexpr| {
                        if (subexpr.payload == .Designator) {
                            print_error(c, subexpr.line_info, "expected expression, but got designator.", .{});
                            std.os.exit(1);
                        }

                        var subexpr_type = typecheck_expr(c, subhint, subexpr);
                        if (!subexpr_type.eql(subhint)) {
                            print_error(c, subexpr.line_info, "expected '{}', but got '{}'.", .{ subhint, subexpr_type });
                            std.os.exit(1);
                        }
                    }
                },
                .Struct,
                .Union,
                => |_struct| {
                    var it = list.iterator();
                    while (it.next()) |subexpr| {
                        if (subexpr.payload != .Designator) {
                            print_error(c, subexpr.line_info, "expected designator, but got expression.", .{});
                            std.os.exit(1);
                        }

                        var designator = &subexpr.payload.Designator;
                        var key = SymbolKey{
                            .text = designator.id.text,
                            .scope = _struct.scope,
                        };
                        var found_symbol = c.symbols.get(key);

                        if (found_symbol) |symbol| {
                            var subhint = switch (symbol.payload) {
                                .Struct_Field => |field| field._type.extract_ptr(),
                                .Union_Field => |field| field._type.extract_ptr(),
                                else => unreachable,
                            };
                            var subexpr_type = typecheck_expr(c, subhint, designator.expr);
                            if (!subhint.eql(subexpr_type)) {
                                print_error(c, designator.expr.line_info, "expected '{}', but got '{}'.", .{ subhint, subexpr_type });
                                std.os.exit(1);
                            }
                        } else {
                            print_error(c, designator.id.line_info, "field '{s}' is not a member of a struct/union.", .{designator.id.text});
                            std.os.exit(1);
                        }
                    }
                },
                else => {
                    print_error(c, expr.line_info, "expected '{}', but got expression list.", .{hint});
                    std.os.exit(1);
                },
            }

            expr._type = hint;
        },
        .Designator => unreachable,
        .Enum_Field_From_Type => |field| {
            typecheck_type(c, field._type);

            var _type = field._type.extract_ptr();
            var subexpr = Expr{
                .payload = .{ .Enum_Field = field.id },
                ._type = undefined,
                .line_info = field.id.line_info,
            };
            _ = typecheck_expr_allow_void(c, _type, &subexpr);
            expr.payload = subexpr.payload;
            expr._type = _type;
        },
        .Enum_Field => |id| {
            if (type_hint == null) {
                print_error(c, expr.line_info, "can't infere the type of enumerator.", .{});
                std.os.exit(1);
            }

            var hint = type_hint.?;
            switch (hint.payload) {
                .Enum => |_enum| {
                    var key = SymbolKey{
                        .text = id.text,
                        .scope = _enum.scope,
                    };
                    var found_symbol = c.symbols.get(key);

                    if (found_symbol) |symbol| {
                        expr.payload = .{ .Symbol = symbol };
                        expr._type = hint;
                    } else {
                        print_error(c, expr.line_info, "enumerator '{s}' is not defined.", .{id.text});
                        std.os.exit(1);
                    }
                },
                else => {
                    print_error(c, expr.line_info, "expected '{}', but got enum value.", .{hint});
                    std.os.exit(1);
                },
            }
        },
        .Cast1 => |subexpr| {
            var expr_type = typecheck_expr(c, type_hint, subexpr);
            _ = expr_type;
            unreachable;
        },
        .Cast2 => |cast| {
            typecheck_type(c, cast._type);
            var expr_type = typecheck_expr(c, cast._type, cast.expr);
            _ = expr_type;
            unreachable;
        },
        .Bool => {
            expr._type = &BOOL_TYPE_HINT;
        },
        .Int64 => {
            expr._type = &INT64_TYPE_HINT;
        },
        .Null => {
            expr._type = &VOID_PTR_TYPE_HINT;
        },
        .Type => {
            print_error(c, expr.line_info, "unexpected type.", .{});
            std.os.exit(1);
        },
        .Symbol => |symbol| {
            switch (symbol.payload) {
                .Variable => |variable| {
                    if (!variable.was_visited) {
                        print_error(c, expr.line_info, "can't use variable in its own definition.", .{});
                        std.os.exit(1);
                    }

                    expr.is_lvalue = true;

                    expr._type = variable._type.?;
                },
                .Parameter => |parameter| {
                    expr.is_lvalue = true;
                    expr._type = parameter._type;
                },
                .Function => |function| {
                    typecheck_type(c, function._type);
                    expr._type = function._type;
                },
                .Type,
                .Struct_Field,
                .Union_Field,
                .Enum_Field,
                .Definition,
                => unreachable,
            }
        },
        .Identifier => unreachable,
    }

    expr._type = expr._type.extract_ptr();

    return expr._type;
}

inline fn cast_ptr(comptime dst: type, src: anytype) dst {
    return @alignCast(@ptrCast(src));
}

fn ir_grab_pointer_from_tmp(c: *Compiler, tmp: IrTmp) u64 {
    switch (tmp.tag) {
        .Global => return @intCast(tmp.offset),
        .Local => {
            var i = tmp.height;
            var rbp = c.rbp;
            while (i > 0) : (i -= 1) {
                rbp = ir_stack_read_u64(c, rbp - -LF_RBP_OFFSET);
            }

            var rbp_i64: i64 = @bitCast(rbp);
            return @bitCast(rbp_i64 + tmp.offset);
        },
    }
}

fn ir_grab_pointer_from_address(c: *Compiler, address: IrMem) u64 {
    var offset: i64 = 0;

    if (address.choose & 0x1 == 0x1) {
        var base: i64 = @bitCast(ir_grab_value_from_tmp(c, address.base));
        offset += base;
    }

    if (address.choose & 0x2 == 0x2) {
        offset += address.disp;
    }

    return @bitCast(offset);
}

fn ir_grab_pointer_from_lvalue(c: *Compiler, lvalue: IrLvalue) u64 {
    switch (lvalue) {
        .Tmp => |tmp| return ir_grab_pointer_from_tmp(c, tmp),
        .Mem => |address| return ir_grab_pointer_from_address(c, address),
    }
}

fn ir_grab_value_from_tmp(c: *Compiler, tmp: IrTmp) u64 {
    var pointer = ir_grab_pointer_from_tmp(c, tmp);
    return ir_stack_read_u64(c, pointer);
}

fn ir_grab_value_from_rvalue(c: *Compiler, rvalue: IrRvalue) u64 {
    switch (rvalue) {
        .Lvalue => |lvalue| {
            var pointer = ir_grab_pointer_from_lvalue(c, lvalue);
            return ir_stack_read_u64(c, pointer);
        },
        .Addr => |lvalue| return ir_grab_pointer_from_lvalue(c, lvalue),
        .Label => |label| return label,
        .Imm => |imm| return @bitCast(imm),
    }
}

fn ir_stack_read_u64(c: *Compiler, pointer: u64) u64 {
    if (pointer < 8) {
        std.debug.print("error: null pointer access (0x{x}).\n", .{pointer});
        std.os.exit(1);
    } else if (pointer + 8 > c.rsp) {
        std.debug.print("error: address 0x{x} is outside of current stack pointer (0x{x}).\n", .{ pointer, c.rsp });
        std.os.exit(1);
    }
    return ir_stack_read_u64_no_check(c, pointer);
}

inline fn ir_stack_read_u64_no_check(c: *Compiler, pointer: u64) u64 {
    return cast_ptr(*u64, &c.stack[pointer]).*;
}

fn ir_stack_write_u64(c: *Compiler, pointer: u64, value: u64) void {
    if (pointer < 8) {
        std.debug.print("error: null pointer access (0x{x}).\n", .{pointer});
        std.os.exit(1);
    } else if (pointer + 8 > c.rsp) {
        std.debug.print("error: address 0x{x} is outside of current stack pointer (0x{x}).\n", .{ pointer, c.rsp });
        std.os.exit(1);
    }
    ir_stack_write_u64_no_check(c, pointer, value);
}

inline fn ir_stack_write_u64_no_check(c: *Compiler, pointer: u64, value: u64) void {
    cast_ptr(*u64, &c.stack[pointer]).* = value;
}

fn ir_stack_push(c: *Compiler, value: u64) void {
    ir_stack_write_u64_no_check(c, c.rsp, value);
    c.rsp += 8;
}

fn ir_stack_pop(c: *Compiler) u64 {
    c.rsp -= 8;
    return ir_stack_read_u64_no_check(c, c.rsp);
}

fn interpret(c: *Compiler) void {
    c.stack = gpa.alloc(u8, 2 * 1024 * 1024) catch {
        std.os.exit(1);
    };
    defer gpa.free(c.stack);

    var labels = gpa.alloc(u64, c.next_label) catch {
        std.os.exit(1);
    };
    defer gpa.free(labels);

    for (c.ir_instrs.items, 0..) |ir_instr, i| {
        switch (ir_instr) {
            .Meta => |meta| {
                switch (meta) {
                    .GFB => |gfb| {
                        labels[gfb.label] = i;
                    },
                    .GFE => |gfe| {
                        labels[gfe.label] = i;
                    },
                    .Label => |label| {
                        labels[label] = i;
                    },
                }
            },
            else => {},
        }
    }

    c.rsp = @intCast(c.next_global);
    c.rbp = c.rsp;

    var ip: u64 = 0;
    while (ip < c.ir_instrs.items.len) {
        switch (c.ir_instrs.items[ip]) {
            .Binary_Op => |op| {
                var dst = ir_grab_pointer_from_lvalue(c, op.dst);
                var src0 = ir_grab_value_from_rvalue(c, op.src0);
                var src1 = ir_grab_value_from_rvalue(c, op.src1);

                switch (op.tag) {
                    .Or => src0 = @intFromBool((src0 != 0) or (src1 != 0)),
                    .And => src0 = @intFromBool((src0 != 0) and (src1 != 0)),
                    .Eq => src0 = @intFromBool(src0 == src1),
                    .Neq => src0 = @intFromBool(src0 != src1),
                    .Lt => src0 = @intFromBool(@as(i64, @bitCast(src0)) < @as(i64, @bitCast(src1))),
                    .Leq => src0 = @intFromBool(@as(i64, @bitCast(src0)) <= @as(i64, @bitCast(src1))),
                    .Gt => src0 = @intFromBool(@as(i64, @bitCast(src0)) > @as(i64, @bitCast(src1))),
                    .Geq => src0 = @intFromBool(@as(i64, @bitCast(src0)) >= @as(i64, @bitCast(src1))),
                    .Add => src0 = @bitCast(@as(i64, @bitCast(src0)) + @as(i64, @bitCast(src1))),
                    .Sub => src0 = @bitCast(@as(i64, @bitCast(src0)) - @as(i64, @bitCast(src1))),
                    .Mul => src0 = @bitCast(@as(i64, @bitCast(src0)) * @as(i64, @bitCast(src1))),
                    .Div => src0 = @bitCast(@divTrunc(@as(i64, @bitCast(src0)), @as(i64, @bitCast(src1)))),
                    .Mod => src0 = @bitCast(@rem(@as(i64, @bitCast(src0)), @as(i64, @bitCast(src1)))),
                }

                ir_stack_write_u64(c, dst, src0);
            },
            .Unary_Op => |op| {
                var dst = ir_grab_pointer_from_lvalue(c, op.dst);
                var src = ir_grab_value_from_rvalue(c, op.src);

                switch (op.tag) {
                    .Not => src = @intFromBool(src == 0),
                    .Neg => src = @bitCast(-@as(i64, @bitCast(src))),
                }

                ir_stack_write_u64(c, dst, src);
            },
            .Jmpc => |jmpc| {
                var src0 = ir_grab_value_from_rvalue(c, jmpc.src0);
                var src1 = ir_grab_value_from_rvalue(c, jmpc.src1);

                switch (jmpc.tag) {
                    .Eq => src0 = @intFromBool(src0 == src1),
                    .Neq => src0 = @intFromBool(src0 != src1),
                    .Lt => src0 = @intFromBool(@as(i64, @bitCast(src0)) < @as(i64, @bitCast(src1))),
                    .Leq => src0 = @intFromBool(@as(i64, @bitCast(src0)) <= @as(i64, @bitCast(src1))),
                    .Gt => src0 = @intFromBool(@as(i64, @bitCast(src0)) > @as(i64, @bitCast(src1))),
                    .Geq => src0 = @intFromBool(@as(i64, @bitCast(src0)) >= @as(i64, @bitCast(src1))),
                }

                if (src0 != 0) {
                    ip = labels[jmpc.label];
                    continue;
                }
            },
            .Instr => |instr| {
                switch (instr) {
                    .Print => |tmp| {
                        var src: i64 = @bitCast(ir_grab_value_from_rvalue(c, tmp));
                        std.debug.print("{}\n", .{src});
                    },
                    .Mov => |mov| {
                        var dst = ir_grab_pointer_from_lvalue(c, mov.dst);
                        var src = ir_grab_value_from_rvalue(c, mov.src);

                        ir_stack_write_u64(c, dst, src);
                    },
                    .Jmp => |label| {
                        ip = labels[label];
                        continue;
                    },
                    .Push_Frame_Pointer => |depth| {
                        var i = depth;
                        var rbp = c.rbp;
                        while (i >= 0) : (i -= 1) {
                            rbp = ir_stack_read_u64(c, rbp - -LF_RBP_OFFSET);
                        }

                        ir_stack_push(c, rbp);
                    },
                    .Push => |src| {
                        var value = ir_grab_value_from_rvalue(c, src);
                        ir_stack_push(c, value);
                    },
                    .Pop => |count| {
                        c.rsp -= count;
                    },
                    .Call => |src| {
                        ir_stack_push(c, ip);
                        ir_stack_push(c, c.rbp);

                        var label = ir_grab_value_from_rvalue(c, src);
                        ip = labels[label];
                        continue;
                    },
                    .Ret => |label| {
                        ip = labels[label];
                        continue;
                    },
                    .Exit => {
                        break;
                    },
                }
            },
            .Meta => |meta| {
                switch (meta) {
                    .GFB => |gfb| {
                        c.rbp = c.rsp;
                        c.rsp += gfb.stack_space_used;
                    },
                    .GFE => |gfe| {
                        c.rsp -= gfe.stack_space_used;
                        c.rbp = ir_stack_pop(c);
                        ip = ir_stack_pop(c);
                    },
                    .Label => {},
                }
            },
        }

        ip += 1;
    }
}

fn generate_ir_instr(c: *Compiler, instr: IrInstr) void {
    c.ir_instrs.append(instr) catch {
        std.os.exit(1);
    };
}

const GF_FIRST_ARG_OFFSET = GF_RETURN_ADDRESS_OFFSET - 8;
const GF_RETURN_ADDRESS_OFFSET = -16 - 8;
const LF_FIRST_ARG_OFFSET = LF_RETURN_ADDRESS_OFFSET - 8;
const LF_RETURN_ADDRESS_OFFSET = -16 - 16;
const LF_RBP_OFFSET = LF_RETURN_ADDRESS_OFFSET + 8;

fn generate_ir(c: *Compiler) void {
    generate_ir_instr(c, .{ .Instr = .{
        .Call = .{
            .Label = c.main_function.payload.Function.label,
        },
    } });
    generate_ir_instr(c, .{ .Instr = .Exit });

    var it = c.globals.iterator();
    while (it.next()) |symbol| {
        generate_ir_global_symbol(c, symbol.*);
    }

    it = c.local_functions.iterator();
    while (it.next()) |symbol| {
        generate_ir_function(c, &symbol.*.payload.Function, LF_FIRST_ARG_OFFSET);
    }
}

fn generate_ir_global_symbol(c: *Compiler, symbol: *Symbol) void {
    switch (symbol.payload) {
        .Variable => |*variable| {
            // Impelement globals as lvalue [imm]?
            variable.tmp = c.grab_global();

            if (variable.expr) |expr| {
                var src_tmp = generate_ir_short_lived_rvalue(c, expr);
                generate_ir_instr(c, .{ .Instr = .{
                    .Mov = .{
                        .dst = variable.tmp.as_lvalue(),
                        .src = src_tmp,
                    },
                } });
            }
        },
        .Function => |function| {
            generate_ir_function(c, &function, GF_FIRST_ARG_OFFSET);
        },
        .Type => {},
        .Parameter,
        .Definition,
        .Struct_Field,
        .Union_Field,
        .Enum_Field,
        => unreachable,
    }
}

fn generate_ir_local_symbol(c: *Compiler, symbol: *Symbol) void {
    switch (symbol.payload) {
        .Variable => |*variable| {
            if (variable.expr) |expr| {
                variable.tmp = generate_ir_expr(c, expr);
            } else {
                variable.tmp = c.grab_local();
            }
        },
        .Function => {
            // Generate local functions separately.
        },
        .Type => {},
        .Parameter,
        .Definition,
        .Struct_Field,
        .Union_Field,
        .Enum_Field,
        => unreachable,
    }
}

fn generate_ir_function(c: *Compiler, function: *const SymbolFunction, first_arg_offset: IrOffset) void {
    {
        var param_tmp = IrTmp{
            .offset = first_arg_offset,
            .tag = .Local,
            .height = 0,
        };
        var it = function._type.payload.Function.params.iterator();
        while (it.next()) |param_ptr| {
            var param = &param_ptr.*.payload.Parameter;
            param.tmp = param_tmp;
            param_tmp.offset -= 8;
        }
    }

    {
        var old_depth = c.function_depth;
        c.function_depth = function.depth;

        var function_end_label = c.grab_label();
        var index = c.ir_instrs.items.len;
        generate_ir_instr(c, undefined);

        {
            var ctx = IrStmtContext{
                .loop_cond_label = undefined,
                .loop_end_label = undefined,
                .function_end_label = function_end_label,
                .return_address_offset = first_arg_offset + 8, // NOTE[0]: Assume that return address is right after first argument.
            };
            var it = function.block.iterator();
            while (it.next()) |stmt| {
                generate_ir_stmt(c, &ctx, stmt);
            }
        }

        {
            var stack_space_used: u32 = @intCast(c.biggest_local_so_far);
            c.ir_instrs.items[index] = .{ .Meta = .{
                .GFB = .{
                    .stack_space_used = stack_space_used,
                    .label = function.label,
                },
            } };

            generate_ir_instr(c, .{ .Meta = .{
                .GFE = .{
                    .stack_space_used = stack_space_used,
                    .label = function_end_label,
                },
            } });
        }

        c.next_local = 0;
        c.biggest_local_so_far = 0;
        c.function_depth = old_depth;
    }
}

fn generate_ir_block(c: *Compiler, ctx: *const IrStmtContext, block: StmtBlock) void {
    var old_next_local = c.next_local;

    var it = block.stmts.iterator();
    while (it.next()) |stmt| {
        generate_ir_stmt(c, ctx, stmt);
    }

    if (block.scope != null) {
        c.next_local = old_next_local;
    }
}

fn generate_ir_stmt(c: *Compiler, ctx: *const IrStmtContext, stmt: *Stmt) void {
    var old_next_local = c.next_local;
    var should_clean_locals = true;

    switch (stmt.payload) {
        .Print => |expr| {
            var src_tmp = generate_ir_rvalue(c, expr, null);
            generate_ir_instr(c, .{ .Instr = .{
                .Print = src_tmp,
            } });
        },
        .Block => |block| {
            generate_ir_block(c, ctx, block);
        },
        .If => |_if| {
            var is_if_true_there: u2 = @intFromBool(_if.if_true.stmts.count != 0);
            var is_if_false_there: u2 = @intFromBool(_if.if_false.stmts.count != 0);
            switch ((is_if_true_there << 1) | is_if_false_there) {
                0b00 => {
                    _ = generate_ir_rvalue(c, _if.cond, null);
                },
                0b01 => {
                    var end_label = c.grab_label();

                    generate_ir_jump(c, _if.cond, true, end_label);
                    generate_ir_block(c, ctx, _if.if_false);
                    generate_ir_instr(c, .{ .Meta = .{
                        .Label = end_label,
                    } });
                },
                0b10 => {
                    var end_label = c.grab_label();

                    generate_ir_jump(c, _if.cond, false, end_label);
                    generate_ir_block(c, ctx, _if.if_true);
                    generate_ir_instr(c, .{ .Meta = .{
                        .Label = end_label,
                    } });
                },
                0b11 => {
                    var false_label = c.grab_label();
                    var end_label = c.grab_label();

                    generate_ir_jump(c, _if.cond, false, false_label);
                    generate_ir_block(c, ctx, _if.if_true);
                    generate_ir_instr(c, .{ .Instr = .{
                        .Jmp = end_label,
                    } });
                    generate_ir_instr(c, .{ .Meta = .{
                        .Label = false_label,
                    } });
                    generate_ir_block(c, ctx, _if.if_false);
                    generate_ir_instr(c, .{ .Meta = .{
                        .Label = end_label,
                    } });
                },
            }
        },
        .While => |_while| {
            var block_label = c.grab_label();
            var cond_label = c.grab_label();
            var end_label = c.grab_label();

            if (!_while.is_do_while) {
                generate_ir_instr(c, .{ .Instr = .{
                    .Jmp = cond_label,
                } });
            }

            generate_ir_instr(c, .{ .Meta = .{
                .Label = block_label,
            } });

            var new_ctx = ctx.*;
            new_ctx.loop_cond_label = cond_label;
            new_ctx.loop_end_label = end_label;

            generate_ir_block(c, &new_ctx, _while.block);
            generate_ir_instr(c, .{ .Meta = .{
                .Label = cond_label,
            } });
            generate_ir_jump(c, _while.cond, true, block_label);
        },
        .Break => {
            generate_ir_instr(c, .{ .Instr = .{
                .Jmp = ctx.loop_end_label,
            } });
        },
        .Continue => {
            generate_ir_instr(c, .{ .Instr = .{
                .Jmp = ctx.loop_cond_label,
            } });
        },
        .Switch => |_switch| {
            var cond_tmp = generate_ir_expr(c, _switch.cond);

            var first_case_label = c.grab_buncha_labels(@intCast(_switch.cases.count));
            var end_label = c.grab_label();

            {
                var label = first_case_label;
                var it = _switch.cases.iterator();
                while (it.next()) |case| {
                    var case_tmp = generate_ir_short_lived_rvalue(c, case.value);
                    generate_ir_instr(c, .{ .Jmpc = .{
                        .tag = .Eq,
                        .src0 = cond_tmp.as_rvalue(),
                        .src1 = case_tmp,
                        .label = label,
                    } });
                    label += 1;
                }
            }

            generate_ir_instr(c, .{ .Instr = .{
                .Jmp = end_label,
            } });

            {
                var label = first_case_label;
                var it = _switch.cases.iterator();
                while (it.next()) |case| {
                    generate_ir_instr(c, .{ .Meta = .{
                        .Label = label,
                    } });
                    generate_ir_block(c, ctx, case.block);
                    label += 1;

                    if (!case.should_fallthrough) {
                        generate_ir_instr(c, .{ .Instr = .{
                            .Jmp = end_label,
                        } });
                    }
                }
            }

            generate_ir_instr(c, .{ .Meta = .{
                .Label = end_label,
            } });
        },
        .Return => {
            generate_ir_instr(c, .{ .Instr = .{
                .Ret = ctx.function_end_label,
            } });
        },
        .Return_Expr => |expr| {
            var dst_tmp = .{ .Mem = .{
                .choose = CHOOSE_BASE,
                .base = .{
                    .offset = ctx.return_address_offset,
                    .tag = .Local,
                    .height = 0,
                },
            } };
            _ = generate_ir_rvalue(c, expr, &dst_tmp);
            generate_ir_instr(c, .{ .Instr = .{
                .Ret = ctx.function_end_label,
            } });
        },
        .Symbol => |symbol| {
            should_clean_locals = false;
            generate_ir_local_symbol(c, symbol);
        },
        .Assign => |assign| {
            var dst_tmp = generate_ir_lvalue(c, assign.lhs);
            _ = generate_ir_rvalue(c, assign.rhs, &dst_tmp);
        },
        .Expr => |expr| {
            _ = generate_ir_rvalue(c, expr, null);
        },
    }

    if (should_clean_locals) {
        c.next_local = old_next_local;
    }
}

fn generate_ir_jump(c: *Compiler, expr: *Expr, jump_if_true: bool, label: IrLabel) void {
    var old_next_local = c.next_local;

    var tag: IrInstrJmpcTag = .Neq;
    var src0_tmp: IrRvalue = undefined;
    var src1_tmp: IrRvalue = .{ .Imm = 0 };

    _ = fill_operands: {
        if (expr.payload == .Binary_Op) {
            var op = &expr.payload.Binary_Op;

            switch (op.tag) {
                .Eq => tag = .Eq,
                .Neq => tag = .Neq,
                .Lt => tag = .Lt,
                .Leq => tag = .Leq,
                .Gt => tag = .Gt,
                .Geq => tag = .Geq,
                else => {
                    src0_tmp = generate_ir_rvalue(c, expr, null);
                    break :fill_operands;
                },
            }

            src0_tmp = generate_ir_rvalue(c, op.lhs, null);
            src1_tmp = generate_ir_rvalue(c, op.rhs, null);
        } else {
            src0_tmp = generate_ir_rvalue(c, expr, null);
        }
    };

    if (!jump_if_true) {
        tag = switch (tag) {
            .Eq => .Neq,
            .Neq => .Eq,
            .Lt => .Geq,
            .Leq => .Gt,
            .Gt => .Leq,
            .Geq => .Lt,
        };
    }

    generate_ir_instr(c, .{ .Jmpc = .{
        .tag = tag,
        .src0 = src0_tmp,
        .src1 = src1_tmp,
        .label = label,
    } });

    c.next_local = old_next_local;
}

fn generate_ir_expr(c: *Compiler, expr: *Expr) IrTmp {
    var dst_tmp = c.grab_local();
    var dst_lvalue = dst_tmp.as_lvalue();
    _ = generate_ir_rvalue(c, expr, &dst_lvalue);
    return dst_tmp;
}

// Can't allocate any locals after this call to avoid trashing other locals.
fn generate_ir_short_lived_rvalue(c: *Compiler, expr: *Expr) IrRvalue {
    var old_next_local = c.next_local;
    var result = generate_ir_rvalue(c, expr, null);
    c.next_local = old_next_local;

    return result;
}

fn generate_ir_rvalue(c: *Compiler, expr: *Expr, has_dst_tmp: ?*IrLvalue) IrRvalue {
    var was_destination_used = false;
    var result: IrRvalue = result: {
        switch (expr.payload) {
            .Binary_Op => |op| {
                var lhs_tmp = generate_ir_rvalue(c, op.lhs, null);
                var rhs_tmp = generate_ir_rvalue(c, op.rhs, null);

                var tag: IrInstrBinaryOpTag = switch (op.tag) {
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
                };

                was_destination_used = true;
                var dst_tmp = c.maybe_grab_local(has_dst_tmp);
                generate_ir_instr(c, .{ .Binary_Op = .{
                    .tag = tag,
                    .dst = dst_tmp,
                    .src0 = lhs_tmp,
                    .src1 = rhs_tmp,
                } });

                break :result dst_tmp.as_rvalue();
            },
            .Unary_Op => |op| {
                switch (op.tag) {
                    .Not => {
                        was_destination_used = true;
                        var dst_tmp = c.maybe_grab_local(has_dst_tmp);
                        var src_tmp = generate_ir_rvalue(c, op.subexpr, null);
                        generate_ir_instr(c, .{ .Unary_Op = .{
                            .tag = .Not,
                            .dst = dst_tmp,
                            .src = src_tmp,
                        } });
                        break :result dst_tmp.as_rvalue();
                    },
                    .Neg => {
                        was_destination_used = true;
                        var dst_tmp = c.maybe_grab_local(has_dst_tmp);
                        var src_tmp = generate_ir_rvalue(c, op.subexpr, null);
                        generate_ir_instr(c, .{ .Unary_Op = .{
                            .tag = .Neg,
                            .dst = dst_tmp,
                            .src = src_tmp,
                        } });
                        break :result dst_tmp.as_rvalue();
                    },
                    .Ref => {
                        break :result .{ .Addr = generate_ir_lvalue(c, op.subexpr) };
                    },
                    .Deref => {
                        var src_tmp = generate_ir_rvalue(c, op.subexpr, null);
                        break :result src_tmp.deref(c);
                    },
                }
            },
            .If => |_if| {
                var false_label = c.grab_label();
                var end_label = c.grab_label();

                generate_ir_jump(c, _if.cond, false, false_label);

                was_destination_used = true;
                var dst_tmp = c.maybe_grab_local(has_dst_tmp);
                _ = generate_ir_rvalue(c, _if.if_true, &dst_tmp);
                generate_ir_instr(c, .{ .Instr = .{
                    .Jmp = end_label,
                } });
                generate_ir_instr(c, .{ .Meta = .{
                    .Label = false_label,
                } });

                _ = generate_ir_rvalue(c, _if.if_false, &dst_tmp);
                generate_ir_instr(c, .{ .Meta = .{
                    .Label = end_label,
                } });

                break :result dst_tmp.as_rvalue();
            },
            .Call => |call| {
                var it = call.args.reverse_iterator();
                while (it.prev()) |arg| {
                    var arg_tmp = generate_ir_short_lived_rvalue(c, arg);
                    generate_ir_instr(c, .{ .Instr = .{
                        .Push = arg_tmp,
                    } });
                }

                var bytes_to_pop: u64 = call.args.count * 8;

                // Return address must the first thing pushed to the stack after arguments, because NOTE[0] relies on that assumption.
                was_destination_used = true;
                var dst_tmp = c.maybe_grab_local(has_dst_tmp);
                generate_ir_instr(c, .{ .Instr = .{
                    .Push = .{ .Addr = dst_tmp },
                } });

                bytes_to_pop += 8;

                if (call.lhs.payload == .Symbol and call.lhs.payload.Symbol.payload == .Function) {
                    var symbol = call.lhs.payload.Symbol;
                    var function = &symbol.payload.Function;

                    if (symbol.key.scope != c.global_scope) {
                        generate_ir_instr(c, .{ .Instr = .{
                            .Push_Frame_Pointer = c.function_depth - function.depth,
                        } });
                        bytes_to_pop += 8;
                    }
                }

                var src_tmp = generate_ir_rvalue(c, call.lhs, null);
                generate_ir_instr(c, .{ .Instr = .{
                    .Call = src_tmp,
                } });
                if (bytes_to_pop > 0) {
                    generate_ir_instr(c, .{ .Instr = .{
                        .Pop = bytes_to_pop,
                    } });
                }

                break :result dst_tmp.as_rvalue();
            },
            .Index,
            .Field,
            .Initializer,
            .Expr_List,
            .Designator,
            .Cast1,
            .Cast2,
            => unreachable,
            .Bool => |value| {
                break :result .{ .Imm = @intFromBool(value) };
            },
            .Int64 => |value| {
                break :result .{ .Imm = value };
            },
            .Null => {
                break :result .{ .Imm = 0 };
            },
            .Symbol => |symbol| {
                switch (symbol.payload) {
                    .Variable => |variable| {
                        var tmp = variable.tmp;
                        tmp.height = c.function_depth - symbol.depth;

                        break :result tmp.as_rvalue();
                    },
                    .Parameter => |parameter| {
                        break :result parameter.tmp.as_rvalue();
                    },
                    .Function => |function| {
                        break :result .{ .Label = function.label };
                    },
                    .Enum_Field => |field| {
                        break :result .{ .Imm = @intCast(field.value) };
                    },
                    .Struct_Field,
                    .Union_Field,
                    .Type,
                    .Definition,
                    => unreachable,
                }
            },
            .Enum_Field_From_Type,
            .Enum_Field,
            .Type,
            .Identifier,
            => unreachable,
        }
    };

    if (!was_destination_used) {
        if (has_dst_tmp) |dst_tmp| {
            generate_ir_instr(c, .{ .Instr = .{
                .Mov = .{
                    .dst = dst_tmp.*,
                    .src = result,
                },
            } });
            return dst_tmp.as_rvalue();
        }
    }

    return result;
}

fn generate_ir_lvalue(c: *Compiler, expr: *Expr) IrLvalue {
    switch (expr.payload) {
        .Unary_Op => |op| {
            std.debug.assert(op.tag == .Deref);
            var src_tmp = generate_ir_rvalue(c, op.subexpr, null);
            return src_tmp.ref(c);
        },
        .Symbol => |symbol| {
            switch (symbol.payload) {
                .Variable => |variable| {
                    var tmp = variable.tmp;
                    tmp.height = c.function_depth - symbol.depth;

                    return tmp.as_lvalue();
                },
                .Parameter => |parameter| {
                    return parameter.tmp.as_lvalue();
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

fn debug_print_ir_tmp(tmp: IrTmp) void {
    switch (tmp.tag) {
        .Global => {
            std.debug.assert(tmp.height == 0);
            std.debug.print("${}", .{tmp.offset});
        },
        .Local => {
            if (tmp.height == 0) {
                std.debug.print("%{:<1}", .{tmp.offset});
            } else {
                std.debug.print("%{:<1}({}^)", .{ tmp.offset, tmp.height });
            }
        },
    }
}

fn debug_print_ir_address(address: IrMem) void {
    std.debug.print("[", .{});

    var should_print_delim = false;

    if (address.choose & CHOOSE_BASE == CHOOSE_BASE) {
        debug_print_ir_tmp(address.base);
        should_print_delim = true;
    }

    if (address.choose & CHOOSE_DISP == CHOOSE_DISP) {
        if (should_print_delim) {
            std.debug.print(" + ", .{});
        }
        std.debug.print("{}", .{address.disp});
        should_print_delim = true;
    }

    std.debug.print("]", .{});
}

fn debug_print_ir_lvalue(lvalue: IrLvalue) void {
    switch (lvalue) {
        .Tmp => |tmp| debug_print_ir_tmp(tmp),
        .Mem => |address| debug_print_ir_address(address),
    }
}

fn debug_print_ir_rvalue(rvalue: IrRvalue) void {
    switch (rvalue) {
        .Lvalue => |lvalue| debug_print_ir_lvalue(lvalue),
        .Addr => |lvalue| {
            std.debug.print("addr ", .{});
            debug_print_ir_lvalue(lvalue);
        },
        .Label => |label| std.debug.print("L{}", .{label}),
        .Imm => |imm| std.debug.print("{}", .{imm}),
    }
}

fn debug_print_ir_instr(ir_instr: IrInstr) void {
    switch (ir_instr) {
        .Binary_Op => |op| {
            var text: []const u8 = switch (op.tag) {
                .Or => "or",
                .And => "and",
                .Eq => "eq ",
                .Neq => "neq",
                .Lt => "lt ",
                .Leq => "leq",
                .Gt => "gt ",
                .Geq => "geq",
                .Add => "add",
                .Sub => "sub",
                .Mul => "mul",
                .Div => "div",
                .Mod => "mod",
            };

            std.debug.print("    {s}    ", .{text});
            debug_print_ir_lvalue(op.dst);
            std.debug.print(", ", .{});
            debug_print_ir_rvalue(op.src0);
            std.debug.print(", ", .{});
            debug_print_ir_rvalue(op.src1);
        },
        .Unary_Op => |op| {
            var text: []const u8 = switch (op.tag) {
                .Not => "not",
                .Neg => "neg",
            };

            std.debug.print("    {s}    ", .{text});
            debug_print_ir_lvalue(op.dst);
            std.debug.print(", ", .{});
            debug_print_ir_rvalue(op.src);
        },
        .Jmpc => |jmpc| {
            var text: []const u8 = switch (jmpc.tag) {
                .Eq => "je ",
                .Neq => "jne",
                .Lt => "jl ",
                .Leq => "jle",
                .Gt => "jg ",
                .Geq => "jge",
            };

            std.debug.print("    {s}    ", .{text});
            debug_print_ir_rvalue(jmpc.src0);
            std.debug.print(", ", .{});
            debug_print_ir_rvalue(jmpc.src1);
            std.debug.print(", L{}", .{jmpc.label});
        },
        .Instr => |instr| {
            switch (instr) {
                .Print => |src| {
                    std.debug.print("    print  ", .{});
                    debug_print_ir_rvalue(src);
                },
                .Mov => |mov| {
                    std.debug.print("    mov    ", .{});
                    debug_print_ir_lvalue(mov.dst);
                    std.debug.print(", ", .{});
                    debug_print_ir_rvalue(mov.src);
                },
                .Jmp => |label| {
                    std.debug.print("    jmp    L{}", .{label});
                },
                .Push_Frame_Pointer => |depth| {
                    std.debug.print("    push   ({}^)", .{depth});
                },
                .Push => |src| {
                    std.debug.print("    push   ", .{});
                    debug_print_ir_rvalue(src);
                },
                .Pop => |count| {
                    std.debug.print("    pop    {}", .{count});
                },
                .Call => |src| {
                    std.debug.print("    call   ", .{});
                    debug_print_ir_rvalue(src);
                },
                .Ret => |label| {
                    std.debug.print("    ret    L{}", .{label});
                },
                .Exit => {
                    std.debug.print("    exit", .{});
                },
            }
        },
        .Meta => |meta| {
            switch (meta) {
                .GFB => |gfb| {
                    std.debug.print("GFB L{}: {}b", .{ gfb.label, gfb.stack_space_used });
                },
                .GFE => |gfe| {
                    std.debug.print("GFE L{}: {}b", .{ gfe.label, gfe.stack_space_used });
                    std.debug.print("\n", .{});
                },
                .Label => |label| {
                    std.debug.print("L{}:", .{label});
                },
            }
        },
    }

    std.debug.print("\n", .{});
}

fn debug_print_ir(c: *Compiler) void {
    for (c.ir_instrs.items) |ir_instr| {
        debug_print_ir_instr(ir_instr);
    }
}

pub fn compile() void {
    var args = std.process.args();
    var filepath: [:0]const u8 = "examples/debug";

    _ = args.next().?;
    if (args.next()) |arg| {
        filepath = arg;
    }

    var compiler = Compiler{
        .globals = .{},
        .local_functions = .{},
        .ir_instrs = IrInstrList.init(gpa),
        .stack = &[0]u8{},
    };

    parse_top_level(&compiler);
    resolve_identifiers(&compiler);
    typecheck(&compiler);
    generate_ir(&compiler);
    debug_print_ir(&compiler);
    interpret(&compiler);

    gpa.free(source_code);
    compiler.symbols.deinit();
    ast_arena.deinit();
    compiler.ir_instrs.deinit();
    std.debug.assert(general_purpose_allocator.deinit() == .ok);
}
