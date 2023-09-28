const std = @import("std");
const utils = @import("utils.zig");
const notstd = @import("notstd.zig");

const ArenaAllocator = std.heap.ArenaAllocator;
const Allocator = std.mem.Allocator;

const stderr = std.io.getStdErr().writer();

var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
var gpa = general_purpose_allocator.allocator();

const GLOBAL_SCOPE_ID = 0;
const MAX_SCOPE_COUNT = 256;
var scopes_buffer: [MAX_SCOPE_COUNT]ScopeId = undefined;
var scopes: []ScopeId = scopes_buffer[0..0];
var next_scope: ScopeId = GLOBAL_SCOPE_ID;

const LOWEST_PREC = -127;

const Compiler = struct {
    const LOOKAHEAD = 2;
    tokens: [LOOKAHEAD]Token = undefined,
    token_start: u8 = 0,
    token_count: u8 = 0,
    filepath: []const u8,
    source_code: [:0]u8,
    line_info: LineInfo = .{},

    globals: SymbolList,
    main_function: *SymbolFunction,
    ast_arena: ArenaAllocator,
    ast_arena_allocator: Allocator,
    symbols: SymbolTable,

    instr_list: IrInstrList,
    next_tmp: IrTmp = 0,
    next_label: IrLabel = 0,
    biggest_next_tmp: IrTmp = 0,
    last_tmp_in_scope: IrTmp = 0,

    pub fn advance(c: *Compiler) void {
        c.token_start += 1;
        c.token_start %= LOOKAHEAD;
        c.token_count -= 1;
    }

    pub fn grab(c: *Compiler) Token {
        if (c.token_count == 0) {
            c.buffer_token();
        }

        return c.tokens[c.token_start];
    }

    pub fn peek(c: *Compiler) TokenTag {
        if (c.token_count == 0) {
            c.buffer_token();
        }

        return c.tokens[c.token_start].tag;
    }

    pub fn peek_ahead(c: *Compiler, index: usize) TokenTag {
        std.debug.assert(index < LOOKAHEAD);

        while (index >= c.token_count) {
            c.buffer_token();
        }

        return c.tokens[(c.token_start + index) % LOOKAHEAD].tag;
    }

    pub fn expect(c: *Compiler, expected: TokenTag) void {
        if (c.peek() != expected) {
            var token = c.grab();
            print_error(c, token.line_info, "expected {s}, but got {s}.", .{ token_tag_to_text(expected), token_tag_to_text(token.tag) });
            std.os.exit(1);
        }
        c.advance();
    }

    fn buffer_token(c: *Compiler) void {
        var text = c.source_code;
        var i: usize = c.line_info.offset;

        while (text[i] != 0) {
            while (std.ascii.isWhitespace(text[i])) : (i += 1) {
                c.advance_line_info();
            }

            if (text[i] == '/' and text[i + 1] == '/') {
                while (text[i] != 0 and text[i] != '\n') : (i += 1) {
                    c.advance_line_info();
                }
            } else {
                break;
            }
        }

        var token = Token{
            .tag = .End_Of_File,
            .text = text[i..i],
            .line_info = c.line_info,
        };

        if (text[i] == 0) {
            // leave.
        } else if (std.ascii.isDigit(text[i])) {
            while (std.ascii.isDigit(text[i])) : (i += 1) {
                c.advance_line_info();
            }

            token.tag = .Integer;
            token.text.len = i - token.line_info.offset;
        } else if (std.ascii.isAlphabetic(text[i]) or text[i] == '_') {
            while (std.ascii.isAlphanumeric(text[i]) or text[i] == '_') : (i += 1) {
                c.advance_line_info();
            }

            token.tag = .Identifier;
            token.text.len = i - token.line_info.offset;

            const ReservedKeyword = struct {
                text: []const u8,
                tag: TokenTag,
            };

            const keywords = [_]ReservedKeyword{
                .{ .text = "print", .tag = .Print },
                .{ .text = "if", .tag = .If },
                .{ .text = "then", .tag = .Then },
                .{ .text = "else", .tag = .Else },
                .{ .text = "while", .tag = .While },
                .{ .text = "do", .tag = .Do },
                .{ .text = "return", .tag = .Return },
                .{ .text = "proc", .tag = .Proc },
                .{ .text = "void", .tag = .Void },
                .{ .text = "bool", .tag = .Bool },
                .{ .text = "int", .tag = .Int },
                .{ .text = "false", .tag = .False },
                .{ .text = "true", .tag = .True },
            };

            for (keywords) |keyword| {
                if (std.mem.eql(u8, token.text, keyword.text)) {
                    token.tag = keyword.tag;
                    break;
                }
            }
        } else {
            const ReservedSymbol = struct {
                text: []const u8,
                tag: TokenTag,
            };

            const symbols = [_]ReservedSymbol{
                .{ .text = "||", .tag = .Or },
                .{ .text = "&&", .tag = .And },
                .{ .text = "==", .tag = .Eq },
                .{ .text = "!=", .tag = .Neq },
                .{ .text = "<=", .tag = .Leq },
                .{ .text = ">=", .tag = .Geq },
                .{ .text = "->", .tag = .Arrow },
                .{ .text = ":=", .tag = .Colon_Equal },
                .{ .text = "<", .tag = .Lt },
                .{ .text = ">", .tag = .Gt },
                .{ .text = "+", .tag = .Add },
                .{ .text = "-", .tag = .Sub },
                .{ .text = "*", .tag = .Mul },
                .{ .text = "/", .tag = .Div },
                .{ .text = "%", .tag = .Mod },
                .{ .text = "!", .tag = .Not },
                .{ .text = "(", .tag = .Open_Paren },
                .{ .text = ")", .tag = .Close_Paren },
                .{ .text = "{", .tag = .Open_Curly },
                .{ .text = "}", .tag = .Close_Curly },
                .{ .text = ":", .tag = .Colon },
                .{ .text = ";", .tag = .Semicolon },
                .{ .text = "=", .tag = .Equal },
                .{ .text = ",", .tag = .Comma },
            };

            var found_symbol = false;

            for (symbols) |symbol| {
                if (utils.is_prefix(symbol.text, text[i..])) {
                    found_symbol = true;
                    i += symbol.text.len;
                    c.line_info.column += symbol.text.len;
                    c.line_info.offset += symbol.text.len;

                    token.tag = symbol.tag;
                    token.text.len = symbol.text.len;
                    break;
                }
            }

            if (!found_symbol) {
                print_error(c, token.line_info, "unrecognized character '{c}'.\n", .{text[i]});
                std.os.exit(1);
            }
        }

        std.debug.assert(c.token_count < LOOKAHEAD);
        var index = (c.token_start + c.token_count) % LOOKAHEAD;
        c.tokens[index] = token;
        c.token_count += 1;
    }

    fn advance_line_info(c: *Compiler) void {
        c.line_info.column += 1;
        c.line_info.offset += 1;
        if (c.source_code[c.line_info.offset - 1] == '\n') {
            c.line_info.line += 1;
            c.line_info.column = 1;
        }
    }
};

const LineInfo = struct {
    line: usize = 1,
    column: usize = 1,
    offset: usize = 0,
};

const TokenTag = enum {
    Or,
    And,
    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Not,
    Open_Paren,
    Close_Paren,
    Open_Curly,
    Close_Curly,
    Arrow,
    Colon_Equal,
    Colon,
    Semicolon,
    Equal,
    Comma,

    Print,
    If,
    Then,
    Else,
    While,
    Do,
    Return,

    Proc,
    Void,
    Bool,
    Int,

    False,
    True,

    Identifier,
    Integer,

    End_Of_File,
};

const Token = struct {
    tag: TokenTag,
    text: []const u8,
    line_info: LineInfo,
};

const ScopeId = u32;

const TypeFunction = struct {
    params: ParameterList,
    return_type: *Type,
};

const TypeTag = enum {
    Function,
    Void,
    Bool,
    Int64,
};

const Type = union(TypeTag) {
    Function: TypeFunction,
    Void: void,
    Bool: void,
    Int64: void,

    pub fn eql(self: Type, other: Type) bool {
        if (self != @as(TypeTag, other)) {
            return false;
        }

        switch (self) {
            .Function => |sfunc| {
                var ofunc = &other.Function;

                if (sfunc.params.count != ofunc.params.count) {
                    return false;
                }

                var sit = sfunc.params.iterator();
                var oit = ofunc.params.iterator();
                while (sit.next()) |sparam_type| {
                    var oparam_type = oit.next().?;

                    if (!sparam_type.*.typ.eql(oparam_type.*.typ)) {
                        return false;
                    }
                }

                return sfunc.return_type.eql(ofunc.return_type.*);
            },
            .Void,
            .Bool,
            .Int64,
            => return true,
        }
    }

    pub fn format(typ: Type, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (typ) {
            .Function => |function| {
                try writer.writeAll("proc(");

                var it = function.params.iterator();
                while (it.next()) |param| {
                    try writer.print("{}", .{param.*.typ});

                    if (it.has_next()) {
                        try writer.writeAll(", ");
                    }
                }

                try writer.print(") -> {}", .{function.return_type});
            },
            .Void => try writer.writeAll("void"),
            .Bool => try writer.writeAll("bool"),
            .Int64 => try writer.writeAll("int64"),
        }
    }
};

const ExprBinaryOpTag = enum {
    Or,
    And,
    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
};

const ExprUnaryOpTag = enum {
    Not,
    Neg,
};

const ExprTag = enum {
    Binary_Op,
    Unary_Op,
    If,
    Call,
    Bool,
    Int64,
    Symbol,
    Identifier,
};

const ExprPayload = union(ExprTag) {
    Binary_Op: struct {
        tag: ExprBinaryOpTag,
        lhs: *Expr,
        rhs: *Expr,
    },
    Unary_Op: struct {
        tag: ExprUnaryOpTag,
        subexpr: *Expr,
    },
    If: struct {
        cond: *Expr,
        if_true: *Expr,
        if_false: *Expr,
    },
    Call: struct {
        lhs: *Expr,
        args: ExprList,
    },
    Bool: bool,
    Int64: i64,
    Symbol: *Symbol,
    Identifier: Token,
};

const Expr = struct {
    payload: ExprPayload,
    line_info: LineInfo,
    is_lvalue: bool = false,
};

const ExprList = notstd.DoublyLinkedList(Expr);

const StmtTag = enum {
    Print,
    Block,
    If,
    While,
    Return,
    Return_Expr,
    Symbol,
    Assign,
    Expr,
};

const StmtPayload = union(StmtTag) {
    Print: *Expr,
    Block: StmtBlock,
    If: struct {
        cond: *Expr,
        if_true: StmtBlock,
        if_false: StmtBlock,
    },
    While: struct {
        cond: *Expr,
        block: StmtBlock,
    },
    Return: void,
    Return_Expr: *Expr,
    Symbol: *Symbol,
    Assign: struct {
        lhs: *Expr,
        rhs: *Expr,
    },
    Expr: *Expr,
};

const Stmt = struct {
    payload: StmtPayload,
    line_info: LineInfo,
};

const StmtList = notstd.DoublyLinkedList(Stmt);

const StmtBlock = struct {
    stmts: StmtList,
    scope: ?ScopeId,
};

const ParameterList = notstd.DoublyLinkedList(*SymbolParameter);

const SymbolVariable = struct {
    typ: ?Type,
    expr: ?Expr,
    was_visited: bool = false,
    storage: IrOperand,
    line_info: LineInfo,
};

const SymbolParameter = struct {
    typ: Type,
    storage: IrOperand,
    line_info: LineInfo,
};

const SymbolFunction = struct {
    typ: TypeFunction,
    block: StmtBlock,
    label: IrLabel,
    line_info: LineInfo,
    is_type_typechecked: bool = false,
};

const SymbolTag = enum {
    Variable,
    Parameter,
    Function,
};

const Symbol = union(SymbolTag) {
    Variable: SymbolVariable,
    Parameter: SymbolParameter,
    Function: SymbolFunction,
};

const SymbolList = notstd.DoublyLinkedList(*Symbol);

const SymbolKey = struct {
    text: []const u8,
    scope: ScopeId,
};

const SymbolTableContext = struct {
    pub fn hash(_: SymbolTableContext, key: SymbolKey) u64 {
        const MurMur = std.hash.Murmur2_64;

        var h0 = MurMur.hash(key.text);
        var h1 = MurMur.hashUint32(key.scope);

        return h0 +% 33 *% h1;
    }

    pub fn eql(_: SymbolTableContext, k0: SymbolKey, k1: SymbolKey) bool {
        return k0.scope == k1.scope and std.mem.eql(u8, k0.text, k1.text);
    }
};

const SymbolTable = std.HashMap(SymbolKey, *Symbol, SymbolTableContext, 80);

const TypecheckerContext = struct {
    return_type: *Type,
};

const IrOperandTag = enum {
    None,
    Tmp,
    Imm,
    Label,
};

const IrOperand = union(IrOperandTag) {
    None: void,
    Tmp: IrTmp,
    Imm: i64,
    Label: IrLabel,
};

const IrOperandFlagsType = u2;
const Is_Operand_Address = 0x1;

const IrInstrTag = enum(u16) {
    Or,
    And,
    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Not,
    Neg,

    Mov,
    Jmp,
    Jz,
    Jnz,
    Push,
    Pop,
    Call,
    Ret,
    Label,

    GV, // Global Variable
    GFB, // Global Function Begin
    GFE, // Global Function End

    Print,
};

const IrInstr = struct {
    tag: IrInstrTag,
    dst_flags: IrOperandFlagsType = 0,
    src0_flags: IrOperandFlagsType = 0,
    src1_flags: IrOperandFlagsType = 0,
    dst: IrOperand = .None,
    src0: IrOperand = .None,
    src1: IrOperand = .None,
};

const IrInstrList = std.ArrayList(IrInstr);

fn token_tag_to_text(tag: TokenTag) []const u8 {
    return switch (tag) {
        .Or => "'||'",
        .And => "'&&'",
        .Eq => "'=='",
        .Neq => "'!='",
        .Lt => "'<'",
        .Leq => "'<='",
        .Gt => "'>'",
        .Geq => "'>='",
        .Add => "'+'",
        .Sub => "'-'",
        .Mul => "'*'",
        .Div => "'/'",
        .Mod => "'%'",
        .Not => "'!'",
        .Open_Paren => "'('",
        .Close_Paren => "')'",
        .Open_Curly => "'{'",
        .Close_Curly => "'}'",
        .Arrow => "'->'",
        .Colon_Equal => "':='",
        .Colon => "':'",
        .Semicolon => "';'",
        .Equal => "'equal'",
        .Comma => "'comma'",
        .Print => "'print'",
        .If => "'if'",
        .Then => "'then'",
        .Else => "'else'",
        .While => "'while'",
        .Do => "'do'",
        .Return => "'return'",
        .Proc => "'proc'",
        .Void => "'void'",
        .Bool => "'bool'",
        .Int => "'int'",
        .False => "'false'",
        .True => "'true'",
        .Identifier => "identifier",
        .Integer => "integer",
        .End_Of_File => "EOF",
    };
}

fn print_error(c: *Compiler, line_info: LineInfo, comptime format: []const u8, args: anytype) void {
    stderr.print("{s}:{}:{}: error: " ++ format ++ "\n", .{ c.filepath, line_info.line, line_info.column } ++ args) catch {
        std.os.exit(1);
    };
}

fn print_note(c: *Compiler, line_info: LineInfo, comptime format: []const u8, args: anytype) void {
    stderr.print("{s}:{}:{}: note: " ++ format ++ "\n", .{ c.filepath, line_info.line, line_info.column } ++ args) catch {
        std.os.exit(1);
    };
}

fn push_scope(c: *Compiler) void {
    if (scopes.len >= MAX_SCOPE_COUNT) {
        print_error(c, c.line_info, "reached maximum amount of scopes ({}).", .{MAX_SCOPE_COUNT});
        std.os.exit(1);
    }

    scopes.len += 1;
    scopes[scopes.len - 1] = next_scope;
    next_scope += 1;
}

fn push_given_scope(scope: ScopeId) void {
    scopes.len += 1;
    scopes[scopes.len - 1] = scope;
}

fn pop_scope() void {
    scopes.len -= 1;
}

fn revert_scope() void {
    scopes.len -= 1;
    next_scope -= 1;
}

fn grab_current_scope() ScopeId {
    return scopes[scopes.len - 1];
}

fn ast_create(c: *Compiler, comptime T: type) *T {
    return c.ast_arena_allocator.create(T) catch {
        std.os.exit(1);
    };
}

fn grab_symbol_line_info(symbol: *Symbol) LineInfo {
    switch (symbol.*) {
        .Variable => |variable| return variable.line_info,
        .Parameter => |parameter| return parameter.line_info,
        .Function => |function| return function.line_info,
    }
}

fn insert_symbol(c: *Compiler, id: Token) *Symbol {
    std.debug.assert(id.tag == .Identifier);

    // Should create arena with identifier strings?
    var key = SymbolKey{
        .text = id.text,
        .scope = grab_current_scope(),
    };
    var get_or_put_result = c.symbols.getOrPut(key) catch {
        std.os.exit(1);
    };

    if (get_or_put_result.found_existing) {
        print_error(c, id.line_info, "symbol '{s}' is already defined.", .{id.text});
        print_note(c, grab_symbol_line_info(get_or_put_result.value_ptr.*), "first defined here.", .{});
        std.os.exit(1);
    }

    var symbol = ast_create(c, Symbol);
    get_or_put_result.value_ptr.* = symbol;

    return symbol;
}

fn find_symbol(c: *Compiler, id: Token) *Symbol {
    std.debug.assert(id.tag == .Identifier);

    var key = SymbolKey{
        .text = id.text,
        .scope = undefined,
    };

    var i = scopes.len;
    while (i > 0) {
        i -= 1;
        key.scope = scopes[i];

        var found_symbol = c.symbols.get(key);
        if (found_symbol) |symbol| {
            switch (symbol.*) {
                .Variable,
                .Parameter,
                => {
                    var line_info = grab_symbol_line_info(symbol);
                    if (line_info.offset < id.line_info.offset) {
                        return symbol;
                    }
                },
                .Function => {
                    return symbol;
                },
            }
        }
    }

    print_error(c, id.line_info, "'{s}' is not defined.", .{id.text});
    std.os.exit(1);
}

fn parse_top_level(c: *Compiler) void {
    push_scope(c);

    while (c.peek() != .End_Of_File) {
        var symbol = parse_symbol(c);
        var node = ast_create(c, SymbolList.Node);
        node.* = .{
            .payload = symbol,
        };
        c.globals.insert_last(node);
    }

    pop_scope();
    std.debug.assert(scopes.len == 0);
}

fn parse_type(c: *Compiler) Type {
    switch (c.peek()) {
        .Open_Paren => {
            c.advance();

            var typ = parse_type(c);
            c.expect(.Close_Paren);

            return typ;
        },
        .Proc => {
            var typ = parse_type_function(c, false);
            return .{ .Function = typ };
        },
        .Void => {
            c.advance();
            return .Void;
        },
        .Bool => {
            c.advance();
            return .Bool;
        },
        .Int => {
            c.advance();
            return .Int64;
        },
        else => {
            var token = c.grab();
            print_error(c, token.line_info, "'{s}' doesn't start a type.", .{token.text});
            std.os.exit(1);
        },
    }
}

fn parse_type_function(c: *Compiler, should_insert_symbol: bool) TypeFunction {
    c.expect(.Proc);
    c.expect(.Open_Paren);

    var params = ParameterList{};

    var tt = c.peek();
    while (tt != .End_Of_File and tt != .Close_Paren) {
        var id = c.grab();
        var has_id = false;

        if (c.peek() == .Identifier) {
            c.advance();
            c.expect(.Colon);
            has_id = true;
        }

        var typ = parse_type(c);
        var symbol = symbol: {
            if (has_id and should_insert_symbol) {
                var symbol = insert_symbol(c, id);
                symbol.* = .{ .Parameter = .{
                    .typ = typ,
                    .storage = undefined,
                    .line_info = id.line_info,
                } };

                break :symbol &symbol.Parameter;
            } else {
                var symbol = ast_create(c, SymbolParameter);
                symbol.* = .{
                    .typ = typ,
                    .storage = undefined,
                    .line_info = id.line_info,
                };

                break :symbol symbol;
            }
        };
        var node = ast_create(c, ParameterList.Node);
        node.* = .{
            .payload = symbol,
        };
        params.insert_last(node);

        tt = c.peek();
        if (tt != .End_Of_File and tt != .Close_Paren) {
            c.expect(.Comma);
            tt = c.peek();
        }
    }

    c.expect(.Close_Paren);

    var return_type = ast_create(c, Type);
    return_type.* = .Void;

    if (c.peek() == .Arrow) {
        c.advance();
        return_type.* = parse_type(c);
    }

    return .{
        .params = params,
        .return_type = return_type,
    };
}

fn is_def(c: *Compiler) bool {
    var fst = c.peek();
    var snd = c.peek_ahead(1);
    return fst == .Identifier and (snd == .Colon_Equal or snd == .Colon);
}

fn parse_symbol(c: *Compiler) *Symbol {
    var id = c.grab();
    c.expect(.Identifier);

    var symbol = insert_symbol(c, id);

    switch (c.peek()) {
        .Colon_Equal => {
            c.advance();

            switch (c.peek()) {
                .Proc => {
                    push_scope(c);
                    var typ = parse_type_function(c, true);
                    revert_scope();

                    var block = parse_scoped_block(c);

                    symbol.* = .{ .Function = .{
                        .typ = typ,
                        .block = block,
                        .label = next_label(c),
                        .line_info = id.line_info,
                    } };

                    return symbol;
                },
                else => {
                    var expr = parse_expr(c);
                    c.expect(.Semicolon);

                    symbol.* = .{ .Variable = .{
                        .typ = null,
                        .expr = expr,
                        .storage = undefined,
                        .line_info = id.line_info,
                    } };

                    return symbol;
                },
            }
        },
        .Colon => {
            c.advance();

            var typ = parse_type(c);
            var expr: ?Expr = null;

            if (c.peek() == .Equal) {
                c.advance();
                expr = parse_expr(c);
            }

            c.expect(.Semicolon);

            symbol.* = .{ .Variable = .{
                .typ = typ,
                .expr = expr,
                .storage = undefined,
                .line_info = id.line_info,
            } };

            return symbol;
        },
        else => {
            var token = c.grab();
            print_error(c, token.line_info, "expected ':' or ':=' to define symbol.", .{});
            std.os.exit(1);
        },
    }
}

fn parse_expr(c: *Compiler) Expr {
    return parse_prec(c, LOWEST_PREC);
}

fn parse_prec(c: *Compiler, min_prec: i32) Expr {
    var lhs = parse_highest_prec(c);
    parse_postfix_unary_ops(c, &lhs);

    var op = c.peek();
    var prev_prec: i32 = 0x7FFF_FFFF;
    var curr_prec: i32 = prec_of_op(op);

    while (curr_prec < prev_prec and curr_prec >= min_prec) {
        while (curr_prec == prec_of_op(op)) {
            c.advance();

            var lhs_ptr = ast_create(c, Expr);
            var rhs_ptr = ast_create(c, Expr);
            lhs_ptr.* = lhs;
            rhs_ptr.* = parse_prec(c, curr_prec + 1);
            lhs = .{
                .payload = .{ .Binary_Op = .{
                    .tag = token_tag_to_expr_binary_op_tag(op),
                    .lhs = lhs_ptr,
                    .rhs = rhs_ptr,
                } },
                .line_info = lhs_ptr.line_info,
            };

            op = c.peek();
        }

        prev_prec = curr_prec;
        curr_prec = prec_of_op(op);
    }

    return lhs;
}

fn parse_highest_prec(c: *Compiler) Expr {
    var token = c.grab();
    c.advance();

    switch (token.tag) {
        .Sub,
        .Not,
        => {
            var subexpr = ast_create(c, Expr);
            subexpr.* = parse_highest_prec(c);

            return .{
                .payload = .{ .Unary_Op = .{
                    .tag = token_tag_to_expr_unary_op_tag(token.tag),
                    .subexpr = subexpr,
                } },
                .line_info = token.line_info,
            };
        },
        .Open_Paren => {
            var expr = parse_expr(c);
            expr.line_info = token.line_info;
            c.expect(.Close_Paren);

            return expr;
        },
        .If => {
            var cond = ast_create(c, Expr);
            var if_true = ast_create(c, Expr);
            var if_false = ast_create(c, Expr);

            cond.* = parse_expr(c);
            if (c.peek() == .Then) {
                c.advance();
            }
            if_true.* = parse_expr(c);
            c.expect(.Else);
            if_false.* = parse_expr(c);

            return .{
                .payload = .{ .If = .{
                    .cond = cond,
                    .if_true = if_true,
                    .if_false = if_false,
                } },
                .line_info = token.line_info,
            };
        },
        .False,
        .True,
        => {
            return .{
                .payload = .{ .Bool = token.tag == .True },
                .line_info = token.line_info,
            };
        },
        .Identifier => {
            return .{
                .payload = .{ .Identifier = token },
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
                .line_info = token.line_info,
            };
        },
        else => {
            print_error(c, token.line_info, "'{s}' doesn't start expression.", .{token.text});
            std.os.exit(1);
        },
    }
}

fn parse_postfix_unary_ops(c: *Compiler, inner: *Expr) void {
    while (true) {
        switch (c.peek()) {
            .Open_Paren => {
                var line_info = c.grab().line_info;
                c.advance();

                var args = ExprList{};

                var tt = c.peek();
                while (tt != .End_Of_File and tt != .Close_Paren) {
                    var expr = parse_expr(c);
                    var node = ast_create(c, ExprList.Node);
                    node.* = .{
                        .payload = expr,
                    };
                    args.insert_last(node);

                    tt = c.peek();
                    if (tt != .End_Of_File and tt != .Close_Paren) {
                        c.expect(.Comma);
                        tt = c.peek();
                    }
                }

                c.expect(.Close_Paren);

                var lhs = ast_create(c, Expr);
                lhs.* = inner.*;
                inner.* = .{
                    .payload = .{ .Call = .{
                        .lhs = lhs,
                        .args = args,
                    } },
                    .line_info = line_info,
                };
            },
            else => {
                break;
            },
        }
    }
}

fn prec_of_op(op: TokenTag) i32 {
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

fn token_tag_to_expr_binary_op_tag(op: TokenTag) ExprBinaryOpTag {
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

fn token_tag_to_expr_unary_op_tag(op: TokenTag) ExprUnaryOpTag {
    return switch (op) {
        .Sub => .Neg,
        .Not => .Not,
        else => unreachable,
    };
}

fn parse_stmt(c: *Compiler) Stmt {
    var line_info = c.grab().line_info;

    switch (c.peek()) {
        .Print => {
            c.advance();

            var expr = ast_create(c, Expr);
            expr.* = parse_expr(c);
            c.expect(.Semicolon);

            return .{
                .payload = .{ .Print = expr },
                .line_info = line_info,
            };
        },
        .Open_Curly => {
            return .{
                .payload = .{ .Block = parse_scoped_block(c) },
                .line_info = line_info,
            };
        },
        .If => {
            c.advance();

            var cond = ast_create(c, Expr);
            cond.* = parse_expr(c);

            if (c.peek() == .Then) {
                c.advance();
            }

            var if_true = parse_stmt_or_scoped_block(c);
            var if_false = StmtBlock{
                .stmts = .{},
                .scope = null,
            };

            if (c.peek() == .Else) {
                c.advance();
                if_false = parse_stmt_or_scoped_block(c);
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
            c.advance();

            var cond = ast_create(c, Expr);
            cond.* = parse_expr(c);

            if (c.peek() == .Do) {
                c.advance();
            }

            var block = parse_stmt_or_scoped_block(c);

            return .{
                .payload = .{ .While = .{
                    .cond = cond,
                    .block = block,
                } },
                .line_info = line_info,
            };
        },
        .Return => {
            c.advance();

            if (c.peek() == .Semicolon) {
                c.advance();

                return .{
                    .payload = .Return,
                    .line_info = line_info,
                };
            }

            var expr = ast_create(c, Expr);
            expr.* = parse_expr(c);
            c.expect(.Semicolon);

            return .{
                .payload = .{ .Return_Expr = expr },
                .line_info = line_info,
            };
        },
        else => {
            if (is_def(c)) {
                var symbol = parse_symbol(c);

                if (symbol.* != .Variable) {
                    print_error(
                        c,
                        grab_symbol_line_info(symbol),
                        "only variable definitions are allowed inside a function.",
                        .{},
                    );
                    std.os.exit(1);
                }

                return .{
                    .payload = .{ .Symbol = symbol },
                    .line_info = line_info,
                };
            } else {
                var lhs = ast_create(c, Expr);
                lhs.* = parse_expr(c);

                if (c.peek() == .Equal) {
                    c.advance();

                    var rhs = ast_create(c, Expr);
                    rhs.* = parse_expr(c);
                    c.expect(.Semicolon);

                    return .{
                        .payload = .{ .Assign = .{
                            .lhs = lhs,
                            .rhs = rhs,
                        } },
                        .line_info = line_info,
                    };
                }

                c.expect(.Semicolon);

                return .{
                    .payload = .{ .Expr = lhs },
                    .line_info = line_info,
                };
            }
        },
    }
}

fn parse_scoped_block(c: *Compiler) StmtBlock {
    push_scope(c);

    var block = StmtBlock{
        .stmts = .{},
        .scope = grab_current_scope(),
    };

    c.expect(.Open_Curly);

    var tt = c.peek();
    while (tt != .End_Of_File and tt != .Close_Curly) {
        var stmt = parse_stmt(c);
        var node = ast_create(c, StmtList.Node);
        node.* = .{
            .payload = stmt,
        };
        block.stmts.insert_last(node);

        tt = c.peek();
    }

    c.expect(.Close_Curly);

    pop_scope();

    return block;
}

fn parse_stmt_or_scoped_block(c: *Compiler) StmtBlock {
    var result = StmtBlock{
        .stmts = .{},
        .scope = null,
    };

    switch (c.peek()) {
        .Open_Curly => {
            result = parse_scoped_block(c);
        },
        .Semicolon => {
            c.advance();
        },
        else => {
            var stmt = parse_stmt(c);
            var node = ast_create(c, StmtList.Node);
            node.* = .{
                .payload = stmt,
            };
            result.stmts.insert_last(node);

            if (stmt.payload == .Symbol) {
                print_error(c, stmt.line_info, "definitions are not allowed inside a single statement block.", .{});
                std.os.exit(1);
            }
        },
    }

    return result;
}

fn resolve_identifiers(c: *Compiler) void {
    push_given_scope(GLOBAL_SCOPE_ID);

    var it = c.globals.iterator();
    while (it.next()) |symbol| {
        resolve_identifiers_symbol(c, symbol.*);
    }

    pop_scope();
    std.debug.assert(scopes.len == 0);
}

fn resolve_identifiers_symbol(c: *Compiler, symbol: *Symbol) void {
    switch (symbol.*) {
        .Variable => |*variable| {
            if (variable.expr) |*expr| {
                resolve_identifiers_expr(c, expr);
            }
        },
        .Parameter => unreachable,
        .Function => |function| {
            resolve_identifiers_block(c, function.block);
        },
    }
}

fn resolve_identifiers_block(c: *Compiler, block: StmtBlock) void {
    if (block.scope) |scope| {
        push_given_scope(scope);
    }

    var it = block.stmts.iterator();
    while (it.next()) |stmt| {
        resolve_identifiers_stmt(c, stmt);
    }

    if (block.scope != null) {
        pop_scope();
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
        .If => |eef| {
            resolve_identifiers_expr(c, eef.cond);
            resolve_identifiers_block(c, eef.if_true);
            resolve_identifiers_block(c, eef.if_false);
        },
        .While => |loop| {
            resolve_identifiers_expr(c, loop.cond);
            resolve_identifiers_block(c, loop.block);
        },
        .Return => {},
        .Return_Expr => |expr| {
            resolve_identifiers_expr(c, expr);
        },
        .Symbol => |symbol| {
            resolve_identifiers_symbol(c, symbol);
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
        .If => |eef| {
            resolve_identifiers_expr(c, eef.cond);
            resolve_identifiers_expr(c, eef.if_true);
            resolve_identifiers_expr(c, eef.if_false);
        },
        .Call => |call| {
            resolve_identifiers_expr(c, call.lhs);

            var it = call.args.iterator();
            while (it.next()) |arg| {
                resolve_identifiers_expr(c, arg);
            }
        },
        .Bool => {},
        .Int64 => {},
        .Symbol => unreachable,
        .Identifier => |id| {
            var symbol = find_symbol(c, id);
            expr.payload = .{ .Symbol = symbol };
        },
    }
}

fn typecheck(c: *Compiler) void {
    var it = c.globals.iterator();
    while (it.next()) |symbol| {
        typecheck_symbol(c, symbol.*);
    }

    var found_symbol = c.symbols.get(SymbolKey{
        .text = "main",
        .scope = GLOBAL_SCOPE_ID,
    });
    if (found_symbol) |symbol| {
        switch (symbol.*) {
            .Function => |*function| {
                if (function.typ.params.count != 0) {
                    print_error(c, function.line_info, "expected 0 arguments, but got {}.", .{function.typ.params.count});
                    std.os.exit(1);
                }

                if (function.typ.return_type.* != .Void) {
                    print_error(c, function.line_info, "expected return type 'void', but got {}.", .{function.typ.return_type});
                    std.os.exit(1);
                }

                c.main_function = function;
            },
            else => {
                var line_info = grab_symbol_line_info(symbol);
                print_error(c, line_info, "'main' is not a function.", .{});
                std.os.exit(1);
            },
        }
    } else {
        // TODO: change the printing function?
        std.debug.print("error: 'main' function is missing.", .{});
        std.os.exit(1);
    }
}

fn typecheck_type(c: *Compiler, typ: *Type) void {
    switch (typ.*) {
        .Function => |*function| {
            typecheck_type_function(c, function);
        },
        .Void,
        .Bool,
        .Int64,
        => {},
    }
}

fn typecheck_type_function(c: *Compiler, function: *TypeFunction) void {
    var it = function.params.iterator();
    while (it.next()) |param| {
        typecheck_type(c, &param.*.typ);
        if (param.*.typ == .Void) {
            print_error(c, param.*.line_info, "parameter can't be 'void'.", .{});
            std.os.exit(1);
        }
    }

    typecheck_type(c, function.return_type);
}

fn typecheck_symbol(c: *Compiler, symbol: *Symbol) void {
    switch (symbol.*) {
        .Variable => |*variable| {
            var variant: u2 = @as(u2, @intFromBool(variable.typ != null)) << 1 | @intFromBool(variable.expr != null);

            switch (variant) {
                0b00 => unreachable,
                0b01 => {
                    var typ = typecheck_expr(c, &variable.expr.?);
                    variable.typ = typ;
                },
                0b10 => {
                    var typ = &variable.typ.?;
                    typecheck_type(c, typ);

                    if (typ.* == .Void) {
                        print_error(c, variable.line_info, "variable can't be 'void'.", .{});
                        std.os.exit(1);
                    }
                },
                0b11 => {
                    var var_type = &variable.typ.?;
                    typecheck_type(c, var_type);

                    var expr_type = typecheck_expr(c, &variable.expr.?);
                    if (!expr_type.eql(var_type.*)) {
                        print_error(c, variable.line_info, "mismatched types: '{}' and '{}'", .{ var_type, expr_type });
                        std.os.exit(1);
                    }
                },
            }

            variable.was_visited = true;
        },
        .Parameter => unreachable,
        .Function => |*function| {
            if (!function.is_type_typechecked) {
                function.is_type_typechecked = true;
                typecheck_type_function(c, &function.typ);
            }

            var ctx = TypecheckerContext{
                .return_type = function.typ.return_type,
            };
            typecheck_block(c, ctx, function.block);
        },
    }
}

fn typecheck_block(c: *Compiler, ctx: TypecheckerContext, block: StmtBlock) void {
    var it = block.stmts.iterator();
    while (it.next()) |stmt| {
        typecheck_stmt(c, ctx, stmt);
    }
}

fn typecheck_stmt(c: *Compiler, ctx: TypecheckerContext, stmt: *Stmt) void {
    switch (stmt.payload) {
        .Print => |expr| {
            _ = typecheck_expr(c, expr);
        },
        .Block => |block| {
            typecheck_block(c, ctx, block);
        },
        .If => |eef| {
            var cond_type = typecheck_expr(c, eef.cond);
            if (cond_type != .Bool) {
                print_error(c, eef.cond.line_info, "expected 'bool', but got '{}'", .{cond_type});
                std.os.exit(1);
            }

            typecheck_block(c, ctx, eef.if_true);
            typecheck_block(c, ctx, eef.if_false);
        },
        .While => |loop| {
            var cond_type = typecheck_expr(c, loop.cond);
            if (cond_type != .Bool) {
                print_error(c, loop.cond.line_info, "expected 'bool', but got '{}'", .{cond_type});
                std.os.exit(1);
            }

            typecheck_block(c, ctx, loop.block);
        },
        .Return => {
            if (ctx.return_type.* != .Void) {
                print_error(c, stmt.line_info, "expected '{}'.", .{ctx.return_type});
                std.os.exit(1);
            }
        },
        .Return_Expr => |expr| {
            var typ = typecheck_expr(c, expr);
            if (ctx.return_type.* == .Void) {
                print_error(c, expr.line_info, "shouldn't have an expression here.", .{});
                std.os.exit(1);
            } else if (!ctx.return_type.eql(typ)) {
                print_error(c, stmt.line_info, "expected '{}', but got '{}'.", .{ ctx.return_type, typ });
                std.os.exit(1);
            }
        },
        .Symbol => |symbol| {
            typecheck_symbol(c, symbol);
        },
        .Assign => |assign| {
            var lhs_type = typecheck_expr(c, assign.lhs);
            var rhs_type = typecheck_expr(c, assign.rhs);

            if (!assign.lhs.is_lvalue) {
                print_error(c, assign.lhs.line_info, "expression is not an lvalue.", .{});
                std.os.exit(1);
            }

            if (!lhs_type.eql(rhs_type)) {
                print_error(c, assign.rhs.line_info, "mismatched types: '{}' and '{}'.", .{ lhs_type, rhs_type });
                std.os.exit(1);
            }
        },
        .Expr => |expr| {
            _ = typecheck_expr_allow_void(c, expr);
        },
    }
}

fn typecheck_expr(c: *Compiler, expr: *Expr) Type {
    var typ = typecheck_expr_allow_void(c, expr);
    if (typ == .Void) {
        print_error(c, expr.line_info, "expression can't be 'void'.", .{});
        std.os.exit(1);
    }
    return typ;
}

fn typecheck_expr_allow_void(c: *Compiler, expr: *Expr) Type {
    switch (expr.payload) {
        .Binary_Op => |op| {
            var lhs_type = typecheck_expr_allow_void(c, op.lhs);
            var rhs_type = typecheck_expr_allow_void(c, op.rhs);

            switch (op.tag) {
                .Or,
                .And,
                => {
                    if (lhs_type != .Bool or rhs_type != .Bool) {
                        print_error(c, expr.line_info, "expected 'bool', but got '{}' and '{}'.", .{ lhs_type, rhs_type });
                        print_note(c, op.lhs.line_info, "left-hand side is here.", .{});
                        print_note(c, op.rhs.line_info, "right-hand side is here.", .{});
                        std.os.exit(1);
                    }

                    return .Bool;
                },
                .Eq,
                .Neq,
                => {
                    if (lhs_type == .Void or !lhs_type.eql(rhs_type)) {
                        print_error(c, expr.line_info, "mismatched types: '{}' and '{}'.", .{ lhs_type, rhs_type });
                        print_note(c, op.lhs.line_info, "left-hand side is here.", .{});
                        print_note(c, op.rhs.line_info, "right-hand side is here.", .{});
                        std.os.exit(1);
                    }

                    return .Bool;
                },
                .Lt,
                .Leq,
                .Gt,
                .Geq,
                => {
                    if (lhs_type != .Int64 or rhs_type != .Int64) {
                        print_error(c, expr.line_info, "expected integer type, but got '{}' and '{}'.", .{ lhs_type, rhs_type });
                        print_note(c, op.lhs.line_info, "left-hand side is here.", .{});
                        print_note(c, op.rhs.line_info, "right-hand side is here.", .{});
                        std.os.exit(1);
                    }

                    return .Bool;
                },
                .Add,
                .Sub,
                .Mul,
                .Div,
                .Mod,
                => {
                    if (lhs_type != .Int64 or rhs_type != .Int64) {
                        print_error(c, expr.line_info, "expected integer type, but got '{}' and '{}'.", .{ lhs_type, rhs_type });
                        print_note(c, op.lhs.line_info, "left-hand side is here.", .{});
                        print_note(c, op.rhs.line_info, "right-hand side is here.", .{});
                        std.os.exit(1);
                    }

                    return .Int64;
                },
            }
        },
        .Unary_Op => |op| {
            var subexpr_type = typecheck_expr_allow_void(c, op.subexpr);

            switch (op.tag) {
                .Not => {
                    if (subexpr_type != .Bool) {
                        print_error(c, op.subexpr.line_info, "expected 'bool', but got '{}'.", .{subexpr_type});
                        std.os.exit(1);
                    }

                    return .Bool;
                },
                .Neg => {
                    if (subexpr_type != .Int64) {
                        print_error(c, op.subexpr.line_info, "expected integer type, but got '{}'.", .{subexpr_type});
                        std.os.exit(1);
                    }

                    return .Int64;
                },
            }
        },
        .If => |eef| {
            var cond_type = typecheck_expr(c, eef.cond);
            if (cond_type != .Bool) {
                print_error(c, eef.cond.line_info, "expected 'bool', but got '{}'", .{cond_type});
                std.os.exit(1);
            }

            var if_true_type = typecheck_expr(c, eef.if_true);
            var if_false_type = typecheck_expr(c, eef.if_false);

            if (!if_true_type.eql(if_false_type)) {
                print_error(c, eef.if_true.line_info, "mismatched types: '{}' and '{}'", .{ if_true_type, if_false_type });
                std.os.exit(1);
            }

            return if_true_type;
        },
        .Call => |*call| {
            var lhs_type = typecheck_expr_allow_void(c, call.lhs);

            if (lhs_type != .Function) {
                print_error(c, call.lhs.line_info, "expected function, but got '{}'.", .{lhs_type});
                std.os.exit(1);
            }

            var function = &lhs_type.Function;

            if (call.args.count != function.params.count) {
                print_error(c, expr.line_info, "expected {} arguments, but got {}.", .{ function.params.count, call.args.count });
                std.os.exit(1);
            }

            var pit = function.params.iterator();
            var ait = call.args.iterator();
            while (pit.next()) |param_type| {
                var arg = ait.next().?;
                var arg_type = typecheck_expr_allow_void(c, arg);

                if (!arg_type.eql(param_type.*.typ)) {
                    print_error(c, arg.line_info, "expected '{}', but got '{}'", .{ param_type.*.typ, arg_type });
                    std.os.exit(1);
                }
            }

            return function.return_type.*;
        },
        .Bool => {
            return .Bool;
        },
        .Int64 => {
            return .Int64;
        },
        .Symbol => |symbol| {
            switch (symbol.*) {
                .Variable => |variable| {
                    expr.is_lvalue = true;

                    if (!variable.was_visited) {
                        print_error(c, expr.line_info, "can't refer to a variable in its own definition.", .{});
                        print_note(c, variable.line_info, "definition is here.", .{});
                        std.os.exit(1);
                    }

                    return variable.typ.?;
                },
                .Parameter => |parameter| {
                    expr.is_lvalue = true;
                    return parameter.typ;
                },
                .Function => |*function| {
                    if (!function.is_type_typechecked) {
                        function.is_type_typechecked = true;
                        typecheck_type_function(c, &function.typ);
                    }

                    return .{ .Function = function.typ };
                },
            }
        },
        .Identifier => unreachable,
    }
}

const IrTmp = u32;
const IrLabel = u32;

fn next_tmp(c: *Compiler) IrTmp {
    var result = c.next_tmp;

    c.next_tmp += 1;
    if (c.biggest_next_tmp < c.next_tmp) {
        c.biggest_next_tmp = c.next_tmp;
    }

    return result;
}

fn return_tmp(c: *Compiler, dst: IrTmp) void {
    c.next_tmp -= 1;
    std.debug.assert(c.next_tmp == dst);
}

fn next_label(c: *Compiler) IrLabel {
    var result = c.next_label;
    c.next_label += 1;
    return result;
}

fn reduce_expr(c: *Compiler, expr: *Expr) i64 {
    var result: i64 = 0;

    switch (expr.payload) {
        .Binary_Op => |op| {
            result = reduce_expr(c, op.lhs);
            var rhs = reduce_expr(c, op.rhs);

            switch (op.tag) {
                .Or => result = @intFromBool((result != 0) or (rhs != 0)),
                .And => result = @intFromBool((result != 0) and (rhs != 0)),
                .Eq => result = @intFromBool(result == rhs),
                .Neq => result = @intFromBool(result != rhs),
                .Lt => result = @intFromBool(result < rhs),
                .Leq => result = @intFromBool(result <= rhs),
                .Gt => result = @intFromBool(result > rhs),
                .Geq => result = @intFromBool(result >= rhs),
                .Add => result += rhs,
                .Sub => result -= rhs,
                .Mul => result *= rhs,
                .Div => result = @divTrunc(result, rhs),
                .Mod => result = @rem(result, rhs),
            }
        },
        .Unary_Op => |op| {
            result = reduce_expr(c, op.subexpr);

            switch (op.tag) {
                .Not => result = @intFromBool(result == 0),
                .Neg => result = -result,
            }
        },
        .If => |eef| {
            var cond = reduce_expr(c, eef.cond);
            var if_true = reduce_expr(c, eef.if_true);
            var if_false = reduce_expr(c, eef.if_false);

            result = if (cond != 0) if_true else if_false;
        },
        .Bool => |boolean| {
            result = @intFromBool(boolean);
        },
        .Int64 => |int64| {
            result = int64;
        },
        .Call,
        .Symbol,
        => {
            print_error(c, expr.line_info, "can't evaluate non-constant expression at compile-time.", .{});
            std.os.exit(1);
        },
        .Identifier => unreachable,
    }

    expr.payload = .{ .Int64 = result };
    return result;
}

fn generate_ir_instr(c: *Compiler, instr: IrInstr) void {
    c.instr_list.append(instr) catch {
        std.os.exit(1);
    };
}

fn generate_ir(c: *Compiler) void {
    var it = c.globals.iterator();
    while (it.next()) |symbol| {
        generate_ir_symbol(c, symbol.*);
    }
}

fn generate_ir_symbol(c: *Compiler, symbol: *Symbol) void {
    switch (symbol.*) {
        .Variable => |*variable| {
            var label = next_label(c);
            var value: i64 = 0;
            variable.storage = .{ .Label = label };

            if (variable.expr) |*expr| {
                value = reduce_expr(c, expr);
            }

            generate_ir_instr(c, .{
                .tag = .GV,
                .dst = .{ .Label = label },
                .src0 = .{ .Imm = value },
            });
        },
        .Parameter => unreachable,
        .Function => |*function| {
            {
                var it = function.typ.params.iterator();
                while (it.next()) |param| {
                    param.*.storage = .{ .Tmp = next_tmp(c) };
                }
            }

            // dst  -> past last temporary
            // src0 -> past last parameter temporary
            // src1 -> label
            var header_index = c.instr_list.items.len;
            generate_ir_instr(c, .{
                .tag = .GFB,
                .dst = undefined,
                .src0 = .{ .Tmp = c.next_tmp },
                .src1 = .{ .Label = function.label },
            });
            generate_ir_block(c, function.block);
            generate_ir_instr(c, .{ .tag = .GFE });
            c.instr_list.items[header_index].dst = .{
                .Tmp = c.last_tmp_in_scope,
            };

            c.next_tmp = 0;
            c.biggest_next_tmp = 0;
        },
    }
}

fn generate_ir_block(c: *Compiler, block: StmtBlock) void {
    var old_next_tmp = c.next_tmp;
    var old_biggest_next_tmp = c.biggest_next_tmp;

    var it = block.stmts.iterator();
    while (it.next()) |stmt| {
        generate_ir_stmt(c, stmt);
    }

    c.last_tmp_in_scope = c.biggest_next_tmp;
    c.biggest_next_tmp = old_biggest_next_tmp;
    c.next_tmp = old_next_tmp;
}

fn generate_ir_stmt(c: *Compiler, stmt: *Stmt) void {
    switch (stmt.payload) {
        .Print => |expr| {
            var src = next_tmp(c);
            generate_ir_expr(c, expr, src);
            generate_ir_instr(c, .{
                .tag = .Print,
                .src0 = .{ .Tmp = src },
            });
            return_tmp(c, src);
        },
        .Block => |block| {
            generate_ir_block(c, block);
        },
        .If => |eef| {
            var cond_tmp = next_tmp(c);
            generate_ir_expr(c, eef.cond, cond_tmp);

            var variant: u2 = @as(u2, @intFromBool(eef.if_true.stmts.count != 0)) << 1 |
                @intFromBool(eef.if_false.stmts.count != 0);
            switch (variant) {
                0b00 => {
                    return_tmp(c, cond_tmp);
                },
                0b01 => {
                    var end_label = next_label(c);

                    generate_ir_instr(c, .{
                        .tag = .Jnz,
                        .src0 = .{ .Tmp = cond_tmp },
                        .src1 = .{ .Label = end_label },
                    });
                    return_tmp(c, cond_tmp);
                    generate_ir_block(c, eef.if_false);
                    generate_ir_instr(c, .{
                        .tag = .Label,
                        .src0 = .{ .Label = end_label },
                    });
                },
                0b10 => {
                    var end_label = next_label(c);

                    generate_ir_instr(c, .{
                        .tag = .Jz,
                        .src0 = .{ .Tmp = cond_tmp },
                        .src1 = .{ .Label = end_label },
                    });
                    return_tmp(c, cond_tmp);
                    generate_ir_block(c, eef.if_true);
                    generate_ir_instr(c, .{
                        .tag = .Label,
                        .src0 = .{ .Label = end_label },
                    });
                },
                0b11 => {
                    var if_false_label = next_label(c);
                    var end_label = next_label(c);

                    generate_ir_instr(c, .{
                        .tag = .Jz,
                        .src0 = .{ .Tmp = cond_tmp },
                        .src1 = .{ .Label = if_false_label },
                    });
                    return_tmp(c, cond_tmp);
                    generate_ir_block(c, eef.if_true);
                    generate_ir_instr(c, .{
                        .tag = .Jmp,
                        .src0 = .{ .Label = end_label },
                    });
                    generate_ir_instr(c, .{
                        .tag = .Label,
                        .src0 = .{ .Label = if_false_label },
                    });
                    generate_ir_block(c, eef.if_false);
                    generate_ir_instr(c, .{
                        .tag = .Label,
                        .src0 = .{ .Label = end_label },
                    });
                },
            }
        },
        .While => |loop| {
            var block_label = next_label(c);
            var cond_label = next_label(c);

            generate_ir_instr(c, .{
                .tag = .Jmp,
                .src0 = .{ .Label = cond_label },
            });
            generate_ir_instr(c, .{
                .tag = .Label,
                .src0 = .{ .Label = block_label },
            });
            generate_ir_block(c, loop.block);
            generate_ir_instr(c, .{
                .tag = .Label,
                .src0 = .{ .Label = cond_label },
            });

            var cond_tmp = next_tmp(c);
            generate_ir_expr(c, loop.cond, cond_tmp);
            generate_ir_instr(c, .{
                .tag = .Jnz,
                .src0 = .{ .Tmp = cond_tmp },
                .src1 = .{ .Label = block_label },
            });
            return_tmp(c, cond_tmp);
        },
        .Return => {
            generate_ir_instr(c, .{ .tag = .Ret });
        },
        .Return_Expr => |expr| {
            var src = next_tmp(c);
            generate_ir_expr(c, expr, src);
            generate_ir_instr(c, .{
                .tag = .Ret,
                .src0 = .{ .Tmp = src },
            });
            return_tmp(c, src);
        },
        .Symbol => |symbol| {
            switch (symbol.*) {
                .Variable => |*variable| {
                    var dst = next_tmp(c);
                    variable.storage = .{ .Tmp = dst };

                    if (variable.expr) |*expr| {
                        generate_ir_expr(c, expr, dst);
                    }
                },
                .Parameter => {},
                .Function => unreachable,
            }
        },
        .Assign => |assign| {
            var src = next_tmp(c);
            generate_ir_expr(c, assign.rhs, src);
            generate_ir_lvalue(c, assign.lhs, src);
            return_tmp(c, src);
        },
        .Expr => |expr| {
            var dst = next_tmp(c);
            generate_ir_expr(c, expr, dst);
            return_tmp(c, dst);
        },
    }
}

fn generate_ir_expr(c: *Compiler, expr: *Expr, dst: IrTmp) void {
    switch (expr.payload) {
        .Binary_Op => |op| {
            var src = next_tmp(c);
            generate_ir_expr(c, op.rhs, src);
            generate_ir_expr(c, op.lhs, dst);
            return_tmp(c, src);

            // TODO: 'or' and 'and' should short circuit.
            var tag: IrInstrTag = switch (op.tag) {
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

            generate_ir_instr(c, .{
                .tag = tag,
                .dst = .{ .Tmp = dst },
                .src0 = .{ .Tmp = src },
            });
        },
        .Unary_Op => |op| {
            generate_ir_expr(c, op.subexpr, dst);

            var tag: IrInstrTag = switch (op.tag) {
                .Not => .Not,
                .Neg => .Neg,
            };

            generate_ir_instr(c, .{
                .tag = tag,
                .dst = .{ .Tmp = dst },
            });
        },
        .If => |eef| {
            var if_false_label = next_label(c);
            var end_label = next_label(c);
            var cond_tmp = next_tmp(c);

            generate_ir_expr(c, eef.cond, cond_tmp);
            generate_ir_instr(c, .{
                .tag = .Jz,
                .src0 = .{ .Tmp = cond_tmp },
                .src1 = .{ .Label = if_false_label },
            });
            return_tmp(c, cond_tmp);
            generate_ir_expr(c, eef.if_true, dst);
            generate_ir_instr(c, .{
                .tag = .Jmp,
                .src0 = .{ .Label = end_label },
            });
            generate_ir_instr(c, .{
                .tag = .Label,
                .src0 = .{ .Label = if_false_label },
            });
            generate_ir_expr(c, eef.if_false, dst);
            generate_ir_instr(c, .{
                .tag = .Label,
                .src0 = .{ .Label = end_label },
            });
        },
        .Call => |call| {
            {
                var arg_tmp = next_tmp(c);
                var it = call.args.reverse_iterator();
                while (it.prev()) |arg| {
                    generate_ir_expr(c, arg, arg_tmp);
                    generate_ir_instr(c, .{
                        .tag = .Push,
                        .src0 = .{ .Tmp = arg_tmp },
                    });
                }
                return_tmp(c, arg_tmp);
            }

            if (call.lhs.payload == .Symbol and
                call.lhs.payload.Symbol.* == .Function)
            {
                generate_ir_instr(c, .{
                    .tag = .Call,
                    .dst = .{ .Tmp = dst },
                    .src0 = .{ .Label = call.lhs.payload.Symbol.Function.label },
                });
            } else {
                var src = next_tmp(c);
                generate_ir_expr(c, call.lhs, src);
                generate_ir_instr(c, .{
                    .tag = .Call,
                    .dst = .{ .Tmp = dst },
                    .src0 = .{ .Tmp = src },
                });
                return_tmp(c, src);
            }

            var stack_space_used: i64 = @intCast(call.args.count * 8);
            if (stack_space_used != 0) {
                generate_ir_instr(c, .{
                    .tag = .Pop,
                    .src0 = .{ .Imm = stack_space_used },
                });
            }
        },
        .Bool => |boolean| {
            generate_ir_instr(c, .{
                .tag = .Mov,
                .dst = .{ .Tmp = dst },
                .src0 = .{ .Imm = @intFromBool(boolean) },
            });
        },
        .Int64 => |int64| {
            generate_ir_instr(c, .{
                .tag = .Mov,
                .dst = .{ .Tmp = dst },
                .src0 = .{ .Imm = int64 },
            });
        },
        .Symbol => |symbol| {
            switch (symbol.*) {
                .Variable => |variable| {
                    if (variable.storage == .Label) {
                        generate_ir_instr(c, .{
                            .tag = .Mov,
                            .dst = .{ .Tmp = dst },
                            .src0_flags = Is_Operand_Address,
                            .src0 = variable.storage,
                        });
                    } else {
                        generate_ir_instr(c, .{
                            .tag = .Mov,
                            .dst = .{ .Tmp = dst },
                            .src0 = variable.storage,
                        });
                    }
                },
                .Parameter => |parameter| {
                    if (parameter.storage == .Label) {
                        generate_ir_instr(c, .{
                            .tag = .Mov,
                            .dst = .{ .Tmp = dst },
                            .src0_flags = Is_Operand_Address,
                            .src0 = parameter.storage,
                        });
                    } else {
                        generate_ir_instr(c, .{
                            .tag = .Mov,
                            .dst = .{ .Tmp = dst },
                            .src0 = parameter.storage,
                        });
                    }
                },
                .Function => |function| {
                    generate_ir_instr(c, .{
                        .tag = .Mov,
                        .dst = .{ .Tmp = dst },
                        .src0 = .{ .Label = function.label },
                    });
                },
            }
        },
        .Identifier => unreachable,
    }
}

fn generate_ir_lvalue(c: *Compiler, expr: *Expr, src: IrTmp) void {
    switch (expr.payload) {
        .Symbol => |symbol| {
            switch (symbol.*) {
                .Variable => |variable| {
                    generate_ir_instr(c, .{
                        .tag = .Mov,
                        .dst = variable.storage,
                        .src0 = .{ .Tmp = src },
                    });
                },
                .Parameter => |parameter| {
                    generate_ir_instr(c, .{
                        .tag = .Mov,
                        .dst = parameter.storage,
                        .src0 = .{ .Tmp = src },
                    });
                },
                .Function => unreachable,
            }
        },
        .Binary_Op,
        .Unary_Op,
        .If,
        .Call,
        .Bool,
        .Int64,
        .Identifier,
        => unreachable,
    }
}

inline fn is_flag_active(flags: anytype, expected: anytype) bool {
    return (flags & expected) == expected;
}

fn debug_print_operand(operand: IrOperand, flags: IrOperandFlagsType) void {
    if (is_flag_active(flags, Is_Operand_Address)) {
        std.debug.print("[", .{});
    }

    switch (operand) {
        .None => {},
        .Tmp => |tmp| {
            std.debug.print("t{}", .{tmp});
        },
        .Imm => |imm| {
            std.debug.print("{}", .{imm});
        },
        .Label => |label| {
            std.debug.print("l{}", .{label});
        },
    }

    if (is_flag_active(flags, Is_Operand_Address)) {
        std.debug.print("]", .{});
    }
}

fn debug_print_ir(c: *Compiler) void {
    for (c.instr_list.items) |instr| {
        var should_print_comma = false;
        var tag_string = switch (instr.tag) {
            .Or => "    or   ",
            .And => "    and  ",
            .Eq => "    eq   ",
            .Neq => "    neq  ",
            .Lt => "    lt   ",
            .Leq => "    leq  ",
            .Gt => "    gt   ",
            .Geq => "    geq  ",
            .Add => "    add  ",
            .Sub => "    sub  ",
            .Mul => "    mul  ",
            .Div => "    div  ",
            .Mod => "    mod  ",
            .Not => "    not  ",
            .Neg => "    neg  ",
            .Mov => "    mov  ",
            .Jmp => "    jmp  ",
            .Jz => "    jz   ",
            .Jnz => "    jnz  ",
            .Push => "    push ",
            .Pop => "    pop  ",
            .Call => "    call ",
            .Ret => "    ret  ",
            .Label => "label:",
            .GV => "GV:",
            .GFB => "GFB:",
            .GFE => "GFE",
            .Print => "    print",
        };

        std.debug.print("{s} ", .{tag_string});

        debug_print_operand(instr.dst, instr.dst_flags);
        should_print_comma = instr.dst != .None;

        if (should_print_comma and instr.src0 != .None) {
            std.debug.print(", ", .{});
        }

        debug_print_operand(instr.src0, instr.src0_flags);
        should_print_comma = instr.src0 != .None;

        if (should_print_comma and instr.src1 != .None) {
            std.debug.print(", ", .{});
        }

        debug_print_operand(instr.src1, instr.src1_flags);

        std.debug.print("\n", .{});
    }
}

pub fn compile() void {
    var args = std.process.args();
    var filepath: [:0]const u8 = "examples/debug";

    _ = args.next().?;
    if (args.next()) |arg| {
        filepath = arg;
    }

    var source_code = utils.read_entire_file(gpa, filepath) catch {
        std.debug.print("error: failed to read file '{s}'.\n", .{filepath});
        std.os.exit(1);
    };
    var ast_arena = ArenaAllocator.init(std.heap.page_allocator);

    var compiler = Compiler{
        .filepath = filepath,
        .source_code = source_code,
        .globals = .{},
        .main_function = undefined,
        .ast_arena = ast_arena,
        .ast_arena_allocator = ast_arena.allocator(),
        .symbols = SymbolTable.init(gpa),
        .instr_list = IrInstrList.init(gpa),
    };

    parse_top_level(&compiler);
    resolve_identifiers(&compiler);
    typecheck(&compiler);
    generate_ir(&compiler);
    debug_print_ir(&compiler);

    gpa.free(source_code);
    compiler.symbols.deinit();
    ast_arena.deinit();
    compiler.instr_list.deinit();
    std.debug.assert(general_purpose_allocator.deinit() == .ok);
}
