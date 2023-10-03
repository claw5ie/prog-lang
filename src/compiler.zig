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

const VOID_TYPE: Type = .{
    .payload = .Void,
};
const BOOL_TYPE: Type = .{
    .payload = .Bool,
};
const INT64_TYPE: Type = .{
    .payload = .Int64,
};

const Compiler = struct {
    const LOOKAHEAD = 2;
    tokens: [LOOKAHEAD]Token = undefined,
    token_start: u8 = 0,
    token_count: u8 = 0,
    filepath: []const u8,
    source_code: [:0]u8,
    line_info: LineInfo = .{},

    globals: SymbolList,
    ast_arena: ArenaAllocator,
    ast_arena_allocator: Allocator,
    symbols: SymbolTable,

    pub fn advance(c: *Compiler) void {
        c.token_start += 1;
        c.token_start %= LOOKAHEAD;
        c.token_count -= 1;
    }

    pub fn advance_many(c: *Compiler, count: u8) void {
        c.token_start += count;
        c.token_start %= LOOKAHEAD;
        c.token_count -= count;
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
                .{ .text = "break", .tag = .Break },
                .{ .text = "continue", .tag = .Continue },
                .{ .text = "switch", .tag = .Switch },
                .{ .text = "return", .tag = .Return },
                .{ .text = "struct", .tag = .Struct },
                .{ .text = "union", .tag = .Union },
                .{ .text = "enum", .tag = .Enum },
                .{ .text = "proc", .tag = .Proc },
                .{ .text = "void", .tag = .Void },
                .{ .text = "bool", .tag = .Bool },
                .{ .text = "int", .tag = .Int },
                .{ .text = "cast1", .tag = .Cast1 },
                .{ .text = "cast2", .tag = .Cast2 },
                .{ .text = "false", .tag = .False },
                .{ .text = "true", .tag = .True },
                .{ .text = "null", .tag = .Null },
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
                .{ .text = "&", .tag = .Ref },
                .{ .text = "!", .tag = .Not },
                .{ .text = "(", .tag = .Open_Paren },
                .{ .text = ")", .tag = .Close_Paren },
                .{ .text = "{", .tag = .Open_Curly },
                .{ .text = "}", .tag = .Close_Curly },
                .{ .text = "[", .tag = .Open_Bracket },
                .{ .text = "]", .tag = .Close_Bracket },
                .{ .text = ":", .tag = .Colon },
                .{ .text = ";", .tag = .Semicolon },
                .{ .text = "=", .tag = .Equal },
                .{ .text = ".", .tag = .Dot },
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

    Ref,
    Not,
    Open_Paren,
    Close_Paren,
    Open_Curly,
    Close_Curly,
    Open_Bracket,
    Close_Bracket,
    Arrow,
    Colon_Equal,
    Colon,
    Semicolon,
    Equal,
    Dot,
    Comma,

    Print,
    If,
    Then,
    Else,
    While,
    Do,
    Break,
    Continue,
    Switch,
    Return,

    Struct,
    Union,
    Enum,
    Proc,
    Void,
    Bool,
    Int,
    Cast1,
    Cast2,

    False,
    True,

    Null,

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

const TypeStruct = struct {
    fields: SymbolList,
    scope: ScopeId,
};

const TypeTag = enum {
    Struct,
    Union,
    Enum,
    Function,
    Array,
    Pointer,
    Void,
    Bool,
    Int64,
    Symbol,
    Identifier,
};

const TypePayload = union(TypeTag) {
    Struct: TypeStruct,
    Union: TypeStruct,
    Enum: TypeStruct,
    Function: struct {
        params: SymbolList,
        return_type: *Type,
    },
    Array: struct {
        size: ?*Expr,
        subtype: *Type,
    },
    Pointer: *Type,
    Void: void,
    Bool: void,
    Int64: void,
    Symbol: *Symbol,
    Identifier: Token,
};

const Type = struct {
    payload: TypePayload,
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
    Ref,
    Deref,
};

const ExprTag = enum {
    Binary_Op,
    Unary_Op,
    If,
    Call,
    Index,
    Field,
    Expr_List,
    Designator,
    Enum_Field,
    Cast1,
    Cast2,
    Bool,
    Int64,
    Null,
    Symbol,
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
    Index: struct {
        lhs: *Expr,
        index: *Expr,
    },
    Field: struct {
        lhs: *Expr,
        id: Token,
    },
    Expr_List: ExprList,
    Designator: struct {
        id: Token,
        expr: *Expr,
    },
    Enum_Field: []const u8,
    Cast1: *Expr,
    Cast2: struct {
        _type: *Type,
        expr: *Expr,
    },
    Bool: bool,
    Int64: i64,
    Null: void,
    Symbol: []const u8,
};

const Expr = struct {
    payload: ExprPayload,
    line_info: LineInfo,
};

const ExprList = notstd.DoublyLinkedList(Expr);

const StmtTag = enum {
    Print,
    Block,
    If,
    While,
    Do_While,
    Break,
    Continue,
    Switch,
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
    Do_While: struct {
        block: StmtBlock,
        cond: *Expr,
    },
    Break: void,
    Continue: void,
    Switch: struct {
        cond: *Expr,
        cases: SwitchCaseList,
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

const SwitchCase = struct {
    value: *Expr,
    block: StmtBlock,
    should_fallthrough: bool,
};

const SwitchCaseList = notstd.DoublyLinkedList(SwitchCase);

const StmtBlock = struct {
    stmts: StmtList,
    scope: ?ScopeId,
};

const SymbolVariable = struct {
    _type: ?*Type,
    expr: ?*Expr,
};

const SymbolParameter = struct {
    _type: *Type,
};

const SymbolFunction = struct {
    _type: *Type,
    block: StmtBlock,
};

const SymbolStructField = struct {
    _type: *Type,
};

const SymbolUnionField = struct {
    _type: *Type,
};

const SymbolEnumField = struct {
    _type: *Type,
};

const SymbolTag = enum {
    Variable,
    Parameter,
    Function,
    Struct,
    Struct_Field,
    Union,
    Union_Field,
    Enum,
    Enum_Field,
};

const SymbolPayload = union(SymbolTag) {
    Variable: SymbolVariable,
    Parameter: SymbolParameter,
    Function: SymbolFunction,
    Struct: *Type,
    Struct_Field: SymbolStructField,
    Union: *Type,
    Union_Field: SymbolUnionField,
    Enum: *Type,
    Enum_Field: SymbolEnumField,
};

const Symbol = struct {
    payload: SymbolPayload,
    key: SymbolKey,
    line_info: LineInfo,
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
        .Ref => "'&'",
        .Not => "'!'",
        .Open_Paren => "'('",
        .Close_Paren => "')'",
        .Open_Curly => "'{'",
        .Close_Curly => "'}'",
        .Open_Bracket => "'['",
        .Close_Bracket => "']'",
        .Arrow => "'->'",
        .Colon_Equal => "':='",
        .Colon => "':'",
        .Semicolon => "';'",
        .Equal => "'='",
        .Dot => "'.'",
        .Comma => "','",
        .Print => "'print'",
        .If => "'if'",
        .Then => "'then'",
        .Else => "'else'",
        .While => "'while'",
        .Do => "'do'",
        .Break => "'break'",
        .Continue => "'continue'",
        .Switch => "'switch'",
        .Return => "'return'",
        .Struct => "'struct'",
        .Union => "'union'",
        .Enum => "'enum'",
        .Proc => "'proc'",
        .Void => "'void'",
        .Bool => "'bool'",
        .Int => "'int'",
        .Cast1 => "'@cast1'",
        .Cast2 => "'@cast2'",
        .False => "'false'",
        .True => "'true'",
        .Null => "'null'",
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

fn grab_next_scope() ScopeId {
    var result = next_scope;
    next_scope += 1;
    return result;
}

fn push_scope() void {
    if (scopes.len >= MAX_SCOPE_COUNT) {
        std.debug.print("error: reached maximum amount of scopes ({}).", .{MAX_SCOPE_COUNT});
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

fn grab_current_scope() ScopeId {
    return scopes[scopes.len - 1];
}

fn ast_create(c: *Compiler, comptime T: type) *T {
    return c.ast_arena_allocator.create(T) catch {
        std.os.exit(1);
    };
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
        print_note(c, get_or_put_result.value_ptr.*.line_info, "first defined here.", .{});
        std.os.exit(1);
    }

    var symbol = ast_create(c, Symbol);
    symbol.* = .{
        .payload = undefined,
        .key = get_or_put_result.key_ptr.*,
        .line_info = id.line_info,
    };
    get_or_put_result.value_ptr.* = symbol;

    return symbol;
}

fn create_unnamed_symbol(c: *Compiler, line_info: LineInfo) *Symbol {
    var symbol = ast_create(c, Symbol);
    symbol.* = .{
        .payload = undefined,
        .key = .{
            .text = "<no name>",
            .scope = grab_current_scope(),
        },
        .line_info = line_info,
    };
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
                    if (symbol.line_info.offset < id.line_info.offset) {
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
    push_scope();

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
        .Mul => {
            c.advance();

            var _type = ast_create(c, Type);
            _type.* = parse_type(c);

            return .{
                .payload = .{ .Pointer = _type },
            };
        },
        .Open_Paren => {
            c.advance();

            var _type = parse_type(c);
            c.expect(.Close_Paren);

            return _type;
        },
        .Open_Bracket => {
            c.advance();

            var size: ?*Expr = null;

            if (c.peek() != .Close_Bracket) {
                size = ast_create(c, Expr);
                size.?.* = parse_expr(c);
            }

            c.expect(.Close_Bracket);

            var subtype = ast_create(c, Type);
            subtype.* = parse_type(c);

            return .{
                .payload = .{ .Array = .{
                    .size = size,
                    .subtype = subtype,
                } },
            };
        },
        .Struct => {
            c.advance();
            var _struct = parse_struct_fields(c, true);

            return .{
                .payload = .{ .Struct = _struct },
            };
        },
        .Union => {
            c.advance();
            var _union = parse_struct_fields(c, false);

            return .{
                .payload = .{ .Union = _union },
            };
        },
        .Enum => {
            c.advance();
            c.expect(.Open_Curly);

            push_scope();

            var _enum = TypeStruct{
                .fields = .{},
                .scope = grab_current_scope(),
            };

            var tt = c.peek();
            while (tt != .End_Of_File and tt != .Close_Curly) {
                var id = c.grab();
                c.expect(.Identifier);

                var symbol = insert_symbol(c, id);
                symbol.payload = .{ .Enum_Field = .{
                    ._type = undefined,
                } };

                var node = ast_create(c, SymbolList.Node);
                node.* = .{
                    .payload = symbol,
                };
                _enum.fields.insert_last(node);

                tt = c.peek();
                if (tt != .End_Of_File and tt != .Close_Curly) {
                    c.expect(.Comma);
                    tt = c.peek();
                }
            }

            pop_scope();

            c.expect(.Close_Curly);

            return .{
                .payload = .{ .Enum = _enum },
            };
        },
        .Proc => {
            return parse_type_function(c, false);
        },
        .Void => {
            c.advance();
            return VOID_TYPE;
        },
        .Bool => {
            c.advance();
            return BOOL_TYPE;
        },
        .Int => {
            c.advance();
            return INT64_TYPE;
        },
        .Identifier => {
            var id = c.grab();
            c.advance();

            return .{
                .payload = .{ .Identifier = id },
            };
        },
        else => {
            var token = c.grab();
            print_error(c, token.line_info, "'{s}' doesn't start a type.", .{token.text});
            std.os.exit(1);
        },
    }
}

fn parse_type_function(c: *Compiler, should_insert_symbol: bool) Type {
    c.expect(.Proc);
    c.expect(.Open_Paren);

    var params = SymbolList{};

    var tt = c.peek();
    while (tt != .End_Of_File and tt != .Close_Paren) {
        var id = c.grab();
        var has_id = false;

        if (c.peek() == .Identifier) {
            c.advance();
            c.expect(.Colon);
            has_id = true;
        }

        var _type = ast_create(c, Type);
        _type.* = parse_type(c);

        var symbol = if (has_id and should_insert_symbol)
            insert_symbol(c, id)
        else
            create_unnamed_symbol(c, id.line_info);

        symbol.payload = .{ .Parameter = .{
            ._type = _type,
        } };

        var node = ast_create(c, SymbolList.Node);
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
    return_type.* = VOID_TYPE;

    if (c.peek() == .Arrow) {
        c.advance();
        return_type.* = parse_type(c);
    }

    return .{
        .payload = .{ .Function = .{
            .params = params,
            .return_type = return_type,
        } },
    };
}

fn parse_struct_fields(c: *Compiler, is_struct: bool) TypeStruct {
    c.expect(.Open_Curly);

    push_scope();

    var _struct = TypeStruct{
        .fields = .{},
        .scope = grab_current_scope(),
    };

    var tt = c.peek();
    while (tt != .End_Of_File and tt != .Close_Curly) {
        var id = c.grab();
        c.expect(.Identifier);
        c.expect(.Colon);

        var _type = ast_create(c, Type);
        _type.* = parse_type(c);

        var symbol = insert_symbol(c, id);

        if (is_struct) {
            symbol.payload = .{ .Struct_Field = .{
                ._type = _type,
            } };
        } else {
            symbol.payload = .{ .Union_Field = .{
                ._type = _type,
            } };
        }

        var node = ast_create(c, SymbolList.Node);
        node.* = .{
            .payload = symbol,
        };
        _struct.fields.insert_last(node);

        tt = c.peek();
        if (tt != .End_Of_File and tt != .Close_Curly) {
            c.expect(.Comma);
            tt = c.peek();
        }
    }

    pop_scope();

    c.expect(.Close_Curly);

    return _struct;
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
                .Struct => {
                    var _type = ast_create(c, Type);
                    _type.* = parse_type(c);

                    symbol.payload = .{ .Struct = _type };

                    return symbol;
                },
                .Union => {
                    var _type = ast_create(c, Type);
                    _type.* = parse_type(c);

                    symbol.payload = .{ .Union = _type };

                    return symbol;
                },
                .Enum => {
                    var _type = ast_create(c, Type);
                    _type.* = parse_type(c);

                    symbol.payload = .{ .Enum = _type };

                    return symbol;
                },
                .Proc => {
                    push_scope();

                    var scope = grab_current_scope();
                    var _type = ast_create(c, Type);
                    _type.* = parse_type_function(c, true);

                    pop_scope();

                    var block = parse_block_given_scope(c, scope);

                    symbol.payload = .{ .Function = .{
                        ._type = _type,
                        .block = block,
                    } };

                    return symbol;
                },
                else => {
                    var expr = ast_create(c, Expr);
                    expr.* = parse_expr(c);
                    c.expect(.Semicolon);

                    symbol.payload = .{ .Variable = .{
                        ._type = null,
                        .expr = expr,
                    } };

                    return symbol;
                },
            }
        },
        .Colon => {
            c.advance();

            var expr: ?*Expr = null;
            var _type = ast_create(c, Type);
            _type.* = parse_type(c);

            if (c.peek() == .Equal) {
                c.advance();
                expr = ast_create(c, Expr);
                expr.?.* = parse_expr(c);
            }

            c.expect(.Semicolon);

            symbol.payload = .{ .Variable = .{
                ._type = _type,
                .expr = expr,
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

    var result = Expr{
        .payload = undefined,
        .line_info = token.line_info,
    };

    result.payload = payload: {
        switch (token.tag) {
            .Sub,
            .Ref,
            .Not,
            => {
                var subexpr = ast_create(c, Expr);
                subexpr.* = parse_highest_prec(c);

                break :payload .{ .Unary_Op = .{
                    .tag = token_tag_to_expr_unary_op_tag(token.tag),
                    .subexpr = subexpr,
                } };
            },
            .And => {
                print_error(c, token.line_info, "can't take a reference of rvalue.", .{});
                std.os.exit(1);
            },
            .Open_Paren => {
                var expr = parse_expr(c);
                c.expect(.Close_Paren);

                break :payload expr.payload;
            },
            .Dot => {
                switch (c.peek()) {
                    .Open_Curly => {
                        c.advance();
                        var expr_list = ExprList{};

                        var tt = c.peek();
                        while (tt != .End_Of_File and tt != .Close_Curly) {
                            var expr: Expr = expr: {
                                if (c.peek() == .Identifier and
                                    c.peek_ahead(1) == .Equal)
                                {
                                    var id = c.grab();
                                    c.advance_many(2);

                                    var value = ast_create(c, Expr);
                                    value.* = parse_expr(c);

                                    break :expr .{
                                        .payload = .{ .Designator = .{
                                            .id = id,
                                            .expr = value,
                                        } },
                                        .line_info = id.line_info,
                                    };
                                } else {
                                    break :expr parse_expr(c);
                                }
                            };

                            var node = ast_create(c, ExprList.Node);
                            node.* = .{
                                .payload = expr,
                            };
                            expr_list.insert_last(node);

                            tt = c.peek();
                            if (tt != .End_Of_File and tt != .Close_Curly) {
                                c.expect(.Comma);
                                tt = c.peek();
                            }
                        }

                        c.expect(.Close_Curly);

                        break :payload .{ .Expr_List = expr_list };
                    },
                    .Identifier => {
                        var id = c.grab();
                        c.advance();

                        break :payload .{ .Enum_Field = id.text };
                    },
                    else => {
                        var _token = c.grab();
                        print_error(c, _token.line_info, "unexpected '{s}' after '.' operator.", .{_token.text});
                        print_note(c, _token.line_info, "expected designator list or identifier.", .{});
                        std.os.exit(1);
                    },
                }
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
                break :payload .{ .Symbol = token.text };
            },
            .Integer => {
                var value: i64 = 0;
                for (token.text) |ch| {
                    value = 10 * value + (ch - '0');
                }

                break :payload .{ .Int64 = value };
            },
            .Cast1 => {
                c.expect(.Open_Paren);

                var expr = ast_create(c, Expr);
                expr.* = parse_expr(c);

                c.expect(.Close_Paren);

                break :payload .{ .Cast1 = expr };
            },
            .Cast2 => {
                c.expect(.Open_Paren);

                var _type = ast_create(c, Type);
                _type.* = parse_type(c);
                c.expect(.Comma);
                var expr = ast_create(c, Expr);
                expr.* = parse_expr(c);

                c.expect(.Close_Paren);

                break :payload .{ .Cast2 = .{
                    ._type = _type,
                    .expr = expr,
                } };
            },
            else => {
                print_error(c, token.line_info, "'{s}' doesn't start expression.", .{token.text});
                std.os.exit(1);
            },
        }
    };

    return result;
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
            .Open_Bracket => {
                var line_info = c.grab().line_info;
                c.advance();

                var expr = ast_create(c, Expr);
                expr.* = parse_expr(c);
                c.expect(.Close_Bracket);

                var lhs = ast_create(c, Expr);
                lhs.* = inner.*;
                inner.* = .{
                    .payload = .{ .Index = .{
                        .lhs = lhs,
                        .index = expr,
                    } },
                    .line_info = line_info,
                };
            },
            .Dot => {
                c.advance();

                if (c.peek() == .Mul) {
                    c.advance();

                    var lhs = ast_create(c, Expr);
                    lhs.* = inner.*;
                    inner.* = .{
                        .payload = .{ .Unary_Op = .{
                            .tag = .Deref,
                            .subexpr = lhs,
                        } },
                        .line_info = lhs.line_info,
                    };
                } else {
                    var id = c.grab();
                    c.expect(.Identifier);

                    var lhs = ast_create(c, Expr);
                    lhs.* = inner.*;
                    inner.* = .{
                        .payload = .{ .Field = .{
                            .lhs = lhs,
                            .id = id,
                        } },
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
        .Ref => .Ref,
        .Not => .Not,
        else => unreachable,
    };
}

fn parse_stmt(c: *Compiler) Stmt {
    var result: Stmt = .{
        .payload = undefined,
        .line_info = c.grab().line_info,
    };

    result.payload = payload: {
        switch (c.peek()) {
            .Print => {
                c.advance();

                var expr = ast_create(c, Expr);
                expr.* = parse_expr(c);
                c.expect(.Semicolon);

                break :payload .{ .Print = expr };
            },
            .Open_Curly => {
                break :payload .{ .Block = parse_scoped_block(c) };
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

                break :payload .{ .If = .{
                    .cond = cond,
                    .if_true = if_true,
                    .if_false = if_false,
                } };
            },
            .While => {
                c.advance();

                var cond = ast_create(c, Expr);
                cond.* = parse_expr(c);

                if (c.peek() == .Do) {
                    c.advance();
                }

                var block = parse_stmt_or_scoped_block(c);

                break :payload .{ .While = .{
                    .cond = cond,
                    .block = block,
                } };
            },
            .Do => {
                c.advance();

                var block = parse_stmt_or_scoped_block(c);

                c.expect(.While);

                var cond = ast_create(c, Expr);
                cond.* = parse_expr(c);

                c.expect(.Semicolon);

                break :payload .{ .Do_While = .{
                    .block = block,
                    .cond = cond,
                } };
            },
            .Break => {
                c.advance();
                c.expect(.Semicolon);
                break :payload .Break;
            },
            .Continue => {
                c.advance();
                c.expect(.Semicolon);
                break :payload .Continue;
            },
            .Switch => {
                c.advance();

                var cond = ast_create(c, Expr);
                cond.* = parse_expr(c);

                c.expect(.Open_Curly);

                var cases = SwitchCaseList{};

                var tt = c.peek();
                while (tt != .End_Of_File and tt != .Close_Curly) {
                    while (true) {
                        var value = ast_create(c, Expr);
                        value.* = parse_expr(c);

                        push_scope();

                        var case = SwitchCase{
                            .value = value,
                            .block = .{
                                .stmts = .{},
                                .scope = grab_current_scope(),
                            },
                            .should_fallthrough = true,
                        };

                        pop_scope();

                        var node = ast_create(c, SwitchCaseList.Node);
                        node.* = .{
                            .payload = case,
                        };
                        cases.insert_last(node);

                        tt = c.peek();
                        if (tt != .End_Of_File and tt != .Colon) {
                            c.expect(.Comma);
                            tt = c.peek();
                        }

                        if (tt == .End_Of_File or tt == .Colon) {
                            break;
                        }
                    }

                    c.expect(.Colon);

                    var case = cases.grab_last();
                    case.should_fallthrough = false;
                    case.block = parse_block_given_scope(c, case.block.scope.?);

                    tt = c.peek();
                }

                c.expect(.Close_Curly);

                break :payload .{ .Switch = .{
                    .cond = cond,
                    .cases = cases,
                } };
            },
            .Return => {
                c.advance();

                if (c.peek() == .Semicolon) {
                    c.advance();

                    break :payload .Return;
                }

                var expr = ast_create(c, Expr);
                expr.* = parse_expr(c);
                c.expect(.Semicolon);

                break :payload .{ .Return_Expr = expr };
            },
            else => {
                if (is_def(c)) {
                    var symbol = parse_symbol(c);

                    if (symbol.payload != .Variable) {
                        print_error(c, symbol.line_info, "only variable definitions are allowed inside a function.", .{});
                        std.os.exit(1);
                    }

                    break :payload .{ .Symbol = symbol };
                } else {
                    var lhs = ast_create(c, Expr);
                    lhs.* = parse_expr(c);

                    if (c.peek() == .Equal) {
                        c.advance();

                        var rhs = ast_create(c, Expr);
                        rhs.* = parse_expr(c);
                        c.expect(.Semicolon);

                        break :payload .{ .Assign = .{
                            .lhs = lhs,
                            .rhs = rhs,
                        } };
                    }

                    c.expect(.Semicolon);

                    break :payload .{ .Expr = lhs };
                }
            },
        }
    };

    return result;
}

fn parse_scoped_block(c: *Compiler) StmtBlock {
    return parse_block_given_scope(c, grab_next_scope());
}

fn parse_block_given_scope(c: *Compiler, scope: ScopeId) StmtBlock {
    push_given_scope(scope);

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
        .ast_arena = ast_arena,
        .ast_arena_allocator = ast_arena.allocator(),
        .symbols = SymbolTable.init(gpa),
    };

    parse_top_level(&compiler);

    gpa.free(source_code);
    compiler.symbols.deinit();
    ast_arena.deinit();
    std.debug.assert(general_purpose_allocator.deinit() == .ok);
}
