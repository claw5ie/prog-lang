const std = @import("std");
const utils = @import("utils.zig");
const notstd = @import("notstd.zig");

const ArenaAllocator = std.heap.ArenaAllocator;
const Allocator = std.mem.Allocator;

const stderr = std.io.getStdErr().writer();

var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
var gpa = general_purpose_allocator.allocator();

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
    ast_arena: ArenaAllocator,
    ast_arena_allocator: Allocator,
    symbols: SymbolTable,
    current_scope: *Scope,
    global_scope: *Scope,

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
                .{ .text = "cast", .tag = .Cast },
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
    Cast,

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

const Scope = struct {
    parent: ?*Scope,
};

const Identifier = struct {
    token: Token,
    scope: *Scope,
};

const TypeStruct = struct {
    fields: SymbolList,
    scope: *Scope,
};

const TypeTag = enum {
    Struct,
    Union,
    Enum,
    Function,
    Array,
    Array_Ref,
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
        size: *Expr,
        subtype: *Type,
    },
    Array_Ref: *Type,
    Pointer: *Type,
    Void: void,
    Bool: void,
    Int64: void,
    Symbol: *Symbol,
    Identifier: Identifier,
};

const Type = struct {
    payload: TypePayload,
    line_info: LineInfo,
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
    Type,
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
    Type: TypePayload,
    Symbol: *Symbol,
    Identifier: Identifier,
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
        is_do_while: bool = false,
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
    scope: ?*Scope,
};

const SymbolVariable = struct {
    _type: ?*Type,
    expr: ?*Expr,
    was_visited: bool = false,
};

const SymbolParameter = struct {
    _type: *Type,
    has_id: bool,
};

const SymbolFunction = struct {
    _type: *Type,
    block: StmtList,
    scope: *Scope,
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

const SymbolDefinition = struct {
    expr: *Expr,
    was_visited: bool = false,
};

const SymbolTag = enum {
    Variable,
    Parameter,
    Function,
    Struct_Field,
    Union_Field,
    Enum_Field,
    Type,
    Definition,
};

const SymbolPayload = union(SymbolTag) {
    Variable: SymbolVariable,
    Parameter: SymbolParameter,
    Function: SymbolFunction,
    Struct_Field: SymbolStructField,
    Union_Field: SymbolUnionField,
    Enum_Field: SymbolEnumField,
    Type: *Type,
    Definition: SymbolDefinition,
};

const Symbol = struct {
    payload: SymbolPayload,
    key: SymbolKey,
    line_info: LineInfo,
};

const SymbolList = notstd.DoublyLinkedList(*Symbol);

const SymbolKey = struct {
    text: []const u8,
    scope: *Scope,
};

const SymbolTableContext = struct {
    pub fn hash(_: SymbolTableContext, key: SymbolKey) u64 {
        const MurMur = std.hash.Murmur2_64;

        var h0 = MurMur.hash(key.text);
        var h1 = MurMur.hashUint64(@intFromPtr(key.scope));

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
        .Cast => "'@cast'",
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

fn create_scope(c: *Compiler) *Scope {
    var result = ast_create(c, Scope);
    result.* = .{
        .parent = c.current_scope,
    };
    return result;
}

fn push_scope(c: *Compiler) void {
    var scope = create_scope(c);
    c.current_scope = scope;
}

fn pop_scope(c: *Compiler) void {
    c.current_scope = c.current_scope.parent.?;
}

fn ast_create(c: *Compiler, comptime T: type) *T {
    return c.ast_arena_allocator.create(T) catch {
        std.os.exit(1);
    };
}

fn create_symbol(c: *Compiler, token: Token) *Symbol {
    var symbol = ast_create(c, Symbol);
    symbol.* = .{
        .payload = undefined,
        .key = .{
            .text = token.text,
            .scope = c.current_scope,
        },
        .line_info = token.line_info,
    };
    return symbol;
}

fn insert_symbol(c: *Compiler, id: Token) *Symbol {
    std.debug.assert(id.tag == .Identifier);

    // Should create arena with identifier strings?
    var symbol = create_symbol(c, id);
    insert_existing_symbol(c, symbol);

    return symbol;
}

fn insert_existing_symbol(c: *Compiler, symbol: *Symbol) void {
    var get_or_put_result = c.symbols.getOrPut(symbol.key) catch {
        std.os.exit(1);
    };

    if (get_or_put_result.found_existing) {
        print_error(c, symbol.line_info, "symbol '{s}' is already defined.", .{symbol.key.text});
        print_note(c, get_or_put_result.value_ptr.*.line_info, "first defined here.", .{});
        std.os.exit(1);
    }

    get_or_put_result.value_ptr.* = symbol;
}

fn find_symbol_from_scope(c: *Compiler, id: Token, scope: *Scope) *Symbol {
    std.debug.assert(id.tag == .Identifier);

    var key = SymbolKey{
        .text = id.text,
        .scope = scope,
    };

    while (true) {
        var found_symbol = c.symbols.get(key);
        if (found_symbol) |symbol| {
            return symbol;
        }

        if (key.scope.parent) |parent| {
            key.scope = parent;
        } else {
            break;
        }
    }

    print_error(c, id.line_info, "'{s}' is not defined.", .{id.text});
    std.os.exit(1);
}

fn parse_top_level(c: *Compiler) void {
    c.global_scope = ast_create(c, Scope);
    c.global_scope.* = .{
        .parent = null,
    };
    c.current_scope = c.global_scope;

    while (c.peek() != .End_Of_File) {
        var symbol = parse_symbol(c);
        var node = ast_create(c, SymbolList.Node);
        node.* = .{
            .payload = symbol,
        };
        c.globals.insert_last(node);
    }

    std.debug.assert(c.current_scope == c.global_scope);
}

fn extract_type(c: *Compiler, expr: Expr) Type {
    var payload: TypePayload = payload: {
        switch (expr.payload) {
            .Type => |_type| {
                break :payload _type;
            },
            .Identifier => |id| {
                break :payload .{ .Identifier = id };
            },
            else => {
                print_error(c, expr.line_info, "expression doesn't look like a type.", .{});
                std.os.exit(1);
            },
        }
    };

    return .{
        .payload = payload,
        .line_info = expr.line_info,
    };
}

fn parse_type(c: *Compiler) Type {
    return extract_type(c, parse_expr(c));
}

fn parse_type_function(c: *Compiler) TypePayload {
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

        var symbol = create_symbol(c, id);
        symbol.payload = .{ .Parameter = .{
            ._type = _type,
            .has_id = has_id,
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
    return_type.* = .{
        .payload = .Void,
        .line_info = c.grab().line_info,
    };

    if (c.peek() == .Arrow) {
        c.advance();
        return_type.* = parse_type(c);
    }

    return .{ .Function = .{
        .params = params,
        .return_type = return_type,
    } };
}

fn parse_struct_fields(c: *Compiler, is_struct: bool) TypeStruct {
    c.expect(.Open_Curly);

    push_scope(c);

    var _struct = TypeStruct{
        .fields = .{},
        .scope = c.current_scope,
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

    pop_scope(c);

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

            if (c.peek() == .Proc) {
                var _type = ast_create(c, Type);
                _type.* = parse_type(c);

                if (c.peek() == .Open_Curly) {
                    push_scope(c);

                    var it = _type.payload.Function.params.iterator();
                    while (it.next()) |param_ptr| {
                        var param = &param_ptr.*.payload.Parameter;
                        if (param.has_id) {
                            param_ptr.*.key.scope = c.current_scope;
                            insert_existing_symbol(c, param_ptr.*);
                        }
                    }

                    var block = parse_block_given_scope(c, c.current_scope);

                    symbol.payload = .{ .Function = .{
                        ._type = _type,
                        .block = block,
                        .scope = c.current_scope,
                    } };

                    pop_scope(c);

                    return symbol;
                } else {
                    c.expect(.Semicolon);

                    symbol.payload = .{ .Type = _type };

                    return symbol;
                }
            } else {
                var expr = ast_create(c, Expr);
                expr.* = parse_expr(c);
                c.expect(.Semicolon);

                symbol.payload = .{ .Definition = .{
                    .expr = expr,
                } };

                return symbol;
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

                if (subexpr.payload != .Type) {
                    break :payload .{ .Unary_Op = .{
                        .tag = token_tag_to_expr_unary_op_tag(token.tag),
                        .subexpr = subexpr,
                    } };
                } else {
                    var _type = &subexpr.payload.Type;
                    if (token.tag != .Ref or _type.* != .Array) {
                        print_error(c, subexpr.line_info, "can only take a reference to array type.", .{});
                        std.os.exit(1);
                    }

                    var subtype = ast_create(c, Type);
                    subtype.* = .{
                        .payload = _type.*,
                        .line_info = subexpr.line_info,
                    };

                    break :payload .{ .Type = .{
                        .Array_Ref = subtype,
                    } };
                }
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
                break :payload .{ .Identifier = .{
                    .token = token,
                    .scope = c.current_scope,
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
                var _type = ast_create(c, Type);
                _type.* = parse_type(c);

                break :payload .{ .Type = .{
                    .Pointer = _type,
                } };
            },
            .Open_Bracket => {
                var size = ast_create(c, Expr);
                size.* = parse_expr(c);
                c.expect(.Close_Bracket);

                var subtype = ast_create(c, Type);
                subtype.* = parse_type(c);

                break :payload .{ .Type = .{ .Array = .{
                    .size = size,
                    .subtype = subtype,
                } } };
            },
            .Struct => {
                var _struct = parse_struct_fields(c, true);

                break :payload .{ .Type = .{
                    .Struct = _struct,
                } };
            },
            .Union => {
                var _union = parse_struct_fields(c, false);

                break :payload .{ .Type = .{
                    .Union = _union,
                } };
            },
            .Enum => {
                c.expect(.Open_Curly);

                push_scope(c);

                var _enum = TypeStruct{
                    .fields = .{},
                    .scope = c.current_scope,
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

                pop_scope(c);

                c.expect(.Close_Curly);

                break :payload .{ .Type = .{
                    .Enum = _enum,
                } };
            },
            .Proc => {
                break :payload .{
                    .Type = parse_type_function(c),
                };
            },
            .Void => {
                break :payload .{ .Type = .Void };
            },
            .Bool => {
                break :payload .{ .Type = .Bool };
            },
            .Int => {
                break :payload .{ .Type = .Int64 };
            },
            .Cast => {
                c.expect(.Open_Paren);

                var lhs = parse_expr(c);

                if (c.peek() == .Comma) {
                    c.advance();
                }

                if (c.peek() == .Close_Paren) {
                    c.advance();

                    var expr = ast_create(c, Expr);
                    expr.* = lhs;
                    break :payload .{ .Cast1 = expr };
                } else {
                    var _type = ast_create(c, Type);
                    _type.* = extract_type(c, lhs);

                    var rhs = ast_create(c, Expr);
                    rhs.* = parse_expr(c);

                    if (c.peek() == .Comma) {
                        c.advance();
                    }

                    c.expect(.Close_Paren);

                    break :payload .{ .Cast2 = .{
                        ._type = _type,
                        .expr = rhs,
                    } };
                }
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

                break :payload .{ .While = .{
                    .block = block,
                    .cond = cond,
                    .is_do_while = true,
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

                        push_scope(c);

                        var case = SwitchCase{
                            .value = value,
                            .block = .{
                                .stmts = .{},
                                .scope = c.current_scope,
                            },
                            .should_fallthrough = true,
                        };

                        pop_scope(c);

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
                    case.block.stmts = parse_block_given_scope(c, case.block.scope.?);

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

                    if (symbol.payload == .Function) {
                        print_error(c, symbol.line_info, "nested functions are not allowed.", .{});
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
    var scope = create_scope(c);
    var stmts = parse_block_given_scope(c, scope);
    return .{
        .stmts = stmts,
        .scope = scope,
    };
}

fn parse_block_given_scope(c: *Compiler, scope: *Scope) StmtList {
    // Previous scope may not be its parent.
    var previous_scope = c.current_scope;
    c.current_scope = scope;

    var block = StmtList{};

    c.expect(.Open_Curly);

    var tt = c.peek();
    while (tt != .End_Of_File and tt != .Close_Curly) {
        var stmt = parse_stmt(c);
        var node = ast_create(c, StmtList.Node);
        node.* = .{
            .payload = stmt,
        };
        block.insert_last(node);

        tt = c.peek();
    }

    c.expect(.Close_Curly);

    c.current_scope = previous_scope;

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

fn is_expr_a_type(c: *Compiler, expr: *Expr) bool {
    switch (expr.payload) {
        .Type => {
            return true;
        },
        .Identifier => |ident| {
            var scope = ident.scope;
            while (true) {
                var symbol = find_symbol_from_scope(c, ident.token, scope);

                if (is_symbol_a_type(c, symbol)) {
                    expr.payload = .{ .Type = .{
                        .Symbol = symbol,
                    } };

                    return true;
                } else {
                    if (symbol.line_info.offset >= ident.token.line_info.offset) {
                        if (symbol.key.scope.parent) |parent| {
                            scope = parent;
                        } else {
                            print_error(c, ident.token.line_info, "'{s}' was not previously defined.", .{ident.token.text});
                            std.os.exit(1);
                        }
                    } else {
                        expr.payload = .{ .Symbol = symbol };

                        return false;
                    }
                }
            }
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
                var subtype = &definition.expr.payload.Type;
                if (subtype.* == .Symbol) {
                    symbol.payload = .{ .Type = subtype.Symbol.payload.Type };
                } else {
                    var _type = ast_create(c, Type);
                    _type.* = extract_type(c, definition.expr.*);

                    symbol.payload = .{ .Type = _type };
                }

                return true;
            } else {
                symbol.payload = .{ .Variable = .{
                    ._type = null,
                    .expr = definition.expr,
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
        .Struct_Field => |field| {
            resolve_identifiers_type(c, field._type);
        },
        .Union_Field => |field| {
            resolve_identifiers_type(c, field._type);
        },
        .Enum_Field => {},
        .Type => |_type| {
            resolve_identifiers_type(c, _type);
        },
        .Definition => unreachable,
    }
}

fn resolve_identifiers_type(c: *Compiler, _type: *Type) void {
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
            resolve_identifiers_expr(c, array.size);
            resolve_identifiers_type(c, array.subtype);
        },
        .Array_Ref => |subtype| {
            resolve_identifiers_type(c, subtype);
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
            var scope = ident.scope;
            while (true) {
                var symbol = find_symbol_from_scope(c, ident.token, scope);

                if (is_symbol_a_type(c, symbol)) {
                    _type.payload = .{ .Symbol = symbol };
                    break;
                } else {
                    if (symbol.key.scope.parent) |parent| {
                        scope = parent;
                    } else {
                        print_error(c, ident.token.line_info, "'{s}' was not defined anywhere.", .{ident.token.text});
                        std.os.exit(1);
                    }
                }
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
        .Type => |payload| {
            var _type: Type = undefined;
            _type.payload = payload;

            resolve_identifiers_type(c, &_type);
        },
        .Symbol => {},
        .Identifier => |ident| {
            var scope = ident.scope;
            while (true) {
                var symbol = find_symbol_from_scope(c, ident.token, scope);

                if (!is_symbol_a_type(c, symbol) and symbol.line_info.offset < ident.token.line_info.offset) {
                    expr.payload = .{ .Symbol = symbol };
                    break;
                } else {
                    if (symbol.key.scope.parent) |parent| {
                        scope = parent;
                    } else {
                        print_error(c, ident.token.line_info, "'{s}' was not previously defined.", .{ident.token.text});
                        std.os.exit(1);
                    }
                }
            }
        },
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
        .ast_arena = ast_arena,
        .ast_arena_allocator = ast_arena.allocator(),
        .symbols = SymbolTable.init(gpa),
        .global_scope = undefined,
        .current_scope = undefined,
    };

    parse_top_level(&compiler);
    resolve_identifiers(&compiler);

    gpa.free(source_code);
    compiler.symbols.deinit();
    ast_arena.deinit();
    std.debug.assert(general_purpose_allocator.deinit() == .ok);
}
