const std = @import("std");
const utils = @import("utils.zig");
const notstd = @import("notstd.zig");

const ArenaAllocator = std.heap.ArenaAllocator;
const Allocator = std.mem.Allocator;

const stderr = std.io.getStdErr().writer();

var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
var gpa = general_purpose_allocator.allocator();

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
        scope: *Scope,
    },
    Array: struct {
        expr: *Expr,
        count: usize,
        subtype: *Type,
    },
    Pointer: *Type,
    Void: void,
    Bool: void,
    Int64: void,
    Symbol: *Symbol,
    Identifier: Identifier,
};

const Type = struct {
    const FlagType = u16;
    const Is_Integer: FlagType = 0x1;
    const Is_Integral: FlagType = 0x2;
    const Is_Comparable: FlagType = 0x4;
    const Is_Ptr: FlagType = 0x8;
    const Is_Void_Ptr: FlagType = 0x10;
    const Can_Be_Dereferenced: FlagType = 0x20;
    const Is_Pointer_To_Struct: FlagType = 0x40;
    const Is_Pointer_To_Union: FlagType = 0x80;
    const Is_Pointer_To_Array: FlagType = 0x100;

    payload: TypePayload,
    size: usize,
    is_resolved: bool = false,
    typechecking_stage: TypecheckingStage = .Not_Typechecked,
    line_info: LineInfo,

    pub fn format(self: Type, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self.payload) {
            .Struct => try writer.writeAll("<struct>"),
            .Union => try writer.writeAll("<union>"),
            .Enum => try writer.writeAll("<enum>"),
            .Function => |function| {
                try writer.print("proc(", .{});

                var it = function.params.iterator();
                while (it.next()) |param_ptr| {
                    var param = &param_ptr.*.payload.Parameter;
                    if (param.has_id) {
                        try writer.print("{s}: ", .{param_ptr.*.key.text});
                    }
                    try writer.print("{}", .{param._type});

                    if (it.has_next()) {
                        try writer.print(", ", .{});
                    }
                }

                try writer.print(") -> {}", .{function.return_type});
            },
            .Array => |array| try writer.print("[{}]{}", .{ array.count, array.subtype }),
            .Pointer => |subtype| try writer.print("*{}", .{subtype}),
            .Void => try writer.writeAll("void"),
            .Bool => try writer.writeAll("bool"),
            .Int64 => try writer.writeAll("int64"),
            .Symbol => |symbol| try writer.writeAll(symbol.key.text),
            .Identifier => unreachable,
        }
    }

    pub fn compare(self: *Type) FlagType {
        var flags: FlagType = 0;

        switch (self.payload) {
            .Enum => flags |= Is_Integer | Is_Integral | Is_Comparable,
            .Function => flags |= Is_Ptr,
            .Pointer => {
                flags |= Is_Ptr | Can_Be_Dereferenced;

                var subtype = self.payload.Pointer.extract_ptr();
                switch (subtype.payload) {
                    .Struct => flags |= Is_Pointer_To_Struct,
                    .Union => flags |= Is_Pointer_To_Union,
                    .Array => flags |= Is_Pointer_To_Array,
                    .Void => {
                        flags |= Is_Void_Ptr;
                        flags &= ~Can_Be_Dereferenced;
                    },
                    else => {},
                }
            },
            .Bool => flags |= Is_Comparable,
            .Int64 => flags |= Is_Integer | Is_Integral | Is_Comparable,
            .Struct,
            .Union,
            .Array,
            .Void,
            => {},
            .Symbol,
            .Identifier,
            => unreachable,
        }

        return flags;
    }

    pub fn extract_ptr(self: *Type) *Type {
        return if (self.payload != .Symbol)
            self
        else
            self.payload.Symbol.payload.Type;
    }

    pub inline fn is(self: *Type, tag: TypeTag) bool {
        return self.payload == tag;
    }

    pub fn eql(self: *Type, other: *Type) bool {
        if (self.payload != @as(TypeTag, other.payload)) {
            return false;
        }

        switch (self.payload) {
            .Struct => {
                var sstruct = &self.payload.Struct;
                var ostruct = &other.payload.Struct;
                return sstruct.scope == ostruct.scope;
            },
            .Union => {
                var sstruct = &self.payload.Union;
                var ostruct = &other.payload.Union;
                return sstruct.scope == ostruct.scope;
            },
            .Enum => {
                var sstruct = &self.payload.Enum;
                var ostruct = &other.payload.Enum;
                return sstruct.scope == ostruct.scope;
            },
            .Function => {
                var sfunc = &self.payload.Function;
                var ofunc = &other.payload.Function;

                if (sfunc.params.count != ofunc.params.count) {
                    return false;
                }

                var sit = sfunc.params.iterator();
                var oit = ofunc.params.iterator();
                while (sit.next()) |sparam| {
                    var oparam = oit.next().?;
                    var otype = oparam.*.payload.Parameter._type.extract_ptr();
                    var stype = sparam.*.payload.Parameter._type.extract_ptr();
                    if (!stype.eql(otype)) {
                        return false;
                    }
                }

                var sreturn_type = sfunc.return_type.extract_ptr();
                var oreturn_type = ofunc.return_type.extract_ptr();

                return sreturn_type.eql(oreturn_type);
            },
            .Array => {
                var sarray = &self.payload.Array;
                var oarray = &other.payload.Array;

                if (sarray.count != oarray.count) {
                    return false;
                }

                var ssubtype = sarray.subtype.extract_ptr();
                var osubtype = oarray.subtype.extract_ptr();

                return ssubtype.eql(osubtype);
            },
            .Pointer => {
                var stype = self.payload.Pointer.extract_ptr();
                var otype = other.payload.Pointer.extract_ptr();

                if (stype.is(.Void) or otype.is(.Void)) {
                    return true;
                }

                return stype.eql(otype);
            },
            .Void,
            .Bool,
            .Int64,
            => return true,
            .Symbol,
            .Identifier,
            => unreachable,
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
    size: usize,
    is_lvalue: bool = false,
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

const TypecheckingStage = enum {
    Not_Typechecked,
    Being_Typechecked,
    Shallow_Typechecked,
    Fully_Typechecked,
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
        .size = undefined,
        .line_info = expr.line_info,
    };
}

fn parse_type(c: *Compiler) Type {
    return extract_type(c, parse_expr(c));
}

fn parse_type_function(c: *Compiler) TypePayload {
    c.expect(.Open_Paren);

    push_scope(c);

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
        .size = undefined,
        .line_info = c.grab().line_info,
    };

    if (c.peek() == .Arrow) {
        c.advance();
        return_type.* = parse_type(c);
    }

    var scope = c.current_scope;

    pop_scope(c);

    return .{ .Function = .{
        .params = params,
        .return_type = return_type,
        .scope = scope,
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
                    var previous_scope = c.current_scope;
                    c.current_scope = _type.payload.Function.scope;

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
                    } };

                    c.current_scope = previous_scope;

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
                .size = undefined,
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
        .size = undefined,
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
                                        .size = undefined,
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
                var expr = ast_create(c, Expr);
                expr.* = parse_expr(c);
                c.expect(.Close_Bracket);

                var subtype = ast_create(c, Type);
                subtype.* = parse_type(c);

                break :payload .{ .Type = .{ .Array = .{
                    .expr = expr,
                    .count = undefined,
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
                    .size = undefined,
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
                    .size = undefined,
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
                        .size = undefined,
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
                        .size = undefined,
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
                var expr = definition.expr;
                symbol.payload = .{ .Variable = .{
                    ._type = null,
                    .expr = expr,
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
                    variable._type = typecheck_expr(c, null, expr);
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
            typecheck_type(c, _type);
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

fn typecheck_type(c: *Compiler, _type: *Type) void {
    _ = typecheck_type_aux(c, _type, REJECT_VOID_TYPE | DO_SHALLOW_TYPECHECK);
    _ = typecheck_type_aux(c, _type, REJECT_VOID_TYPE);
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
            var size: usize = 0;

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
            var size: usize = 0;

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
        .Enum => {
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
    var result: *Type = undefined;

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

                    result = &BOOL_TYPE_HINT;
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

                    result = &BOOL_TYPE_HINT;
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

                    result = &BOOL_TYPE_HINT;
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
                        result = lhs_type;
                    } else if (check_flags(lhs_flags, Type.Is_Integer) and check_flags(rhs_flags, Type.Is_Ptr)) {
                        result = rhs_type;
                    } else if (check_flags(lhs_flags, Type.Is_Integer) and lhs_type.eql(rhs_type)) {
                        result = lhs_type;
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
                        result = lhs_type;
                    } else if (check_flags(lhs_flags, Type.Is_Integer) and lhs_type.eql(rhs_type)) {
                        result = lhs_type;
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

                    result = lhs_type;
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

                    result = &BOOL_TYPE_HINT;
                },
                .Neg => {
                    var subexpr_type = typecheck_expr(c, null, op.subexpr);
                    var subexpr_flags = subexpr_type.compare();

                    if (!check_flags(subexpr_flags, Type.Is_Integer)) {
                        print_error(c, op.subexpr.line_info, "expected integer, but got '{}'.", .{subexpr_type});
                        std.os.exit(1);
                    }

                    result = subexpr_type;
                },
                .Ref => {
                    var subexpr_type = typecheck_expr(c, null, op.subexpr);

                    if (!op.subexpr.is_lvalue) {
                        print_error(c, op.subexpr.line_info, "expression is not an lvalue.", .{});
                        std.os.exit(1);
                    }

                    result = ast_create(c, Type);
                    result.* = .{
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

                    result = subexpr_type.payload.Pointer.extract_ptr();
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

            result = if_true_type;
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

            result = function.return_type.extract_ptr();
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
                    result = array.subtype.extract_ptr();
                },
                .Pointer => {
                    var subtype = lhs_type.payload.Pointer.extract_ptr();
                    if (subtype.payload == .Array) {
                        result = subtype.payload.Array.subtype.extract_ptr();
                    } else {
                        result = subtype;
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
                        result = struct_field._type.extract_ptr();
                    },
                    .Union_Field => |struct_field| {
                        result = struct_field._type.extract_ptr();
                    },
                    else => unreachable,
                }
            } else {
                print_error(c, field.lhs.line_info, "field '{s}' is not a member of a struct/union.", .{key.text});
                std.os.exit(1);
            }
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

            result = hint;
        },
        .Designator => unreachable,
        .Enum_Field => |text| {
            if (type_hint == null) {
                print_error(c, expr.line_info, "can't infere the type of enumerator.", .{});
                std.os.exit(1);
            }

            var hint = type_hint.?;
            switch (hint.payload) {
                .Enum => |_enum| {
                    var key = SymbolKey{
                        .text = text,
                        .scope = _enum.scope,
                    };
                    var found_symbol = c.symbols.get(key);

                    if (found_symbol != null) {
                        // TODO: store pointer to a enum field symbol in 'Enum_Field'.
                        result = hint;
                    } else {
                        print_error(c, expr.line_info, "enumerator '{s}' is not defined.", .{text});
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
            result = &BOOL_TYPE_HINT;
        },
        .Int64 => {
            result = &INT64_TYPE_HINT;
        },
        .Null => {
            result = &VOID_PTR_TYPE_HINT;
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

                    result = variable._type.?;
                },
                .Parameter => |parameter| {
                    expr.is_lvalue = true;
                    result = parameter._type;
                },
                .Function => |function| {
                    typecheck_type(c, function._type);
                    result = function._type;
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

    expr.size = result.size;

    return result.extract_ptr();
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
    typecheck(&compiler);

    gpa.free(source_code);
    compiler.symbols.deinit();
    ast_arena.deinit();
    std.debug.assert(general_purpose_allocator.deinit() == .ok);
}
