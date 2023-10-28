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

const FunctionDepth = i32;

const Compiler = struct {
    const LOOKAHEAD = 2;
    tokens: [LOOKAHEAD]Token = undefined,
    token_start: u8 = 0,
    token_count: u8 = 0,
    filepath: []const u8,
    source_code: [:0]u8,
    line_info: LineInfo = .{},

    globals: SymbolList,
    local_functions: SymbolList,
    ast_arena: ArenaAllocator,
    ast_arena_allocator: Allocator,
    symbols: SymbolTable,
    current_scope: *Scope,
    global_scope: *Scope,
    main_function: *Symbol,
    function_depth: FunctionDepth = 0,

    ir_instrs: IrInstrList,
    next_global: IrOffset = 0,
    next_local: IrOffset = 0,
    next_label: IrLabel = 0,
    biggest_local_so_far: IrOffset = 0,

    stack: []u8,
    rsp: u64 = 0,
    rbp: u64 = 0,

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
        count: TypeSize,
        subtype: *Type,
    },
    Pointer: *Type,
    Void: void,
    Bool: void,
    Int64: void,
    Symbol: *Symbol,
    Identifier: Identifier,
};

const TypeSize = u31;

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
    size: TypeSize,
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
    Initializer,
    Expr_List,
    Designator,
    Enum_Field_From_Type,
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
    Initializer: struct {
        _type: *Type,
        expr_list: ExprList,
    },
    Expr_List: ExprList,
    Designator: struct {
        id: Token,
        expr: *Expr,
    },
    Enum_Field_From_Type: struct {
        _type: *Type,
        id: Token,
    },
    Enum_Field: Token,
    Cast1: *Expr,
    Cast2: struct {
        _type: *Type,
        expr: *Expr,
    },
    Bool: bool,
    Int64: i64,
    Null: void,
    Type: *Type,
    Symbol: *Symbol,
    Identifier: Identifier,
};

const Expr = struct {
    payload: ExprPayload,
    _type: *Type,
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
    tmp: IrTmp,
};

const SymbolParameter = struct {
    _type: *Type,
    has_id: bool,
    tmp: IrTmp,
};

const SymbolFunction = struct {
    _type: *Type,
    block: StmtList,
    label: IrLabel,
    depth: FunctionDepth,
};

const SymbolStructField = struct {
    _type: *Type,
};

const SymbolUnionField = struct {
    _type: *Type,
};

const SymbolEnumField = struct {
    _type: *Type,
    value: u64,
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
    depth: FunctionDepth,
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

const IrInstrList = std.ArrayList(IrInstr);

const CHOOSE_BASE: u3 = 0x1;
const CHOOSE_DISP: u3 = 0x2;

const IrOffset = i31;

const IrTmpTag = enum {
    Global,
    Local,
};

const IrTmp = packed struct {
    offset: IrOffset,
    tag: IrTmpTag,
    height: FunctionDepth,

    pub inline fn as_lvalue(tmp: IrTmp) IrLvalue {
        return .{ .Tmp = tmp };
    }

    pub inline fn as_rvalue(tmp: IrTmp) IrRvalue {
        return .{ .Lvalue = .{ .Tmp = tmp } };
    }
};

const IrLvalueTag = enum {
    Tmp,
    Mem,
};

const IrLvalue = union(IrLvalueTag) {
    Tmp: IrTmp,
    Mem: IrMem,

    pub inline fn as_rvalue(lvalue: IrLvalue) IrRvalue {
        return .{ .Lvalue = lvalue };
    }
};

const IrRvalueTag = enum {
    Lvalue,
    Addr,
    Label,
    Imm,
};

const IrRvalue = union(IrRvalueTag) {
    Lvalue: IrLvalue,
    Addr: IrLvalue,
    Label: IrLabel,
    Imm: i64,

    pub fn ref(src_tmp: IrRvalue, c: *Compiler) IrLvalue {
        switch (src_tmp) {
            .Lvalue => |lvalue| {
                switch (lvalue) {
                    .Tmp => |tmp| {
                        return .{ .Mem = .{
                            .choose = CHOOSE_BASE,
                            .base = tmp,
                        } };
                    },
                    .Mem => {
                        var dst_tmp = c.grab_local();

                        generate_ir_instr(c, .{ .Instr = .{
                            .Mov = .{
                                .dst = dst_tmp.as_lvalue(),
                                .src = src_tmp,
                            },
                        } });

                        return .{ .Mem = .{
                            .choose = CHOOSE_BASE,
                            .base = dst_tmp,
                        } };
                    },
                }
            },
            .Addr => |dst_tmp| {
                return dst_tmp;
            },
            .Label => |label| {
                return .{ .Mem = .{
                    .choose = CHOOSE_DISP,
                    .disp = label,
                } };
            },
            .Imm => |imm| {
                return .{ .Mem = .{
                    .choose = CHOOSE_DISP,
                    .disp = imm,
                } };
            },
        }
    }

    pub fn deref(src_tmp: IrRvalue, c: *Compiler) IrRvalue {
        return .{ .Lvalue = src_tmp.ref(c) };
    }
};

const IrMem = struct {
    choose: u2 = 0,
    base: IrTmp = .{
        .offset = 0,
        .tag = .Global,
        .height = 0,
    },
    disp: i64 = 0,
};

const IrLabel = u32;

const IrInstrJmpcTag = enum {
    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,
};

const IrInstrBinaryOpTag = enum {
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

const IrInstrUnaryOpTag = enum {
    Not,
    Neg,
};

const IrInstrOpcode = enum {
    Print,
    Mov,
    Jmp,
    Push_Frame_Pointer,
    Push,
    Pop,
    Call,
    Ret,
    Exit,
};

const IrInstrMetaTag = enum {
    GFB,
    GFE,
    Label,
};

const IrInstrType = enum {
    Binary_Op,
    Unary_Op,
    Jmpc,
    Instr,
    Meta,
};

const IrInstr = union(IrInstrType) {
    Binary_Op: struct {
        tag: IrInstrBinaryOpTag,
        dst: IrLvalue,
        src0: IrRvalue,
        src1: IrRvalue,
    },
    Unary_Op: struct {
        tag: IrInstrUnaryOpTag,
        dst: IrLvalue,
        src: IrRvalue,
    },
    Jmpc: struct {
        tag: IrInstrJmpcTag,
        src0: IrRvalue,
        src1: IrRvalue,
        label: IrLabel,
    },
    Instr: union(IrInstrOpcode) {
        Print: IrRvalue,
        Mov: struct {
            dst: IrLvalue,
            src: IrRvalue,
        },
        Jmp: IrLabel,
        Push_Frame_Pointer: i32,
        Push: IrRvalue,
        Pop: u64,
        Call: IrRvalue,
        Ret: IrLabel,
        Exit: void,
    },
    Meta: union(IrInstrMetaTag) {
        GFB: struct {
            stack_space_used: u32,
            label: IrLabel,
        },
        GFE: struct {
            stack_space_used: u32,
            label: IrLabel,
        },
        Label: IrLabel,
    },
};

const IrStmtContext = struct {
    loop_cond_label: IrLabel,
    loop_end_label: IrLabel,
    function_end_label: IrLabel,
    return_address_offset: IrOffset,
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
        .depth = c.function_depth,
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

fn extract_type(c: *Compiler, expr: Expr) *Type {
    switch (expr.payload) {
        .Type => |_type| {
            return _type;
        },
        .Identifier => |id| {
            var _type = ast_create(c, Type);
            _type.* = .{
                .payload = .{ .Identifier = id },
                .size = undefined,
                .line_info = expr.line_info,
            };

            return _type;
        },
        else => {
            print_error(c, expr.line_info, "expression doesn't look like a type.", .{});
            std.os.exit(1);
        },
    }
}

fn parse_type(c: *Compiler) *Type {
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

        var _type = parse_type(c);
        var symbol = create_symbol(c, id);
        symbol.payload = .{ .Parameter = .{
            ._type = _type,
            .has_id = has_id,
            .tmp = undefined,
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

    var return_type: *Type = undefined;

    if (c.peek() == .Arrow) {
        c.advance();
        return_type = parse_type(c);
    } else {
        return_type = ast_create(c, Type);
        return_type.* = .{
            .payload = .Void,
            .size = undefined,
            .line_info = c.grab().line_info,
        };
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

        var _type = parse_type(c);
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
                c.function_depth += 1;

                var _type = parse_type(c);

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
                        .label = c.grab_label(),
                        .depth = c.function_depth,
                    } };
                    c.current_scope = previous_scope;
                } else {
                    c.expect(.Semicolon);
                    symbol.payload = .{ .Type = _type };
                }

                c.function_depth -= 1;

                return symbol;
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
            var _type = parse_type(c);

            if (c.peek() == .Equal) {
                c.advance();
                expr = ast_create(c, Expr);
                expr.?.* = parse_expr(c);
            }

            c.expect(.Semicolon);

            symbol.payload = .{ .Variable = .{
                ._type = _type,
                .expr = expr,
                .tmp = undefined,
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

fn parse_expr_list(c: *Compiler) ExprList {
    c.expect(.Open_Curly);

    var result = ExprList{};

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
                    ._type = undefined,
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
        result.insert_last(node);

        tt = c.peek();
        if (tt != .End_Of_File and tt != .Close_Curly) {
            c.expect(.Comma);
            tt = c.peek();
        }
    }

    c.expect(.Close_Curly);

    return result;
}

fn parse_expr(c: *Compiler) Expr {
    return parse_prec(c, LOWEST_PREC);
}

fn parse_prec(c: *Compiler, min_prec: i32) Expr {
    var lhs = parse_highest_prec(c);
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
                ._type = undefined,
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
        ._type = undefined,
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
                    .Open_Curly => break :payload .{ .Expr_List = parse_expr_list(c) },
                    .Identifier => {
                        var id = c.grab();
                        c.advance();

                        break :payload .{ .Enum_Field = id };
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
                var subtype = parse_type(c);
                var _type = ast_create(c, Type);
                _type.* = .{
                    .payload = .{ .Pointer = subtype },
                    .size = undefined,
                    .line_info = token.line_info,
                };

                break :payload .{ .Type = _type };
            },
            .Open_Bracket => {
                var expr = ast_create(c, Expr);
                expr.* = parse_expr(c);
                c.expect(.Close_Bracket);

                var subtype = parse_type(c);
                var _type = ast_create(c, Type);
                _type.* = .{
                    .payload = .{ .Array = .{
                        .expr = expr,
                        .count = undefined,
                        .subtype = subtype,
                    } },
                    .size = undefined,
                    .line_info = token.line_info,
                };

                break :payload .{ .Type = _type };
            },
            .Struct => {
                var _struct = parse_struct_fields(c, true);
                var _type = ast_create(c, Type);
                _type.* = .{
                    .payload = .{ .Struct = _struct },
                    .size = undefined,
                    .line_info = token.line_info,
                };

                break :payload .{ .Type = _type };
            },
            .Union => {
                var _union = parse_struct_fields(c, false);
                var _type = ast_create(c, Type);
                _type.* = .{
                    .payload = .{ .Union = _union },
                    .size = undefined,
                    .line_info = token.line_info,
                };

                break :payload .{ .Type = _type };
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
                        .value = undefined,
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

                var _type = ast_create(c, Type);
                _type.* = .{
                    .payload = .{ .Enum = _enum },
                    .size = undefined,
                    .line_info = token.line_info,
                };

                break :payload .{ .Type = _type };
            },
            .Proc => {
                var _type = ast_create(c, Type);
                _type.* = .{
                    .payload = parse_type_function(c),
                    .size = undefined,
                    .line_info = token.line_info,
                };

                break :payload .{ .Type = _type };
            },
            .Void => {
                var _type = ast_create(c, Type);
                _type.* = .{
                    .payload = .Void,
                    .size = undefined,
                    .line_info = token.line_info,
                };
                break :payload .{ .Type = _type };
            },
            .Bool => {
                var _type = ast_create(c, Type);
                _type.* = .{
                    .payload = .Bool,
                    .size = undefined,
                    .line_info = token.line_info,
                };
                break :payload .{ .Type = _type };
            },
            .Int => {
                var _type = ast_create(c, Type);
                _type.* = .{
                    .payload = .Int64,
                    .size = undefined,
                    .line_info = token.line_info,
                };
                break :payload .{ .Type = _type };
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
                    var _type = extract_type(c, lhs);
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

    parse_postfix_unary_ops(c, &result);

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
                    ._type = undefined,
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
                    ._type = undefined,
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
                        ._type = undefined,
                        .line_info = lhs.line_info,
                    };
                } else if (c.peek() == .Open_Curly) {
                    var expr_list = parse_expr_list(c);
                    var _type = extract_type(c, inner.*);
                    inner.* = .{
                        .payload = .{ .Initializer = .{
                            ._type = _type,
                            .expr_list = expr_list,
                        } },
                        ._type = undefined,
                        .line_info = _type.line_info,
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
        .local_functions = .{},
        .ast_arena = ast_arena,
        .ast_arena_allocator = ast_arena.allocator(),
        .symbols = SymbolTable.init(gpa),
        .global_scope = undefined,
        .current_scope = undefined,
        .main_function = undefined,
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
