const std = @import("std");
const common = @import("common.zig");
const notstd = @import("notstd.zig");
const Lexer = @import("lexer.zig");
const Ir = @import("ircode.zig");

const LineInfo = common.LineInfo;
const ArenaAllocator = std.heap.ArenaAllocator;
const Allocator = std.mem.Allocator;

globals: SymbolList,
locals: SymbolList,
main: *Symbol,
symbols: SymbolTable,
ast_arena: ArenaAllocator,

global_scope: *Scope,
next_label: Ir.Label,

filepath: []const u8,

const This = @This();

pub const FunctionDepth = i32;

pub const Scope = struct {
    parent: ?*Scope,
};

pub const Identifier = struct {
    token: Lexer.Token,
    scope: *Scope,
};

pub const TypeStruct = struct {
    fields: SymbolList,
    scope: *Scope,
};

pub const TypeTag = enum {
    Struct,
    Union,
    Enum,
    Function,
    Array,
    Pointer,
    Void,
    Bool,
    Int64,
    Identifier,
};

pub const TypePayload = union(TypeTag) {
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
    Identifier: Identifier,
};

pub const TypeSize = u31;

pub const TypecheckingStage = enum {
    Not_Typechecked,
    Being_Shallow_Typechecked,
    Shallow_Typechecked,
    Being_Fully_Typechecked,
    Fully_Typechecked,
};

pub const Type = struct {
    pub const Flags = packed struct {
        is_integer: bool = false,
        is_comparable: bool = false,
        is_ptr: bool = false,
        is_void_ptr: bool = false,
        can_be_dereferenced: bool = false,
        is_pointer_to_struct: bool = false,
        is_pointer_to_union: bool = false,
        is_pointer_to_array: bool = false,
    };

    payload: TypePayload,
    symbol: ?*Symbol = null,
    size: TypeSize,
    is_resolved: bool = false,
    typechecking_stage: TypecheckingStage = .Not_Typechecked,
    line_info: LineInfo,

    pub fn format(self: Type, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        if (self.symbol) |symbol| {
            try writer.writeAll(symbol.key.text);
            return;
        }

        switch (self.payload) {
            .Struct => try writer.writeAll("<struct>"),
            .Union => try writer.writeAll("<union>"),
            .Enum => try writer.writeAll("<enum>"),
            .Function => |function| {
                try writer.print("proc(", .{});

                var it = function.params.iterator();
                while (it.next()) |param_ptr| {
                    const param = &param_ptr.*.payload.Parameter;
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
            .Identifier => unreachable,
        }
    }

    pub fn compare(self: *Type) Flags {
        var flags: Flags = .{};

        switch (self.payload) {
            .Enum => {
                flags.is_integer = true;
                flags.is_comparable = true;
            },
            .Function => flags.is_ptr = true,
            .Pointer => |subtype| {
                flags.is_ptr = true;
                flags.can_be_dereferenced = true;

                switch (subtype.payload) {
                    .Struct => flags.is_pointer_to_struct = true,
                    .Union => flags.is_pointer_to_union = true,
                    .Array => flags.is_pointer_to_array = true,
                    .Void => {
                        flags.is_void_ptr = true;
                        flags.can_be_dereferenced = false;
                    },
                    else => {},
                }
            },
            .Bool => flags.is_comparable = true,
            .Int64 => {
                flags.is_integer = true;
                flags.is_comparable = true;
            },
            .Struct, .Union, .Array, .Void => {},
            .Identifier => unreachable,
        }

        return flags;
    }

    pub fn eql(self: *Type, other: *Type) bool {
        if (self.payload != @as(TypeTag, other.payload)) {
            return false;
        }

        switch (self.payload) {
            .Struct => {
                const sstruct = &self.payload.Struct;
                const ostruct = &other.payload.Struct;
                return sstruct.scope == ostruct.scope;
            },
            .Union => {
                const sstruct = &self.payload.Union;
                const ostruct = &other.payload.Union;
                return sstruct.scope == ostruct.scope;
            },
            .Enum => {
                const sstruct = &self.payload.Enum;
                const ostruct = &other.payload.Enum;
                return sstruct.scope == ostruct.scope;
            },
            .Function => {
                const sfunc = &self.payload.Function;
                const ofunc = &other.payload.Function;

                if (sfunc.params.count != ofunc.params.count) {
                    return false;
                }

                var sit = sfunc.params.iterator();
                var oit = ofunc.params.iterator();
                while (sit.next()) |sparam| {
                    const oparam = oit.next().?;
                    const otype = oparam.*.payload.Parameter._type;
                    const stype = sparam.*.payload.Parameter._type;
                    if (!stype.eql(otype)) {
                        return false;
                    }
                }

                const sreturn_type = sfunc.return_type;
                const oreturn_type = ofunc.return_type;

                return sreturn_type.eql(oreturn_type);
            },
            .Array => {
                const sarray = &self.payload.Array;
                const oarray = &other.payload.Array;

                if (sarray.count != oarray.count) {
                    return false;
                }

                const ssubtype = sarray.subtype;
                const osubtype = oarray.subtype;

                return ssubtype.eql(osubtype);
            },
            .Pointer => {
                const stype = self.payload.Pointer;
                const otype = other.payload.Pointer;

                if (stype.payload == .Void or otype.payload == .Void) {
                    return true;
                }

                return stype.eql(otype);
            },
            .Void,
            .Bool,
            .Int64,
            => return true,
            .Identifier => unreachable,
        }
    }
};

pub const ExprBinaryOpTag = enum {
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

pub const ExprUnaryOpTag = enum {
    Not,
    Neg,
    Ref,
    Deref,
};

pub const ExprTag = enum {
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

pub const ExprPayload = union(ExprTag) {
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
        id: Lexer.Token,
    },
    Initializer: struct {
        _type: *Type,
        expr_list: ExprList,
    },
    Expr_List: ExprList,
    Designator: struct {
        id: Lexer.Token,
        expr: *Expr,
    },
    Enum_Field_From_Type: struct {
        _type: *Type,
        id: Lexer.Token,
    },
    Enum_Field: Lexer.Token,
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

pub const Expr = struct {
    payload: ExprPayload,
    _type: *Type,
    is_lvalue: bool = false,
    line_info: LineInfo,
};

pub const ExprList = notstd.DoublyLinkedList(Expr);

pub const StmtTag = enum {
    Print,
    Block,
    If,
    While,
    Break,
    Continue,
    Switch,
    Case,
    Return,
    Return_Expr,
    Symbol,
    Assign,
    Expr,
};

pub const StmtPayload = union(StmtTag) {
    Print: *Expr,
    Block: StmtBlock,
    If: struct {
        cond: *Expr,
        if_true: *Stmt,
        if_false: ?*Stmt,
    },
    While: struct {
        cond: *Expr,
        block: *Stmt,
        is_do_while: bool = false,
    },
    Break: void,
    Continue: void,
    Switch: struct {
        cond: *Expr,
        cases: StmtBlock,
    },
    Case: struct {
        expr: *Expr,
        stmt: *Stmt,
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

pub const Stmt = struct {
    payload: StmtPayload,
    line_info: LineInfo,
};

pub const StmtBlock = notstd.DoublyLinkedList(Stmt);

pub const SymbolVariable = struct {
    _type: ?*Type,
    expr: ?*Expr,
    was_visited: bool = false,
    tmp: Ir.Tmp,
};

pub const SymbolParameter = struct {
    _type: *Type,
    has_id: bool,
    tmp: Ir.Tmp,
};

pub const SymbolFunction = struct {
    _type: *Type,
    block: StmtBlock,
    label: Ir.Label,
    depth: FunctionDepth,
};

pub const SymbolStructField = struct {
    _type: *Type,
};

pub const SymbolEnumField = struct {
    _type: *Type,
    value: u64,
};

pub const SymbolDefinition = struct {
    expr: *Expr,
    was_visited: bool = false,
};

pub const SymbolTag = enum {
    Variable,
    Parameter,
    Function,
    Struct_Field,
    Enum_Field,
    Type,
    Definition,
};

pub const SymbolPayload = union(SymbolTag) {
    Variable: SymbolVariable,
    Parameter: SymbolParameter,
    Function: SymbolFunction,
    Struct_Field: SymbolStructField,
    Enum_Field: SymbolEnumField,
    Type: *Type,
    Definition: SymbolDefinition,
};

pub const Symbol = struct {
    payload: SymbolPayload,
    key: SymbolKey,
    depth: FunctionDepth,
    line_info: LineInfo,
};

pub const SymbolList = notstd.DoublyLinkedList(*Symbol);

pub const SymbolKey = struct {
    text: []const u8,
    scope: *Scope,
};

pub const SymbolTableContext = struct {
    pub fn hash(_: SymbolTableContext, key: SymbolKey) u64 {
        const MurMur = std.hash.Murmur2_64;

        const h0 = MurMur.hash(key.text);
        const h1 = MurMur.hashUint64(@intFromPtr(key.scope));

        return h0 +% 33 *% h1;
    }

    pub fn eql(_: SymbolTableContext, k0: SymbolKey, k1: SymbolKey) bool {
        return k0.scope == k1.scope and std.mem.eql(u8, k0.text, k1.text);
    }
};

pub const SymbolTable = std.HashMap(SymbolKey, *Symbol, SymbolTableContext, 80);

pub fn create(ast: *This, comptime T: type) *T {
    return ast.ast_arena.allocator().create(T) catch {
        std.posix.exit(1);
    };
}

pub fn extract_type(allocator: Allocator, expr: Expr) ?*Type {
    switch (expr.payload) {
        .Type => |_type| {
            return _type;
        },
        .Identifier => |id| {
            const _type = allocator.create(Type) catch {
                std.posix.exit(1);
            };
            _type.* = .{
                .payload = .{ .Identifier = id },
                .size = undefined,
                .line_info = expr.line_info,
            };

            return _type;
        },
        else => {
            return null;
        },
    }
}
