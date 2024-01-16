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
    Symbol,
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
    Symbol: *Symbol,
    Identifier: Identifier,
};

pub const TypeSize = u31;

pub const TypecheckingStage = enum {
    Not_Typechecked,
    Being_Typechecked,
    Shallow_Typechecked,
    Fully_Typechecked,
};

pub const Type = struct {
    pub const FlagType = u16;
    pub const Is_Integer: FlagType = 0x1;
    pub const Is_Integral: FlagType = 0x2;
    pub const Is_Comparable: FlagType = 0x4;
    pub const Is_Ptr: FlagType = 0x8;
    pub const Is_Void_Ptr: FlagType = 0x10;
    pub const Can_Be_Dereferenced: FlagType = 0x20;
    pub const Is_Pointer_To_Struct: FlagType = 0x40;
    pub const Is_Pointer_To_Union: FlagType = 0x80;
    pub const Is_Pointer_To_Array: FlagType = 0x100;

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

        var h0 = MurMur.hash(key.text);
        var h1 = MurMur.hashUint64(@intFromPtr(key.scope));

        return h0 +% 33 *% h1;
    }

    pub fn eql(_: SymbolTableContext, k0: SymbolKey, k1: SymbolKey) bool {
        return k0.scope == k1.scope and std.mem.eql(u8, k0.text, k1.text);
    }
};

pub const SymbolTable = std.HashMap(SymbolKey, *Symbol, SymbolTableContext, 80);

pub fn create(ast: *This, comptime T: type) *T {
    return ast.ast_arena.allocator().create(T) catch {
        std.os.exit(1);
    };
}

pub fn extract_type(allocator: Allocator, expr: Expr) ?*Type {
    switch (expr.payload) {
        .Type => |_type| {
            return _type;
        },
        .Identifier => |id| {
            var _type = allocator.create(Type) catch {
                std.os.exit(1);
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
