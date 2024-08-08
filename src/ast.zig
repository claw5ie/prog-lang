global_symbols: SymbolList,
main: ?*Symbol,
symbol_table: SymbolTable,
string_pool: common.StringPool,
arena: common.ArenaAllocator,
filepath: [:0]const u8,

const Ast = @This();

pub var global_scope = Scope{
    .parent = null,
};

pub var void_pointer_type: *Type = undefined;
pub var integer_types = [_]?*Type{null} ** 130;
pub var bool_type: *Type = undefined;
pub var void_type: *Type = undefined;

pub const pointer_byte_size = 8;
pub const pointer_alignment: Alignment = .QWORD;

pub const default_stages_none = Ast.Type.Stages{
    .unpacking = .None,
    .shallow_check = .None,
    .void_array_check = .None,
    .full_check = .None,
};

pub const default_stages_done = Ast.Type.Stages{
    .unpacking = .Done,
    .shallow_check = .Done,
    .void_array_check = .Done,
    .full_check = .Done,
};

pub fn create(ast: *Ast, comptime T: type) *T {
    return ast.arena.allocator().create(T) catch {
        common.exit(1);
    };
}

pub fn lookup_integer_type(bits: u8, is_signed: bool) *Type {
    std.debug.assert(bits <= 64);

    const index = 65 * @as(u8, @intFromBool(is_signed)) + bits;
    if (integer_types[index] == null) {
        const byte_size = @max(1, utils.round_to_next_pow2(bits) / 8);
        const data = common.gpa.create(Ast.Type.SharedData) catch {
            common.exit(1);
        };
        data.* = .{
            .as = .{ .Integer = .{
                .bits = bits,
                .is_signed = is_signed,
            } },
            .byte_size = byte_size,
            .alignment = switch (byte_size) {
                1 => .BYTE,
                2 => .WORD,
                4 => .DWORD,
                8 => .QWORD,
                else => unreachable,
            },
            .stages = Ast.default_stages_done,
        };
        const typ = common.gpa.create(Ast.Type) catch {
            common.exit(1);
        };
        typ.* = .{
            .line_info = .{ .line = 1, .column = 1, .offset = 0 },
            .data = data,
            .symbol = null,
        };

        integer_types[index] = typ;
    }

    return integer_types[index].?;
}

const std = @import("std");
const common = @import("common.zig");
const utils = @import("utils.zig");

pub const Alignment = common.Alignment;
const LineInfo = common.LineInfo;

pub const Scope = struct {
    parent: ?*Scope,
};

pub const Stage = enum {
    None,
    Going,
    Done,
};

pub const Type = struct {
    line_info: LineInfo,
    data: *SharedData,
    symbol: ?*Symbol,

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
        if (self.symbol != null and self.symbol == other.symbol) {
            return true;
        }

        switch (self.as) {
            .Struct, .Union, .Enum => return false,
            .Procedure => |sProcedure| {
                if (other.as != .Procedure) {
                    return false;
                }

                const oProcedure = &other.as.Function;

                if (sProcedure.params.count != oProcedure.params.count) {
                    return false;
                }

                var sit = sProcedure.params.iterator();
                var oit = oProcedure.params.iterator();
                while (sit.next()) |sparam| {
                    const oparam = oit.next().?;
                    const otype = oparam.*.payload.Parameter._type;
                    const stype = sparam.*.payload.Parameter._type;
                    if (!stype.eql(otype)) {
                        return false;
                    }
                }

                const sreturn_type = sProcedure.return_type;
                const oreturn_type = oProcedure.return_type;

                return sreturn_type.eql(oreturn_type);
            },
            .Array => |sArray| {
                if (other.as != .Array) {
                    return false;
                } else {
                    // TODO: check if they have the same size.
                    return sArray.subtype.equal(other.as.Array.subtype);
                }
            },
            .Pointer => |ssubtype| {
                if (other.as != .Pointer) {
                    return false;
                } else {
                    return ssubtype.equal(other.as.Pointer);
                }
            },
            .Integer => |sInteger| {
                if (other.as != .Integer) {
                    return false;
                } else {
                    const oInteger = &other.as.Integer;
                    return sInteger.bits == oInteger.bits and sInteger.is_signed == oInteger.is_signed;
                }
            },
            .Bool => {
                return other.as == .Bool;
            },
            .Void => {
                return other.as == .Void;
            },
            .Identifier => unreachable,
        }
    }

    pub const SharedData = struct {
        as: As,
        byte_size: u64,
        alignment: Alignment,
        stages: Stages,
    };

    pub const As = union(enum) {
        Struct: Type.Struct,
        Union: Type.Struct,
        Enum: Type.Struct,
        Proc: Type.Proc,
        Array: Type.Array,
        Pointer: *Type,
        Integer: Type.IntegerType,
        Bool: void,
        Void: void,
        Identifier: Type.Identifier,
    };

    pub const Struct = struct {
        fields: SymbolList,
        scope: *Scope,
    };

    pub const Proc = struct {
        params: SymbolList,
        return_type: *Type,
        scope: *Scope,
    };

    pub const Array = struct {
        subtype: *Type,
        size: *Expr,
    };

    pub const IntegerType = struct {
        bits: u8,
        is_signed: bool,
    };

    pub const Identifier = struct {
        name: []const u8,
        scope: *Scope,
    };

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

    pub const Stages = struct {
        unpacking: Stage,
        shallow_check: Stage,
        void_array_check: Stage,
        full_check: Stage,
    };
};

pub const Expr = struct {
    line_info: LineInfo,
    as: As,
    typ: *Type,
    flags: Flags,

    pub const As = union(enum) {
        Binary_Op: Expr.BinaryOp,
        Unary_Op: Expr.UnaryOp,
        Ref: *Expr,
        Deref: *Expr,
        If: Expr.If,
        Field: Expr.Field,
        Call: Expr.Call,
        Constructor: Expr.Constructor,
        Subscript: Expr.Subscript,
        Cast: Expr.Cast,
        Type: Type,
        Integer: u64,
        Boolean: bool,
        Null: void,
        Symbol: *Symbol,
        Identifier: Expr.Identifier,
    };

    pub const BinaryOp = struct {
        line_info: LineInfo,
        lhs: *Expr,
        rhs: *Expr,
        tag: Tag,

        pub const Tag = enum {
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
    };

    pub const UnaryOp = struct {
        subexpr: *Expr,
        tag: Tag,

        pub const Tag = enum {
            Pos,
            Neg,
            Not,
        };
    };

    pub const If = struct {
        condition: *Expr,
        true_branch: *Expr,
        false_branch: *Expr,
    };

    pub const Call = struct {
        subexpr: *Expr,
        args: ExprList,
    };

    pub const Constructor = struct {
        typ: *Type,
        args: ExprList,
    };

    pub const Subscript = struct {
        subexpr: *Expr,
        index: *Expr,
    };

    pub const Field = struct {
        subexpr: *Expr,
        field: *Expr,
    };

    pub const Cast = struct {
        typ: *Type,
        expr: *Expr,
    };

    pub const Identifier = struct {
        name: []const u8,
        scope: *Scope,
    };

    pub const Flags = packed struct {
        is_lvalue: bool = false,
    };
};

pub const ExprListNode = union(enum) {
    Designator: struct {
        lhs: *Expr,
        rhs: *Expr,
    },
    Expr: *Expr,
};

pub const ExprList = std.DoublyLinkedList(*ExprListNode);

pub const Stmt = struct {
    line_info: LineInfo,
    as: As,

    pub const As = union(enum) {
        Print: *Expr,
        Block: StmtList,
        If: Stmt.If,
        While: Stmt.While,
        Do_While: Stmt.While,
        Break: void,
        Continue: void,
        Switch: Stmt.Switch,
        Return: ?*Expr,
        Symbol: *Symbol,
        Assign: Stmt.Assign,
        Expr: *Expr,
    };

    pub const If = struct {
        condition: *Expr,
        true_branch: *Stmt,
        false_branch: ?*Stmt,
    };

    pub const While = struct {
        condition: *Expr,
        body: *Stmt,
    };

    pub const Switch = struct {
        condition: *Expr,
        cases: CaseList,
        default_case: ?*Stmt,

        pub const Case = union(enum) {
            Case: struct {
                value: *Expr,
                subcase: *Case,
            },
            Stmt: *Stmt,
        };

        pub const CaseList = std.DoublyLinkedList(*Case);
    };

    pub const Assign = struct {
        lhs: *Expr,
        rhs: *Expr,
    };
};

pub const StmtList = std.DoublyLinkedList(*Stmt);

pub const Symbol = struct {
    line_info: LineInfo,
    as: As,
    key: Key,

    pub const As = union(enum) {
        Variable: Symbol.Variable,
        Parameter: Symbol.Parameter,
        Procedure: Symbol.Procedure,
        Struct_Field: Symbol.StructField,
        Union_Field: Symbol.StructField,
        Enum_Field: Symbol.EnumField,
        Type: *Type,
    };

    pub const Variable = struct {
        typ: ?*Type,
        value: ?*Expr,
    };

    pub const Parameter = struct {
        typ: *Type,
        value: ?*Expr,
    };

    pub const Procedure = struct {
        typ: *Type,
        block: StmtList,
    };

    pub const StructField = struct {
        typ: *Type,
        value: ?*Expr,
    };

    pub const EnumField = struct {
        typ: *Type,
        value: ?*Expr,
    };

    pub const Key = struct {
        name: []const u8,
        scope: *Scope,
    };
};

pub const SymbolList = std.DoublyLinkedList(*Symbol);

pub const SymbolTable = struct {
    map: HashMap,

    pub fn init(allocator: common.Allocator) SymbolTable {
        return .{ .map = HashMap.init(allocator) };
    }

    pub fn deinit(table: *SymbolTable) void {
        table.map.deinit();
    }

    pub fn insert(table: *SymbolTable, key: Key) InsertResult {
        return table.map.getOrPut(key) catch {
            common.exit(1);
        };
    }

    pub const Key = Symbol.Key;
    pub const Value = *Symbol;

    pub const InsertResult = HashMap.GetOrPutResult;

    pub const Context = struct {
        pub fn hash(_: Context, key: Key) u64 {
            const MurMur = std.hash.Murmur2_64;

            const h0 = MurMur.hash(key.name);
            const h1 = MurMur.hashUint64(@intFromPtr(key.scope));

            return h0 +% 33 *% h1;
        }

        pub fn eql(_: Context, k0: Key, k1: Key) bool {
            return k0.scope == k1.scope and std.mem.eql(u8, k0.name, k1.name);
        }
    };

    pub const HashMap = std.HashMap(Key, Value, Context, 80);
};
