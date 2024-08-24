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

pub fn integer_type_from_u64(value: u64) *Ast.Type {
    const bits = utils.count_bits(value);
    return Ast.lookup_integer_type(bits, false);
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

pub fn find_symbol_in_scope(ast: *Ast, key: Symbol.Key, offset: usize) ?*Symbol {
    const has_symbol = ast.symbol_table.find(key);

    if (has_symbol) |symbol| {
        switch (symbol.as) {
            .Variable,
            .Parameter,
            => {
                if (symbol.line_info.offset < offset) {
                    return symbol;
                }
            },
            .Procedure,
            .Struct_Field,
            .Union_Field,
            .Enum_Field,
            .Type,
            => return symbol,
        }
    }

    return null;
}

pub fn find_symbol(ast: *Ast, key: Symbol.Key, offset: usize) ?*Symbol {
    var k = key;
    while (true) {
        const has_symbol = find_symbol_in_scope(ast, k, offset);
        if (has_symbol) |symbol| {
            return symbol;
        } else if (k.scope.parent) |parent| {
            k.scope = parent;
        } else {
            break;
        }
    }
    return null;
}

const std = @import("std");
const common = @import("common.zig");
const utils = @import("utils.zig");
const Lexer = @import("lexer.zig");
const IRC = @import("irc.zig");

pub const Alignment = common.Alignment;
const LineInfo = common.LineInfo;

pub const Attributes = Lexer.Token.Attribute;

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
            try writer.writeAll(symbol.key.name);
            return;
        }

        switch (self.data.as) {
            .Struct => try writer.writeAll("<struct>"),
            .Union => try writer.writeAll("<union>"),
            .Enum => try writer.writeAll("<enum>"),
            .Proc => |Procedure| {
                try writer.print("proc(", .{});

                var it = Procedure.params.first;
                while (it) |node| {
                    const Parameter = &node.data.as.Parameter;
                    try writer.print("{s}: {}", .{ node.data.key.name, Parameter.typ });

                    it = node.next;
                    if (it != null) {
                        try writer.print(", ", .{});
                    }
                }

                try writer.print(") {}", .{Procedure.return_type});
            },
            .Array => |Arr| try writer.print("{}[{}]", .{ Arr.subtype, Arr.computed_size }),
            .Pointer => |subtype| try writer.print("{}^", .{subtype}),
            .Void => try writer.writeAll("void"),
            .Bool => try writer.writeAll("bool"),
            .Integer => |Int| try writer.print("{c}{}", .{ @as(u8, if (Int.is_signed) 'i' else 'u'), Int.bits }),
            .Identifier, .Type_Of => unreachable,
        }
    }

    pub fn compare(self: *Type) Flags {
        var flags = Flags{};

        switch (self.data.as) {
            .Struct, .Union, .Array, .Void => {},
            .Enum => {
                flags.is_comparable = true;
                flags.is_ordered = true;
            },
            .Proc => {
                flags.is_comparable = true;
                flags.is_pointer = true;
            },
            .Pointer => |subtype| {
                flags.is_comparable = true;
                flags.is_ordered = true;
                flags.is_pointer = true;
                flags.can_be_dereferenced = true;

                switch (subtype.data.as) {
                    .Struct => flags.is_pointer_to_struct = true,
                    .Union => flags.is_pointer_to_union = true,
                    .Array => flags.is_pointer_to_array = true,
                    .Void => {
                        flags.is_void_pointer = true;
                        flags.can_be_dereferenced = false;
                    },
                    else => {},
                }
            },
            .Integer => {
                flags.is_comparable = true;
                flags.is_ordered = true;
            },
            .Bool => flags.is_comparable = true,
            .Identifier, .Type_Of => unreachable,
        }

        return flags;
    }

    pub fn is_signed(typ: *Type) bool {
        switch (typ.data.as) {
            .Enum => |Enumerator| {
                return Enumerator.integer_type.data.as.Integer.is_signed;
            },
            .Integer => |Integer| {
                return Integer.is_signed;
            },
            else => return false,
        }
    }

    pub fn equal(self: *Type, other: *Type) bool {
        if (self.symbol != null and self.symbol == other.symbol) {
            return true;
        }

        switch (self.data.as) {
            .Struct, .Union, .Enum => return false,
            .Proc => |sProc| {
                if (other.data.as != .Proc) {
                    return false;
                }

                const oProc = &other.data.as.Proc;

                if (sProc.params.len != oProc.params.len) {
                    return false;
                }

                var sit = sProc.params.first;
                var oit = oProc.params.first;
                while (sit != null) {
                    const snode = sit.?;
                    const onode = oit.?;

                    const stype = snode.data.as.Parameter.typ;
                    const otype = onode.data.as.Parameter.typ;
                    if (!stype.equal(otype)) {
                        return false;
                    }

                    sit = snode.next;
                    oit = onode.next;
                }

                const sreturn_type = sProc.return_type;
                const oreturn_type = oProc.return_type;

                return sreturn_type.equal(oreturn_type);
            },
            .Array => |sArray| {
                if (other.data.as != .Array) {
                    return false;
                } else {
                    const oArray = &other.data.as.Array;
                    return sArray.computed_size == oArray.computed_size and sArray.subtype.equal(oArray.subtype);
                }
            },
            .Pointer => |ssubtype| {
                if (other.data.as != .Pointer) {
                    return false;
                } else {
                    return ssubtype.equal(other.data.as.Pointer);
                }
            },
            .Integer => |sInteger| {
                if (other.data.as != .Integer) {
                    return false;
                } else {
                    const oInteger = &other.data.as.Integer;
                    return sInteger.bits == oInteger.bits and sInteger.is_signed == oInteger.is_signed;
                }
            },
            .Bool => {
                return other.data.as == .Bool;
            },
            .Void => {
                return other.data.as == .Void;
            },
            .Identifier, .Type_Of => unreachable,
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
        Enum: Type.Enum,
        Proc: Type.Proc,
        Array: Type.Array,
        Pointer: *Type,
        Integer: Type.IntegerType,
        Bool: void,
        Void: void,
        Identifier: Type.Identifier,
        Type_Of: *Ast.Expr,
    };

    pub const Struct = struct {
        fields: SymbolList,
        scope: *Scope,
    };

    pub const Enum = struct {
        fields: SymbolList,
        integer_type: *Type,
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
        computed_size: u64,
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
        is_comparable: bool = false,
        is_ordered: bool = false,
        is_pointer: bool = false,
        is_void_pointer: bool = false,
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
    typechecking: Stage,

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
        Byte_Size_Of: *Ast.Expr,
        Alignment_Of: *Ast.Expr,
        As: Expr.Cast,
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
        is_const: bool = false,
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
    typechecking: Stage,

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
        storage: IRC.Lvalue,
    };

    pub const Parameter = struct {
        typ: *Type,
        value: ?*Expr,
        storage: IRC.Lvalue,
    };

    pub const Procedure = struct {
        typ: *Type,
        block: StmtList,
        label: ?IRC.Label,
    };

    pub const StructField = struct {
        typ: *Type,
        value: ?*Expr,
        offset: u64,
    };

    pub const EnumField = struct {
        value: ?*Expr,
        computed_value: u64,
    };

    pub const Key = struct {
        name: []const u8,
        scope: *Scope,
    };
};

pub const SymbolList = std.DoublyLinkedList(*Symbol);

pub const SymbolTable = struct {
    map: HashMap,

    pub inline fn init(allocator: common.Allocator) SymbolTable {
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

    pub inline fn find(table: *SymbolTable, key: Key) ?Value {
        return table.map.get(key);
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
