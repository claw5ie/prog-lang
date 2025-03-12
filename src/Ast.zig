globals: SymbolList,
local_procedures: SymbolList,
namespaces: TypeList,
main: ?*Symbol,
symbol_table: SymbolTable,
arena: ArenaAllocator,

const Ast = @This();

pub var global_scope: Scope = .{
    .parent = null,
    .tag = .Global,
};

pub var void_pointer_type: *Type = undefined;
pub var bool_type: *Type = undefined;
pub var void_type: *Type = undefined;

pub const pointer_byte_size = 8;
pub const pointer_alignment: Alignment = .Qword;

pub const default_stages_none = Type.Stages{
    .unpacking = .None,
    .shallow_check = .None,
    .full_check = .None,
};

pub const default_stages_done = Type.Stages{
    .unpacking = .Done,
    .shallow_check = .Done,
    .full_check = .Done,
};

pub fn init() Ast {
    const state = struct {
        pub var s_void_pointer_type_data = Type.SharedData{
            .as = .{ .Pointer = &s_void_type },
            .byte_size = pointer_byte_size,
            .alignment = pointer_alignment,
            .stages = default_stages_done,
        };
        pub var s_bool_type_data = Type.SharedData{
            .as = .Bool,
            .byte_size = 1,
            .alignment = .Byte,
            .stages = default_stages_done,
        };
        pub var s_void_type_data = Type.SharedData{
            .as = .Void,
            .byte_size = 0,
            .alignment = .Byte,
            .stages = default_stages_done,
        };

        pub var s_void_pointer_type = Type{
            .position = .{ .line = 1, .column = 1, .offset = 0 },
            .data = &s_void_pointer_type_data,
            .symbol = null,
        };
        pub var s_bool_type = Type{
            .position = .{ .line = 1, .column = 1, .offset = 0 },
            .data = &s_bool_type_data,
            .symbol = null,
        };
        pub var s_void_type = Type{
            .position = .{ .line = 1, .column = 1, .offset = 0 },
            .data = &s_void_type_data,
            .symbol = null,
        };
    };

    void_pointer_type = &state.s_void_pointer_type;
    bool_type = &state.s_bool_type;
    void_type = &state.s_void_type;

    return .{
        .globals = .{},
        .local_procedures = .{},
        .namespaces = .{},
        .main = null,
        .symbol_table = SymbolTable.init(nostd.general_allocator),
        .arena = ArenaAllocator.init(std.heap.page_allocator),
    };
}

pub fn deinit(ast: *Ast) void {
    ast.arena.deinit();
    ast.symbol_table.deinit();
}

pub fn create(ast: *Ast, comptime T: type) *T {
    return ast.arena.allocator().create(T) catch {
        exit(1);
    };
}

pub fn integer_type_from_u64(ast: *Ast, value: u64) *Type {
    const bits = nostd.highest_bit_count(value);
    return lookup_integer_type(ast, .{ .bits = bits, .is_signed = false });
}

pub fn lookup_integer_type(ast: *Ast, integer: Type.IntegerType) *Type {
    std.debug.assert(integer.bits <= 64 and if (integer.is_signed) integer.bits != 0 else true);

    const state = struct {
        pub const unsigned_integer_type_count = (64 - 0) + 1; // u0 to u64
        pub const signed_integer_type_count = (64 - 1) + 1; // i1 to i64
        pub const total_integer_type_count = unsigned_integer_type_count + signed_integer_type_count;
        pub var integer_types = [1]?*Type{null} ** total_integer_type_count;
    };

    const index = if (integer.is_signed)
        state.unsigned_integer_type_count + integer.bits - 1 // - 1 because signed integers start at bits = 1
    else
        integer.bits;

    if (state.integer_types[index]) |typ| {
        return typ;
    }

    const byte_size = @max(1, nostd.round_to_next_pow2(integer.bits) / 8);
    const data = create(ast, Type.SharedData);
    data.* = .{
        .as = .{ .Integer = integer },
        .byte_size = byte_size,
        .alignment = switch (byte_size) {
            1 => .Byte,
            2 => .Word,
            4 => .Dword,
            8 => .Qword,
            else => unreachable,
        },
        .stages = default_stages_done,
    };
    const typ = create(ast, Type);
    typ.* = .{
        .position = .{ .line = 1, .column = 1, .offset = 0 },
        .data = data,
        .symbol = null,
    };
    state.integer_types[index] = typ;

    return typ;
}

pub fn find_symbol_in_scope(ast: *Ast, key: Symbol.Key, symbol_offset: usize, skip_local_variables: bool) ?*Symbol {
    if (ast.symbol_table.find(key)) |symbol| {
        switch (symbol.as) {
            .Variable => |Variable| {
                if (Variable.attributes.is_const or Variable.attributes.is_static or
                    (!skip_local_variables and symbol.position.offset < symbol_offset))
                {
                    return symbol;
                }
            },
            .Parameter,
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

pub fn find_symbol(ast: *Ast, key: Symbol.Key, symbol_offset: usize) ?*Symbol {
    var _key = key;
    var skip_local_variables = false;

    while (true) {
        if (find_symbol_in_scope(ast, _key, symbol_offset, skip_local_variables)) |symbol| {
            return symbol;
        } else if (_key.scope.parent) |parent| {
            skip_local_variables = skip_local_variables or _key.scope.tag == .Procedure;
            _key.scope = parent;
        } else return null;
    }
}

const exit = nostd.exit;

const std = @import("std");
const nostd = @import("nostd.zig");
const Compiler = @import("Compiler.zig");
const IR = @import("IR.zig");

pub const Allocator = std.mem.Allocator;

const ArenaAllocator = std.heap.ArenaAllocator;
const FilePosition = Compiler.FilePosition;
const Stage = Compiler.Stage;

pub const Alignment = nostd.Alignment;

pub const Scope = struct {
    parent: ?*Scope,
    tag: Tag,

    pub const Tag = enum {
        Global,
        Structure,
        Union,
        Enumerator,
        Procedure,
        Statement_Block,
    };
};

pub const Type = struct {
    position: FilePosition,
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
            .Field, .Identifier, .Type_Of => unreachable,
        }
    }

    pub fn compare(self: *Type) Flags {
        var flags = Flags{};

        // bool:    rank 1
        // enum     rank 2
        // pointer: rank 3
        // proc:    rank 4
        // integer: rank 5
        // else:    rank 0 -> cant cast

        switch (self.data.as) {
            .Struct,
            .Union,
            .Array,
            .Void,
            => {},
            .Enum => {
                flags.is_comparable = true;
                flags.is_ordered = true;
                flags.rank = 2;
            },
            .Proc => {
                flags.is_comparable = true;
                flags.is_pointer = true;
                flags.rank = 4;
            },
            .Pointer => |subtype| {
                flags.is_comparable = true;
                flags.is_ordered = true;
                flags.is_pointer = true;
                flags.can_be_dereferenced = true;
                flags.rank = 3;

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
                flags.rank = 5;
            },
            .Bool => {
                flags.is_comparable = true;
                flags.rank = 1;
            },
            .Field,
            .Identifier,
            .Type_Of,
            => unreachable,
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

    pub fn bits(typ: *Type) bool {
        switch (typ.data.as) {
            .Enum => |Enumerator| {
                return Enumerator.integer_type.data.as.Integer.bits;
            },
            .Integer => |Integer| {
                return Integer.bits;
            },
            else => unreachable,
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
            .Field, .Identifier, .Type_Of => unreachable,
        }
    }

    pub const SharedData = struct {
        as: As,
        byte_size: u64, // TODO: create alias for this type.
        alignment: Alignment,
        stages: Stages,
    };

    pub const As = union(enum) {
        Struct: Struct,
        Union: Struct,
        Enum: Enum,
        Proc: Proc,
        Array: Array,
        Field: Field,
        Pointer: *Type,
        Integer: IntegerType,
        Bool: void,
        Void: void,
        Identifier: Identifier,
        Type_Of: *Expression,
    };

    pub const Struct = struct {
        fields: *SymbolList,
        rest: *SymbolList,
        scope: *Scope,
    };

    pub const Enum = struct {
        fields: *SymbolList,
        rest: *SymbolList,
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
        size: *Expression,
        computed_size: u64,
    };

    pub const Field = struct {
        subtype: *Type,
        field: *Expression,
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
        rank: Rank = invalid_rank,

        pub const Rank = u8;

        pub const invalid_rank: Rank = 0;
    };

    pub const Stages = struct {
        unpacking: Stage,
        shallow_check: Stage,
        full_check: Stage,
    };
};

pub const TypeList = std.DoublyLinkedList(*Type);

pub const Expression = struct {
    position: FilePosition,
    as: As,
    typ: *Type,
    flags: Flags,

    pub const As = union(enum) {
        Binary_Op: BinaryOp,
        Unary_Op: UnaryOp,
        Ref: *Expression,
        Deref: *Expression,
        If: If,
        Field: Field,
        Call: Call,
        Constructor: Constructor,
        Subscript: Subscript,
        Byte_Size_Of: *Expression,
        Alignment_Of: *Expression,
        As: Cast,
        Cast: Cast,
        Type: Type,
        Integer: u64,
        Boolean: bool,
        Null: void,
        Symbol: *Symbol,
        Identifier: Identifier,
    };

    pub const BinaryOp = struct {
        position: FilePosition,
        lhs: *Expression,
        rhs: *Expression,
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
        subexpression: *Expression,
        tag: Tag,

        pub const Tag = enum {
            Pos,
            Neg,
            Not,
        };
    };

    pub const If = struct {
        condition: *Expression,
        true_branch: *Expression,
        false_branch: *Expression,
    };

    pub const Call = struct {
        subexpression: *Expression,
        args: ExpressionList,
    };

    pub const Constructor = struct {
        typ: *Type,
        args: ExpressionList,
    };

    pub const Subscript = struct {
        subexpression: *Expression,
        index: *Expression,
    };

    pub const Field = struct {
        subexpression: *Expression,
        field: *Expression,
    };

    pub const Cast = struct {
        typ: *Type,
        expression: *Expression,
    };

    pub const Identifier = struct {
        name: []const u8,
        scope: *Scope,
    };

    pub const Flags = packed struct {
        is_lvalue: bool = false,
        is_const: bool = false,
        is_static: bool = false,
    };
};

pub const ExpressionListNode = union(enum) {
    Designator: struct {
        lhs: *Expression,
        rhs: *Expression,
    },
    Expression: *Expression,
};

pub const ExpressionList = std.DoublyLinkedList(*ExpressionListNode);

pub const Statement = struct {
    position: FilePosition,
    as: As,

    pub const As = union(enum) {
        Print: *Expression,
        Block: StatementList,
        If: If,
        While: While,
        Do_While: While,
        Break: void,
        Continue: void,
        Switch: Switch,
        Return: ?*Expression,
        Symbol: *Symbol,
        Assign: Assign,
        Expression: *Expression,
    };

    pub const If = struct {
        condition: *Expression,
        true_branch: *Statement,
        false_branch: ?*Statement,
    };

    pub const While = struct {
        condition: *Expression,
        body: *Statement,
    };

    pub const Switch = struct {
        condition: *Expression,
        cases: CaseList,
        default_case: ?*Statement,

        pub const Case = union(enum) {
            Case: struct {
                value: *Expression,
                subcase: *Case,
            },
            Statement: *Statement,
        };

        pub const CaseList = std.DoublyLinkedList(*Case);
    };

    pub const Assign = struct {
        lhs: *Expression,
        rhs: *Expression,
    };
};

pub const StatementList = std.DoublyLinkedList(*Statement);

pub const Symbol = struct {
    position: FilePosition,
    as: As,
    key: Key,
    typechecking: Stage,

    pub const As = union(enum) {
        Variable: Variable,
        Parameter: Parameter,
        Procedure: Procedure,
        Struct_Field: StructField,
        Union_Field: StructField,
        Enum_Field: EnumField,
        Type: *Type,
    };

    pub const Variable = struct {
        attributes: Attributes,
        typ: ?*Type,
        value: ?*Expression,
        storage: ?IR.Encoded.Operand,

        pub const Attributes = packed struct {
            is_const: bool,
            is_static: bool,
        };
    };

    pub const Parameter = struct {
        typ: *Type,
        value: ?*Expression,
        storage: ?IR.Encoded.Operand,
    };

    pub const Procedure = struct {
        typ: *Type,
        block: StatementList,
        labels: ?LabelPair,

        pub const LabelPair = struct {
            start: IR.Encoded.Label,
            end: IR.Encoded.Label,
        };
    };

    pub const StructField = struct {
        typ: *Type,
        value: ?*Expression,
        offset: u64,
    };

    pub const EnumField = struct {
        value: ?*Expression,
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

    pub inline fn init(allocator: Allocator) SymbolTable {
        return .{ .map = HashMap.init(allocator) };
    }

    pub fn deinit(table: *SymbolTable) void {
        table.map.deinit();
    }

    pub fn insert(table: *SymbolTable, key: Key) InsertResult {
        return table.map.getOrPut(key) catch {
            nostd.exit(1);
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
