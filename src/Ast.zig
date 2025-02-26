globals: SymbolList,
local_procedures: SymbolList,
namespaces: TypeList,
main: ?*Symbol,
symbol_table: SymbolTable,
arena: ArenaAllocator,
c: *Compiler,

const Ast = @This();

pub var global_scope: Scope = .{
    .parent = null,
};

pub var void_pointer_type: *Type = undefined;
pub var integer_types = [_]?*Type{null} ** 130;
pub var bool_type: *Type = undefined;
pub var void_type: *Type = undefined;

pub const pointer_byte_size = 8;
pub const pointer_alignment: Alignment = .Qword;

pub const default_stages_none = Type.Stages{
    .unpacking = .None,
    .shallow_check = .None,
    .void_array_check = .None,
    .full_check = .None,
};

pub const default_stages_done = Type.Stages{
    .unpacking = .Done,
    .shallow_check = .Done,
    .void_array_check = .Done,
    .full_check = .Done,
};

pub fn init(c: *Compiler) Ast {
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
        .c = c,
    };
}

pub fn deinit(ast: *Ast) void {
    ast.arena.deinit();
    for (integer_types) |has_type| {
        if (has_type) |typ| {
            nostd.general_allocator.destroy(typ.data);
            nostd.general_allocator.destroy(typ);
        }
    }
    ast.symbol_table.deinit();
}

pub fn create(ast: *Ast, comptime T: type) *T {
    return ast.arena.allocator().create(T) catch {
        exit(1);
    };
}

pub fn integer_type_from_u64(value: u64) *Type {
    const bits = nostd.highest_bit_count(value);
    return lookup_integer_type(bits, false);
}

pub fn lookup_integer_type(bits: u8, is_signed: bool) *Type {
    std.debug.assert(bits <= 64);

    const index = 65 * @as(u8, @intFromBool(is_signed)) + bits;
    if (integer_types[index] == null) {
        const byte_size = @max(1, nostd.round_to_next_pow2(bits) / 8);
        const data = nostd.general_allocator.create(Type.SharedData) catch {
            exit(1);
        };
        data.* = .{
            .as = .{ .Integer = .{
                .bits = bits,
                .is_signed = is_signed,
            } },
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
        const typ = nostd.general_allocator.create(Type) catch {
            exit(1);
        };
        typ.* = .{
            .position = .{ .line = 1, .column = 1, .offset = 0 },
            .data = data,
            .symbol = null,
        };

        integer_types[index] = typ;
    }

    return integer_types[index].?;
}

pub fn find_symbol_in_scope(ast: *Ast, key: Symbol.Key, skip_local_variables: bool, offset: usize) ?*Ast.Symbol {
    const has_symbol = ast.symbol_table.find(key);

    if (has_symbol) |symbol| {
        switch (symbol.as) {
            .Variable, .Parameter => {
                if (symbol.attributes.is_const or symbol.attributes.is_static or (symbol.position.offset < offset and !skip_local_variables)) {
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

pub fn find_symbol(ast: *Ast, key: Ast.Symbol.Key, offset: usize) ?*Ast.Symbol {
    return find_symbol_with_scope_bound(ast, key, &Ast.global_scope, offset);
}

pub fn find_symbol_with_scope_bound(ast: *Ast, key: Ast.Symbol.Key, scope_bound: *Ast.Scope, offset: usize) ?*Ast.Symbol {
    var skip_local_variables = false;
    var k = key;
    while (true) {
        const has_symbol = find_symbol_in_scope(ast, k, skip_local_variables, offset);
        if (has_symbol) |symbol| {
            return symbol;
        } else if (k.scope.parent) |parent| {
            skip_local_variables = skip_local_variables or k.scope == scope_bound;
            k.scope = parent;
        } else {
            break;
        }
    }
    return null;
}

// Assume 'expr' is heap allocated and can be reused.
pub fn expr_to_type(ast: *Ast, expr: *Expr) *Type {
    switch (expr.as) {
        .Deref => |subexpr| {
            const subtype = expr_to_type(ast, subexpr);

            const data = ast.create(Type.SharedData);
            data.* = .{
                .as = .{ .Pointer = subtype },
                .byte_size = pointer_byte_size,
                .alignment = pointer_alignment,
                .stages = default_stages_none,
            };
            expr.as = .{ .Type = .{
                .position = expr.position,
                .data = data,
                .symbol = null,
            } };

            return &expr.as.Type;
        },
        .Field => |Field| {
            const subtype = expr_to_type(ast, Field.subexpr);

            const data = ast.create(Type.SharedData);
            data.* = .{
                .as = .{ .Field = .{
                    .subtype = subtype,
                    .field = Field.field,
                } },
                .byte_size = 0,
                .alignment = .Byte,
                .stages = default_stages_none,
            };
            expr.as = .{ .Type = .{
                .position = expr.position,
                .data = data,
                .symbol = null,
            } };

            return &expr.as.Type;
        },
        .Subscript => |Subscript| {
            const subtype = expr_to_type(ast, Subscript.subexpr);

            const data = ast.create(Type.SharedData);
            data.* = .{
                .as = .{ .Array = .{
                    .subtype = subtype,
                    .size = Subscript.index,
                    .computed_size = 0,
                } },
                .byte_size = 0,
                .alignment = .Byte,
                .stages = default_stages_none,
            };
            expr.as = .{ .Type = .{
                .position = expr.position,
                .data = data,
                .symbol = null,
            } };

            return &expr.as.Type;
        },
        .Type => |*typ| {
            return typ;
        },
        .Identifier => |Identifier| {
            const data = ast.create(Type.SharedData);
            data.* = .{
                .as = .{ .Identifier = .{
                    .name = Identifier.name,
                    .scope = Identifier.scope,
                } },
                .byte_size = 0,
                .alignment = .Byte,
                .stages = default_stages_none,
            };
            expr.as = .{ .Type = .{
                .position = expr.position,
                .data = data,
                .symbol = null,
            } };

            return &expr.as.Type;
        },
        else => {
            ast.c.report_error(expr.position, "expected expression, not a type", .{});
            exit(1);
        },
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
            .Field, .Identifier, .Type_Of => unreachable,
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
        Struct: Type.Struct,
        Union: Type.Struct,
        Enum: Type.Enum,
        Proc: Type.Proc,
        Array: Type.Array,
        Field: Type.Field,
        Pointer: *Type,
        Integer: Type.IntegerType,
        Bool: void,
        Void: void,
        Identifier: Type.Identifier,
        Type_Of: *Ast.Expr,
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
        size: *Expr,
        computed_size: u64,
    };

    pub const Field = struct {
        subtype: *Type,
        field: *Expr,
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

pub const TypeList = std.DoublyLinkedList(*Type);

pub const Expr = struct {
    position: FilePosition,
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
        position: FilePosition,
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
        is_static: bool = false,
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
    position: FilePosition,
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
    position: FilePosition,
    as: As,
    key: Key,
    typechecking: Stage,
    attributes: Attributes,

    pub const Tag = enum {
        Variable,
        Parameter,
        Procedure,
        Struct_Field,
        Union_Field,
        Enum_Field,
        Type,
    };

    pub const As = union(Tag) {
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
        storage: ?IR.Encoded.Operand,
    };

    pub const Parameter = struct {
        typ: *Type,
        value: ?*Expr,
        storage: ?IR.Encoded.Operand,
    };

    pub const Procedure = struct {
        typ: *Type,
        block: StmtList,
        labels: ?LabelPair,

        pub const LabelPair = struct {
            start: IR.Encoded.Label,
            end: IR.Encoded.Label,
        };
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

    pub const Attributes = @import("Lexer.zig").Token.Attributes;
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

    pub const Key = Ast.Symbol.Key;
    pub const Value = *Ast.Symbol;

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
