symbols: SymbolList,
symbol_table: SymbolTable,
arena: ArenaAllocator,

const Ast = @This();

pub fn deinit(ast: *Ast) void {
    ast.symbol_table.deinit();
    ast.arena.deinit();
}

pub const global_scope: Scope = .{
    .parent = null,
    .tag = .Global,
};

pub fn create(ast: *Ast, comptime T: type) *T {
    return ast.arena.allocator().create(T) catch {
        exit(1);
    };
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
            .Alias,
            .Structure,
            .Union,
            .Enumerator,
            .Procedure,
            .Structure_Field,
            .Union_Field,
            .Enumerator_Value,
            .Parameter,
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

pub const Allocator = std.mem.Allocator;

pub const ArenaAllocator = std.heap.ArenaAllocator;
pub const FilePosition = Compiler.FilePosition;

pub const Alignment = nostd.Alignment;

pub const Scope = struct {
    parent: ?*const Scope,
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

pub const Expression = struct {
    position: FilePosition,
    as: As,

    pub const As = union(enum) {
        Structure: Structure,
        Union: Union,
        Enumerator: Enumerator,
        Procedure_Type: ProcedureType,
        Subscript: Subscript,
        Dereference: *Expression,
        Integer_Type: IntegerType,
        Boolean_Type: void,
        Void_Type: void,
        Member_Access: MemberAccess,
        Identifier: Identifier,
        Type_Of: *Expression,

        Procedure: Procedure,
        Procedure_Call: ProcedureCall,

        Reference: *Expression,
        Unary_Operator: UnaryOperator,
        Binary_Operator: BinaryOperator,

        Size_Of: *Expression,
        Alignment_Of: *Expression,
        As: Cast,
        Cast: Cast,

        Integer_Literal: u64,
        Boolean_Literal: bool,
        Null_Literal: void,
    };

    pub const Structure = struct {
        fields: *SymbolList,
        rest: *SymbolList,
        scope: *const Ast.Scope,
    };

    pub const Union = Structure;

    pub const Enumerator = Structure;

    pub const ProcedureType = struct {
        parameters: SymbolList,
        return_type: *Expression,
        scope: *const Scope,
    };

    pub const Subscript = struct {
        lhs: *Expression,
        index: *Expression,
    };

    pub const IntegerType = Lexer.Token.IntegerType;

    pub const MemberAccess = struct {
        lhs: *Expression,
        name: *Lexer.Token,
    };

    pub const Identifier = Symbol.Key;

    pub const Procedure = struct {
        typ: *Expression,
        block: StatementList,
    };

    pub const ProcedureCall = struct {
        lhs: *Expression,
        arguments: ArgumentList,

        pub const Argument = union(enum) {
            Field_Designator: struct {
                name: *Lexer.Token,
                value: *Expression,
            },
            Expression: *Expression,
        };

        pub const ArgumentList = std.DoublyLinkedList(Argument);
    };

    pub const UnaryOperator = struct {
        subexpression: *Expression,
        tag: Tag,

        pub const Tag = enum {
            Plus,
            Minus,
            Not,
        };
    };

    pub const BinaryOperator = struct {
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

    pub const Cast = struct {
        typ: *Expression,
        expression: *Expression,
    };
};

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
    key: Key,
    as: As,
    type_checking_context: ?*anyopaque = null,

    pub const As = union(enum) {
        Alias: *Expression,

        Structure: Expression.Structure,
        Union: Expression.Union,
        Enumerator: Expression.Enumerator,
        Procedure: Expression.Procedure,

        Variable: Variable,

        Structure_Field: StructureField,
        Union_Field: UnionField,
        Enumerator_Value: EnumeratorValue,

        Parameter: Parameter,
    };

    pub const Variable = struct {
        attributes: Attributes,
        typ: ?*Expression,
        value: ?*Expression,

        pub const Attributes = packed struct {
            is_const: bool,
            is_static: bool,
        };
    };

    pub const StructureField = struct {
        typ: *Expression,
        value: ?*Expression,
    };

    pub const UnionField = StructureField;

    pub const EnumeratorValue = struct {
        value: ?*Expression,
    };

    pub const Parameter = struct {
        typ: *Expression,
    };

    pub const Key = struct {
        name: []const u8,
        scope: *const Scope,
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

const std = @import("std");
const nostd = @import("nostd.zig");
const Compiler = @import("Compiler.zig");
const Lexer = @import("Lexer.zig");
