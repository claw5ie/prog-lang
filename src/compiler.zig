lexer: Lexer,
parser: Parser,
ast: Ast,
typechecker: Typechecker,
ircgen: IRCGenerator,
irc: IRC,
interp: Interpreter,

symbol_table: SymbolTable,
string_pool: StringPool,
filepath: [:0]const u8,
source_code: [:0]u8,
had_error: bool,

const Compiler = @This();

const std = @import("std");
const utils = @import("utils.zig");

const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const Alignment = utils.Alignment;
pub const oprint = utils.oprint;
pub const eprint = utils.eprint;
pub const exit = utils.exit;

const magic_number_string: []const u8 = "PROGLANG";
const magic_number_value: u64 = magic_number_value: {
    const ptr: *const u64 = @alignCast(@ptrCast(magic_number_string.ptr));
    break :magic_number_value ptr.*;
};

pub var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
pub var gpa = general_purpose_allocator.allocator();

const Options = struct {
    has_filepath: ?[:0]const u8 = null,
    mode: Mode = .Build,

    const Mode = enum {
        Build,
        Run,
        Print,
    };
};

pub var global_scope: Ast.Scope = .{
    .parent = null,
};

pub const LineInfo = struct {
    line: u32,
    column: u32,
    offset: usize,
};

pub const Attributes = packed struct {
    is_static: bool = false,
    is_const: bool = false,

    pub fn is_empty(attr: Attributes) bool {
        return attr.is_static == false and attr.is_const == false;
    }

    pub fn combine(self: Attributes, other: Attributes) Attributes {
        return .{
            .is_static = self.is_static or other.is_static,
            .is_const = self.is_const or other.is_const,
        };
    }
};

pub const Lexer = struct {
    buffer: [LOOKAHEAD]Token = undefined,
    token_start: u8 = 0,
    token_count: u8 = 0,
    line_info: LineInfo = .{
        .line = 1,
        .column = 1,
        .offset = 0,
    },

    pub const LOOKAHEAD = 2;

    pub const Token = struct {
        line_info: LineInfo,
        as: As,

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

            Ref,
            Deref,
            Not,
            Open_Paren,
            Close_Paren,
            Open_Curly,
            Close_Curly,
            Open_Bracket,
            Close_Bracket,
            Colon_Equal,
            Colon,
            Semicolon,
            Dot,
            Comma,
            Equal,

            Byte_Size_Of,
            Alignment_Of,
            As,
            Cast,
            Boolean,
            Integer,
            Identifier,
            Null,

            Struct,
            Union,
            Enum,
            Proc,
            Integer_Type,
            Bool_Type,
            Void_Type,
            Type_Of,

            Attribute,
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
            Alias,
            Case,

            End_Of_File,
        };

        pub const As = union(Tag) {
            Or: void,
            And: void,
            Eq: void,
            Neq: void,
            Lt: void,
            Leq: void,
            Gt: void,
            Geq: void,
            Add: void,
            Sub: void,
            Mul: void,
            Div: void,
            Mod: void,

            Ref: void,
            Deref: void,
            Not: void,
            Open_Paren: void,
            Close_Paren: void,
            Open_Curly: void,
            Close_Curly: void,
            Open_Bracket: void,
            Close_Bracket: void,
            Colon_Equal: void,
            Colon: void,
            Semicolon: void,
            Dot: void,
            Comma: void,
            Equal: void,

            Byte_Size_Of: void,
            Alignment_Of: void,
            As: void,
            Cast: void,
            Boolean: bool,
            Integer: u64,
            Identifier: []const u8,
            Null: void,

            Struct: void,
            Union: void,
            Enum: void,
            Proc: void,
            Integer_Type: Token.IntegerType,
            Bool_Type: void,
            Void_Type: void,
            Type_Of: void,

            Attribute: Attributes,
            Print: void,
            If: void,
            Then: void,
            Else: void,
            While: void,
            Do: void,
            Break: void,
            Continue: void,
            Switch: void,
            Return: void,
            Alias: void,
            Case: void,

            End_Of_File: void,
        };

        pub const IntegerType = struct {
            bits: u8,
            is_signed: bool,
        };
    };
};

pub const Parser = struct {
    current_scope: *Ast.Scope,
};

pub const Ast = struct {
    globals: SymbolList,
    main: ?*Symbol,
    arena: ArenaAllocator,

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

    pub const Scope = struct {
        parent: ?*Scope,
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
            start_label: ?IRC.Label,
            end_label: ?IRC.Label,
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

    pub fn create(ast: *Ast, comptime T: type) *T {
        return ast.arena.allocator().create(T) catch {
            exit(1);
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
            const data = gpa.create(Ast.Type.SharedData) catch {
                exit(1);
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
            const typ = gpa.create(Ast.Type) catch {
                exit(1);
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
};

pub const Typechecker = struct {
    enum_type: ?*Ast.Type,
    return_type: ?*Ast.Type,
    is_in_loop: bool,
};

pub const IRC = struct {
    instrs: InstrList,
    label_count: u64,
    globals_count: u64,

    // Size of return pointer + IP + rbp
    pub const FIRST_PARAM_OFFSET = 8 + 8 + 8;

    pub const Instr = union(enum) {
        Print: Instr.Print,
        Binary_Op: Instr.BinaryOp,
        Unary_Op: Instr.UnaryOp,
        Jmp: Label,
        Jmpc: Instr.Jmpc,
        Setnz: Instr.Setnz,
        Mov: Instr.Mov,
        Movsx: Instr.Movsx,
        Call: Instr.Call,
        Push: Rvalue,
        Pop: u64,
        Ret0: Label,
        Ret1: Instr.Ret1,
        GFB: Instr.GF,
        GFE: Instr.GF,
        Label: Label,
        Exit: void,

        pub const Print = union(enum) {
            Pointer: Rvalue,
            Integer: struct {
                src: Rvalue,
                is_signed: bool,
            },
            Boolean: Rvalue,
        };

        pub const BinaryOp = struct {
            dst: Lvalue,
            src0: Rvalue,
            src1: Rvalue,
            tag: Tag,
            is_signed: bool,

            pub const Tag = enum {
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
            dst: Lvalue,
            src: Rvalue,
            tag: Tag,
            is_signed: bool,

            pub const Tag = enum {
                Neg,
                Not,
            };
        };

        pub const Jmpc = struct {
            src0: Rvalue,
            src1: Rvalue,
            label: Label,
            tag: Tag,
            is_signed: bool,

            pub const Tag = enum {
                Eq,
                Neq,
                Lt,
                Leq,
                Gt,
                Geq,
            };
        };

        pub const Setnz = struct {
            dst: Lvalue,
            src: Rvalue,
        };

        pub const Mov = struct {
            dst: Lvalue,
            src: Rvalue,
        };

        pub const Movsx = struct {
            dst: Lvalue,
            src: Rvalue,
            src_bits: u8,
        };

        pub const Call = struct {
            dst: Lvalue,
            src: Rvalue,
        };

        pub const Ret1 = struct {
            dst: Lvalue,
            src: Rvalue,
            label: Label,
        };

        pub const GF = struct {
            label: Label,
            stack_space_used: u64,
        };
    };

    pub const Label = u64;

    pub const Offset = union(enum) {
        Local: i64,
        Global: u64,
    };

    pub const Tmp = struct {
        offset: Offset,
        size: u64,

        pub fn format(tmp: Tmp, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            switch (tmp.offset) {
                .Local => |offset| {
                    try writer.print("%({}B,{})", .{ tmp.size, offset });
                },
                .Global => |offset| {
                    try writer.print("$({}B,{})", .{ tmp.size, offset });
                },
            }
        }

        pub fn as_lvalue(tmp: Tmp) Lvalue {
            return .{ .Tmp = tmp };
        }

        pub fn as_rvalue(tmp: Tmp) Rvalue {
            return .{ .Lvalue = .{ .Tmp = tmp } };
        }
    };

    pub const Mem = struct {
        base: ?Tmp,
        offset: u64,
        size: u64,
    };

    pub const Lvalue = union(enum) {
        Tmp: Tmp,
        Mem: Mem,

        pub fn format(lvalue: Lvalue, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            switch (lvalue) {
                .Tmp => |tmp| {
                    try writer.print("{}", .{tmp});
                },
                .Mem => |mem| {
                    try writer.print("{}B [", .{mem.size});
                    if (mem.base) |base| {
                        try writer.print("{}", .{base});
                    }
                    if (mem.offset != 0) {
                        try writer.print("+{}", .{mem.offset});
                    }
                    try writer.print("]", .{});
                },
            }
        }

        pub fn as_rvalue(lvalue: Lvalue) Rvalue {
            return .{ .Lvalue = lvalue };
        }

        pub fn grab_size(lvalue: Lvalue) u64 {
            return switch (lvalue) {
                .Tmp => |tmp| tmp.size,
                .Mem => |mem| mem.size,
            };
        }
    };

    pub const Rvalue = union(enum) {
        Lvalue: Lvalue,
        Addr: Lvalue,
        Label: Label,
        Imm: u64,

        pub fn format(rvalue: Rvalue, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            switch (rvalue) {
                .Lvalue => |lvalue| {
                    try writer.print("{}", .{lvalue});
                },
                .Addr => |lvalue| {
                    try writer.print("addr {}", .{lvalue});
                },
                .Label => |label| {
                    try writer.print("l{}", .{label});
                },
                .Imm => |imm| {
                    try writer.print("{}", .{imm});
                },
            }
        }

        pub fn grab_size(rvalue: Rvalue) u64 {
            return switch (rvalue) {
                .Lvalue => |lvalue| lvalue.grab_size(),
                .Addr, .Label, .Imm => 8,
            };
        }
    };

    pub const InstrList = std.ArrayList(Instr);

    pub fn print(irc: *IRC) void {
        const IDENT = "    ";

        Compiler.oprint("label count:   {}\nglobals count: {}\n\n", .{ irc.label_count, irc.globals_count });

        for (irc.instrs.items, 0..) |instr, ip| {
            Compiler.oprint("{:>4}: ", .{ip});

            switch (instr) {
                .Print => |Print| {
                    switch (Print) {
                        .Pointer => |src| {
                            Compiler.oprint(IDENT ++ "printp   {}", .{src});
                        },
                        .Integer => |Integer| {
                            Compiler.oprint(IDENT ++ "print{c}   {}", .{ @as(u8, if (Integer.is_signed) 'i' else 'u'), Integer.src });
                        },
                        .Boolean => |src| {
                            Compiler.oprint(IDENT ++ "printb   {}", .{src});
                        },
                    }
                },
                .Binary_Op => |Binary_Op| {
                    const op: []const u8 = switch (Binary_Op.tag) {
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
                    Compiler.oprint(IDENT ++ "{c}{s}     {}, {}, {}", .{ @as(u8, if (Binary_Op.is_signed) 'i' else 'u'), op, Binary_Op.dst, Binary_Op.src0, Binary_Op.src1 });
                },
                .Unary_Op => |Unary_Op| {
                    const op: []const u8 = switch (Unary_Op.tag) {
                        .Neg => "neg",
                        .Not => "not",
                    };
                    Compiler.oprint(IDENT ++ "{c}{s}     {}, {}", .{ @as(u8, if (Unary_Op.is_signed) 'i' else 'u'), op, Unary_Op.dst, Unary_Op.src });
                },
                .Jmp => |label| {
                    Compiler.oprint(IDENT ++ "jmp      l{}", .{label});
                },
                .Jmpc => |Jmpc| {
                    const op = switch (Jmpc.tag) {
                        .Eq => "je ",
                        .Neq => "jne",
                        .Lt => "jl ",
                        .Leq => "jle",
                        .Gt => "jg ",
                        .Geq => "jge",
                    };
                    Compiler.oprint(IDENT ++ "{c}{s}     {}, {}, l{}", .{ @as(u8, if (Jmpc.is_signed) 'i' else 'u'), op, Jmpc.src0, Jmpc.src1, Jmpc.label });
                },
                .Setnz => |Setnz| {
                    Compiler.oprint(IDENT ++ "setnz    {}, {}", .{ Setnz.dst, Setnz.src });
                },
                .Mov => |Mov| {
                    Compiler.oprint(IDENT ++ "mov      {}, {}", .{ Mov.dst, Mov.src });
                },
                .Movsx => |Mov| {
                    Compiler.oprint(IDENT ++ "movsx    {}, {}, {}", .{ Mov.dst, Mov.src, Mov.src_bits });
                },
                .Call => |Call| {
                    Compiler.oprint(IDENT ++ "call     {}, {}", .{ Call.dst, Call.src });
                },
                .Push => |src| {
                    Compiler.oprint(IDENT ++ "push     {}", .{src});
                },
                .Pop => |bytes| {
                    Compiler.oprint(IDENT ++ "pop      {}", .{bytes});
                },
                .Ret0 => |label| {
                    Compiler.oprint(IDENT ++ "ret      l{}", .{label});
                },
                .Ret1 => |Ret1| {
                    Compiler.oprint(IDENT ++ "ret      {}, {}, l{}", .{ Ret1.dst, Ret1.src, Ret1.label });
                },
                .GFB => |GFB| {
                    Compiler.oprint("GFB {}B, l{}:", .{ GFB.stack_space_used, GFB.label });
                },
                .GFE => |GFE| {
                    Compiler.oprint("GFE {}B, l{}:", .{ GFE.stack_space_used, GFE.label });
                },
                .Label => |label| {
                    Compiler.oprint("l{}:", .{label});
                },
                .Exit => {
                    Compiler.oprint(IDENT ++ "exit", .{});
                },
            }

            Compiler.oprint("\n", .{});
        }
    }
};

pub const IRCGenerator = struct {
    next_local: i64,
    biggest_next_local: i64,
    next_global: u64,
    next_label: IRC.Label,
    loop_condition_label: ?IRC.Label,
    loop_end_label: ?IRC.Label,
    return_label: ?IRC.Label,
};

pub const Interpreter = struct {
    stack: []u8,
    labels: LabelList,
    rsp: u64,
    rbp: u64,

    pub const LabelList = std.ArrayList(u64);
};

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
            exit(1);
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

pub const StringPool = struct {
    map: HashMap,

    pub fn init(allocator: Allocator) StringPool {
        return .{ .map = HashMap.init(allocator) };
    }

    pub fn deinit(pool: *StringPool) void {
        var it = pool.map.valueIterator();
        while (it.next()) |value_ptr| {
            pool.map.allocator.free(value_ptr.*);
        }
        pool.map.deinit();
    }

    pub fn insert(pool: *StringPool, string: []const u8) []const u8 {
        const insert_result = pool.map.getOrPut(string) catch {
            exit(1);
        };

        if (!insert_result.found_existing) {
            const value = pool.map.allocator.alloc(u8, string.len) catch {
                exit(1);
            };
            @memcpy(value, string);
            insert_result.key_ptr.* = value;
            insert_result.value_ptr.* = value;
        }

        return insert_result.value_ptr.*;
    }

    pub const Key = []const u8;
    pub const Value = []u8;

    pub const Context = struct {
        pub fn hash(_: Context, key: Key) u64 {
            const MurMur = std.hash.Murmur2_64;
            return MurMur.hash(key);
        }

        pub fn eql(_: Context, k0: Key, k1: Key) bool {
            return std.mem.eql(u8, k0, k1);
        }
    };

    pub const HashMap = std.HashMap(Key, Value, Context, 80);
};

pub const Stage = enum {
    None,
    Going,
    Done,
};

fn init() Compiler {
    const state = struct {
        pub var void_pointer_type_data = Ast.Type.SharedData{
            .as = .{ .Pointer = &void_type },
            .byte_size = Ast.pointer_byte_size,
            .alignment = Ast.pointer_alignment,
            .stages = Ast.default_stages_done,
        };
        pub var bool_type_data = Ast.Type.SharedData{
            .as = .Bool,
            .byte_size = 1,
            .alignment = .BYTE,
            .stages = Ast.default_stages_done,
        };
        pub var void_type_data = Ast.Type.SharedData{
            .as = .Void,
            .byte_size = 0,
            .alignment = .BYTE,
            .stages = Ast.default_stages_done,
        };

        pub var void_pointer_type = Ast.Type{
            .line_info = .{ .line = 1, .column = 1, .offset = 0 },
            .data = &void_pointer_type_data,
            .symbol = null,
        };
        pub var bool_type = Ast.Type{
            .line_info = .{ .line = 1, .column = 1, .offset = 0 },
            .data = &bool_type_data,
            .symbol = null,
        };
        pub var void_type = Ast.Type{
            .line_info = .{ .line = 1, .column = 1, .offset = 0 },
            .data = &void_type_data,
            .symbol = null,
        };

        pub var empty_filepath = [0:0]u8{};
    };

    const empty_source_code = empty_source_code: {
        const data = gpa.alloc(u8, 1) catch {
            exit(1);
        };
        data[0] = 0;
        break :empty_source_code data[0..0 :0];
    };
    const stack = gpa.alloc(u8, 2 * 1024 * 1024) catch {
        exit(1);
    };

    Ast.void_pointer_type = &state.void_pointer_type;
    Ast.bool_type = &state.bool_type;
    Ast.void_type = &state.void_type;

    return .{
        .lexer = .{},
        .parser = .{
            .current_scope = &global_scope,
        },
        .ast = .{
            .globals = .{},
            .main = null,
            .arena = ArenaAllocator.init(std.heap.page_allocator),
        },
        .typechecker = .{
            .enum_type = null,
            .return_type = null,
            .is_in_loop = false,
        },
        .irc = .{
            .instrs = IRC.InstrList.init(gpa),
            .label_count = 0,
            .globals_count = 0,
        },
        .ircgen = .{
            .next_local = 0,
            .biggest_next_local = 0,
            .next_global = 0,
            .next_label = 0,
            .loop_condition_label = null,
            .loop_end_label = null,
            .return_label = null,
        },
        .interp = .{
            .stack = stack,
            .labels = Interpreter.LabelList.init(gpa),
            .rsp = 0,
            .rbp = 0,
        },
        .symbol_table = SymbolTable.init(gpa),
        .string_pool = StringPool.init(gpa),
        .filepath = &state.empty_filepath,
        .source_code = empty_source_code,
        .had_error = false,
    };
}

fn deinit(c: *Compiler) void {
    c.ast.arena.deinit();
    for (Ast.integer_types) |has_type| {
        if (has_type) |typ| {
            gpa.destroy(typ.data);
            gpa.destroy(typ);
        }
    }
    c.irc.instrs.deinit();
    gpa.free(c.interp.stack);
    c.interp.labels.deinit();
    c.symbol_table.deinit();
    c.string_pool.deinit();
    gpa.free(c.source_code);
}

pub fn compile() void {
    const parse = @import("parser.zig").parse;
    const typecheck = @import("typechecker.zig").typecheck;
    const generate_irc = @import("irc-generator.zig").generate_irc;
    const interpret = @import("interpreter.zig").interpret;

    var c = Compiler.init();

    const options = parse_cmd_options();
    switch (options.mode) {
        .Build => {
            parse(&c, options.has_filepath.?);
            typecheck(&c);
            generate_irc(&c);
            write_irc(&c, options.has_filepath.?);
        },
        .Run => {
            c.irc = read_irc(options.has_filepath.?);
            interpret(&c);
        },
        .Print => {
            c.irc = read_irc(options.has_filepath.?);
            c.irc.print();
        },
    }

    c.deinit();
}

fn write_irc(c: *Compiler, filepath: [:0]const u8) void {
    var filepath_buffer: [1024]u8 = undefined;

    // TODO: Abolish magic numbers!
    std.debug.assert(filepath.len < filepath_buffer.len);
    @memcpy(filepath_buffer[0..filepath.len], filepath);
    @memcpy(filepath_buffer[filepath.len .. filepath.len + 5], ".irc\x00");

    const fd = std.posix.open(
        filepath_buffer[0 .. filepath.len + 4 :0],
        .{
            .ACCMODE = .WRONLY,
            .CREAT = true,
            .TRUNC = true,
        },
        0o644,
    ) catch {
        eprint("error: couldn't open a file '{s}'\n", .{filepath});
        exit(1);
    };

    utils.write_to_file_v(fd, magic_number_string);
    utils.write_to_file_u64(fd, c.irc.label_count);
    utils.write_to_file_u64(fd, c.irc.globals_count);

    var instrs: []const u8 = undefined;
    instrs.ptr = @ptrCast(c.irc.instrs.items.ptr);
    instrs.len = c.irc.instrs.items.len * @sizeOf(IRC.Instr);
    utils.write_to_file_v(fd, instrs);
}

fn read_irc(filepath: [:0]const u8) IRC {
    const fd = std.posix.open(filepath, .{}, 0) catch {
        eprint("error: couldn't open a file '{s}'\n", .{filepath});
        exit(1);
    };
    const size: usize = size: {
        const stats = std.posix.fstat(fd) catch {
            exit(1);
        };
        break :size @intCast(stats.size);
    };

    const magic_number = utils.read_from_file_u64(fd);
    std.debug.assert(magic_number == magic_number_value);
    const label_count = utils.read_from_file_u64(fd);
    const globals_count = utils.read_from_file_u64(fd);
    const instrs = gpa.alloc(u8, size - 8 * 3) catch {
        exit(1);
    };
    utils.read_from_file_v(fd, instrs);

    var items: []IRC.Instr = undefined;
    items.ptr = @alignCast(@ptrCast(instrs.ptr));
    items.len = @divExact(instrs.len, @sizeOf(IRC.Instr));

    return .{
        .instrs = .{
            .items = items,
            .capacity = items.len,
            .allocator = gpa,
        },
        .label_count = label_count,
        .globals_count = globals_count,
    };
}

fn parse_cmd_options() Options {
    var options = Options{};
    var activated_modes_count: u32 = 0;
    var had_error = false;
    var args = std.process.args();

    _ = args.next().?;
    while (args.next()) |arg| {
        if (arg[0] == '-') {
            if (arg.len > 2) {
                had_error = true;
                eprint("error: unrecognized option '{s}'\n", .{arg});
            } else {
                switch (arg[1]) {
                    'r' => {
                        activated_modes_count += 1;
                        options.mode = .Run;
                    },
                    'p' => {
                        activated_modes_count += 1;
                        options.mode = .Print;
                    },
                    else => {
                        had_error = true;
                        eprint("error: unrecognized option '{s}'\n", .{arg});
                    },
                }
            }
        } else {
            if (options.has_filepath) |filepath| {
                had_error = true;
                eprint("error: filepath was already given ('{s}')\n", .{filepath});
            } else {
                options.has_filepath = arg;
            }
        }
    }

    if (activated_modes_count > 1) {
        had_error = true;
        eprint("error: only one options needs to be enabled\n", .{});
    }

    if (options.has_filepath == null) {
        had_error = true;
        eprint("error: no file supplied\n", .{});
    }

    if (had_error) {
        exit(1);
    }

    return options;
}

pub fn find_symbol_in_scope(c: *Compiler, key: Ast.Symbol.Key, offset: usize) ?*Ast.Symbol {
    const has_symbol = c.symbol_table.find(key);

    if (has_symbol) |symbol| {
        switch (symbol.as) {
            .Variable, .Parameter => {
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

pub fn find_symbol(c: *Compiler, key: Ast.Symbol.Key, offset: usize) ?*Ast.Symbol {
    var k = key;
    while (true) {
        const has_symbol = find_symbol_in_scope(c, k, offset);
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

pub fn report_error(c: *Compiler, line_info: LineInfo, comptime format: []const u8, args: anytype) void {
    c.had_error = true;
    eprint("{s}:{}:{}: error: " ++ format ++ "\n", .{ c.filepath, line_info.line, line_info.column } ++ args);
}

pub fn report_note(c: *Compiler, line_info: LineInfo, comptime format: []const u8, args: anytype) void {
    eprint("{s}:{}:{}: note: " ++ format ++ "\n", .{ c.filepath, line_info.line, line_info.column } ++ args);
}
