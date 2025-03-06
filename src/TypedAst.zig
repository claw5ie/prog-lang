symbols: SymbolList,
arena: ArenaAllocator,

const TypedAst = @This();

pub const void_pointer_type: Type = .{
    .size = 8,
    .alignment = .Qword,
    .as = .{ .Pointer = &void_type },
};

pub const boolean_type: Type = .{
    .size = 1,
    .alignment = .Byte,
    .as = .Boolean,
};

pub const void_type: Type = .{
    .size = 0,
    .alignment = .Byte,
    .as = .Void,
};

pub fn deinit(ast: *TypedAst) void {
    ast.arena.deinit();
}

pub fn create(ast: *TypedAst, comptime T: type) *T {
    return ast.arena.allocator().create(T) catch {
        exit(1);
    };
}

pub fn alloc(ast: *TypedAst, comptime T: type, count: usize) []T {
    return ast.arena.allocator().alloc(T, count) catch {
        exit(1);
    };
}

pub fn lookup_integer_type(ast: *TypedAst, integer: Type.Integer) *const Type {
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

    const size = @max(1, nostd.round_to_next_pow2(integer.bits) / 8);
    const typ = create(ast, Type);
    typ.* = .{
        .size = size,
        .alignment = switch (size) {
            1 => .Byte,
            2 => .Word,
            4 => .Dword,
            8 => .Qword,
            else => unreachable,
        },
        .as = .{ .Integer = integer },
    };
    state.integer_types[index] = typ;

    return typ;
}

pub fn lookup_integer_type_from_u64(ast: *TypedAst, value: u64) *const Type {
    const bits = nostd.highest_bit_count(value);
    return lookup_integer_type(ast, .{ .bits = bits, .is_signed = false });
}

pub const Sign = enum {
    Any,
    Unsigned,
    Signed,
};

const CastResult = union(enum) {
    Cant_Cast: void,
    Has_Necessary_Type_Already: *const Type,
    Need_Implicit_Cast: *const Type,
    Need_Explicit_Cast: *const Type,

    pub fn get_type(result: CastResult) *const Type {
        return switch (result) {
            .Cant_Cast => unreachable,
            .Has_Necessary_Type_Already,
            .Need_Implicit_Cast,
            .Need_Explicit_Cast,
            => |typ| typ,
        };
    }
};

pub fn perform_cast(ast: *TypedAst, result: CastResult, expression: *Expression) bool {
    switch (result) {
        .Cant_Cast => return false,
        .Has_Necessary_Type_Already => return true,
        .Need_Implicit_Cast => |typ| {
            expression.typ = typ;
            return true;
        },
        .Need_Explicit_Cast => |typ| {
            const subexpression = ast.create(Expression);
            subexpression.* = expression.*;
            expression.* = .{
                .flags = .{
                    .is_lvalue = false,
                    .is_const = subexpression.flags.is_const,
                    .is_static = false,
                },
                .typ = typ,
                .as = .{ .Cast = subexpression },
            };
            return true;
        },
    }
}

pub fn can_safe_cast_to_integer(ast: *TypedAst, typ: *const Type, sign: Sign) CastResult {
    const result: CastResult = switch (typ.as) {
        .Structure,
        .Union,
        .Array,
        .Void,
        => return .Cant_Cast,
        .Enumerator => |Enum| .{ .Has_Necessary_Type_Already = Enum.underlying_type },
        .Procedure,
        .Pointer,
        => .{ .Need_Implicit_Cast = ast.lookup_integer_type(.{ .bits = 64, .is_signed = false }) },
        .Integer => .{ .Has_Necessary_Type_Already = typ },
        .Boolean => .{ .Need_Implicit_Cast = ast.lookup_integer_type(.{ .bits = 1, .is_signed = false }) },
    };

    const integer_type = result.get_type();
    const Integer = &integer_type.as.Integer;

    switch (sign) {
        .Any => return result,
        .Unsigned => {
            if (Integer.is_signed) {
                return .Cant_Cast;
            } else {
                return result;
            }
        },
        .Signed => {
            if (Integer.is_signed) {
                return result;
            } else if (Integer.bits < 64) {
                const new_integer_type = ast.lookup_integer_type(.{ .bits = Integer.bits + 1, .is_signed = true });
                return .{ .Need_Explicit_Cast = new_integer_type };
            } else {
                return .Cant_Cast;
            }
        },
    }
}

pub fn safe_cast_to_integer(ast: *TypedAst, expression: *Expression, sign: Sign) bool {
    const result = can_safe_cast_to_integer(ast, expression.typ, sign);
    return perform_cast(ast, result, expression);
}

pub const DoubleCastResult = struct {
    lhs: CastResult,
    rhs: CastResult,
};

pub fn perform_double_cast(ast: *TypedAst, result: DoubleCastResult, lhs: *Expression, rhs: *Expression) bool {
    return perform_cast(ast, result.lhs, lhs) and perform_cast(ast, result.rhs, rhs);
}

pub fn can_safe_cast_to_two_compatible_integers(ast: *TypedAst, lhs: *const Type, rhs: *const Type) DoubleCastResult {
    const lhs_result = can_safe_cast_to_integer(ast, lhs, .Any);
    const rhs_result = can_safe_cast_to_integer(ast, rhs, .Any);

    if (lhs_result == .Cant_Cast or rhs_result == .Cant_Cast) {
        return .{
            .lhs = .Cant_Cast,
            .rhs = .Cant_Cast,
        };
    }

    const lhs_type = lhs_result.get_type();
    const rhs_type = rhs_result.get_type();

    const lInteger = &lhs_type.as.Integer;
    const rInteger = &rhs_type.as.Integer;

    const Case = enum(u8) {
        ULU = 0,
        UGU = 1,
        UEU = 2,

        ULI = 3,
        UGI = 4,
        UEI = 5,

        ILU = 6,
        IGU = 7,
        IEU = 8,

        ILI = 9,
        IGI = 10,
        IEI = 11,
    };

    const case: Case = case: {
        const left_sign: u8 = @intFromBool(lInteger.is_signed);
        const right_sign: u8 = @intFromBool(rInteger.is_signed);
        const order: u8 = if (lInteger.bits < rInteger.bits)
            0
        else if (lInteger.bits > rInteger.bits)
            1
        else
            2;
        break :case @enumFromInt(3 * (2 * left_sign + right_sign) + order);
    };

    switch (case) {
        .ULU,
        .ILI,
        .ULI,
        => return .{
            .lhs = .{ .Need_Explicit_Cast = rhs_type },
            .rhs = rhs_result,
        },
        .UGU,
        .IGI,
        .IGU,
        => return .{
            .lhs = lhs_result,
            .rhs = .{ .Need_Explicit_Cast = lhs_type },
        },
        .UGI,
        .UEI,
        .ILU,
        .IEU,
        => {
            const bits = @max(lInteger.bits, rInteger.bits) + 1;

            if (bits <= 64) {
                const new_type = ast.lookup_integer_type(.{ .bits = bits, .is_signed = true });

                return .{
                    .lhs = .{ .Need_Explicit_Cast = new_type },
                    .rhs = .{ .Need_Explicit_Cast = new_type },
                };
            } else {
                return .{
                    .lhs = .Cant_Cast,
                    .rhs = .Cant_Cast,
                };
            }
        },
        .UEU,
        .IEI,
        => return .{
            .lhs = lhs_result,
            .rhs = rhs_result,
        },
    }
}

pub fn safe_cast_to_two_compatible_integers(ast: *TypedAst, lhs: *Expression, rhs: *Expression) bool {
    const result = can_safe_cast_to_two_compatible_integers(ast, lhs.typ, rhs.typ);
    return perform_double_cast(ast, result, lhs, rhs);
}

pub fn can_safe_cast(ast: *TypedAst, typ: *const Type, cast_to: *const Type) CastResult {
    if (typ.equal(cast_to)) {
        return .{ .Has_Necessary_Type_Already = typ };
    }

    switch (cast_to.as) {
        .Structure, // TODO: could consider two structures/unions/enumerators with the same layout to be equal.
        .Union,
        .Enumerator, // TODO: it is possible to convert integers to enumerator under certain conditions.
        .Array, // TODO: arrays of the same type can be safely casted to array with smaller size. Also, '[1]type' can be cast to 'type'.
        .Void,
        => return .Cant_Cast,
        .Procedure => {
            if (typ.equal(&void_pointer_type)) {
                return .{ .Need_Implicit_Cast = cast_to };
            } else {
                return .Cant_Cast;
            }
        },
        .Pointer => {
            switch (typ.as) {
                .Procedure,
                .Pointer,
                => {
                    if (cast_to.equal(&void_pointer_type)) {
                        return .{ .Need_Implicit_Cast = cast_to };
                    } else if (typ.equal(&void_pointer_type)) {
                        return .{ .Need_Implicit_Cast = cast_to };
                    } else {
                        return .Cant_Cast;
                    }
                },
                else => return .Cant_Cast,
            }
        },
        .Integer => {
            const result = can_safe_cast_to_two_compatible_integers(ast, cast_to, typ);

            if (result.lhs == .Has_Necessary_Type_Already) {
                return result.rhs;
            }

            return .Cant_Cast;
        },
        .Boolean => {
            switch (typ.as) {
                .Integer => |Integer| {
                    if (!Integer.is_signed) {
                        if (Integer.bits == 1) {
                            return .{ .Need_Implicit_Cast = cast_to };
                        } else if (Integer.bits == 0) {
                            return .{ .Need_Explicit_Cast = cast_to };
                        } else {
                            return .Cant_Cast;
                        }
                    } else {
                        return .Cant_Cast;
                    }
                },
                else => return .Cant_Cast, // TODO: enumerator can be converted to boolean under certain conditions.
            }
        },
    }
}

pub fn safe_cast(ast: *TypedAst, expression: *Expression, cast_to: *const Type) bool {
    const result = can_safe_cast(ast, expression.typ, cast_to);
    return perform_cast(ast, result, expression);
}

pub fn can_ranked_safe_cast(ast: *TypedAst, lhs: *const Type, rhs: *const Type) DoubleCastResult {
    if (lhs.equal(rhs)) {
        return .{
            .lhs = .{ .Has_Necessary_Type_Already = lhs },
            .rhs = .{ .Has_Necessary_Type_Already = rhs },
        };
    }

    const lhs_rank = lhs.compare().rank;
    const rhs_rank = rhs.compare().rank;

    if (lhs_rank == Type.Flags.invalid_rank or
        rhs_rank == Type.Flags.invalid_rank)
    {
        return .{
            .lhs = .Cant_Cast,
            .rhs = .Cant_Cast,
        };
    }

    if (lhs_rank < rhs_rank) {
        var result = can_safe_cast(ast, rhs, lhs);

        if (result != .Cant_Cast) {
            return .{
                .lhs = .{ .Has_Necessary_Type_Already = lhs },
                .rhs = result,
            };
        } else {
            result = can_safe_cast(ast, lhs, rhs);

            return .{
                .lhs = result,
                .rhs = .{ .Has_Necessary_Type_Already = rhs },
            };
        }
    } else if (lhs_rank > rhs_rank) {
        var result = can_safe_cast(ast, lhs, rhs);

        if (result != .Cant_Cast) {
            return .{
                .lhs = result,
                .rhs = .{ .Has_Necessary_Type_Already = rhs },
            };
        } else {
            result = can_safe_cast(ast, rhs, lhs);

            return .{
                .lhs = .{ .Has_Necessary_Type_Already = lhs },
                .rhs = result,
            };
        }
    } else if (lhs.as == .Integer and rhs.as == .Integer) {
        return can_safe_cast_to_two_compatible_integers(ast, lhs, rhs);
    } else {
        return .{
            .lhs = .Cant_Cast,
            .rhs = .Cant_Cast,
        };
    }
}

pub fn ranked_safe_cast(ast: *TypedAst, lhs: *Expression, rhs: *Expression) bool {
    const result = can_ranked_safe_cast(ast, lhs.typ, rhs.typ);
    return perform_double_cast(ast, result, lhs, rhs);
}

pub fn can_unsafe_cast(ast: *TypedAst, typ: *const Type, cast_to: *const Type) CastResult {
    const result = can_safe_cast(ast, typ, cast_to);

    if (result != .Cant_Cast) {
        return result;
    }

    switch (cast_to.as) {
        .Enumerator,
        .Procedure,
        .Pointer,
        .Integer,
        .Boolean,
        => {
            switch (typ.as) {
                .Enumerator,
                .Procedure,
                .Pointer,
                .Integer,
                .Boolean,
                => return .{ .Need_Explicit_Cast = cast_to },
                else => return .Cant_Cast,
            }
        },
        .Structure,
        .Union,
        .Array,
        .Void,
        => return .Cant_Cast,
    }
}

pub fn unsafe_cast(ast: *TypedAst, expression: *Expression, cast_to: *const Type) bool {
    const result = can_unsafe_cast(ast, expression.typ, cast_to);
    return perform_cast(ast, result, expression);
}

const exit = nostd.exit;

pub const Type = struct {
    size: ByteSize,
    alignment: Alignment,
    as: As,

    pub fn equal(self: Type, other: *const Type) bool {
        // if (self.symbol != null and self.symbol == other.symbol) {
        //     return true;
        // }

        switch (self.as) {
            .Structure,
            .Union,
            .Enumerator,
            => return false,
            .Array => |sArray| {
                if (other.as != .Array) {
                    return false;
                }

                const oArray = &other.as.Array;

                return sArray.count == oArray.count and sArray.subtype.equal(oArray.subtype);
            },
            .Procedure => |sProcedure| {
                if (other.as != .Procedure) {
                    return false;
                }

                const oProcedure = &other.as.Procedure;

                if (sProcedure.parameters.len != oProcedure.parameters.len) {
                    return false;
                }

                {
                    const count = sProcedure.parameters.len;
                    var i: usize = 0;

                    while (i < count) : (i += 1) {
                        const stype = sProcedure.parameters[i];
                        const otype = oProcedure.parameters[i];

                        if (!stype.typ.equal(otype.typ)) {
                            return false;
                        }
                    }
                }

                const sreturn_type = sProcedure.return_type;
                const oreturn_type = oProcedure.return_type;

                return sreturn_type.equal(oreturn_type);
            },
            .Pointer => |subtype| {
                if (other.as != .Pointer) {
                    return false;
                } else {
                    return subtype.equal(other.as.Pointer);
                }
            },
            .Integer => |sInteger| {
                if (other.as != .Integer) {
                    return false;
                }

                const oInteger = &other.as.Integer;

                return sInteger.bits == oInteger.bits and sInteger.is_signed == oInteger.is_signed;
            },
            .Boolean => return other.as == .Boolean,
            .Void => return other.as == .Void,
        }
    }

    pub const Flags = packed struct {
        is_comparable: bool = false,
        is_ordered: bool = false,
        can_be_dereferenced: bool = false,
        rank: Rank = invalid_rank,

        pub const Rank = u8;

        pub const invalid_rank: Rank = 0;
    };

    pub fn compare(self: Type) Flags {
        var flags = Flags{};

        // bool:    rank 1
        // enum     rank 2
        // pointer: rank 3
        // proc:    rank 4
        // integer: rank 5
        // else:    rank 0 -> cant cast

        switch (self.as) {
            .Enumerator => {
                flags.is_comparable = true;
                flags.is_ordered = true;
                flags.rank = 2;
            },
            .Procedure => {
                flags.is_comparable = true;
                flags.rank = 4;
            },
            .Pointer => |subtype| {
                flags.is_comparable = true;
                flags.is_ordered = true;
                flags.can_be_dereferenced = true;
                flags.rank = 3;

                switch (subtype.as) {
                    .Void => {
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
            .Boolean => {
                flags.is_comparable = true;
                flags.rank = 1;
            },
            .Structure,
            .Union,
            .Array,
            .Void,
            => {},
        }

        return flags;
    }

    pub fn format(self: Type, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        // if (self.symbol) |symbol| {
        //     try writer.writeAll(symbol.key.name);
        //     return;
        // }

        switch (self.as) {
            .Structure => try writer.writeAll("<structure>"),
            .Union => try writer.writeAll("<union>"),
            .Enumerator => try writer.writeAll("<enumerator>"),
            .Array => |array| try writer.print("{}[{}]", .{ array.subtype, array.count }),
            .Procedure => |procedure| {
                try writer.print("proc(", .{});

                for (procedure.parameters, 0..) |parameter, i| {
                    try writer.print("{s}: {}", .{ parameter.name, parameter.typ });

                    if (i + 1 < procedure.parameters.len) {
                        try writer.print(", ", .{});
                    }
                }

                try writer.print(") {}", .{procedure.return_type});
            },
            .Pointer => |subtype| try writer.print("{}^", .{subtype}),
            .Integer => |integer| try writer.print("{c}{}", .{ @as(u8, if (integer.is_signed) 'i' else 'u'), integer.bits }),
            .Boolean => try writer.writeAll("bool"),
            .Void => try writer.writeAll("void"),
        }
    }

    pub const As = union(enum) {
        Structure: Structure,
        Union: Union,
        Enumerator: Enumerator,
        Array: Array,
        Procedure: Procedure,
        Pointer: *const Type,
        Integer: Integer,
        Boolean: void,
        Void: void,
    };

    pub const Structure = struct {
        fields: []Field,
        rest: SymbolList,

        pub const Field = struct {
            typ: *const Type,
            offset: ByteSize,
        };
    };

    pub const Union = struct {
        fields: []Field,
        rest: SymbolList,

        pub const Field = struct {
            typ: *const Type,
            offset: ByteSize = 0,
        };
    };

    pub const Enumerator = struct {
        underlying_type: *const Type,
        values: []Value,
        rest: SymbolList,

        pub const Value = u64;
    };

    pub const Array = struct {
        subtype: *const Type,
        count: u64,
    };

    pub const Procedure = struct {
        parameters: []Parameter,
        return_type: *const Type,

        pub const Parameter = struct {
            name: []const u8,
            typ: *const Type,
        };
    };

    pub const Integer = struct {
        bits: u8,
        is_signed: bool,
    };

    pub const ByteSize = u64;
    pub const Alignment = nostd.Alignment;
};

pub const Expression = struct {
    flags: Flags,
    typ: *const Type,
    as: As,

    pub const As = union(enum) {
        Subscript: Subscript,
        Procedure: Symbol.Procedure,
        Procedure_Call: ProcedureCall,

        If: If,

        Reference: *Expression,
        Dereference: *Expression,
        Unary_Operator: UnaryOperator,
        Binary_Operator: BinaryOperator,

        Cast: *Expression,

        Integer_Literal: u64,
        Null_Literal: void,
        Symbol: *Symbol,
    };

    pub const Subscript = struct {
        lhs: *Expression,
        index: *Expression,
    };
    pub const ProcedureCall = struct {
        lhs: *Expression,
        arguments: []*Expression,
    };

    pub const If = struct {
        condition: *Expression,
        true_branch: *Expression,
        false_branch: *Expression,
    };

    pub const UnaryOperator = struct {
        subexpression: *Expression,
        tag: Tag,

        pub const Tag = enum {
            Minus,
            Not,
        };
    };

    pub const BinaryOperator = struct {
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

    pub const Flags = packed struct {
        is_lvalue: bool,
        is_const: bool,
        is_static: bool,
    };
};

pub const Statement = struct {
    as: As,

    pub const As = union(enum) {
        Assign: Assign,
        Symbol: *Symbol,
        Expression: *Expression,
    };

    pub const Assign = struct {
        lhs: *Expression,
        rhs: *Expression,
    };
};

pub const Symbol = struct {
    as: As,

    pub const As = union(enum) {
        Alias: *Symbol,
        Type: *const Type,
        Procedure: Procedure,
        Variable: Variable,
    };

    pub const Procedure = struct {
        typ: *const Type,
        block: []*Statement,
    };

    pub const Variable = struct {
        attributes: Attributes,
        typ: *const Type,
        value: ?*Expression,

        pub const Attributes = packed struct {
            is_const: bool,
            is_static: bool,
        };
    };
};

pub const SymbolList = std.DoublyLinkedList(*Symbol);

pub const ArenaAllocator = std.heap.ArenaAllocator;

const std = @import("std");
const nostd = @import("nostd.zig");
