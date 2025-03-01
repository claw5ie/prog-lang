pub var void_pointer_type: *Type = undefined;
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
        void_array_check: Stage,
        full_check: Stage,
    };
};
