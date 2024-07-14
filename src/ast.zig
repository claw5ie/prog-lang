stmt_list: StmtList,
arena: ArenaAllocator,
filepath: []const u8,

const std = @import("std");

const ArenaAllocator = std.heap.ArenaAllocator;
pub const LineInfo = @import("lexer.zig").LineInfo;
pub const StmtList = std.DoublyLinkedList(*Stmt);
const Ast = @This();

pub const MAX_BITS_IN_INTEGER = 64;

pub var bool_type = Type{
    .Bool = .{
        .line_info = .{ .line = 0, .column = 0, .offset = 0 },
    },
};

pub const Type = union(Type.Tag) {
    Bool: Type.Bool,
    Integer: Type.Integer,

    pub const Tag = enum {
        Bool,
        Integer,
    };

    pub const Bool = struct {
        line_info: LineInfo,
    };

    pub const Integer = struct {
        line_info: LineInfo,
        bits: u16,
        is_signed: bool,
    };

    pub fn format(typ: *Type, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (typ.*) {
            .Bool => {
                _ = try writer.write("bool");
            },
            .Integer => |Int| {
                _ = try writer.print("{c}{}", .{ @as(u8, if (Int.is_signed) 'i' else 'u'), Int.bits });
            },
        }
    }
};

pub const Expr = struct {
    line_info: LineInfo,
    as: Data,
    typ: ?*Ast.Type,

    pub const Tag = enum {
        Binary_Op,
        Unary_Op,
        Cast,
        Bool,
        Integer,
    };

    pub const Data = union(Tag) {
        Binary_Op: Expr.BinaryOp,
        Unary_Op: Expr.UnaryOp,
        Cast: Expr.Cast,
        Bool: bool,
        Integer: u64,
    };

    pub const BinaryOp = struct {
        line_info: LineInfo,
        lhs: *Ast.Expr,
        rhs: *Ast.Expr,
        tag: BinaryOp.Tag,

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
        subexpr: *Ast.Expr,
        tag: UnaryOp.Tag,

        pub const Tag = enum {
            Plus,
            Minus,
            Not,
        };
    };

    pub const Cast = struct {
        typ: *Ast.Type,
        expr: *Ast.Expr,
    };
};

pub const Stmt = union(Stmt.Tag) {
    Print: *Expr,
    Expr: *Expr,

    pub const Tag = enum {
        Print,
        Expr,
    };
};

pub fn create(ast: *Ast, T: type) *T {
    return ast.arena.allocator().create(T) catch {
        std.posix.exit(1);
    };
}
