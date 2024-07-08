stmt_list: StmtList,

const std = @import("std");

pub const LineInfo = @import("lexer.zig").LineInfo;

pub const StmtList = std.DoublyLinkedList(*Stmt);

pub const Expr = union(Expr.Tag) {
    Binary_Op: Expr.BinaryOp,
    Unary_Op: Expr.UnaryOp,
    Bool: Expr.Bool,
    Integer: Expr.Integer,

    pub const Tag = enum {
        Binary_Op,
        Unary_Op,
        Bool,
        Integer,
    };

    pub const BinaryOp = struct {
        line_info: LineInfo,
        lhs: *Expr,
        rhs: *Expr,
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
        line_info: LineInfo,
        subexpr: *Expr,
        tag: UnaryOp.Tag,

        pub const Tag = enum {
            Plus,
            Minus,
            Not,
        };
    };

    pub const Bool = struct {
        line_info: LineInfo,
        value: bool,
    };

    pub const Integer = struct {
        line_info: LineInfo,
        value: u64,
    };
};

pub const Stmt = union(Stmt.Tag) {
    Expr: *Expr,

    pub const Tag = enum {
        Expr,
    };
};
