stmt_list: StmtList,

const std = @import("std");

pub const LineInfo = @import("lexer.zig").LineInfo;

pub const StmtList = std.DoublyLinkedList(*Stmt);

pub const Expr = union(Expr.Tag) {
    Integer: Integer,

    pub const Tag = enum {
        Integer,
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
