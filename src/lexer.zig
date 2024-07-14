tokens: [LOOKAHEAD]Token = undefined,
token_start: u8 = 0,
token_count: u8 = 0,
line_info: LineInfo = .{
    .line = 1,
    .column = 1,
    .offset = 0,
},
filepath: [:0]const u8 = "<no file>",
source_code: [:0]const u8 = "",
allocator: Allocator,

const std = @import("std");
const utils = @import("utils.zig");

const is_space = std.ascii.isWhitespace;
const is_digit = std.ascii.isDigit;
const is_alpha = std.ascii.isAlphabetic;
const is_alnum = std.ascii.isAlphanumeric;
const Allocator = std.mem.Allocator;
const Lexer = @This();

pub const LOOKAHEAD = 2;

pub const LineInfo = struct {
    line: u8,
    column: u8,
    offset: usize,
};

pub const Token = struct {
    line_info: LineInfo,
    as: Data,

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

        Not,
        Open_Paren,
        Close_Paren,
        Semicolon,

        Print,

        Bool,
        Integer,
        Identifier,

        End_Of_File,
    };

    pub const Data = union(Token.Tag) {
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
        Not: void,
        Open_Paren: void,
        Close_Paren: void,
        Semicolon: void,
        Print: void,
        Bool: bool,
        Integer: u64,
        Identifier: void,
        End_Of_File: void,
    };
};

pub fn init(allocator: Allocator, filepath: [:0]const u8) Lexer {
    const source_code = utils.read_entire_file(allocator, filepath) catch {
        utils.eprint("error: couldn't open file '{s}'\n", .{filepath});
        std.posix.exit(1);
    };
    return .{
        .filepath = filepath,
        .source_code = source_code,
        .allocator = allocator,
    };
}

fn advance_line_info(lexer: *Lexer) void {
    lexer.line_info.offset += 1;
    lexer.line_info.column += 1;
    if (lexer.source_code[lexer.line_info.offset - 1] == '\n') {
        lexer.line_info.line += 1;
        lexer.line_info.column = 1;
    }
}

fn buffer_token(lexer: *Lexer) void {
    const text = lexer.source_code;
    var i = lexer.line_info.offset;

    while (true) {
        while (is_space(text[i])) : (i += 1) {
            advance_line_info(lexer);
        }

        if (text[i] == '/' and text[i + 1] == '/') {
            while (text[i] != 0 and text[i] != '\n') : (i += 1) {
                advance_line_info(lexer);
            }
        } else {
            break;
        }
    }

    const old_i = i;
    var token: Token = .{
        .line_info = lexer.line_info,
        .as = .End_Of_File,
    };

    if (text[i] == 0) {
        // break;
    } else if (is_digit(text[i])) {
        while (true) {
            advance_line_info(lexer);
            i += 1;

            if (!is_digit(text[i])) {
                break;
            }
        }

        const string = text[old_i..i];
        const value = std.fmt.parseInt(u64, string, 10) catch {
            report_error(lexer, token.line_info, "value '{s}' can't fit in 64 bits", .{string});
            std.posix.exit(1);
        };

        token.as = .{ .Integer = value };
    } else if (is_alpha(text[i]) or text[i] == '_') {
        while (true) {
            advance_line_info(lexer);
            i += 1;

            if (!is_alnum(text[i]) and text[i] != '_') {
                break;
            }
        }

        const Keyword = struct {
            text: []const u8,
            as: Token.Data,
        };

        const keywords = [_]Keyword{
            .{ .text = "print", .as = .Print },
            .{ .text = "false", .as = .{ .Bool = false } },
            .{ .text = "true", .as = .{ .Bool = true } },
        };

        const string = text[old_i..i];
        var found = false;

        for (keywords) |keyword| {
            if (std.mem.eql(u8, keyword.text, string)) {
                token.as = keyword.as;
                found = true;
                break;
            }
        }

        if (!found) {
            token.as = .Identifier;
        }
    } else {
        const Symbol = struct {
            text: []const u8,
            as: Token.Data,
        };

        const symbols = [_]Symbol{
            .{ .text = "||", .as = .Or },
            .{ .text = "&&", .as = .And },
            .{ .text = "==", .as = .Eq },
            .{ .text = "!=", .as = .Neq },
            .{ .text = "<=", .as = .Leq },
            .{ .text = ">=", .as = .Geq },
            .{ .text = "<", .as = .Lt },
            .{ .text = ">", .as = .Gt },
            .{ .text = "+", .as = .Add },
            .{ .text = "-", .as = .Sub },
            .{ .text = "*", .as = .Mul },
            .{ .text = "/", .as = .Div },
            .{ .text = "%", .as = .Mod },
            .{ .text = "!", .as = .Not },
            .{ .text = "(", .as = .Open_Paren },
            .{ .text = ")", .as = .Close_Paren },
            .{ .text = ";", .as = .Semicolon },
        };

        const string = text[old_i..];
        var found = false;

        for (symbols) |symbol| {
            if (utils.is_prefix(symbol.text, string)) {
                const count: u8 = @intCast(symbol.text.len);
                i += count;
                lexer.line_info.offset += count;
                lexer.line_info.column += count;

                token.as = symbol.as;
                found = true;
                break;
            }
        }

        if (!found) {
            report_error(lexer, token.line_info, "unexpected character '{c}'", .{text[i]});
            std.posix.exit(1);
        }
    }

    std.debug.assert(lexer.token_count < LOOKAHEAD);
    const index = (lexer.token_start + lexer.token_count) % LOOKAHEAD;
    lexer.tokens[index] = token;
    lexer.token_count += 1;
}

fn take_by_ptr(lexer: *Lexer, index: u8) *Token {
    std.debug.assert(index < LOOKAHEAD);

    while (index >= lexer.token_count) {
        buffer_token(lexer);
    }

    return &lexer.tokens[(lexer.token_start + index) % LOOKAHEAD];
}

pub fn take_ahead(lexer: *Lexer, index: u8) Token {
    const ptr = take_by_ptr(lexer, index);
    advance_many(lexer, index + 1);
    return ptr.*;
}

pub fn take(lexer: *Lexer) Token {
    return take_ahead(lexer, 0);
}

pub fn peek_ahead(lexer: *Lexer, index: u8) Token.Tag {
    const ptr = take_by_ptr(lexer, index);
    return ptr.as;
}

pub fn peek(lexer: *Lexer) Token.Tag {
    return peek_ahead(lexer, 0);
}

pub fn advance_many(lexer: *Lexer, count: u8) void {
    std.debug.assert(count <= LOOKAHEAD);
    lexer.token_start += count;
    lexer.token_start %= LOOKAHEAD;
    lexer.token_count -= count;
}

pub fn advance(lexer: *Lexer) void {
    advance_many(lexer, 1);
}

pub fn expect(lexer: *Lexer, tag: Token.Tag) void {
    const token = take(lexer);
    if (token.as != tag) {
        report_error(lexer, token.line_info, "expected {s}", .{to_token_tag_name(tag)});
        std.posix.exit(1);
    }
}

fn to_token_tag_name(tag: Token.Tag) []const u8 {
    return switch (tag) {
        .Or => "'||'",
        .And => "'&&'",
        .Eq => "'=='",
        .Neq => "'!='",
        .Lt => "'<'",
        .Leq => "'<='",
        .Gt => "'>'",
        .Geq => "'>='",
        .Add => "'+'",
        .Sub => "'-'",
        .Mul => "'*'",
        .Div => "'/'",
        .Mod => "'%'",
        .Not => "'!'",
        .Open_Paren => "'('",
        .Close_Paren => "')'",
        .Semicolon => "';'",
        .Print => "'print'",
        .Bool => "boolean literal",
        .Integer => "integer literal",
        .Identifier => "identifier",
        .End_Of_File => "EOF",
    };
}

fn report_error(lexer: *Lexer, line_info: LineInfo, comptime format: []const u8, args: anytype) void {
    utils.eprint("{s}:{}:{}: error: " ++ format ++ "\n", .{ lexer.filepath, line_info.line, line_info.column } ++ args);
}
