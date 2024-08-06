buffer: [LOOKAHEAD]Token = undefined,
token_start: u8 = 0,
token_count: u8 = 0,
line_info: LineInfo = .{
    .line = 1,
    .column = 1,
    .offset = 0,
},
filepath: [:0]const u8,
source_code: [:0]u8,
allocator: common.Allocator,

const Lexer = @This();
pub const LOOKAHEAD = 2;

pub fn init(filepath: [:0]const u8) Lexer {
    const allocator = common.gpa;
    const source_code = utils.read_entire_file(allocator, filepath) catch {
        common.eprint("error: failed to read from a file '{s}'.\n", .{filepath});
        common.exit(1);
    };

    return .{
        .filepath = filepath,
        .source_code = source_code,
        .allocator = allocator,
    };
}

pub fn deinit(l: *Lexer) void {
    l.allocator.free(l.source_code);
}

pub fn grab(l: *Lexer) Token {
    return grab_by_ptr(l, 0).*;
}

pub fn peek_ahead(l: *Lexer, index: u8) Token.Tag {
    return grab_by_ptr(l, index).as;
}

pub fn peek(l: *Lexer) Token.Tag {
    return peek_ahead(l, 0);
}

pub fn advance_many(l: *Lexer, count: u8) void {
    l.token_start += count;
    l.token_start %= LOOKAHEAD;
    l.token_count -= count;
}

pub fn advance(l: *Lexer) void {
    advance_many(l, 1);
}

pub fn expect(l: *Lexer, expected: Token.Tag) void {
    if (peek(l) != expected) {
        const tok = grab(l);
        common.print_error(l.filepath, tok.line_info, "expected {s}.", .{token_tag_to_text(expected)});
        common.exit(1);
    }
    advance(l);
}

fn grab_by_ptr(l: *Lexer, index: u8) *Token {
    std.debug.assert(index < LOOKAHEAD);
    while (index >= l.token_count) {
        buffer_token(l);
    }
    return &l.buffer[(l.token_start + index) % LOOKAHEAD];
}

fn advance_line_info(l: *Lexer) void {
    l.line_info.column += 1;
    l.line_info.offset += 1;
    if (l.source_code[l.line_info.offset - 1] == '\n') {
        l.line_info.line += 1;
        l.line_info.column = 1;
    }
}

fn buffer_token(l: *Lexer) void {
    var src = l.source_code[l.line_info.offset..];
    var at: usize = 0;
    var ch = src[at];

    while (ch != 0) {
        while (is_space(ch)) {
            at += 1;
            ch = src[at];
            advance_line_info(l);
        }

        if (ch == '/' and src[at + 1] == '/') {
            while (ch != 0 and ch != '\n') {
                at += 1;
                ch = src[at];
                advance_line_info(l);
            }
        } else {
            break;
        }
    }

    src = src[at..];
    at = 0;

    var tok = Token{
        .line_info = l.line_info,
        .as = .End_Of_File,
    };

    if (ch == 0) {
        // leave.
    } else if (is_digit(ch)) {
        while (true) {
            at += 1;
            ch = src[at];
            advance_line_info(l);
            if (!is_digit(ch)) break;
        }

        const text = src[0..at];
        const value = std.fmt.parseInt(u64, text, 10) catch {
            common.print_error(l.filepath, tok.line_info, "integer literal '{s}' is too big (> 64 bits).", .{text});
            common.exit(1);
        };

        tok.as = .{ .Integer = value };
    } else if (is_alpha(ch) or ch == '_') {
        while (true) {
            at += 1;
            ch = src[at];
            advance_line_info(l);
            if (!is_alnum(ch) and ch != '_') break;
        }

        const Keyword = struct {
            text: []const u8,
            as: Token.As,
        };

        const state = struct {
            pub const keywords = [_]Keyword{
                .{ .text = "continue", .as = .Continue },
                .{ .text = "switch", .as = .Switch },
                .{ .text = "return", .as = .Return },
                .{ .text = "struct", .as = .Struct },
                .{ .text = "union", .as = .Union },
                .{ .text = "while", .as = .While },
                .{ .text = "break", .as = .Break },
                .{ .text = "false", .as = .{ .Boolean = false } },
                .{ .text = "print", .as = .Print },
                .{ .text = "enum", .as = .Enum },
                .{ .text = "proc", .as = .Proc },
                .{ .text = "void", .as = .Void_Type },
                .{ .text = "bool", .as = .Bool_Type },
                .{ .text = "cast", .as = .Cast },
                .{ .text = "true", .as = .{ .Boolean = true } },
                .{ .text = "null", .as = .Null },
                .{ .text = "then", .as = .Then },
                .{ .text = "else", .as = .Else },
                .{ .text = "if", .as = .If },
                .{ .text = "do", .as = .Do },
            };
        };

        tok.as = as: {
            const text = src[0..at];

            if (text[0] == 'i' or text[0] == 'u') {
                switch (text.len) {
                    2 => {
                        if (is_digit(text[1])) {
                            break :as .{ .Integer_Type = .{
                                .bits = text[1] - '0',
                                .is_signed = text[0] == 'i',
                            } };
                        }
                    },
                    3 => {
                        if (is_digit(text[1]) and is_digit(text[2]) and text[1] != '0') {
                            const bits = 10 * (text[1] - '0') + (text[2] - '0');
                            if (bits <= 64) {
                                break :as .{ .Integer_Type = .{
                                    .bits = bits,
                                    .is_signed = text[0] == 'i',
                                } };
                            }
                        }
                    },
                    else => {},
                }
            }

            for (state.keywords) |keyword| {
                if (std.mem.eql(u8, text, keyword.text)) {
                    break :as keyword.as;
                }
            }

            break :as .{ .Identifier = text };
        };
    } else if (!is_print(ch)) {
        common.print_error(l.filepath, tok.line_info, "non-printable character with code '{}'.\n", .{ch});
        common.exit(1);
    } else {
        const Symbol = struct {
            text: []const u8,
            as: Token.As,
        };

        const state = struct {
            pub const symbols = [_]Symbol{
                .{ .text = "||", .as = .Or },
                .{ .text = "&&", .as = .And },
                .{ .text = "==", .as = .Eq },
                .{ .text = "!=", .as = .Neq },
                .{ .text = "<=", .as = .Leq },
                .{ .text = ">=", .as = .Geq },
                .{ .text = "->", .as = .Arrow },
                .{ .text = ":=", .as = .Colon_Equal },
                .{ .text = "<", .as = .Lt },
                .{ .text = ">", .as = .Gt },
                .{ .text = "+", .as = .Add },
                .{ .text = "-", .as = .Sub },
                .{ .text = "*", .as = .Mul },
                .{ .text = "/", .as = .Div },
                .{ .text = "%", .as = .Mod },
                .{ .text = "&", .as = .Ref },
                .{ .text = "!", .as = .Not },
                .{ .text = "(", .as = .Open_Paren },
                .{ .text = ")", .as = .Close_Paren },
                .{ .text = "{", .as = .Open_Curly },
                .{ .text = "}", .as = .Close_Curly },
                .{ .text = "[", .as = .Open_Bracket },
                .{ .text = "]", .as = .Close_Bracket },
                .{ .text = ":", .as = .Colon },
                .{ .text = ";", .as = .Semicolon },
                .{ .text = "=", .as = .Equal },
                .{ .text = ".", .as = .Dot },
                .{ .text = ",", .as = .Comma },
            };
        };

        tok.as = as: {
            for (state.symbols) |symbol| {
                if (utils.is_prefix(symbol.text, src)) {
                    const count: u32 = @intCast(symbol.text.len);
                    at += count;
                    l.line_info.column += count;
                    l.line_info.offset += count;
                    break :as symbol.as;
                }
            }

            common.print_error(l.filepath, tok.line_info, "unrecognized character '{c}'.\n", .{src[at]});
            common.exit(1);
        };
    }

    std.debug.assert(l.token_count < LOOKAHEAD);
    const index = (l.token_start + l.token_count) % LOOKAHEAD;
    l.buffer[index] = tok;
    l.token_count += 1;
}

fn token_tag_to_text(tag: Token.Tag) []const u8 {
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

        .Ref => "'&'",
        .Not => "'!'",
        .Open_Paren => "'('",
        .Close_Paren => "')'",
        .Open_Curly => "'{'",
        .Close_Curly => "'}'",
        .Open_Bracket => "'['",
        .Close_Bracket => "']'",
        .Colon_Equal => "':='",
        .Colon => "':'",
        .Semicolon => "';'",
        .Dot => "'.'",
        .Comma => "','",
        .Equal => "'='",
        .Arrow => "'->'",

        .Cast => "'@cast'",
        .Boolean => "boolean literal",
        .Integer => "integer literal",
        .Identifier => "identifier",
        .Null => "'null'",

        .Struct => "'struct'",
        .Union => "'union'",
        .Enum => "'enum'",
        .Proc => "'proc'",
        .Void_Type => "'void'",
        .Bool_Type => "'bool'",
        .Integer_Type => "integer type",

        .Print => "'print'",
        .If => "'if'",
        .Then => "'then'",
        .Else => "'else'",
        .While => "'while'",
        .Do => "'do'",
        .Break => "'break'",
        .Continue => "'continue'",
        .Switch => "'switch'",
        .Return => "'return'",

        .End_Of_File => "EOF",
    };
}

const is_space = std.ascii.isWhitespace;
const is_digit = std.ascii.isDigit;
const is_alpha = std.ascii.isAlphabetic;
const is_alnum = std.ascii.isAlphanumeric;
const is_print = std.ascii.isPrint;

const std = @import("std");
const common = @import("common.zig");
const utils = @import("utils.zig");

const LineInfo = common.LineInfo;

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
        Arrow,

        Cast,
        Boolean,
        Integer,
        Identifier,
        Null,

        Struct,
        Union,
        Enum,
        Proc,
        Void_Type,
        Bool_Type,
        Integer_Type,

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
        Arrow: void,

        Cast: void,
        Boolean: bool,
        Integer: u64,
        Identifier: []const u8,
        Null: void,

        Struct: void,
        Union: void,
        Enum: void,
        Proc: void,
        Void_Type: void,
        Bool_Type: void,
        Integer_Type: Token.IntegerType,

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

        End_Of_File: void,
    };

    pub const IntegerType = struct {
        bits: u8,
        is_signed: bool,
    };
};
