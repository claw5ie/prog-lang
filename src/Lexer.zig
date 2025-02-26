buffer: [TOKEN_BUFFER_COUNT]Token = [1]Token{
    .{
        .line_info = .{
            .line = 1,
            .column = 1,
            .offset = 0,
        },
        .as = .End_Of_File,
    },
} ** TOKEN_BUFFER_COUNT,
token_start: u8 = 0,
token_count: u8 = 0,
line_info: LineInfo = .{
    .line = 1,
    .column = 1,
    .offset = 0,
},
c: *Compiler,

const Lexer = @This();

pub const LOOKAHEAD = 2;

const TOKEN_BUFFER_COUNT = 4;

pub fn putback(l: *Lexer, tok: Token) void {
    std.debug.assert(l.token_count < LOOKAHEAD);
    l.token_start -%= 1;
    l.token_start %= TOKEN_BUFFER_COUNT;
    l.token_count += 1;
    l.buffer[l.token_start] = tok;
}

pub fn grab(l: *Lexer) Token {
    return grab_by_ptr(l, 0).*;
}

pub fn grab_line_info(l: *Lexer) LineInfo {
    return grab_by_ptr(l, 0).line_info;
}

pub fn peek_ahead(l: *Lexer, index: u8) Token.Tag {
    return grab_by_ptr(l, index).as;
}

pub fn peek(l: *Lexer) Token.Tag {
    return peek_ahead(l, 0);
}

pub fn advance_many(l: *Lexer, count: u8) void {
    l.token_start += count;
    l.token_start %= TOKEN_BUFFER_COUNT;
    l.token_count -= count;
}

pub fn advance(l: *Lexer) void {
    advance_many(l, 1);
}

pub fn expect(l: *Lexer, expected: Token.Tag) void {
    if (peek(l) != expected) {
        const tok = grab(l);
        l.c.report_error(tok.line_info, "expected {s}", .{token_tag_to_text(expected)});
        Compiler.exit(1);
    }
    advance(l);
}

fn grab_by_ptr(l: *Lexer, index: u8) *Token {
    std.debug.assert(index < LOOKAHEAD);
    while (index >= l.token_count) {
        buffer_token(l);
    }
    return &l.buffer[(l.token_start + index) % TOKEN_BUFFER_COUNT];
}

fn advance_line_info(l: *Lexer) void {
    l.line_info.column += 1;
    l.line_info.offset += 1;
    if (l.c.source_code[l.line_info.offset - 1] == '\n') {
        l.line_info.line += 1;
        l.line_info.column = 1;
    }
}

fn parse_token(l: *Lexer) Token {
    const State = struct {
        src: [:0]const u8,
        at: usize,
        ch: u8,
        l: *Lexer,
    };

    const fns = struct {
        pub fn _advance(s: *State) void {
            s.at += 1;
            s.ch = s.src[s.at];
            advance_line_info(s.l);
        }
    };

    var s: State = s: {
        const src = l.c.source_code[l.line_info.offset..];
        break :s .{
            .src = src,
            .at = 0,
            .ch = src[0],
            .l = l,
        };
    };

    while (s.ch != 0) {
        while (is_space(s.ch)) {
            fns._advance(&s);
        }

        if (s.ch == '/' and s.src[s.at + 1] == '/') {
            while (s.ch != 0 and s.ch != '\n') {
                fns._advance(&s);
            }
        } else {
            break;
        }
    }

    s.src = s.src[s.at..];
    s.at = 0;

    var tok = Token{
        .line_info = l.line_info,
        .as = .End_Of_File,
    };

    if (s.ch == 0) {
        // leave.
    } else if (is_digit(s.ch)) {
        while (true) {
            fns._advance(&s);
            if (!is_digit(s.ch)) break;
        }

        const text = s.src[0..s.at];
        const value = std.fmt.parseInt(u64, text, 10) catch {
            l.c.report_error(tok.line_info, "integer literal '{s}' is too big (> 64 bits).", .{text});
            Compiler.exit(1);
        };

        tok.as = .{ .Integer = value };
    } else if (is_alpha(s.ch) or s.ch == '_') {
        while (true) {
            fns._advance(&s);
            if (!is_alnum(s.ch) and s.ch != '_') break;
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
                .{ .text = "alias", .as = .Alias },
                .{ .text = "enum", .as = .Enum },
                .{ .text = "proc", .as = .Proc },
                .{ .text = "void", .as = .Void_Type },
                .{ .text = "bool", .as = .Bool_Type },
                .{ .text = "true", .as = .{ .Boolean = true } },
                .{ .text = "null", .as = .Null },
                .{ .text = "then", .as = .Then },
                .{ .text = "else", .as = .Else },
                .{ .text = "case", .as = .Case },
                .{ .text = "if", .as = .If },
                .{ .text = "do", .as = .Do },
            };
        };

        tok.as = as: {
            const text = s.src[0..s.at];

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

            const new_text = l.c.string_pool.insert(text);

            break :as .{ .Identifier = new_text };
        };
    } else if (s.ch == '#' and (is_alpha(s.src[s.at + 1]) or s.src[s.at + 1] == '_')) {
        while (true) {
            fns._advance(&s);

            if (!is_alnum(s.ch) and s.ch != '_') break;
        }

        const SpicyKeyword = struct {
            text: []const u8,
            as: Token.As,
        };

        const state = struct {
            pub const keywords = [_]SpicyKeyword{
                .{ .text = "#byte_size_of", .as = .Byte_Size_Of },
                .{ .text = "#alignment_of", .as = .Alignment_Of },
                .{ .text = "#type_of", .as = .Type_Of },
                .{ .text = "#static", .as = .{ .Attribute = .{
                    .is_static = true,
                } } },
                .{ .text = "#const", .as = .{ .Attribute = .{
                    .is_const = true,
                } } },
                .{ .text = "#print", .as = .Print },
                .{ .text = "#cast", .as = .Cast },
                .{ .text = "#as", .as = .As },
            };
        };

        const text = s.src[0..s.at];

        tok.as = as: {
            for (state.keywords) |keyword| {
                if (std.mem.eql(u8, text, keyword.text)) {
                    break :as keyword.as;
                }
            }

            l.c.report_error(tok.line_info, "invalid keyword '{s}'\n", .{text});
            Compiler.exit(1);
        };
    } else if (!is_print(s.ch)) {
        l.c.report_error(tok.line_info, "non-printable character with code '{}'\n", .{s.ch});
        Compiler.exit(1);
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
                .{ .text = ":=", .as = .Colon_Equal },
                .{ .text = "<", .as = .Lt },
                .{ .text = ">", .as = .Gt },
                .{ .text = "+", .as = .Add },
                .{ .text = "-", .as = .Sub },
                .{ .text = "*", .as = .Mul },
                .{ .text = "/", .as = .Div },
                .{ .text = "%", .as = .Mod },
                .{ .text = "&", .as = .Ref },
                .{ .text = "^", .as = .Deref },
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
                if (nostd.is_prefix(symbol.text, s.src)) {
                    const count: u32 = @intCast(symbol.text.len);
                    s.at += count;
                    l.line_info.column += count;
                    l.line_info.offset += count;
                    break :as symbol.as;
                }
            }

            l.c.report_error(tok.line_info, "unrecognized character '{c}'\n", .{s.ch});
            Compiler.exit(1);
        };
    }

    return tok;
}

fn buffer_token(l: *Lexer) void {
    var curr = parse_token(l);

    switch (curr.as) {
        .Attribute => |*cattr| {
            while (true) {
                const next = parse_token(l);
                switch (next.as) {
                    .Attribute => |nattr| {
                        cattr.combine(nattr);
                    },
                    else => {
                        append_token(l, curr);
                        append_token(l, next);
                        return;
                    },
                }
            }
        },
        else => {
            append_token(l, curr);
        },
    }
}

fn append_token(l: *Lexer, tok: Token) void {
    std.debug.assert(l.token_count < TOKEN_BUFFER_COUNT);
    const index = (l.token_start + l.token_count) % TOKEN_BUFFER_COUNT;
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
        .Deref => "'^'",
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

        .Byte_Size_Of => "#byte_size_of",
        .Alignment_Of => "#alignment_of",
        .As => "#as",
        .Cast => "'#cast'",
        .Boolean => "boolean literal",
        .Integer => "integer literal",
        .Identifier => "identifier",
        .Null => "'null'",

        .Struct => "'struct'",
        .Union => "'union'",
        .Enum => "'enum'",
        .Proc => "'proc'",
        .Integer_Type => "integer type",
        .Bool_Type => "'bool'",
        .Void_Type => "'void'",
        .Type_Of => "#type_of",

        .Attribute => "attribute",
        .Print => "'#print'",
        .If => "'if'",
        .Then => "'then'",
        .Else => "'else'",
        .While => "'while'",
        .Do => "'do'",
        .Break => "'break'",
        .Continue => "'continue'",
        .Switch => "'switch'",
        .Return => "'return'",
        .Alias => "'alias'",
        .Case => "'case'",

        .End_Of_File => "EOF",
    };
}

const is_space = std.ascii.isWhitespace;
const is_digit = std.ascii.isDigit;
const is_alpha = std.ascii.isAlphabetic;
const is_alnum = std.ascii.isAlphanumeric;
const is_print = std.ascii.isPrint;

const std = @import("std");
const nostd = @import("nostd.zig");
const Compiler = @import("Compiler.zig");

const LineInfo = Compiler.LineInfo;
const Attributes = Compiler.Attributes;

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
