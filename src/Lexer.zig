token_buffer: [token_buffer_count]Token = [1]Token{default_token} ** token_buffer_count,
token_start: TokenIndex = 0,
token_count: TokenIndex = 0,
position: FilePosition = default_token.position,
c: *Compiler,

const Lexer = @This();

const default_token: Token = .{
    .position = .{
        .line = 1,
        .column = 1,
        .offset = 0,
    },
    .as = .End_Of_File,
};

const Keyword = struct {
    text: []const u8,
    as: Token.As,

    pub fn make_with_custom_text(text: []const u8, as: Token.As) Keyword {
        return .{ .text = text, .as = as };
    }

    pub fn make(as: Token.As) Keyword {
        const tag: Token.Tag = as;
        return make_with_custom_text(tag.to_unquoted_text(), as);
    }
};

pub const keywords = [_]Keyword{
    Keyword.make_with_custom_text("false", .{ .Boolean = false }),
    Keyword.make_with_custom_text("true", .{ .Boolean = true }),
    Keyword.make(.Null),

    Keyword.make(.Struct),
    Keyword.make(.Union),
    Keyword.make(.Enum),
    Keyword.make(.Proc),
    Keyword.make(.Boolean_Type),
    Keyword.make(.Void_Type),

    Keyword.make(.If),
    Keyword.make(.Then),
    Keyword.make(.Else),
    Keyword.make(.While),
    Keyword.make(.Do),
    Keyword.make(.Break),
    Keyword.make(.Continue),
    Keyword.make(.Switch),
    Keyword.make(.Return),
    Keyword.make(.Alias),
    Keyword.make(.Case),
};

pub const hashed_keywords = [_]Keyword{
    Keyword.make(.Byte_Size_Of),
    Keyword.make(.Alignment_Of),
    Keyword.make(.As),
    Keyword.make(.Cast),
    Keyword.make(.Type_Of),

    Keyword.make_with_custom_text("#static", .{ .Attributes = .{ .is_static = true } }),
    Keyword.make_with_custom_text("#const", .{ .Attributes = .{ .is_const = true } }),

    Keyword.make(.Print),
};

pub const symbols = symbols: {
    var ss = [_]Keyword{
        Keyword.make(.Or),
        Keyword.make(.And),
        Keyword.make(.Double_Equal),
        Keyword.make(.Not_Equal),
        Keyword.make(.Less_Than),
        Keyword.make(.Less_Equal),
        Keyword.make(.Greater_Than),
        Keyword.make(.Greater_Equal),
        Keyword.make(.Plus),
        Keyword.make(.Minus),
        Keyword.make(.Asterisk),
        Keyword.make(.Slash),
        Keyword.make(.Percent_Sign),
        Keyword.make(.Reference),
        Keyword.make(.Dereference),
        Keyword.make(.Not),
        Keyword.make(.Left_Parenthesis),
        Keyword.make(.Right_Parenthesis),
        Keyword.make(.Left_Brace),
        Keyword.make(.Right_Brace),
        Keyword.make(.Left_Bracket),
        Keyword.make(.Right_Bracket),
        Keyword.make(.Colon),
        Keyword.make(.Semicolon),
        Keyword.make(.Equal),
        Keyword.make(.Dot),
        Keyword.make(.Comma),
    };

    const Sort = struct {
        const Context = struct {};

        pub fn less_than(_: Context, lhs: Keyword, rhs: Keyword) bool {
            return lhs.text.len > rhs.text.len;
        }
    };

    std.mem.sortUnstable(Keyword, &ss, Sort.Context{}, Sort.less_than);

    break :symbols ss;
};

pub const lookahead = 2;
const token_buffer_count = 4;

pub fn putback(lexer: *Lexer, token: Token) void {
    std.debug.assert(lexer.token_count < lookahead);
    lexer.token_start = compute_token_index(lexer.*, token_buffer_count - 1);
    lexer.token_count += 1;
    lexer.token_buffer[lexer.token_start] = token;
}

pub fn grab(lexer: *Lexer) Token {
    return grab_by_ptr(lexer, 0).*;
}

pub fn peek_at(lexer: *Lexer, index: TokenIndex) Token.Tag {
    return grab_by_ptr(lexer, index).as;
}

pub fn peek(lexer: *Lexer) Token.Tag {
    return peek_at(lexer, 0);
}

pub fn advance_many(lexer: *Lexer, count: TokenIndex) void {
    lexer.token_start = compute_token_index(lexer.*, count);
    lexer.token_count -= count;
}

pub fn advance(lexer: *Lexer) void {
    advance_many(lexer, 1);
}

pub fn expect(lexer: *Lexer, expected: Token.Tag) void {
    if (peek(lexer) != expected) {
        const token = grab(lexer);
        lexer.c.report_fatal_error(token.position, "expected {s}", .{expected.to_quoted_text()});
    }
    advance(lexer);
}

inline fn compute_token_index(lexer: Lexer, index: TokenIndex) TokenIndex {
    return (lexer.token_start + index) % token_buffer_count;
}

fn grab_by_ptr(lexer: *Lexer, index: TokenIndex) *Token {
    std.debug.assert(index < lookahead);
    while (index >= lexer.token_count) {
        buffer_next_token(lexer);
    }
    return &lexer.token_buffer[compute_token_index(lexer.*, index)];
}

fn peek_character_at(lexer: Lexer, index: usize) u8 {
    const source_code = lexer.c.source_code;
    const absolute_offset = lexer.position.offset + index;

    return if (absolute_offset < source_code.len) source_code[absolute_offset] else 0;
}

fn advance_many_characters_assume_no_newlines(lexer: *Lexer, count: usize) void {
    const _count: u32 = @intCast(count);
    lexer.position.column += _count;
    lexer.position.offset += _count;
}

fn advance_to_next_character(lexer: *Lexer) u8 {
    const ch = peek_character_at(lexer.*, 0);

    std.debug.assert(ch != 0);

    advance_many_characters_assume_no_newlines(lexer, 1);

    if (ch == '\n') {
        lexer.position.line += 1;
        lexer.position.column = 1;
    }

    return peek_character_at(lexer.*, 0);
}

fn parse_token(lexer: *Lexer) Token {
    var current = peek_character_at(lexer.*, 0);

    while (current != 0) {
        while (is_space(current)) {
            current = advance_to_next_character(lexer);
        }

        const next = peek_character_at(lexer.*, 1);

        if (current == '/' and next == '/') {
            while (current != 0 and current != '\n') {
                current = advance_to_next_character(lexer);
            }
        } else break;
    }

    const position = lexer.position;
    const next = peek_character_at(lexer.*, 1);

    if (current == 0) {
        return .{
            .position = position,
            .as = .End_Of_File,
        };
    } else if (is_digit(current)) {
        while (true) {
            current = advance_to_next_character(lexer);
            if (!is_digit(current)) break;
        }

        const text = lexer.c.source_code[position.offset..lexer.position.offset];
        const value = std.fmt.parseInt(u64, text, 10) catch {
            lexer.c.report_fatal_error(position, "integer literal '{s}' is too big (> 64 bits)", .{text});
        };

        return .{
            .position = position,
            .as = .{ .Integer = value },
        };
    } else if (is_alpha(current) or current == '_') {
        while (true) {
            current = advance_to_next_character(lexer);
            if (!(is_alnum(current) or current == '_')) break;
        }

        const text = lexer.c.source_code[position.offset..lexer.position.offset];

        if (text[0] == 'i' or text[0] == 'u') {
            const is_signed = text[0] == 'i';

            switch (text.len) {
                2 => {
                    if (is_digit(text[1]) and if (is_signed) text[1] != '0' else true) {
                        return .{
                            .position = position,
                            .as = .{ .Integer_Type = .{
                                .bits = text[1] - '0',
                                .is_signed = is_signed,
                            } },
                        };
                    }
                },
                3 => {
                    if (is_digit(text[1]) and is_digit(text[2]) and text[1] != '0') {
                        const bits = 10 * (text[1] - '0') + (text[2] - '0');

                        if (bits <= 64) {
                            return .{
                                .position = position,
                                .as = .{ .Integer_Type = .{
                                    .bits = bits,
                                    .is_signed = is_signed,
                                } },
                            };
                        }
                    }
                },
                else => {},
            }
        }

        for (keywords) |keyword| {
            if (std.mem.eql(u8, text, keyword.text)) {
                return .{
                    .position = position,
                    .as = keyword.as,
                };
            }
        }

        const new_text = lexer.c.string_pool.insert(text);

        return .{
            .position = position,
            .as = .{ .Identifier = new_text },
        };
    } else if (current == '#' and (is_alpha(next) or next == '_')) {
        while (true) {
            current = advance_to_next_character(lexer);
            if (!(is_alnum(current) or current == '_')) break;
        }

        const text = lexer.c.source_code[position.offset..lexer.position.offset];

        for (hashed_keywords) |keyword| {
            if (std.mem.eql(u8, text, keyword.text)) {
                return .{
                    .position = position,
                    .as = keyword.as,
                };
            }
        }

        lexer.c.report_fatal_error(position, "invalid keyword", .{});
    } else if (!is_print(current)) {
        lexer.c.report_fatal_error(position, "non-printable character with code '{}'", .{current});
    } else {
        const text = lexer.c.source_code[position.offset..];

        for (symbols) |symbol| {
            if (nostd.is_prefix(symbol.text, text)) {
                advance_many_characters_assume_no_newlines(lexer, symbol.text.len);
                return .{
                    .position = position,
                    .as = symbol.as,
                };
            }
        }

        lexer.c.report_fatal_error(position, "unrecognized character '{c}'", .{current});
    }
}

fn buffer_next_token(lexer: *Lexer) void {
    var current = parse_token(lexer);

    while (current.as == .Attributes) {
        const next = parse_token(lexer);

        switch (next.as) {
            .Attributes => |attributes| {
                current.as.Attributes.merge(attributes);
            },
            else => {
                append_token(lexer, current);
                append_token(lexer, next);
                return;
            },
        }
    }

    append_token(lexer, current);
}

fn append_token(lexer: *Lexer, token: Token) void {
    std.debug.assert(lexer.token_count < token_buffer_count);
    const index = compute_token_index(lexer.*, lexer.token_count);
    lexer.token_buffer[index] = token;
    lexer.token_count += 1;
}

const is_space = std.ascii.isWhitespace;
const is_digit = std.ascii.isDigit;
const is_alpha = std.ascii.isAlphabetic;
const is_alnum = std.ascii.isAlphanumeric;
const is_print = std.ascii.isPrint;

const exit = nostd.exit;

pub const FilePosition = Compiler.FilePosition;

pub const TokenIndex = u8;

pub const Token = struct {
    position: FilePosition,
    as: As,

    pub const Tag = enum {
        Or,
        And,
        Double_Equal,
        Not_Equal,
        Less_Than,
        Less_Equal,
        Greater_Than,
        Greater_Equal,
        Plus,
        Minus,
        Asterisk,
        Slash,
        Percent_Sign,

        Reference,
        Dereference,
        Not,
        Left_Parenthesis,
        Right_Parenthesis,
        Left_Brace,
        Right_Brace,
        Left_Bracket,
        Right_Bracket,
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
        Boolean_Type,
        Void_Type,
        Type_Of,

        Attributes,
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

        pub fn to_quoted_text(tag: Tag) []const u8 {
            return switch (tag) {
                .Or => "'||'",
                .And => "'&&'",
                .Double_Equal => "'=='",
                .Not_Equal => "'!='",
                .Less_Than => "'<'",
                .Less_Equal => "'<='",
                .Greater_Than => "'>'",
                .Greater_Equal => "'>='",
                .Plus => "'+'",
                .Minus => "'-'",
                .Asterisk => "'*'",
                .Slash => "'/'",
                .Percent_Sign => "'%'",

                .Reference => "'&'",
                .Dereference => "'^'",
                .Not => "'!'",
                .Left_Parenthesis => "'('",
                .Right_Parenthesis => "')'",
                .Left_Brace => "'{'",
                .Right_Brace => "'}'",
                .Left_Bracket => "'['",
                .Right_Bracket => "']'",
                .Colon => "':'",
                .Semicolon => "';'",
                .Dot => "'.'",
                .Comma => "','",
                .Equal => "'='",

                .Byte_Size_Of => "'#byte_size_of'",
                .Alignment_Of => "'#alignment_of'",
                .As => "'#as'",
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
                .Boolean_Type => "'bool'",
                .Void_Type => "'void'",
                .Type_Of => "'#type_of'",

                .Attributes => "'attributes'",
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

                .End_Of_File => "end-of-file",
            };
        }

        pub fn to_unquoted_text(tag: Tag) []const u8 {
            const string = tag.to_quoted_text();

            if (string[0] == '\'') {
                std.debug.assert(string[string.len - 1] == '\'');
                return string[1 .. string.len - 1];
            } else {
                return string;
            }
        }
    };

    pub const As = union(Tag) {
        Or: void,
        And: void,
        Double_Equal: void,
        Not_Equal: void,
        Less_Than: void,
        Less_Equal: void,
        Greater_Than: void,
        Greater_Equal: void,
        Plus: void,
        Minus: void,
        Asterisk: void,
        Slash: void,
        Percent_Sign: void,

        Reference: void,
        Dereference: void,
        Not: void,
        Left_Parenthesis: void,
        Right_Parenthesis: void,
        Left_Brace: void,
        Right_Brace: void,
        Left_Bracket: void,
        Right_Bracket: void,
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
        Boolean_Type: void,
        Void_Type: void,
        Type_Of: void,

        Attributes: Attributes,
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

    pub const Attributes = packed struct {
        is_const: bool = false,
        is_static: bool = false,
        is_global: bool = false,

        pub fn is_empty(attr: Attributes) bool {
            const ptr: *const u8 = @ptrCast(&attr);
            return ptr.* == 0;
        }

        pub fn merge(self: *Attributes, other: Attributes) void {
            const d: *u8 = @ptrCast(self);
            const s: *const u8 = @ptrCast(&other);
            d.* |= s.*;
        }
    };
};

const std = @import("std");
const nostd = @import("nostd.zig");
const Compiler = @import("Compiler.zig");
