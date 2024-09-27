const std = @import("std");
const utils = @import("utils.zig");
const Compiler = @import("compiler.zig");

const LineInfo = Compiler.LineInfo;
const Token = Compiler.Lexer.Token;
const LOOKAHEAD = Compiler.Lexer.LOOKAHEAD;

const is_space = std.ascii.isWhitespace;
const is_digit = std.ascii.isDigit;
const is_alpha = std.ascii.isAlphabetic;
const is_alnum = std.ascii.isAlphanumeric;
const is_print = std.ascii.isPrint;

pub fn putback(c: *Compiler, tok: Token) void {
    std.debug.assert(c.lexer.token_count < LOOKAHEAD);
    c.lexer.token_start -%= 1;
    c.lexer.token_start %= LOOKAHEAD;
    c.lexer.token_count += 1;
    c.lexer.buffer[c.lexer.token_start] = tok;
}

pub fn grab(c: *Compiler) Token {
    return grab_by_ptr(c, 0).*;
}

pub fn grab_line_info(c: *Compiler) LineInfo {
    return grab_by_ptr(c, 0).line_info;
}

pub fn peek_ahead(c: *Compiler, index: u8) Token.Tag {
    return grab_by_ptr(c, index).as;
}

pub fn peek(c: *Compiler) Token.Tag {
    return peek_ahead(c, 0);
}

pub fn advance_many(c: *Compiler, count: u8) void {
    c.lexer.token_start += count;
    c.lexer.token_start %= LOOKAHEAD;
    c.lexer.token_count -= count;
}

pub fn advance(c: *Compiler) void {
    advance_many(c, 1);
}

pub fn expect(c: *Compiler, expected: Token.Tag) void {
    if (peek(c) != expected) {
        const tok = grab(c);
        c.report_error(tok.line_info, "expected {s}", .{token_tag_to_text(expected)});
        Compiler.exit(1);
    }
    advance(c);
}

fn grab_by_ptr(c: *Compiler, index: u8) *Token {
    std.debug.assert(index < LOOKAHEAD);
    while (index >= c.lexer.token_count) {
        buffer_token(c);
    }
    return &c.lexer.buffer[(c.lexer.token_start + index) % LOOKAHEAD];
}

fn advance_line_info(c: *Compiler) void {
    c.lexer.line_info.column += 1;
    c.lexer.line_info.offset += 1;
    if (c.source_code[c.lexer.line_info.offset - 1] == '\n') {
        c.lexer.line_info.line += 1;
        c.lexer.line_info.column = 1;
    }
}

fn buffer_token(c: *Compiler) void {
    var src = c.source_code[c.lexer.line_info.offset..];
    var at: usize = 0;
    var ch = src[at];

    while (ch != 0) {
        while (is_space(ch)) {
            at += 1;
            ch = src[at];
            advance_line_info(c);
        }

        if (ch == '/' and src[at + 1] == '/') {
            while (ch != 0 and ch != '\n') {
                at += 1;
                ch = src[at];
                advance_line_info(c);
            }
        } else {
            break;
        }
    }

    src = src[at..];
    at = 0;

    var tok = Token{
        .line_info = c.lexer.line_info,
        .as = .End_Of_File,
    };

    if (ch == 0) {
        // leave.
    } else if (is_digit(ch)) {
        while (true) {
            at += 1;
            ch = src[at];
            advance_line_info(c);
            if (!is_digit(ch)) break;
        }

        const text = src[0..at];
        const value = std.fmt.parseInt(u64, text, 10) catch {
            c.report_error(tok.line_info, "integer literal '{s}' is too big (> 64 bits).", .{text});
            Compiler.exit(1);
        };

        tok.as = .{ .Integer = value };
    } else if (is_alpha(ch) or ch == '_') {
        while (true) {
            at += 1;
            ch = src[at];
            advance_line_info(c);
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

            const new_text = c.string_pool.insert(text);

            break :as .{ .Identifier = new_text };
        };
    } else if (ch == '#' and (is_alpha(src[at + 1]) or src[at + 1] == '_')) {
        while (true) {
            at += 1;
            ch = src[at];
            advance_line_info(c);

            if (!is_alnum(ch) and ch != '_') break;
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

        const text = src[0..at];

        tok.as = as: {
            for (state.keywords) |keyword| {
                if (std.mem.eql(u8, text, keyword.text)) {
                    break :as keyword.as;
                }
            }

            c.report_error(tok.line_info, "invalid keyword '{s}'\n", .{text});
            Compiler.exit(1);
        };
    } else if (!is_print(ch)) {
        c.report_error(tok.line_info, "non-printable character with code '{}'\n", .{ch});
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
                if (utils.is_prefix(symbol.text, src)) {
                    const count: u32 = @intCast(symbol.text.len);
                    at += count;
                    c.lexer.line_info.column += count;
                    c.lexer.line_info.offset += count;
                    break :as symbol.as;
                }
            }

            c.report_error(tok.line_info, "unrecognized character '{c}'\n", .{src[at]});
            Compiler.exit(1);
        };
    }

    std.debug.assert(c.lexer.token_count < LOOKAHEAD);
    const index = (c.lexer.token_start + c.lexer.token_count) % LOOKAHEAD;
    c.lexer.buffer[index] = tok;
    c.lexer.token_count += 1;
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
