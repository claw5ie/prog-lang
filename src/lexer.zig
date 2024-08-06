const std = @import("std");
const utils = @import("utils.zig");
const common = @import("common.zig");

const LineInfo = common.LineInfo;

pub const LOOKAHEAD = 2;

tokens: [LOOKAHEAD]Token = undefined,
token_start: u8 = 0,
token_count: u8 = 0,
filepath: []const u8,
source_code: [:0]u8,
line_info: LineInfo = .{},

const This = @This();

pub const TokenTag = enum {
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
    Arrow,
    Colon_Equal,
    Colon,
    Semicolon,
    Equal,
    Dot,
    Comma,

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

    Struct,
    Union,
    Enum,
    Proc,
    Void,
    Bool,
    Int,
    Cast,

    False,
    True,

    Null,

    Identifier,
    Integer,

    End_Of_File,
};

pub const Token = struct {
    tag: TokenTag,
    text: []const u8,
    line_info: LineInfo,
};

pub fn init(filepath: []const u8) This {
    const source_code = utils.read_entire_file(common.gpa, filepath) catch {
        std.debug.print("error: failed to read file '{s}'.\n", .{filepath});
        std.posix.exit(1);
    };

    return .{
        .filepath = filepath,
        .source_code = source_code,
    };
}

pub fn deinit(l: *This) void {
    common.gpa.free(l.source_code);
}

pub fn advance(l: *This) void {
    l.token_start += 1;
    l.token_start %= LOOKAHEAD;
    l.token_count -= 1;
}

pub fn advance_many(l: *This, count: u8) void {
    l.token_start += count;
    l.token_start %= LOOKAHEAD;
    l.token_count -= count;
}

pub fn grab(l: *This) Token {
    if (l.token_count == 0) {
        l.buffer_token();
    }

    return l.tokens[l.token_start];
}

pub fn peek(l: *This) TokenTag {
    if (l.token_count == 0) {
        l.buffer_token();
    }

    return l.tokens[l.token_start].tag;
}

pub fn peek_ahead(l: *This, index: u8) TokenTag {
    std.debug.assert(index < LOOKAHEAD);

    while (index >= l.token_count) {
        l.buffer_token();
    }

    return l.tokens[(l.token_start + index) % LOOKAHEAD].tag;
}

pub fn expect(l: *This, expected: TokenTag) void {
    if (l.peek() != expected) {
        const token = l.grab();
        common.print_error(l.filepath, token.line_info, "expected {s}, but got {s}.", .{ token_tag_to_text(expected), token_tag_to_text(token.tag) });
        std.posix.exit(1);
    }
    l.advance();
}

pub fn advance_line_info(l: *This) void {
    l.line_info.column += 1;
    l.line_info.offset += 1;
    if (l.source_code[l.line_info.offset - 1] == '\n') {
        l.line_info.line += 1;
        l.line_info.column = 1;
    }
}

fn buffer_token(l: *This) void {
    var text = l.source_code;
    var i: usize = l.line_info.offset;

    while (text[i] != 0) {
        while (std.ascii.isWhitespace(text[i])) : (i += 1) {
            l.advance_line_info();
        }

        if (text[i] == '/' and text[i + 1] == '/') {
            while (text[i] != 0 and text[i] != '\n') : (i += 1) {
                l.advance_line_info();
            }
        } else {
            break;
        }
    }

    var token = Token{
        .tag = .End_Of_File,
        .text = text[i..i],
        .line_info = l.line_info,
    };

    if (text[i] == 0) {
        // leave.
    } else if (std.ascii.isDigit(text[i])) {
        while (std.ascii.isDigit(text[i])) : (i += 1) {
            l.advance_line_info();
        }

        token.tag = .Integer;
        token.text.len = i - token.line_info.offset;
    } else if (std.ascii.isAlphabetic(text[i]) or text[i] == '_') {
        while (std.ascii.isAlphanumeric(text[i]) or text[i] == '_') : (i += 1) {
            l.advance_line_info();
        }

        token.tag = .Identifier;
        token.text.len = i - token.line_info.offset;

        const ReservedKeyword = struct {
            text: []const u8,
            tag: TokenTag,
        };

        const keywords = [_]ReservedKeyword{
            .{ .text = "print", .tag = .Print },
            .{ .text = "if", .tag = .If },
            .{ .text = "then", .tag = .Then },
            .{ .text = "else", .tag = .Else },
            .{ .text = "while", .tag = .While },
            .{ .text = "do", .tag = .Do },
            .{ .text = "break", .tag = .Break },
            .{ .text = "continue", .tag = .Continue },
            .{ .text = "switch", .tag = .Switch },
            .{ .text = "return", .tag = .Return },
            .{ .text = "struct", .tag = .Struct },
            .{ .text = "union", .tag = .Union },
            .{ .text = "enum", .tag = .Enum },
            .{ .text = "proc", .tag = .Proc },
            .{ .text = "void", .tag = .Void },
            .{ .text = "bool", .tag = .Bool },
            .{ .text = "int", .tag = .Int },
            .{ .text = "cast", .tag = .Cast },
            .{ .text = "false", .tag = .False },
            .{ .text = "true", .tag = .True },
            .{ .text = "null", .tag = .Null },
        };

        for (keywords) |keyword| {
            if (std.mem.eql(u8, token.text, keyword.text)) {
                token.tag = keyword.tag;
                break;
            }
        }
    } else {
        const ReservedSymbol = struct {
            text: []const u8,
            tag: TokenTag,
        };

        const symbols = [_]ReservedSymbol{
            .{ .text = "||", .tag = .Or },
            .{ .text = "&&", .tag = .And },
            .{ .text = "==", .tag = .Eq },
            .{ .text = "!=", .tag = .Neq },
            .{ .text = "<=", .tag = .Leq },
            .{ .text = ">=", .tag = .Geq },
            .{ .text = "->", .tag = .Arrow },
            .{ .text = ":=", .tag = .Colon_Equal },
            .{ .text = "<", .tag = .Lt },
            .{ .text = ">", .tag = .Gt },
            .{ .text = "+", .tag = .Add },
            .{ .text = "-", .tag = .Sub },
            .{ .text = "*", .tag = .Mul },
            .{ .text = "/", .tag = .Div },
            .{ .text = "%", .tag = .Mod },
            .{ .text = "&", .tag = .Ref },
            .{ .text = "!", .tag = .Not },
            .{ .text = "(", .tag = .Open_Paren },
            .{ .text = ")", .tag = .Close_Paren },
            .{ .text = "{", .tag = .Open_Curly },
            .{ .text = "}", .tag = .Close_Curly },
            .{ .text = "[", .tag = .Open_Bracket },
            .{ .text = "]", .tag = .Close_Bracket },
            .{ .text = ":", .tag = .Colon },
            .{ .text = ";", .tag = .Semicolon },
            .{ .text = "=", .tag = .Equal },
            .{ .text = ".", .tag = .Dot },
            .{ .text = ",", .tag = .Comma },
        };

        var found_symbol = false;

        for (symbols) |symbol| {
            if (utils.is_prefix(symbol.text, text[i..])) {
                found_symbol = true;
                i += symbol.text.len;
                l.line_info.column += symbol.text.len;
                l.line_info.offset += symbol.text.len;

                token.tag = symbol.tag;
                token.text.len = symbol.text.len;
                break;
            }
        }

        if (!found_symbol) {
            common.print_error(l.filepath, token.line_info, "unrecognized character '{c}'.\n", .{text[i]});
            std.posix.exit(1);
        }
    }

    std.debug.assert(l.token_count < LOOKAHEAD);
    const index = (l.token_start + l.token_count) % LOOKAHEAD;
    l.tokens[index] = token;
    l.token_count += 1;
}

fn token_tag_to_text(tag: TokenTag) []const u8 {
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
        .Arrow => "'->'",
        .Colon_Equal => "':='",
        .Colon => "':'",
        .Semicolon => "';'",
        .Equal => "'='",
        .Dot => "'.'",
        .Comma => "','",
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
        .Struct => "'struct'",
        .Union => "'union'",
        .Enum => "'enum'",
        .Proc => "'proc'",
        .Void => "'void'",
        .Bool => "'bool'",
        .Int => "'int'",
        .Cast => "'@cast'",
        .False => "'false'",
        .True => "'true'",
        .Null => "'null'",
        .Identifier => "identifier",
        .Integer => "integer",
        .End_Of_File => "EOF",
    };
}
