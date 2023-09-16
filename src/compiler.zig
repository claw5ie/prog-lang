const std = @import("std");
const utils = @import("utils.zig");

var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
var gpa = general_purpose_allocator.allocator();

const Compiler = struct {
    const LOOKAHEAD = 2;

    tokens: [LOOKAHEAD]Token = undefined,
    token_start: u8 = 0,
    token_count: u8 = 0,
    filepath: []const u8,
    source_code: [:0]u8,
    line_info: LineInfo = .{},

    pub fn advance(c: *Compiler) void {
        c.token_start += 1;
        c.token_start %= LOOKAHEAD;
        c.token_count -= 1;
    }

    pub fn next(c: *Compiler) Token {
        if (c.token_count == 0) {
            c.buffer_token();
        }

        return c.tokens[c.token_start];
    }

    fn buffer_token(c: *Compiler) void {
        var text = c.source_code;
        var i: usize = c.line_info.offset;

        while (text[i] != 0) {
            while (std.ascii.isWhitespace(text[i])) : (i += 1) {
                c.advance_line_info();
            }

            if (text[i] == '/' and text[i + 1] == '/') {
                while (text[i] != 0 and text[i] != '\n') : (i += 1) {
                    c.advance_line_info();
                }
            } else {
                break;
            }
        }

        var token = Token{
            .tag = .End_Of_File,
            .text = text[i..i],
            .line_info = c.line_info,
        };

        if (text[i] == 0) {
            // leave.
        } else if (std.ascii.isDigit(text[i])) {
            while (std.ascii.isDigit(text[i])) : (i += 1) {
                c.advance_line_info();
            }

            token.tag = .Integer;
            token.text.len = i - token.line_info.offset;
        } else if (std.ascii.isAlphabetic(text[i]) or text[i] == '_') {
            while (std.ascii.isAlphanumeric(text[i]) or text[i] == '_') : (i += 1) {
                c.advance_line_info();
            }

            token.tag = .Identifier;
            token.text.len = i - token.line_info.offset;

            const ReservedKeyword = struct {
                text: []const u8,
                tag: TokenTag,
            };

            const keywords = [_]ReservedKeyword{
                .{ .text = "print", .tag = .Print },
                .{ .text = "proc", .tag = .Proc },
                .{ .text = "return", .tag = .Return },
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
                .{ .text = "+", .tag = .Token_Add },
                .{ .text = "(", .tag = .Open_Paren },
                .{ .text = ")", .tag = .Close_Paren },
                .{ .text = "{", .tag = .Open_Curly },
                .{ .text = "}", .tag = .Close_Curly },
                .{ .text = ":=", .tag = .Colon_Equal },
                .{ .text = ";", .tag = .Semicolon },
                .{ .text = ",", .tag = .Comma },
            };

            var found_symbol = false;

            for (symbols) |symbol| {
                if (utils.is_prefix(symbol.text, text[i..])) {
                    found_symbol = true;
                    i += symbol.text.len;
                    c.line_info.column += symbol.text.len;
                    c.line_info.offset += symbol.text.len;

                    token.tag = symbol.tag;
                    token.text.len = symbol.text.len;
                    break;
                }
            }

            if (!found_symbol) {
                std.debug.print("{s}:{}:{}: error: unrecognized character '{c}'.\n", .{ c.filepath, token.line_info.line, token.line_info.column, text[i] });
                std.os.exit(1);
            }
        }

        std.debug.assert(c.token_count < LOOKAHEAD);
        var index = (c.token_start + c.token_count) % LOOKAHEAD;
        c.tokens[index] = token;
        c.token_count += 1;
    }

    fn advance_line_info(c: *Compiler) void {
        c.line_info.column += 1;
        c.line_info.offset += 1;
        if (c.source_code[c.line_info.offset - 1] == '\n') {
            c.line_info.line += 1;
            c.line_info.column = 1;
        }
    }
};

const LineInfo = struct {
    line: usize = 1,
    column: usize = 1,
    offset: usize = 0,
};

const TokenTag = enum {
    Token_Add,

    Open_Paren,
    Close_Paren,
    Open_Curly,
    Close_Curly,
    Colon_Equal,
    Semicolon,
    Comma,

    Print,
    Proc,
    Return,

    Identifier,
    Integer,

    End_Of_File,
};

const Token = struct {
    tag: TokenTag,
    text: []const u8,
    line_info: LineInfo,
};

pub fn compile() void {
    var filepath = "examples/debug";
    var source_code = utils.read_entire_file(gpa, filepath) catch {
        std.debug.print("error: failed to read file '{s}'.\n", .{filepath});
        std.os.exit(1);
    };

    var compiler = Compiler{
        .filepath = filepath,
        .source_code = source_code,
    };

    while (true) {
        var token = compiler.next();
        compiler.advance();

        std.debug.print("{}\n", .{token});

        if (token.tag == .End_Of_File) {
            break;
        }
    }

    gpa.free(source_code);
    std.debug.assert(general_purpose_allocator.deinit() == .ok);
}
