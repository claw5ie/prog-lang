symbol_table: SymbolTable,
string_pool: StringPool,
filepath: [:0]const u8,
source_code: [:0]u8,
had_error: bool,

const Compiler = @This();

pub const StringPool = nostd.StringPool;

pub const magic_number_string: []const u8 = "PROGLANG";
pub const magic_number_value: u64 = magic_number_value: {
    const ptr: *const u64 = @alignCast(@ptrCast(magic_number_string.ptr));
    break :magic_number_value ptr.*;
};

fn init() Compiler {
    const source_code = nostd.general_allocator.allocSentinel(u8, 0, 0) catch {
        Compiler.exit(1);
    };
    return .{
        .symbol_table = SymbolTable.init(nostd.general_allocator),
        .string_pool = StringPool.init(nostd.general_allocator),
        .filepath = &[0:0]u8{},
        .source_code = source_code,
        .had_error = false,
    };
}

fn deinit(c: *Compiler) void {
    c.symbol_table.deinit();
    c.string_pool.deinit();
    nostd.general_allocator.free(c.source_code);
}

pub fn compile() void {
    var c = Compiler.init();

    const options = parse_cmd_options();
    switch (options.mode) {
        .Build => {
            var buffer = [1]u8{0} ** std.posix.PATH_MAX;

            const input_filepath = options.has_input_filepath.?;
            const output_filepath: [:0]const u8 = output_filepath: {
                if (options.has_output_filepath) |path| {
                    @memcpy(buffer[0..path.len], path);
                    break :output_filepath buffer[0..path.len :0];
                } else {
                    const extension: []const u8 = ".ir";
                    @memcpy(buffer[0..input_filepath.len], input_filepath);
                    @memcpy(buffer[input_filepath.len .. input_filepath.len + extension.len], extension);
                    break :output_filepath buffer[0 .. input_filepath.len + extension.len :0];
                }
            };

            nostd.general_allocator.free(c.source_code);
            c.filepath = input_filepath;
            c.source_code = nostd.read_entire_file(nostd.general_allocator, input_filepath) catch {
                Compiler.eprint("error: failed to read from a file '{s}'\n", .{input_filepath});
                Compiler.exit(1);
            };

            var ast = Ast.init(&c);
            var parser = Parser.init(&c, &ast);
            var ir = IR.init();
            var ir_generator = IRGenerator.init(&ast, &ir);

            parser.parse();
            TypeChecker.check(&c, &ast, &ir_generator);
            ir_generator.generate();
            ir.write_to_file(output_filepath);

            ir_generator.deinit();
            ir.deinit();
            parser.deinit();
            ast.deinit();
        },
        .Run => {
            var ir = IR.read_from_file(options.has_input_filepath.?);
            var interp = Interpreter.init(&ir);

            interp.interpret();

            interp.deinit();
            ir.deinit();
        },
        .Print => {
            var ir = IR.read_from_file(options.has_input_filepath.?);
            ir.print();

            ir.deinit();
        },
    }

    c.deinit();
}

fn parse_cmd_options() Options {
    var options = Options{};
    var activated_modes_count: u32 = 0;
    var had_error = false;
    var args = std.process.args();

    _ = args.next().?;
    while (args.next()) |arg| {
        if (arg[0] == '-') {
            if (arg.len > 2) {
                had_error = true;
                eprint("error: unrecognized option '{s}'\n", .{arg});
            } else {
                switch (arg[1]) {
                    'r' => {
                        activated_modes_count += 1;
                        options.mode = .Run;
                    },
                    'p' => {
                        activated_modes_count += 1;
                        options.mode = .Print;
                    },
                    'o' => {
                        if (options.has_output_filepath) |path| {
                            had_error = true;
                            eprint("error: output filepath was already provided ('{s}')\n", .{path});
                        } else if (args.next()) |path| {
                            options.has_output_filepath = path;
                        } else {
                            had_error = true;
                            eprint("error: expected argument for '-o' option\n", .{});
                        }
                    },
                    else => {
                        had_error = true;
                        eprint("error: unrecognized option '{s}'\n", .{arg});
                    },
                }
            }
        } else {
            if (options.has_input_filepath) |filepath| {
                had_error = true;
                eprint("error: filepath was already given ('{s}')\n", .{filepath});
            } else {
                options.has_input_filepath = arg;
            }
        }
    }

    if (activated_modes_count > 1) {
        had_error = true;
        eprint("error: only one options needs to be enabled\n", .{});
    }

    if (options.has_input_filepath == null) {
        had_error = true;
        eprint("error: no file supplied\n", .{});
    }

    if (had_error) {
        exit(1);
    }

    return options;
}

pub fn find_symbol_in_scope(c: *Compiler, key: Ast.Symbol.Key, skip_local_variables: bool, offset: usize) ?*Ast.Symbol {
    const has_symbol = c.symbol_table.find(key);

    if (has_symbol) |symbol| {
        switch (symbol.as) {
            .Variable, .Parameter => {
                if (symbol.attributes.is_const or symbol.attributes.is_static or (symbol.line_info.offset < offset and !skip_local_variables)) {
                    return symbol;
                }
            },
            .Procedure,
            .Struct_Field,
            .Union_Field,
            .Enum_Field,
            .Type,
            => return symbol,
        }
    }

    return null;
}

pub fn find_symbol(c: *Compiler, key: Ast.Symbol.Key, offset: usize) ?*Ast.Symbol {
    return find_symbol_with_scope_bound(c, key, &Ast.global_scope, offset);
}

pub fn find_symbol_with_scope_bound(c: *Compiler, key: Ast.Symbol.Key, scope_bound: *Ast.Scope, offset: usize) ?*Ast.Symbol {
    var skip_local_variables = false;
    var k = key;
    while (true) {
        const has_symbol = find_symbol_in_scope(c, k, skip_local_variables, offset);
        if (has_symbol) |symbol| {
            return symbol;
        } else if (k.scope.parent) |parent| {
            skip_local_variables = skip_local_variables or k.scope == scope_bound;
            k.scope = parent;
        } else {
            break;
        }
    }
    return null;
}

pub fn report_error(c: *Compiler, line_info: LineInfo, comptime format: []const u8, args: anytype) void {
    c.had_error = true;
    eprint("{s}:{}:{}: error: " ++ format ++ "\n", .{ c.filepath, line_info.line, line_info.column } ++ args);
}

pub fn report_note(c: *Compiler, line_info: LineInfo, comptime format: []const u8, args: anytype) void {
    eprint("{s}:{}:{}: note: " ++ format ++ "\n", .{ c.filepath, line_info.line, line_info.column } ++ args);
}

const std = @import("std");
const nostd = @import("nostd.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Ast = @import("Ast.zig");
const TypeChecker = @import("TypeChecker.zig");
const IR = @import("IR.zig");
const IRGenerator = @import("IRGenerator.zig");
const Interpreter = @import("Interpreter.zig");

const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const Alignment = nostd.Alignment;
pub const oprint = nostd.oprint;
pub const eprint = nostd.eprint;
pub const exit = nostd.exit;

const Options = struct {
    has_input_filepath: ?[:0]const u8 = null,
    has_output_filepath: ?[:0]const u8 = null,
    mode: Mode = .Build,

    const Mode = enum {
        Build,
        Run,
        Print,
    };
};

pub const LineInfo = struct {
    line: u32,
    column: u32,
    offset: usize,
};

pub const Attributes = packed struct {
    is_const: bool = false,
    is_static: bool = false,
    is_global: bool = false,

    pub fn is_empty(attr: Attributes) bool {
        const ptr: *const u8 = @ptrCast(&attr);
        return ptr.* == 0;
    }

    pub fn combine(self: *Attributes, other: Attributes) void {
        const d: *u8 = @ptrCast(self);
        const s: *const u8 = @ptrCast(&other);
        d.* |= s.*;
    }
};

pub const SymbolTable = struct {
    map: HashMap,

    pub inline fn init(allocator: Allocator) SymbolTable {
        return .{ .map = HashMap.init(allocator) };
    }

    pub fn deinit(table: *SymbolTable) void {
        table.map.deinit();
    }

    pub fn insert(table: *SymbolTable, key: Key) InsertResult {
        return table.map.getOrPut(key) catch {
            exit(1);
        };
    }

    pub inline fn find(table: *SymbolTable, key: Key) ?Value {
        return table.map.get(key);
    }

    pub const Key = Ast.Symbol.Key;
    pub const Value = *Ast.Symbol;

    pub const InsertResult = HashMap.GetOrPutResult;

    pub const Context = struct {
        pub fn hash(_: Context, key: Key) u64 {
            const MurMur = std.hash.Murmur2_64;

            const h0 = MurMur.hash(key.name);
            const h1 = MurMur.hashUint64(@intFromPtr(key.scope));

            return h0 +% 33 *% h1;
        }

        pub fn eql(_: Context, k0: Key, k1: Key) bool {
            return k0.scope == k1.scope and std.mem.eql(u8, k0.name, k1.name);
        }
    };

    pub const HashMap = std.HashMap(Key, Value, Context, 80);
};

pub const Stage = enum {
    None,
    Going,
    Done,
};
