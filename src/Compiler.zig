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

pub fn compile() void {
    const IR = @import("IR.zig");
    const Interpreter = @import("Interpreter.zig");

    const options = parse_cmd_options();

    switch (options.mode) {
        .Build => {
            const Parser = @import("Parser.zig");
            const TypeChecker = @import("TypeChecker.zig");
            const IRGenerator = @import("IRGenerator.zig");

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

            const source_code = nostd.read_entire_file(nostd.general_allocator, input_filepath) catch {
                report_simple_fatal_error("{s}: failed to open the file", .{input_filepath});
            };

            var compiler = Compiler{
                .string_pool = StringPool.init(nostd.general_allocator),
                .filepath = input_filepath,
                .source_code = source_code,
                .had_error = false,
            };
            defer compiler.string_pool.deinit();
            defer nostd.general_allocator.free(compiler.source_code);

            var ast = Parser.parse(&compiler);
            var ir = IR.init();
            var ir_generator = IRGenerator.init(&ast, &ir);

            TypeChecker.check(&compiler, &ast, &ir_generator);
            ir_generator.generate();
            ir.write_to_file(output_filepath);

            ir_generator.deinit();
            ir.deinit();
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
}

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
                report_simple_error("unrecognized option '{s}'", .{arg});
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
                            report_simple_error("output filepath was already provided ('{s}')", .{path});
                        } else if (args.next()) |path| {
                            options.has_output_filepath = path;
                        } else {
                            had_error = true;
                            report_simple_error("expected argument for '-o' option", .{});
                        }
                    },
                    else => {
                        had_error = true;
                        report_simple_error("unrecognized option '{s}'", .{arg});
                    },
                }
            }
        } else {
            if (options.has_input_filepath) |filepath| {
                had_error = true;
                report_simple_error("filepath was already given ('{s}')", .{filepath});
            } else {
                options.has_input_filepath = arg;
            }
        }
    }

    if (activated_modes_count > 1) {
        had_error = true;
        report_simple_error("only one options needs to be enabled", .{});
    }

    if (options.has_input_filepath == null) {
        had_error = true;
        report_simple_error("no file supplied", .{});
    }

    if (had_error) {
        exit(1);
    }

    return options;
}

pub const FilePosition = nostd.FilePosition;

pub fn report_error(c: *Compiler, position: FilePosition, comptime format: []const u8, args: anytype) void {
    c.had_error = true;
    nostd.eprint("{s}:{}:{}: error: " ++ format ++ "\n", .{ c.filepath, position.line, position.column } ++ args);
}

pub fn report_fatal_error(c: *Compiler, position: FilePosition, comptime format: []const u8, args: anytype) noreturn {
    report_error(c, position, format, args);
    exit(1);
}

pub fn report_note(c: Compiler, position: FilePosition, comptime format: []const u8, args: anytype) void {
    eprint("{s}:{}:{}: note: " ++ format ++ "\n", .{ c.filepath, position.line, position.column } ++ args);
}

const eprint = nostd.eprint;
const exit = nostd.exit;
const report_simple_error = nostd.report_error;
const report_simple_fatal_error = nostd.report_fatal_error;

const std = @import("std");
const nostd = @import("nostd.zig");

pub const Stage = enum {
    None,
    Going,
    Done,
};
