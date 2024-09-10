const std = @import("std");
const utils = @import("utils.zig");
const Compiler = @import("compiler.zig");

const Ast = Compiler.Ast;
const IRC = Compiler.IRC;
const Alignment = utils.Alignment;

pub fn generate_ir(c: *Compiler) void {
    generate_ir_top_level(c);

    c.irc.label_count = c.ircgen.next_label;
    c.irc.globals_count = c.ircgen.next_global;
}

fn generate_ir_top_level(c: *Compiler) void {
    {
        const Procedure = &c.ast.main.?.as.Procedure;
        const dst = grab_global(c, 8, .QWORD).as_lvalue();
        generate_ir_instr(c, .{ .Call = .{
            .dst = dst,
            .src = .{ .Label = Procedure.start_label.? },
        } });
        generate_ir_instr(c, .Exit);
    }

    var it = c.ast.globals.first;
    while (it) |node| {
        generate_ir_global_symbol(c, node.data);
        it = node.next;
    }
}

fn generate_ir_global_symbol(c: *Compiler, symbol: *Ast.Symbol) void {
    switch (symbol.as) {
        .Variable => {
            std.debug.print("{s}:{}:{}: ewwow: oopsie doopsie, cawn't cweate gwobaw vawiabwes fow now, UwU\n", .{ c.filepath, symbol.line_info.line, symbol.line_info.column });
            Compiler.exit(69);

            // const dst = grab_global_from_type(c, Variable.typ.?.data).as_lvalue();
            // Variable.storage = dst;

            // if (Variable.value) |value| {
            //     _ = generate_ir_rvalue(c, dst, value);
            // }
        },
        .Procedure => |*Procedure| {
            const Proc = &Procedure.typ.data.as.Proc;

            var offset: i64 = -IRC.FIRST_PARAM_OFFSET;
            var it = Proc.params.first;
            while (it) |node| {
                const Parameter = &node.data.as.Parameter;

                const size = utils.align_u64(Parameter.typ.data.byte_size, .QWORD);
                offset -= @intCast(size);
                Parameter.storage = .{ .Tmp = .{
                    .offset = .{ .Local = offset },
                    .size = size,
                } };

                it = node.next;
            }

            const start_label = Procedure.start_label.?;
            const end_label = Procedure.end_label.?;

            const old_return_label = c.ircgen.return_label;
            c.ircgen.return_label = end_label;

            const index = c.irc.instrs.items.len;
            generate_ir_instr(c, undefined);
            generate_ir_stmt_list(c, Procedure.block);
            generate_ir_instr(c, .{ .GFE = .{
                .label = end_label,
                .stack_space_used = @intCast(c.ircgen.biggest_next_local),
            } });
            c.irc.instrs.items[index] = .{ .GFB = .{
                .label = start_label,
                .stack_space_used = @intCast(c.ircgen.biggest_next_local),
            } };
            c.ircgen.next_local = 0; // Global function, so there the previous value should have been 0 as well.
            c.ircgen.biggest_next_local = 0;

            c.ircgen.return_label = old_return_label;
        },
        .Parameter, .Struct_Field, .Union_Field, .Enum_Field => unreachable,
        .Type => {},
    }
}

fn generate_ir_local_symbol(c: *Compiler, symbol: *Ast.Symbol) void {
    switch (symbol.as) {
        .Variable => |*Variable| {
            const dst = grab_local_from_type(c, Variable.typ.?.data).as_lvalue();
            Variable.storage = dst;

            if (Variable.value) |value| {
                _ = generate_ir_rvalue(c, dst, value);
            }
        },
        .Procedure, .Parameter, .Struct_Field, .Union_Field, .Enum_Field => unreachable,
        .Type => {},
    }
}

fn generate_ir_stmt(c: *Compiler, stmt: *Ast.Stmt) void {
    switch (stmt.as) {
        .Print => |expr| {
            const src = generate_ir_rvalue(c, null, expr);
            switch (expr.typ.data.as) {
                .Enum => {
                    generate_ir_instr(c, .{ .Print = .{
                        .Integer = .{
                            .src = src,
                            .is_signed = expr.typ.is_signed(),
                        },
                    } });
                },
                .Pointer => {
                    generate_ir_instr(c, .{ .Print = .{
                        .Pointer = src,
                    } });
                },
                .Integer => {
                    generate_ir_instr(c, .{ .Print = .{
                        .Integer = .{
                            .src = src,
                            .is_signed = expr.typ.is_signed(),
                        },
                    } });
                },
                .Bool => {
                    generate_ir_instr(c, .{ .Print = .{
                        .Boolean = src,
                    } });
                },
                .Struct,
                .Union,
                .Proc,
                .Array,
                .Void,
                .Identifier,
                .Type_Of,
                => unreachable,
            }
        },
        .Block => |block| {
            // Clean locals allocated on the stack.
            const old_next_local = c.ircgen.next_local;
            generate_ir_stmt_list(c, block);
            c.ircgen.next_local = old_next_local;
        },
        .If => |If| {
            if (If.false_branch) |false_branch| {
                const false_branch_label = grab_label(c);
                const end_label = grab_label(c);

                generate_ir_jmpc(c, If.condition, false_branch_label, false);

                _ = generate_ir_stmt(c, If.true_branch);
                generate_ir_instr(c, .{ .Jmp = end_label });
                generate_ir_instr(c, .{ .Label = false_branch_label });
                _ = generate_ir_stmt(c, false_branch);
                generate_ir_instr(c, .{ .Label = end_label });
            } else {
                const end_label = grab_label(c);
                generate_ir_jmpc(c, If.condition, end_label, false);

                _ = generate_ir_stmt(c, If.true_branch);
                generate_ir_instr(c, .{ .Label = end_label });
            }
        },
        .While, .Do_While => |While| {
            const start_label = grab_label(c);
            const condition_label = grab_label(c);
            const end_label = grab_label(c);

            const old_loop_condition_label = c.ircgen.loop_condition_label;
            const old_loop_end_label = c.ircgen.loop_end_label;

            c.ircgen.loop_condition_label = condition_label;
            c.ircgen.loop_end_label = end_label;

            if (stmt.as == .While) {
                generate_ir_instr(c, .{ .Jmp = condition_label });
            }
            generate_ir_instr(c, .{ .Label = start_label });
            generate_ir_stmt(c, While.body);
            generate_ir_instr(c, .{ .Label = condition_label });
            generate_ir_jmpc(c, While.condition, start_label, true);
            generate_ir_instr(c, .{ .Label = end_label });

            c.ircgen.loop_condition_label = old_loop_condition_label;
            c.ircgen.loop_end_label = old_loop_end_label;
        },
        .Break => {
            generate_ir_instr(c, .{ .Jmp = c.ircgen.loop_end_label.? });
        },
        .Continue => {
            generate_ir_instr(c, .{ .Jmp = c.ircgen.loop_condition_label.? });
        },
        .Switch => |Switch| {
            const condition = generate_ir_rvalue(c, null, Switch.condition);
            const is_signed = Switch.condition.typ.is_signed();

            const first_label = grab_many_labels(c, Switch.cases.len);
            const end_label = grab_label(c);

            {
                var label = first_label;
                var it = Switch.cases.first;
                while (it) |node| {
                    var case = node.data;
                    while (true) {
                        switch (case.*) {
                            .Case => |Case| {
                                const src = generate_ir_rvalue(c, null, Case.value);
                                generate_ir_instr(c, .{ .Jmpc = .{
                                    .src0 = condition,
                                    .src1 = src,
                                    .label = label,
                                    .tag = .Eq,
                                    .is_signed = is_signed,
                                } });
                                case = Case.subcase;
                            },
                            .Stmt => break,
                        }
                    }
                    label += 1;

                    it = node.next;
                }
            }

            if (Switch.default_case) |else_stmt| {
                generate_ir_stmt(c, else_stmt);
            }

            generate_ir_instr(c, .{ .Jmp = end_label });

            {
                var label = first_label;
                var it = Switch.cases.first;
                while (it) |node| {
                    var case = node.data;
                    while (true) {
                        switch (case.*) {
                            .Case => |Case| case = Case.subcase,
                            .Stmt => |substmt| {
                                generate_ir_instr(c, .{ .Label = label });
                                generate_ir_stmt(c, substmt);
                                generate_ir_instr(c, .{ .Jmp = end_label });
                                break;
                            },
                        }
                    }
                    label += 1;

                    it = node.next;
                }
            }

            generate_ir_instr(c, .{ .Label = end_label });
        },
        .Return => |has_expr| {
            if (has_expr) |expr| {
                const dst = IRC.Lvalue{
                    .Mem = .{
                        .base = .{
                            .offset = .{ .Local = -IRC.FIRST_PARAM_OFFSET },
                            .size = 8,
                        },
                        .offset = 0,
                        .size = expr.typ.data.byte_size,
                    },
                };
                const src = generate_ir_rvalue(c, null, expr); // Can't move directly to 'dst', because of pointer aliasing. Example: src = foo(&src) may not generate correct code.
                generate_ir_instr(c, .{ .Ret1 = .{
                    .dst = dst,
                    .src = src,
                    .label = c.ircgen.return_label.?,
                } });
            } else {
                generate_ir_instr(c, .{ .Ret0 = c.ircgen.return_label.? });
            }
        },
        .Symbol => |symbol| {
            generate_ir_local_symbol(c, symbol);
        },
        .Assign => |Assign| {
            const dst = generate_ir_rvalue(c, null, Assign.lhs);
            _ = generate_ir_rvalue(c, dst.Lvalue, Assign.rhs);
        },
        .Expr => |expr| {
            _ = generate_ir_rvalue(c, null, expr);
        },
    }
}

fn generate_ir_stmt_list(c: *Compiler, list: Ast.StmtList) void {
    var it = list.first;
    while (it) |node| {
        generate_ir_stmt(c, node.data);
        it = node.next;
    }
}

fn generate_ir_rvalue(c: *Compiler, has_dst: ?IRC.Lvalue, expr: *Ast.Expr) IRC.Rvalue {
    var move_src = false;
    const src: IRC.Rvalue = src: {
        switch (expr.as) {
            .Binary_Op => |Binary_Op| {
                const src0 = generate_ir_rvalue(c, null, Binary_Op.lhs);
                const src1 = generate_ir_rvalue(c, null, Binary_Op.rhs);

                const dst = maybe_grab_local_from_type(c, has_dst, expr.typ.data);

                const tag: IRC.Instr.BinaryOp.Tag = switch (Binary_Op.tag) {
                    .Or, .And => unreachable,
                    .Eq => .Eq,
                    .Neq => .Neq,
                    .Lt => .Lt,
                    .Leq => .Leq,
                    .Gt => .Gt,
                    .Geq => .Geq,
                    .Add => .Add,
                    .Sub => .Sub,
                    .Mul => .Mul,
                    .Div => .Div,
                    .Mod => .Mod,
                };

                const is_signed = Binary_Op.lhs.typ.is_signed();
                std.debug.assert(!is_signed or Binary_Op.rhs.typ.is_signed());
                generate_ir_instr(c, .{ .Binary_Op = .{
                    .dst = dst,
                    .src0 = src0,
                    .src1 = src1,
                    .tag = tag,
                    .is_signed = is_signed,
                } });

                break :src dst.as_rvalue();
            },
            .Unary_Op => |Unary_Op| {
                const src = generate_ir_rvalue(c, null, Unary_Op.subexpr);
                const is_signed = Unary_Op.subexpr.typ.is_signed();

                switch (Unary_Op.tag) {
                    .Pos => {
                        move_src = true;
                        break :src src;
                    },
                    .Neg => {
                        const dst = maybe_grab_local_from_type(c, has_dst, expr.typ.data);
                        generate_ir_instr(c, .{ .Unary_Op = .{
                            .dst = dst,
                            .src = src,
                            .tag = .Neg,
                            .is_signed = is_signed,
                        } });
                        break :src dst.as_rvalue();
                    },
                    .Not => {
                        const dst = maybe_grab_local_from_type(c, has_dst, expr.typ.data);
                        generate_ir_instr(c, .{ .Unary_Op = .{
                            .dst = dst,
                            .src = src,
                            .tag = .Not,
                            .is_signed = is_signed,
                        } });
                        break :src dst.as_rvalue();
                    },
                }
            },
            .Ref => |subexpr| {
                move_src = true;
                const src = generate_ir_rvalue(c, null, subexpr).Lvalue;
                break :src .{ .Addr = src };
            },
            .Deref => |subexpr| {
                move_src = true;
                const src = generate_ir_rvalue(c, null, subexpr);
                const new_src = deref_rvalue(c, src, 0, expr.typ.data.byte_size);
                break :src new_src.as_rvalue();
            },
            .If => |If| {
                const false_branch_label = grab_label(c);
                const end_label = grab_label(c);

                generate_ir_jmpc(c, If.condition, false_branch_label, false);
                const dst = maybe_grab_local_from_type(c, has_dst, expr.typ.data);

                _ = generate_ir_rvalue(c, dst, If.true_branch);
                generate_ir_instr(c, .{ .Jmp = end_label });
                generate_ir_instr(c, .{ .Label = false_branch_label });
                _ = generate_ir_rvalue(c, dst, If.false_branch);
                generate_ir_instr(c, .{ .Label = end_label });

                break :src dst.as_rvalue();
            },
            .Field => |Field| {
                move_src = true;

                const offset: u64 = switch (Field.field.as.Symbol.as) {
                    .Struct_Field, .Union_Field => |Struct_Field| Struct_Field.offset,
                    else => unreachable,
                };
                const src = generate_ir_rvalue(c, null, Field.subexpr);

                if (Field.subexpr.typ.data.as != .Pointer) {
                    break :src bump_lvalue(src.Lvalue, offset, expr.typ.data.byte_size).as_rvalue();
                } else {
                    break :src deref_rvalue(c, src, offset, expr.typ.data.byte_size).as_rvalue();
                }
            },
            .Call => |Call| {
                const dst = maybe_grab_local_from_type(c, has_dst, expr.typ.data);
                const old_next_local = c.ircgen.next_local;

                var bytes_pushed: u64 = 0;
                var it = Call.args.last;
                while (it) |node| {
                    const arg = node.data.Expr;
                    const src = generate_ir_rvalue(c, null, arg);
                    generate_ir_instr(c, .{ .Push = src });
                    bytes_pushed += utils.align_u64(arg.typ.data.byte_size, .QWORD);

                    it = node.prev;
                }

                const src = generate_ir_rvalue(c, null, Call.subexpr);
                generate_ir_instr(c, .{ .Call = .{
                    .dst = dst,
                    .src = src,
                } });
                if (bytes_pushed > 0) {
                    generate_ir_instr(c, .{ .Pop = bytes_pushed });
                }

                c.ircgen.next_local = old_next_local;

                return dst.as_rvalue();
            },
            .Constructor => |Constructor| {
                switch (Constructor.typ.data.as) {
                    .Struct, .Union => {
                        const dst = maybe_grab_local_from_type(c, has_dst, expr.typ.data);

                        var it = Constructor.args.first;
                        while (it) |node| {
                            const Designator = &node.data.Designator;
                            var size: u64 = 0;
                            var offset: u64 = 0;
                            switch (Designator.lhs.as.Symbol.as) {
                                .Struct_Field, .Union_Field => |Field| {
                                    offset = Field.offset;
                                    size = Field.typ.data.byte_size;
                                },
                                else => unreachable,
                            }

                            const new_dst = bump_lvalue(dst, offset, size);
                            _ = generate_ir_rvalue(c, new_dst, Designator.rhs);

                            it = node.next;
                        }

                        break :src dst.as_rvalue();
                    },
                    .Array => |Array| {
                        const dst = maybe_grab_local_from_type(c, has_dst, expr.typ.data);

                        var offset: u64 = 0;
                        var it = Constructor.args.first;
                        while (it) |node| {
                            const size = Array.subtype.data.byte_size;
                            const arg = node.data.Expr;

                            const new_dst = bump_lvalue(dst, offset, size);
                            _ = generate_ir_rvalue(c, new_dst, arg);

                            offset += size;
                            offset = utils.align_u64(offset, Array.subtype.data.alignment);
                            it = node.next;
                        }

                        break :src dst.as_rvalue();
                    },
                    .Enum,
                    .Proc,
                    .Pointer,
                    .Integer,
                    .Bool,
                    => {
                        const arg = Constructor.args.first.?.data.Expr;
                        break :src generate_ir_rvalue(c, null, arg);
                    },
                    .Void, .Identifier, .Type_Of => unreachable,
                }
            },
            .Subscript => |Subscript| {
                move_src = true;

                const subexpr = generate_ir_rvalue(c, null, Subscript.subexpr);
                const index = generate_ir_rvalue(c, null, Subscript.index);
                const dst = grab_local(c, 8, .QWORD);

                const offset = utils.align_u64(expr.typ.data.byte_size, expr.typ.data.alignment);
                generate_ir_instr(c, .{
                    .Binary_Op = .{
                        .dst = dst.as_lvalue(),
                        .src0 = index,
                        .src1 = .{ .Imm = offset },
                        .tag = .Mul,
                        .is_signed = false,
                    },
                });

                if (Subscript.subexpr.typ.data.as != .Pointer) {
                    switch (subexpr.Lvalue) {
                        .Tmp => {
                            generate_ir_instr(c, .{ .Binary_Op = .{
                                .dst = dst.as_lvalue(),
                                .src0 = dst.as_rvalue(),
                                .src1 = .{ .Addr = subexpr.Lvalue },
                                .tag = .Add,
                                .is_signed = false,
                            } });

                            break :src .{ .Lvalue = .{
                                .Mem = .{
                                    .base = dst,
                                    .offset = 0,
                                    .size = expr.typ.data.byte_size,
                                },
                            } };
                        },
                        .Mem => |mem| {
                            if (mem.base) |src0| {
                                generate_ir_instr(c, .{ .Binary_Op = .{
                                    .dst = dst.as_lvalue(),
                                    .src0 = dst.as_rvalue(),
                                    .src1 = src0.as_rvalue(),
                                    .tag = .Add,
                                    .is_signed = false,
                                } });
                            }

                            break :src .{ .Lvalue = .{
                                .Mem = .{
                                    .base = dst,
                                    .offset = mem.offset,
                                    .size = expr.typ.data.byte_size,
                                },
                            } };
                        },
                    }
                } else {
                    generate_ir_instr(c, .{ .Binary_Op = .{
                        .dst = dst.as_lvalue(),
                        .src0 = dst.as_rvalue(),
                        .src1 = subexpr,
                        .tag = .Add,
                        .is_signed = false,
                    } });
                    break :src .{ .Lvalue = .{
                        .Mem = .{
                            .base = dst,
                            .offset = 0,
                            .size = expr.typ.data.byte_size,
                        },
                    } };
                }
            },
            .Byte_Size_Of => unreachable,
            .Alignment_Of => unreachable,
            .As, .Cast => |Cast| {
                var typ = Cast.typ;
                if (typ.data.as == .Enum) { // TODO: fix this? Not a good solution if we need to do some enum specific stuff.
                    typ = typ.data.as.Enum.integer_type;
                }
                switch (typ.data.as) {
                    .Struct,
                    .Union,
                    .Array,
                    .Proc,
                    .Pointer,
                    => {
                        move_src = true;
                        break :src generate_ir_rvalue(c, null, Cast.expr);
                    },
                    .Enum => unreachable,
                    .Integer => |dInteger| {
                        const src = generate_ir_rvalue(c, null, Cast.expr);

                        switch (Cast.expr.typ.data.as) {
                            .Integer => |sInteger| {
                                if (dInteger.bits > sInteger.bits and dInteger.is_signed and sInteger.is_signed) {
                                    const dst = maybe_grab_local_from_type(c, has_dst, expr.typ.data);
                                    generate_ir_instr(c, .{ .Movsx = .{
                                        .dst = dst,
                                        .src = src,
                                        .src_bits = sInteger.bits,
                                    } });
                                    break :src dst.as_rvalue();
                                } else {}
                            },
                            else => {},
                        }

                        move_src = true;
                        break :src src;
                    },
                    .Bool => {
                        const dst = maybe_grab_local_from_type(c, has_dst, expr.typ.data);
                        const src = generate_ir_rvalue(c, null, Cast.expr);
                        generate_ir_instr(c, .{ .Setnz = .{
                            .dst = dst,
                            .src = src,
                        } });
                        break :src dst.as_rvalue();
                    },
                    .Void, .Identifier, .Type_Of => unreachable,
                }
            },
            .Integer => |value| {
                move_src = true;
                break :src .{ .Imm = value };
            },
            .Boolean => |value| {
                move_src = true;
                break :src .{ .Imm = @intFromBool(value) };
            },
            .Null => {
                move_src = true;
                break :src .{ .Imm = 0 };
            },
            .Symbol => |symbol| {
                switch (symbol.as) {
                    .Variable => |Variable| {
                        move_src = true;
                        break :src Variable.storage.as_rvalue();
                    },
                    .Parameter => |Parameter| {
                        move_src = true;
                        break :src Parameter.storage.as_rvalue();
                    },
                    .Procedure => |*Procedure| {
                        move_src = true;
                        break :src .{ .Label = Procedure.start_label.? };
                    },
                    .Struct_Field, .Union_Field => unreachable,
                    .Enum_Field => |Enum_Field| {
                        move_src = true;
                        break :src .{ .Imm = Enum_Field.computed_value };
                    },
                    .Type => unreachable,
                }
            },
            .Type, .Identifier => unreachable,
        }
    };

    if (has_dst) |dst| { // TODO: Do I need to sign extend if 'expr.typ' is signed integer?
        if (move_src) {
            generate_ir_instr(c, .{ .Mov = .{
                .dst = dst,
                .src = src,
            } });
            return dst.as_rvalue();
        }
    }

    return src;
}

fn generate_ir_jmpc(c: *Compiler, expr: *Ast.Expr, label: IRC.Label, jump_if_true: bool) void {
    var src0: IRC.Rvalue = undefined;
    var src1: IRC.Rvalue = .{ .Imm = 0 };
    var tag: IRC.Instr.Jmpc.Tag = .Neq;
    var is_signed = false;

    _ = fill: {
        switch (expr.as) {
            .Binary_Op => |Binary_Op| {
                switch (Binary_Op.tag) {
                    .Eq, .Neq, .Lt, .Leq, .Gt, .Geq => {
                        src0 = generate_ir_rvalue(c, null, Binary_Op.lhs);
                        src1 = generate_ir_rvalue(c, null, Binary_Op.rhs);

                        tag = switch (Binary_Op.tag) {
                            .Eq => .Eq,
                            .Neq => .Neq,
                            .Lt => .Lt,
                            .Leq => .Leq,
                            .Gt => .Gt,
                            .Geq => .Geq,
                            else => unreachable,
                        };

                        is_signed = Binary_Op.lhs.typ.is_signed();

                        break :fill;
                    },
                    .Or, .And, .Add, .Sub, .Mul, .Div, .Mod => {},
                }
            },
            .Unary_Op => |Unary_Op| {
                switch (Unary_Op.tag) {
                    .Not => {
                        generate_ir_jmpc(c, Unary_Op.subexpr, label, !jump_if_true);
                        return;
                    },
                    else => {},
                }
            },
            else => {},
        }

        src0 = generate_ir_rvalue(c, null, expr);
    };

    if (!jump_if_true) {
        tag = switch (tag) {
            .Eq => .Neq,
            .Neq => .Eq,
            .Lt => .Geq,
            .Leq => .Gt,
            .Gt => .Leq,
            .Geq => .Lt,
        };
    }

    generate_ir_instr(c, .{ .Jmpc = .{
        .src0 = src0,
        .src1 = src1,
        .label = label,
        .tag = tag,
        .is_signed = is_signed,
    } });
}

fn generate_ir_instr(c: *Compiler, instr: IRC.Instr) void {
    c.irc.instrs.append(instr) catch {
        Compiler.exit(1);
    };
}

fn bump_lvalue(lvalue: IRC.Lvalue, offset: u64, size: u64) IRC.Lvalue {
    var r = lvalue;
    switch (r) {
        .Tmp => |*tmp| {
            switch (tmp.offset) {
                .Local => |*base_offset| {
                    base_offset.* += @intCast(offset);
                },
                .Global => |*base_offset| {
                    base_offset.* += offset;
                },
            }
            tmp.size = size;
        },
        .Mem => |*mem| {
            mem.offset += offset;
            mem.size = size;
        },
    }
    return r;
}

fn deref_lvalue(c: *Compiler, src: IRC.Lvalue, offset: u64, size: u64) IRC.Lvalue {
    switch (src) {
        .Tmp => |tmp| {
            return .{ .Mem = .{
                .base = tmp,
                .offset = offset,
                .size = size,
            } };
        },
        .Mem => |mem| {
            const dst = grab_local(c, mem.size, .QWORD);
            generate_ir_instr(c, .{ .Mov = .{
                .dst = dst.as_lvalue(),
                .src = src.as_rvalue(),
            } });
            return .{ .Mem = .{
                .base = dst,
                .offset = offset,
                .size = size,
            } };
        },
    }
}

fn deref_rvalue(c: *Compiler, src: IRC.Rvalue, offset: u64, size: u64) IRC.Lvalue {
    switch (src) {
        .Lvalue => |lvalue| {
            return deref_lvalue(c, lvalue, offset, size);
        },
        .Addr => {
            const dst = grab_local(c, 8, .QWORD);
            // TODO: Should make 'Addr' a valid memory operand?
            generate_ir_instr(c, .{ .Mov = .{
                .dst = dst.as_lvalue(),
                .src = src,
            } });
            return .{ .Mem = .{
                .base = dst,
                .offset = offset,
                .size = size,
            } };
        },
        .Label => |label| {
            return .{ .Mem = .{
                .base = null,
                .offset = label + offset,
                .size = size,
            } };
        },
        .Imm => |imm| {
            return .{ .Mem = .{
                .base = null,
                .offset = imm + offset,
                .size = size,
            } };
        },
    }
}

fn grab_local(c: *Compiler, byte_size: u64, alignment: Alignment) IRC.Tmp {
    const offset = utils.align_u64(@intCast(c.ircgen.next_local), alignment);
    c.ircgen.next_local = @intCast(offset + byte_size);
    c.ircgen.biggest_next_local = @max(c.ircgen.biggest_next_local, c.ircgen.next_local);
    return .{
        .offset = .{ .Local = @intCast(offset) },
        .size = byte_size,
    };
}

fn grab_local_from_type(c: *Compiler, data: *Ast.Type.SharedData) IRC.Tmp {
    return grab_local(c, data.byte_size, data.alignment);
}

fn maybe_grab_local_from_type(c: *Compiler, has_dst: ?IRC.Lvalue, data: *Ast.Type.SharedData) IRC.Lvalue {
    return if (has_dst) |dst| dst else grab_local_from_type(c, data).as_lvalue();
}

fn grab_global(c: *Compiler, byte_size: u64, alignment: Alignment) IRC.Tmp {
    const offset = utils.align_u64(c.ircgen.next_global, alignment);
    c.ircgen.next_global = offset + byte_size;
    return .{
        .offset = .{ .Global = offset },
        .size = byte_size,
    };
}

fn grab_global_from_type(c: *Compiler, data: *Ast.Type.SharedData) IRC.Tmp {
    return grab_global(c, data.byte_size, data.alignment);
}

pub fn grab_label(c: *Compiler) IRC.Label {
    return grab_many_labels(c, 1);
}

pub fn grab_many_labels(c: *Compiler, count: usize) IRC.Label {
    const label = c.ircgen.next_label;
    c.ircgen.next_label += count;
    return label;
}
