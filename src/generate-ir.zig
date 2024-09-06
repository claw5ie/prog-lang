instrs: IRC.InstrList,
ast: *Ast,
next_local: i64,
biggest_next_local: i64,
next_global: u64,
next_label: IRC.Label,
loop_condition_label: IRC.Label,
loop_end_label: IRC.Label,
return_label: IRC.Label,

const Context = @This();

pub fn generate_ir(ast: *Ast) IRC {
    var context = Context{
        .instrs = IRC.InstrList.init(common.gpa),
        .ast = ast,
        .next_local = 0,
        .biggest_next_local = 0,
        .next_global = 0,
        .next_label = 0,
        .loop_condition_label = 0,
        .loop_end_label = 0,
        .return_label = 0,
    };

    generate_ir_top_level(&context);

    return .{
        .instrs = context.instrs,
        .label_count = context.next_label,
        .globals_count = context.next_global,
    };
}

fn generate_ir_top_level(ctx: *Context) void {
    {
        const Procedure = &ctx.ast.main.?.as.Procedure;

        if (Procedure.label == null) {
            Procedure.label = grab_label(ctx);
        }

        const dst = grab_global(ctx, 8, .QWORD).as_lvalue();
        generate_ir_instr(ctx, .{ .Call = .{
            .dst = dst,
            .src = .{ .Label = Procedure.label.? },
        } });
        generate_ir_instr(ctx, .Exit);
    }

    var it = ctx.ast.global_symbols.first;
    while (it) |node| {
        generate_ir_global_symbol(ctx, node.data);
        it = node.next;
    }
}

fn generate_ir_global_symbol(ctx: *Context, symbol: *Ast.Symbol) void {
    switch (symbol.as) {
        .Variable => {
            std.debug.print("{s}:{}:{}: ewwow: oopsie doopsie, cawn't cweate gwobaw vawiabwes fow now, UwU\n", .{ ctx.ast.filepath, symbol.line_info.line, symbol.line_info.column });
            common.exit(69);

            // const dst = grab_global_from_type(ctx, Variable.typ.?.data).as_lvalue();
            // Variable.storage = dst;

            // if (Variable.value) |value| {
            //     _ = generate_ir_rvalue(ctx, dst, value);
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

            const start_label = start_label: {
                if (Procedure.label) |label| {
                    break :start_label label;
                } else {
                    const label = grab_label(ctx);
                    Procedure.label = label;
                    break :start_label label;
                }
            };
            const end_label = grab_label(ctx);

            const old_return_label = ctx.return_label;
            ctx.return_label = end_label;

            const index = ctx.instrs.items.len;
            generate_ir_instr(ctx, undefined);
            generate_ir_stmt_list(ctx, Procedure.block);
            generate_ir_instr(ctx, .{ .GFE = .{
                .label = end_label,
                .stack_space_used = @intCast(ctx.biggest_next_local),
            } });
            ctx.instrs.items[index] = .{ .GFB = .{
                .label = start_label,
                .stack_space_used = @intCast(ctx.biggest_next_local),
            } };
            ctx.next_local = 0; // Global function, so there the previous value should have been 0 as well.
            ctx.biggest_next_local = 0;

            ctx.return_label = old_return_label;
        },
        .Parameter, .Struct_Field, .Union_Field, .Enum_Field => unreachable,
        .Type => {},
    }
}

fn generate_ir_local_symbol(ctx: *Context, symbol: *Ast.Symbol) void {
    switch (symbol.as) {
        .Variable => |*Variable| {
            const dst = grab_local_from_type(ctx, Variable.typ.?.data).as_lvalue();
            Variable.storage = dst;

            if (Variable.value) |value| {
                _ = generate_ir_rvalue(ctx, dst, value);
            }
        },
        .Procedure, .Parameter, .Struct_Field, .Union_Field, .Enum_Field => unreachable,
        .Type => {},
    }
}

fn generate_ir_stmt(ctx: *Context, stmt: *Ast.Stmt) void {
    switch (stmt.as) {
        .Print => |expr| {
            const src = generate_ir_rvalue(ctx, null, expr);
            switch (expr.typ.data.as) {
                .Enum => {
                    generate_ir_instr(ctx, .{ .Print = .{
                        .Integer = .{
                            .src = src,
                            .is_signed = expr.typ.is_signed(),
                        },
                    } });
                },
                .Pointer => {
                    generate_ir_instr(ctx, .{ .Print = .{
                        .Pointer = src,
                    } });
                },
                .Integer => {
                    generate_ir_instr(ctx, .{ .Print = .{
                        .Integer = .{
                            .src = src,
                            .is_signed = expr.typ.is_signed(),
                        },
                    } });
                },
                .Bool => {
                    generate_ir_instr(ctx, .{ .Print = .{
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
            const old_next_local = ctx.next_local;
            generate_ir_stmt_list(ctx, block);
            ctx.next_local = old_next_local;
        },
        .If => |If| {
            if (If.false_branch) |false_branch| {
                const false_branch_label = grab_label(ctx);
                const end_label = grab_label(ctx);

                generate_ir_jmpc(ctx, If.condition, false_branch_label, false);

                _ = generate_ir_stmt(ctx, If.true_branch);
                generate_ir_instr(ctx, .{ .Jmp = end_label });
                generate_ir_instr(ctx, .{ .Label = false_branch_label });
                _ = generate_ir_stmt(ctx, false_branch);
                generate_ir_instr(ctx, .{ .Label = end_label });
            } else {
                const end_label = grab_label(ctx);
                generate_ir_jmpc(ctx, If.condition, end_label, false);

                _ = generate_ir_stmt(ctx, If.true_branch);
                generate_ir_instr(ctx, .{ .Label = end_label });
            }
        },
        .While, .Do_While => |While| {
            const start_label = grab_label(ctx);
            const condition_label = grab_label(ctx);
            const end_label = grab_label(ctx);

            const old_loop_condition_label = ctx.loop_condition_label;
            const old_loop_end_label = ctx.loop_end_label;

            ctx.loop_condition_label = condition_label;
            ctx.loop_end_label = end_label;

            if (stmt.as == .While) {
                generate_ir_instr(ctx, .{ .Jmp = condition_label });
            }
            generate_ir_instr(ctx, .{ .Label = start_label });
            generate_ir_stmt(ctx, While.body);
            generate_ir_instr(ctx, .{ .Label = condition_label });
            generate_ir_jmpc(ctx, While.condition, start_label, true);
            generate_ir_instr(ctx, .{ .Label = end_label });

            ctx.loop_condition_label = old_loop_condition_label;
            ctx.loop_end_label = old_loop_end_label;
        },
        .Break => {
            generate_ir_instr(ctx, .{ .Jmp = ctx.loop_end_label });
        },
        .Continue => {
            generate_ir_instr(ctx, .{ .Jmp = ctx.loop_condition_label });
        },
        .Switch => |Switch| {
            const condition = generate_ir_rvalue(ctx, null, Switch.condition);
            const is_signed = Switch.condition.typ.is_signed();

            const first_label = grab_many_labels(ctx, Switch.cases.len);
            const end_label = grab_label(ctx);

            {
                var label = first_label;
                var it = Switch.cases.first;
                while (it) |node| {
                    var case = node.data;
                    while (true) {
                        switch (case.*) {
                            .Case => |Case| {
                                const src = generate_ir_rvalue(ctx, null, Case.value);
                                generate_ir_instr(ctx, .{ .Jmpc = .{
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
                generate_ir_stmt(ctx, else_stmt);
            }

            generate_ir_instr(ctx, .{ .Jmp = end_label });

            {
                var label = first_label;
                var it = Switch.cases.first;
                while (it) |node| {
                    var case = node.data;
                    while (true) {
                        switch (case.*) {
                            .Case => |Case| case = Case.subcase,
                            .Stmt => |substmt| {
                                generate_ir_instr(ctx, .{ .Label = label });
                                generate_ir_stmt(ctx, substmt);
                                generate_ir_instr(ctx, .{ .Jmp = end_label });
                                break;
                            },
                        }
                    }
                    label += 1;

                    it = node.next;
                }
            }

            generate_ir_instr(ctx, .{ .Label = end_label });
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
                const src = generate_ir_rvalue(ctx, null, expr); // Can't move directly to 'dst', because of pointer aliasing. Example: src = foo(&src) may not generate correct code.
                generate_ir_instr(ctx, .{ .Ret1 = .{
                    .dst = dst,
                    .src = src,
                    .label = ctx.return_label,
                } });
            } else {
                generate_ir_instr(ctx, .{ .Ret0 = ctx.return_label });
            }
        },
        .Symbol => |symbol| {
            generate_ir_local_symbol(ctx, symbol);
        },
        .Assign => |Assign| {
            const dst = generate_ir_rvalue(ctx, null, Assign.lhs);
            _ = generate_ir_rvalue(ctx, dst.Lvalue, Assign.rhs);
        },
        .Expr => |expr| {
            _ = generate_ir_rvalue(ctx, null, expr);
        },
    }
}

fn generate_ir_stmt_list(ctx: *Context, list: Ast.StmtList) void {
    var it = list.first;
    while (it) |node| {
        generate_ir_stmt(ctx, node.data);
        it = node.next;
    }
}

fn generate_ir_rvalue(ctx: *Context, has_dst: ?IRC.Lvalue, expr: *Ast.Expr) IRC.Rvalue {
    var move_src = false;
    const src: IRC.Rvalue = src: {
        switch (expr.as) {
            .Binary_Op => |Binary_Op| {
                const src0 = generate_ir_rvalue(ctx, null, Binary_Op.lhs);
                const src1 = generate_ir_rvalue(ctx, null, Binary_Op.rhs);

                const dst = maybe_grab_local_from_type(ctx, has_dst, expr.typ.data);

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
                generate_ir_instr(ctx, .{ .Binary_Op = .{
                    .dst = dst,
                    .src0 = src0,
                    .src1 = src1,
                    .tag = tag,
                    .is_signed = is_signed,
                } });

                break :src dst.as_rvalue();
            },
            .Unary_Op => |Unary_Op| {
                const src = generate_ir_rvalue(ctx, null, Unary_Op.subexpr);
                const is_signed = Unary_Op.subexpr.typ.is_signed();

                switch (Unary_Op.tag) {
                    .Pos => {
                        move_src = true;
                        break :src src;
                    },
                    .Neg => {
                        const dst = maybe_grab_local_from_type(ctx, has_dst, expr.typ.data);
                        generate_ir_instr(ctx, .{ .Unary_Op = .{
                            .dst = dst,
                            .src = src,
                            .tag = .Neg,
                            .is_signed = is_signed,
                        } });
                        break :src dst.as_rvalue();
                    },
                    .Not => {
                        const dst = maybe_grab_local_from_type(ctx, has_dst, expr.typ.data);
                        generate_ir_instr(ctx, .{ .Unary_Op = .{
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
                const src = generate_ir_rvalue(ctx, null, subexpr).Lvalue;
                break :src .{ .Addr = src };
            },
            .Deref => |subexpr| {
                move_src = true;
                const src = generate_ir_rvalue(ctx, null, subexpr);
                const new_src = deref_rvalue(ctx, src, 0, expr.typ.data.byte_size);
                break :src new_src.as_rvalue();
            },
            .If => |If| {
                const false_branch_label = grab_label(ctx);
                const end_label = grab_label(ctx);

                generate_ir_jmpc(ctx, If.condition, false_branch_label, false);
                const dst = maybe_grab_local_from_type(ctx, has_dst, expr.typ.data);

                _ = generate_ir_rvalue(ctx, dst, If.true_branch);
                generate_ir_instr(ctx, .{ .Jmp = end_label });
                generate_ir_instr(ctx, .{ .Label = false_branch_label });
                _ = generate_ir_rvalue(ctx, dst, If.false_branch);
                generate_ir_instr(ctx, .{ .Label = end_label });

                break :src dst.as_rvalue();
            },
            .Field => |Field| {
                move_src = true;

                const offset: u64 = switch (Field.field.as.Symbol.as) {
                    .Struct_Field, .Union_Field => |Struct_Field| Struct_Field.offset,
                    else => unreachable,
                };
                const src = generate_ir_rvalue(ctx, null, Field.subexpr);

                if (Field.subexpr.typ.data.as != .Pointer) {
                    break :src bump_lvalue(src.Lvalue, offset, expr.typ.data.byte_size).as_rvalue();
                } else {
                    break :src deref_rvalue(ctx, src, offset, expr.typ.data.byte_size).as_rvalue();
                }
            },
            .Call => |Call| {
                const dst = maybe_grab_local_from_type(ctx, has_dst, expr.typ.data);
                const old_next_local = ctx.next_local;

                var bytes_pushed: u64 = 0;
                var it = Call.args.last;
                while (it) |node| {
                    const arg = node.data.Expr;
                    const src = generate_ir_rvalue(ctx, null, arg);
                    generate_ir_instr(ctx, .{ .Push = src });
                    bytes_pushed += utils.align_u64(arg.typ.data.byte_size, .QWORD);

                    it = node.prev;
                }

                const src = generate_ir_rvalue(ctx, null, Call.subexpr);
                generate_ir_instr(ctx, .{ .Call = .{
                    .dst = dst,
                    .src = src,
                } });
                if (bytes_pushed > 0) {
                    generate_ir_instr(ctx, .{ .Pop = bytes_pushed });
                }

                ctx.next_local = old_next_local;

                return dst.as_rvalue();
            },
            .Constructor => |Constructor| {
                switch (Constructor.typ.data.as) {
                    .Struct, .Union => {
                        const dst = maybe_grab_local_from_type(ctx, has_dst, expr.typ.data);

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
                            _ = generate_ir_rvalue(ctx, new_dst, Designator.rhs);

                            it = node.next;
                        }

                        break :src dst.as_rvalue();
                    },
                    .Array => |Array| {
                        const dst = maybe_grab_local_from_type(ctx, has_dst, expr.typ.data);

                        var offset: u64 = 0;
                        var it = Constructor.args.first;
                        while (it) |node| {
                            const size = Array.subtype.data.byte_size;
                            const arg = node.data.Expr;

                            const new_dst = bump_lvalue(dst, offset, size);
                            _ = generate_ir_rvalue(ctx, new_dst, arg);

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
                        break :src generate_ir_rvalue(ctx, null, arg);
                    },
                    .Void, .Identifier, .Type_Of => unreachable,
                }
            },
            .Subscript => |Subscript| {
                move_src = true;

                const subexpr = generate_ir_rvalue(ctx, null, Subscript.subexpr);
                const index = generate_ir_rvalue(ctx, null, Subscript.index);
                const dst = grab_local(ctx, 8, .QWORD);

                const offset = utils.align_u64(expr.typ.data.byte_size, expr.typ.data.alignment);
                generate_ir_instr(ctx, .{
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
                            generate_ir_instr(ctx, .{ .Binary_Op = .{
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
                                generate_ir_instr(ctx, .{ .Binary_Op = .{
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
                    generate_ir_instr(ctx, .{ .Binary_Op = .{
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
                        break :src generate_ir_rvalue(ctx, null, Cast.expr);
                    },
                    .Enum => unreachable,
                    .Integer => |dInteger| {
                        const src = generate_ir_rvalue(ctx, null, Cast.expr);

                        switch (Cast.expr.typ.data.as) {
                            .Integer => |sInteger| {
                                if (dInteger.bits > sInteger.bits and dInteger.is_signed and sInteger.is_signed) {
                                    const dst = maybe_grab_local_from_type(ctx, has_dst, expr.typ.data);
                                    generate_ir_instr(ctx, .{ .Movsx = .{
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
                        const dst = maybe_grab_local_from_type(ctx, has_dst, expr.typ.data);
                        const src = generate_ir_rvalue(ctx, null, Cast.expr);
                        generate_ir_instr(ctx, .{ .Setnz = .{
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
                        if (Procedure.label) |label| {
                            break :src .{ .Label = label };
                        } else {
                            const label = grab_label(ctx);
                            Procedure.label = label;
                            break :src .{ .Label = label };
                        }
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
            generate_ir_instr(ctx, .{ .Mov = .{
                .dst = dst,
                .src = src,
            } });
            return dst.as_rvalue();
        }
    }

    return src;
}

fn generate_ir_jmpc(ctx: *Context, expr: *Ast.Expr, label: IRC.Label, jump_if_true: bool) void {
    var src0: IRC.Rvalue = undefined;
    var src1: IRC.Rvalue = .{ .Imm = 0 };
    var tag: IRC.Instr.Jmpc.Tag = .Neq;
    var is_signed = false;

    _ = fill: {
        switch (expr.as) {
            .Binary_Op => |Binary_Op| {
                switch (Binary_Op.tag) {
                    .Eq, .Neq, .Lt, .Leq, .Gt, .Geq => {
                        src0 = generate_ir_rvalue(ctx, null, Binary_Op.lhs);
                        src1 = generate_ir_rvalue(ctx, null, Binary_Op.rhs);

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
                        generate_ir_jmpc(ctx, Unary_Op.subexpr, label, !jump_if_true);
                        return;
                    },
                    else => {},
                }
            },
            else => {},
        }

        src0 = generate_ir_rvalue(ctx, null, expr);
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

    generate_ir_instr(ctx, .{ .Jmpc = .{
        .src0 = src0,
        .src1 = src1,
        .label = label,
        .tag = tag,
        .is_signed = is_signed,
    } });
}

fn generate_ir_instr(ctx: *Context, instr: IRC.Instr) void {
    ctx.instrs.append(instr) catch {
        common.exit(1);
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

fn deref_lvalue(ctx: *Context, src: IRC.Lvalue, offset: u64, size: u64) IRC.Lvalue {
    switch (src) {
        .Tmp => |tmp| {
            return .{ .Mem = .{
                .base = tmp,
                .offset = offset,
                .size = size,
            } };
        },
        .Mem => |mem| {
            const dst = grab_local(ctx, mem.size, .QWORD);
            generate_ir_instr(ctx, .{ .Mov = .{
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

fn deref_rvalue(ctx: *Context, src: IRC.Rvalue, offset: u64, size: u64) IRC.Lvalue {
    switch (src) {
        .Lvalue => |lvalue| {
            return deref_lvalue(ctx, lvalue, offset, size);
        },
        .Addr => {
            const dst = grab_local(ctx, 8, .QWORD);
            // TODO: Should make 'Addr' a valid memory operand?
            generate_ir_instr(ctx, .{ .Mov = .{
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

fn grab_local(ctx: *Context, byte_size: u64, alignment: Alignment) IRC.Tmp {
    const offset = utils.align_u64(@intCast(ctx.next_local), alignment);
    ctx.next_local = @intCast(offset + byte_size);
    ctx.biggest_next_local = @max(ctx.biggest_next_local, ctx.next_local);
    return .{
        .offset = .{ .Local = @intCast(offset) },
        .size = byte_size,
    };
}

fn grab_local_from_type(ctx: *Context, data: *Ast.Type.SharedData) IRC.Tmp {
    return grab_local(ctx, data.byte_size, data.alignment);
}

fn maybe_grab_local_from_type(ctx: *Context, has_dst: ?IRC.Lvalue, data: *Ast.Type.SharedData) IRC.Lvalue {
    return if (has_dst) |dst| dst else grab_local_from_type(ctx, data).as_lvalue();
}

fn grab_global(ctx: *Context, byte_size: u64, alignment: Alignment) IRC.Tmp {
    const offset = utils.align_u64(ctx.next_global, alignment);
    ctx.next_global = offset + byte_size;
    return .{
        .offset = .{ .Global = offset },
        .size = byte_size,
    };
}

fn grab_global_from_type(ctx: *Context, data: *Ast.Type.SharedData) IRC.Tmp {
    return grab_global(ctx, data.byte_size, data.alignment);
}

fn grab_label(ctx: *Context) IRC.Label {
    return grab_many_labels(ctx, 1);
}

fn grab_many_labels(ctx: *Context, count: usize) IRC.Label {
    const label = ctx.next_label;
    ctx.next_label += count;
    return label;
}

const std = @import("std");
const common = @import("common.zig");
const utils = @import("utils.zig");
const Ast = @import("ast.zig");
const IRC = @import("irc.zig");

const Alignment = Ast.Alignment;
