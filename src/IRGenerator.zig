labels: LabelMap,
next_local: u64,
biggest_next_local: u64,
next_global: u64,
next_label: IR.Encoded.Label,
loop_condition_label: ?IR.Encoded.Label,
loop_end_label: ?IR.Encoded.Label,
return_label: ?IR.Encoded.Label,
ast: *Ast,
ir: *IR,

const IRGenerator = @This();

pub fn init(ast: *Ast, ir: *IR) IRGenerator {
    var labels = IRGenerator.LabelMap.init(nostd.general_allocator);
    labels.resize(256) catch {
        Compiler.exit(1);
    };

    return .{
        .labels = labels,
        .next_local = 0,
        .biggest_next_local = 0,
        .next_global = 0,
        .next_label = 0,
        .loop_condition_label = null,
        .loop_end_label = null,
        .return_label = null,
        .ast = ast,
        .ir = ir,
    };
}

pub fn deinit(generator: *IRGenerator) void {
    generator.labels.deinit();
}

pub fn generate(generator: *IRGenerator) void {
    generate_top_level(generator);
    generate_instr0(generator, .exit);
    remove_labels(generator);
}

pub fn remove_labels(generator: *IRGenerator) void {
    const vtable = Interpreter.init_vtable(generator.ir);
    var instrs = &generator.ir.instrs;
    var at: usize = 0;
    while (at < instrs.items.len) {
        var instr, const count = IRD.decode_instr(instrs.items[at..]);

        var update_instr = false;
        for (instr.ops[0..instr.ops_count]) |*op| {
            switch (op.*) {
                .Label => |label| {
                    op.* = .{ .Imm = vtable.start_instr + generator.labels.items[label] };
                    update_instr = true;
                },
                else => {},
            }
        }
        if (update_instr) {
            write_instr_at(generator, at, instr);
        }

        at += count;
    }
}

fn generate_top_level(generator: *IRGenerator) void {
    {
        const labels = grab_procedure_labels(generator, &generator.ast.main.?.as.Procedure);
        generate_instr1(generator, .call, .{ .Label = labels.start });
        generate_instr0(generator, .exit);
    }

    {
        var it = generator.ast.namespaces.first;
        while (it) |node| {
            generate_type(generator, node.data);
            it = node.next;
        }
    }

    {
        var it = generator.ast.local_procedures.first;
        while (it) |node| {
            generate_global_symbol(generator, node.data);
            it = node.next;
        }
    }

    {
        var it = generator.ast.globals.first;
        while (it) |node| {
            generate_global_symbol(generator, node.data);
            it = node.next;
        }
    }
}

fn generate_global_symbol(generator: *IRGenerator, symbol: *Ast.Symbol) void {
    switch (symbol.as) {
        .Variable => |Variable| {
            std.debug.assert(Variable.storage != null);
        },
        .Procedure => |*Procedure| {
            const Proc = &Procedure.typ.data.as.Proc;

            var offset: IRE.Tmp.OffsetType = -IRE.FIRST_PARAM_OFFSET;

            if (Proc.return_type.equal(Ast.void_type)) { // TODO: replace this condition with a flag.
                offset += 8; // There is no return pointer.
            }

            var it = Proc.params.first;
            while (it) |node| {
                const Parameter = &node.data.as.Parameter;

                const size = nostd.align_up(Parameter.typ.data.byte_size, .Qword);
                offset -= @intCast(size);
                Parameter.storage = .{ .Tmp = .{
                    .offset = offset,
                    .tag = .Relative,
                    .size = size,
                } };

                it = node.next;
            }

            const labels = grab_procedure_labels(generator, Procedure);

            std.debug.assert(generator.return_label == null and
                generator.next_local == 0 and
                generator.biggest_next_local == 0);
            generator.return_label = labels.end;

            generate_label(generator, labels.start);
            const start_proc_index = generator.ir.instrs.items.len;
            generate_instr1(
                generator,
                .start_proc,
                .{ .Imm = 0 },
            );

            generate_stmt_list(generator, Procedure.block);

            const stack_space_used = nostd.align_up(generator.biggest_next_local, .Qword);
            generate_label(generator, labels.end);
            generate_instr1(
                generator,
                .end_proc,
                .{ .Imm = stack_space_used },
            );
            generate_instr_at1(
                generator,
                start_proc_index,
                .start_proc,
                .{ .Imm = stack_space_used },
            );

            generator.next_local = 0;
            generator.biggest_next_local = 0;
            generator.return_label = null;
        },
        .Type => {
            // NOTE[global-symbols-generation].
        },
        .Parameter,
        .Struct_Field,
        .Union_Field,
        .Enum_Field,
        => unreachable,
    }
}

fn generate_local_symbol(generator: *IRGenerator, symbol: *Ast.Symbol) void {
    switch (symbol.as) {
        .Variable => |*Variable| {
            if (!(symbol.attributes.is_static or symbol.attributes.is_const)) {
                Variable.storage = grab_local_from_type(generator, Variable.typ.?.data);
            }

            if (Variable.value) |value| {
                _ = generate_expr(generator, Variable.storage, value);
            }
        },
        .Procedure,
        .Type,
        => {
            // NOTE[global-symbols-generation]: those are already generated separately.
        },
        .Parameter,
        .Struct_Field,
        .Union_Field,
        .Enum_Field,
        => unreachable,
    }
}

fn generate_type(generator: *IRGenerator, typ: *Ast.Type) void {
    switch (typ.data.as) {
        .Struct, .Union => |Struct| {
            var it = Struct.rest.first;
            while (it) |node| {
                generate_global_symbol(generator, node.data);
                it = node.next;
            }
        },
        .Enum => |Enum| {
            var it = Enum.rest.first;
            while (it) |node| {
                generate_global_symbol(generator, node.data);
                it = node.next;
            }
        },
        .Proc,
        .Array,
        .Pointer,
        .Integer,
        .Bool,
        .Void,
        => {},
        .Field,
        .Identifier,
        .Type_Of,
        => unreachable,
    }
}

fn generate_stmt(generator: *IRGenerator, stmt: *Ast.Stmt) void {
    switch (stmt.as) {
        .Print => |expr| {
            const src = generate_expr(generator, null, expr);
            switch (expr.typ.data.as) {
                .Enum => {
                    generate_instr1(
                        generator,
                        if (expr.typ.is_signed()) .printi else .printu,
                        src,
                    );
                },
                .Pointer => {
                    generate_instr1(generator, .printp, src);
                },
                .Integer => {
                    generate_instr1(
                        generator,
                        if (expr.typ.is_signed()) .printi else .printu,
                        src,
                    );
                },
                .Bool => {
                    generate_instr1(generator, .printb, src);
                },
                .Struct,
                .Union,
                .Proc,
                .Array,
                .Field,
                .Void,
                .Identifier,
                .Type_Of,
                => unreachable,
            }
        },
        .Block => |block| {
            // Clean locals allocated on the stack.
            const old_next_local = generator.next_local;
            generate_stmt_list(generator, block);
            generator.next_local = old_next_local;
        },
        .If => |If| {
            if (If.false_branch) |false_branch| {
                const false_branch_label = grab_label(generator);
                const end_label = grab_label(generator);

                generate_jmpc(generator, If.condition, false_branch_label, false);

                _ = generate_stmt(generator, If.true_branch);
                generate_instr1(generator, .jmp, .{ .Label = end_label });
                generate_label(generator, false_branch_label);
                _ = generate_stmt(generator, false_branch);
                generate_label(generator, end_label);
            } else {
                const end_label = grab_label(generator);
                generate_jmpc(generator, If.condition, end_label, false);

                _ = generate_stmt(generator, If.true_branch);
                generate_label(generator, end_label);
            }
        },
        .While, .Do_While => |While| {
            const start_label = grab_label(generator);
            const condition_label = grab_label(generator);
            const end_label = grab_label(generator);

            const old_loop_condition_label = generator.loop_condition_label;
            const old_loop_end_label = generator.loop_end_label;

            generator.loop_condition_label = condition_label;
            generator.loop_end_label = end_label;

            if (stmt.as == .While) {
                generate_instr1(generator, .jmp, .{ .Label = condition_label });
            }
            generate_label(generator, start_label);
            generate_stmt(generator, While.body);
            generate_label(generator, condition_label);
            generate_jmpc(generator, While.condition, start_label, true);
            generate_label(generator, end_label);

            generator.loop_condition_label = old_loop_condition_label;
            generator.loop_end_label = old_loop_end_label;
        },
        .Break => {
            generate_instr1(generator, .jmp, .{ .Label = generator.loop_end_label.? });
        },
        .Continue => {
            generate_instr1(generator, .jmp, .{ .Label = generator.loop_condition_label.? });
        },
        .Switch => |Switch| {
            const condition = generate_expr(generator, null, Switch.condition);
            const is_signed = Switch.condition.typ.is_signed();

            const first_label = grab_many_labels(generator, Switch.cases.len);
            const end_label = grab_label(generator);

            {
                var label = first_label;
                var it = Switch.cases.first;
                while (it) |node| {
                    var case = node.data;
                    while (true) {
                        switch (case.*) {
                            .Case => |Case| {
                                const src0 = generate_expr(generator, null, Case.value);
                                generate_instr3(
                                    generator,
                                    if (is_signed) .ije else .uje,
                                    condition,
                                    src0,
                                    .{ .Label = label },
                                );
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
                generate_stmt(generator, else_stmt);
            }

            generate_instr1(generator, .jmp, .{ .Label = end_label });

            {
                var label = first_label;
                var it = Switch.cases.first;
                while (it) |node| {
                    var case = node.data;
                    while (true) {
                        switch (case.*) {
                            .Case => |Case| case = Case.subcase,
                            .Stmt => |substmt| {
                                generate_label(generator, label);
                                generate_stmt(generator, substmt);
                                generate_instr1(generator, .jmp, .{ .Label = end_label });
                                break;
                            },
                        }
                    }
                    label += 1;

                    it = node.next;
                }
            }

            generate_label(generator, end_label);
        },
        .Return => |has_expr| {
            if (has_expr) |expr| {
                const dst = IRE.Operand{ .Mem = .{
                    .base = .{
                        .offset = -IRE.FIRST_PARAM_OFFSET,
                        .size_minus_one = 8 - 1,
                        .tag = .Relative,
                    },
                    .offset = 0,
                    .size = expr.typ.data.byte_size,
                } };
                // Can't move directly to 'dst', because of pointer aliasing. Example: src = foo(&src) may not generate correct code.
                const src = generate_expr(generator, null, expr);
                generate_instr2(generator, .mov, dst, src);
            }

            generate_instr1(generator, .jmp, .{ .Label = generator.return_label.? });
        },
        .Symbol => |symbol| {
            generate_local_symbol(generator, symbol);
        },
        .Assign => |Assign| {
            const dst = generate_expr(generator, null, Assign.lhs);
            _ = generate_expr(generator, dst, Assign.rhs);
        },
        .Expr => |expr| {
            _ = generate_expr(generator, null, expr);
        },
    }
}

fn generate_stmt_list(generator: *IRGenerator, list: Ast.StmtList) void {
    var it = list.first;
    while (it) |node| {
        generate_stmt(generator, node.data);
        it = node.next;
    }
}

pub fn generate_expr(generator: *IRGenerator, has_dst: ?IRE.Operand, expr: *Ast.Expr) IRE.Operand {
    const State = struct {
        generator: *IRGenerator,
        has_dst: ?IRE.Operand,
    };

    const fns = struct {
        pub fn maybe_grab_local_from_type(state: *State, data: *Ast.Type.SharedData) *IRE.Operand {
            if (state.has_dst) |*dst| {
                return dst;
            } else {
                state.has_dst = grab_local_from_type(state.generator, data);
                return &state.has_dst.?;
            }
        }

        pub fn move_to_dst(state: *State, src: IRE.Operand) void {
            if (state.has_dst) |dst| {
                generate_instr2(state.generator, .mov, dst, src);
            } else {
                state.has_dst = src;
            }
        }
    };

    var state = State{
        .generator = generator,
        .has_dst = has_dst,
    };

    switch (expr.as) {
        .Binary_Op => |Binary_Op| {
            const src0 = generate_expr(generator, null, Binary_Op.lhs);
            const src1 = generate_expr(generator, null, Binary_Op.rhs);
            const dst = fns.maybe_grab_local_from_type(&state, expr.typ.data);

            var opcode: IRE.Opcode = switch (Binary_Op.tag) {
                .Or, .And => unreachable,
                .Eq => .ue,
                .Neq => .une,
                .Lt => .ul,
                .Leq => .ule,
                .Gt => .ug,
                .Geq => .uge,
                .Add => .uadd,
                .Sub => .usub,
                .Mul => .umul,
                .Div => .udiv,
                .Mod => .umod,
            };

            const is_signed = Binary_Op.lhs.typ.is_signed();
            std.debug.assert(!is_signed or Binary_Op.rhs.typ.is_signed());

            if (is_signed) {
                opcode = opcode.to_signed();
            }

            generate_instr3(generator, opcode, dst.*, src0, src1);
        },
        .Unary_Op => |Unary_Op| {
            const src = generate_expr(generator, null, Unary_Op.subexpr);

            switch (Unary_Op.tag) {
                .Pos => {
                    fns.move_to_dst(&state, src);
                },
                .Neg => {
                    const dst = fns.maybe_grab_local_from_type(&state, expr.typ.data);
                    generate_instr2(generator, .neg, dst.*, src);
                },
                .Not => {
                    const dst = fns.maybe_grab_local_from_type(&state, expr.typ.data);
                    generate_instr2(generator, .not, dst.*, src);
                },
            }
        },
        .Ref => |subexpr| {
            const src = generate_expr(generator, null, subexpr);
            const new_src = src.addr_of();
            fns.move_to_dst(&state, new_src);
        },
        .Deref => |subexpr| {
            const src = generate_expr(generator, null, subexpr);
            const new_src = deref(generator, src, 0, expr.typ.data.byte_size);
            fns.move_to_dst(&state, new_src);
        },
        .If => |If| {
            const false_branch_label = grab_label(generator);
            const end_label = grab_label(generator);

            generate_jmpc(generator, If.condition, false_branch_label, false);
            const dst = fns.maybe_grab_local_from_type(&state, expr.typ.data);

            _ = generate_expr(generator, dst.*, If.true_branch);
            generate_instr1(generator, .jmp, .{ .Label = end_label });
            generate_label(generator, false_branch_label);
            _ = generate_expr(generator, dst.*, If.false_branch);
            generate_label(generator, end_label);
        },
        .Field => |Field| {
            const offset: u64 = switch (Field.field.as.Symbol.as) {
                .Struct_Field, .Union_Field => |Struct_Field| Struct_Field.offset,
                else => unreachable,
            };

            const src = generate_expr(generator, null, Field.subexpr);
            const new_src: IRE.Operand = if (Field.subexpr.typ.data.as != .Pointer)
                src.bump(offset, expr.typ.data.byte_size)
            else
                deref(generator, src, offset, expr.typ.data.byte_size);

            fns.move_to_dst(&state, new_src);
        },
        .Call => |Call| {
            const dst = fns.maybe_grab_local_from_type(&state, expr.typ.data);
            const old_next_local = generator.next_local;

            var bytes_pushed: u64 = 0;

            var it = Call.args.last;
            while (it) |node| {
                const arg = node.data.Expr;
                const src = generate_expr(generator, null, arg);
                generate_instr1(generator, .push, src);
                bytes_pushed += nostd.align_up(arg.typ.data.byte_size, .Qword);

                it = node.prev;
            }

            if (!expr.typ.equal(Ast.void_type)) {
                generate_instr1(generator, .push, dst.addr_of());
                bytes_pushed += 8;
            }

            const src = generate_expr(generator, null, Call.subexpr);
            generate_instr1(generator, .call, src);

            if (bytes_pushed > 0) {
                generate_instr1(generator, .pop, .{ .Imm = bytes_pushed });
            }

            generator.next_local = old_next_local;
        },
        .Constructor => |Constructor| {
            switch (Constructor.typ.data.as) {
                .Struct, .Union => {
                    const dst = fns.maybe_grab_local_from_type(&state, expr.typ.data);

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

                        const new_dst = dst.bump(offset, size);
                        _ = generate_expr(generator, new_dst, Designator.rhs);

                        it = node.next;
                    }
                },
                .Array => |Array| {
                    const dst = fns.maybe_grab_local_from_type(&state, expr.typ.data);

                    var offset: u64 = 0;
                    var it = Constructor.args.first;
                    while (it) |node| {
                        const size = Array.subtype.data.byte_size;
                        const arg = node.data.Expr;

                        const new_dst = dst.bump(offset, size);
                        _ = generate_expr(generator, new_dst, arg);

                        offset += size;
                        offset = nostd.align_up(offset, Array.subtype.data.alignment);
                        it = node.next;
                    }
                },
                .Enum,
                .Proc,
                .Pointer,
                .Integer,
                .Bool,
                => {
                    const arg = Constructor.args.first.?.data.Expr;
                    const src = generate_expr(generator, null, arg);
                    fns.move_to_dst(&state, src);
                },
                .Void,
                .Field,
                .Identifier,
                .Type_Of,
                => unreachable,
            }
        },
        .Subscript => |Subscript| {
            const subexpr = generate_expr(generator, null, Subscript.subexpr);
            const dst = grab_local(generator, 8, .Qword);

            {
                const index = generate_expr(generator, null, Subscript.index);
                const offset = nostd.align_up(expr.typ.data.byte_size, expr.typ.data.alignment);
                generate_instr3(generator, .umul, dst, index, .{ .Imm = offset });
            }

            if (Subscript.subexpr.typ.data.as != .Pointer) {
                generate_instr3(generator, .uadd, dst, dst, subexpr.addr_of());
            } else {
                generate_instr3(generator, .uadd, dst, dst, subexpr);
            }

            const src = dst.mem_from_tmp(0, expr.typ.data.byte_size);
            fns.move_to_dst(&state, src);
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
                    const src = generate_expr(generator, null, Cast.expr);
                    fns.move_to_dst(&state, src);
                },
                .Enum => unreachable,
                .Integer => |dInteger| {
                    const src = generate_expr(generator, null, Cast.expr);

                    _ = src: {
                        switch (Cast.expr.typ.data.as) {
                            .Integer => |sInteger| {
                                if (dInteger.bits > sInteger.bits and dInteger.is_signed and sInteger.is_signed) {
                                    const dst = fns.maybe_grab_local_from_type(&state, expr.typ.data);
                                    generate_instr3(generator, .movsx, dst.*, src, .{ .Imm = sInteger.bits });
                                    break :src;
                                } else {}
                            },
                            else => {},
                        }

                        fns.move_to_dst(&state, src);

                        break :src;
                    };
                },
                .Bool => {
                    const src = generate_expr(generator, null, Cast.expr);
                    const dst = fns.maybe_grab_local_from_type(&state, expr.typ.data);
                    generate_instr2(generator, .setnz, dst.*, src);
                },
                .Void,
                .Field,
                .Identifier,
                .Type_Of,
                => unreachable,
            }
        },
        .Integer => |value| {
            fns.move_to_dst(&state, .{ .Imm = value });
        },
        .Boolean => |value| {
            fns.move_to_dst(&state, .{ .Imm = @intFromBool(value) });
        },
        .Null => {
            fns.move_to_dst(&state, .{ .Imm = 0 });
        },
        .Symbol => |symbol| {
            switch (symbol.as) {
                .Variable => |*Variable| {
                    const storage = grab_static_variable_storage(generator, Variable);
                    fns.move_to_dst(&state, storage);
                },
                .Parameter => |Parameter| {
                    fns.move_to_dst(&state, Parameter.storage.?);
                },
                .Procedure => |*Procedure| {
                    const labels = grab_procedure_labels(generator, Procedure);
                    fns.move_to_dst(&state, .{ .Label = labels.start });
                },
                .Enum_Field => |Enum_Field| {
                    fns.move_to_dst(&state, .{ .Imm = Enum_Field.computed_value });
                },
                .Struct_Field,
                .Union_Field,
                .Type,
                => unreachable,
            }
        },
        .Type,
        .Identifier,
        => unreachable,
    }

    std.debug.assert(state.has_dst != null);

    return state.has_dst.?;
}

fn generate_jmpc(generator: *IRGenerator, expr: *Ast.Expr, label: IRE.Label, jump_if_true: bool) void {
    var src0: IRE.Operand = undefined;
    var src1: IRE.Operand = .{ .Imm = 0 };
    var opcode: IRE.Opcode = .ujne;
    var is_signed = false;

    _ = fill: {
        switch (expr.as) {
            .Binary_Op => |Binary_Op| {
                switch (Binary_Op.tag) {
                    .Eq, .Neq, .Lt, .Leq, .Gt, .Geq => {
                        src0 = generate_expr(generator, null, Binary_Op.lhs);
                        src1 = generate_expr(generator, null, Binary_Op.rhs);

                        opcode = switch (Binary_Op.tag) {
                            .Eq => .uje,
                            .Neq => .ujne,
                            .Lt => .ujl,
                            .Leq => .ujle,
                            .Gt => .ujg,
                            .Geq => .ujge,
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
                        generate_jmpc(generator, Unary_Op.subexpr, label, !jump_if_true);
                        return;
                    },
                    else => {},
                }
            },
            else => {},
        }

        src0 = generate_expr(generator, null, expr);
    };

    if (!jump_if_true) {
        opcode = switch (opcode) {
            .uje => .ujne,
            .ujne => .uje,
            .ujl => .ujge,
            .ujle => .ujg,
            .ujg => .ujle,
            .ujge => .ujl,
            else => unreachable,
        };
    }

    if (is_signed) {
        opcode = opcode.to_signed();
    }

    generate_instr3(generator, opcode, src0, src1, .{ .Label = label });
}

fn generate_label(generator: *IRGenerator, label: IRE.Label) void {
    var labels = &generator.labels;

    var capacity = labels.capacity;
    if (capacity < label) {
        capacity = label + capacity / 2 + 1;
        labels.resize(capacity) catch {
            Compiler.exit(1);
        };
    }

    labels.items[label] = generator.ir.instrs.items.len;
}

pub fn generate_instr0(generator: *IRGenerator, opcode: IRE.Opcode) void {
    var params = IRE.Instr{
        .opcode = opcode,
        .ops = .{ undefined, undefined, undefined },
        .ops_count = 0,
    };
    generate_instr(generator, &params);
}

pub fn generate_instr1(generator: *IRGenerator, opcode: IRE.Opcode, op0: IRE.Operand) void {
    var params = IRE.Instr{
        .opcode = opcode,
        .ops = .{ op0, undefined, undefined },
        .ops_count = 1,
    };
    generate_instr(generator, &params);
}

pub fn generate_instr2(generator: *IRGenerator, opcode: IRE.Opcode, op0: IRE.Operand, op1: IRE.Operand) void {
    var params = IRE.Instr{
        .opcode = opcode,
        .ops = .{ op0, op1, undefined },
        .ops_count = 2,
    };
    generate_instr(generator, &params);
}

pub fn generate_instr3(generator: *IRGenerator, opcode: IRE.Opcode, op0: IRE.Operand, op1: IRE.Operand, op2: IRE.Operand) void {
    var params = IRE.Instr{
        .opcode = opcode,
        .ops = .{ op0, op1, op2 },
        .ops_count = 3,
    };
    generate_instr(generator, &params);
}

pub fn generate_instr_at0(generator: *IRGenerator, index: usize, opcode: IRE.Opcode) void {
    var params = IRE.Instr{
        .opcode = opcode,
        .ops = .{ undefined, undefined, undefined },
        .ops_count = 0,
    };
    generate_instr_at(generator, index, &params);
}

pub fn generate_instr_at1(generator: *IRGenerator, index: usize, opcode: IRE.Opcode, op0: IRE.Operand) void {
    var params = IRE.Instr{
        .opcode = opcode,
        .ops = .{ op0, undefined, undefined },
        .ops_count = 1,
    };
    generate_instr_at(generator, index, &params);
}

pub fn generate_instr2_at(generator: *IRGenerator, index: usize, opcode: IRE.Opcode, op0: IRE.Operand, op1: IRE.Operand) void {
    var params = IRE.Instr{
        .opcode = opcode,
        .ops = .{ op0, op1, undefined },
        .ops_count = 2,
    };
    generate_instr_at(generator, index, &params);
}

pub fn generate_instr_at3(generator: *IRGenerator, index: usize, opcode: IRE.Opcode, op0: IRE.Operand, op1: IRE.Operand, op2: IRE.Operand) void {
    var params = IRE.Instr{
        .opcode = opcode,
        .ops = .{ op0, op1, op2 },
        .ops_count = 3,
    };
    generate_instr_at(generator, index, &params);
}

fn generate_instr(generator: *IRGenerator, params: *IRE.Instr) void {
    generate_instr_at(generator, generator.ir.instrs.items.len, params);
}

fn generate_instr_at(generator: *IRGenerator, index: usize, p: *IRE.Instr) void {
    std.debug.assert(is_valid_instr: {
        if (p.ops_count != p.opcode.operand_count()) {
            break :is_valid_instr false;
        }

        switch (p.opcode) {
            .exit,
            .printp,
            .printi,
            .printu,
            .printb,
            => break :is_valid_instr true,
            .ue,
            .une,
            .ul,
            .ule,
            .ug,
            .uge,
            .uadd,
            .usub,
            .umul,
            .udiv,
            .umod,
            .ie,
            .ine,
            .il,
            .ile,
            .ig,
            .ige,
            .iadd,
            .isub,
            .imul,
            .idiv,
            .imod,
            => break :is_valid_instr p.ops[0].is_lvalue(),
            .neg,
            .not,
            => break :is_valid_instr p.ops[0].is_lvalue(),
            .jmp,
            .uje,
            .ujne,
            .ujl,
            .ujle,
            .ujg,
            .ujge,
            .ije,
            .ijne,
            .ijl,
            .ijle,
            .ijg,
            .ijge,
            => break :is_valid_instr true,
            .setnz,
            .mov,
            => break :is_valid_instr p.ops[0].is_lvalue(),
            .mov_big => break :is_valid_instr p.ops[0].is_lvalue() and p.ops[1].is_lvalue(),
            .movsx => break :is_valid_instr p.ops[0].is_lvalue(),
            .call,
            .push,
            .pop,
            => break :is_valid_instr true,
            .push_big => break :is_valid_instr p.ops[0].is_lvalue(),
            .start_proc,
            .end_proc,
            => break :is_valid_instr true,
        }
    });

    switch (p.opcode) {
        .mov => {
            const size = p.ops[0].grab_size();
            if (size > 8) {
                std.debug.assert(p.ops[1].grab_size() == size);
                p.opcode = .mov_big;
                p.ops[0] = p.ops[0].addr_of();
                p.ops[1] = p.ops[1].addr_of();
                p.ops[2] = .{ .Imm = size };
                p.ops_count = 3;
            }
        },
        .push => {
            const size = p.ops[0].grab_size_no_fail();
            if (size > 8) {
                p.opcode = .push_big;
                p.ops[0] = p.ops[0].addr_of();
                p.ops[1] = .{ .Imm = size };
                p.ops_count = 2;
            }
        },
        else => {},
    }

    var instr = IRD.Instr{
        .opcode = p.opcode,
        .ops = undefined,
        .ops_count = p.ops_count,
    };

    for (p.ops[0..p.ops_count], 0..) |op, i| {
        instr.ops[i] = op.decode();
    }

    write_instr_at(generator, index, instr);
}

fn write_instr_at(generator: *IRGenerator, index: usize, instr: IRD.Instr) void {
    var data = [1]u32{0} ** (1 + 6 * 2);
    var off: u5 = 0;

    data[0] |= @as(u32, @intFromEnum(instr.opcode)) << off;
    off += 8;

    var at: u8 = 1;
    for (instr.ops[0..instr.ops_count]) |op| {
        data[0] |= @as(u32, @intFromEnum(op)) << off;
        off += @bitSizeOf(IRD.Operand.Tag);

        switch (op) {
            .Tmp => |tmp| {
                const ptr: [*]const u32 = @ptrCast(&tmp);
                data[at + 0] = ptr[0];
                data[at + 1] = ptr[1];
                at += 2;
            },
            .Mem_B => |mem| {
                data[0] |= @as(u32, mem.size_minus_one) << off;
                off += 3;

                const ptr: [*]const u32 = @ptrCast(&mem);
                data[at + 0] = ptr[0];
                data[at + 1] = ptr[1];
                at += 2;
            },
            .Mem_BO => |mem| {
                data[0] |= @as(u32, mem.size_minus_one) << off;
                off += 3;

                const ptr: [*]const u32 = @ptrCast(&mem);
                data[at + 0] = ptr[0];
                data[at + 1] = ptr[1];
                data[at + 2] = ptr[2];
                data[at + 3] = ptr[3];
                at += 4;
            },
            .Addr_T => |tmp| {
                const ptr: [*]const u32 = @ptrCast(&tmp);
                data[at + 0] = ptr[0];
                data[at + 1] = ptr[1];
                at += 2;
            },
            .Addr_BO => |mem| {
                const ptr: [*]const u32 = @ptrCast(&mem);
                data[at + 0] = ptr[0];
                data[at + 1] = ptr[1];
                data[at + 2] = ptr[2];
                data[at + 3] = ptr[3];
                at += 4;
            },
            .Imm => |imm| {
                const ptr: [*]const u32 = @ptrCast(&imm);
                data[at + 0] = ptr[0];
                data[at + 1] = ptr[1];
                at += 2;
            },
            .Label => |label| {
                const ptr: [*]const u32 = @ptrCast(&label);
                data[at + 0] = ptr[0];
                data[at + 1] = ptr[1];
                at += 2;
            },
        }
    }

    {
        var bytes: []const u8 = undefined;
        bytes.ptr = @ptrCast(&data[0]);
        bytes.len = at * 4;
        write_instr_bytes_at(generator, index, bytes);
    }
}

fn write_instr_bytes_at(generator: *IRGenerator, index: usize, bytes: []const u8) void {
    if (index == generator.ir.instrs.items.len) {
        generator.ir.instrs.insertSlice(index, bytes) catch {
            Compiler.exit(1);
        };
    } else {
        std.debug.assert(index + bytes.len <= generator.ir.instrs.items.len);
        generator.ir.instrs.replaceRange(index, bytes.len, bytes) catch {
            Compiler.exit(1);
        };
    }
}

fn deref(generator: *IRGenerator, src: IRE.Operand, offset: u64, size: u64) IRE.Operand {
    switch (src) {
        .Tmp => {
            return src.mem_from_tmp(offset, size);
        },
        .Mem => |mem| {
            std.debug.assert(mem.size <= 8);
            const dst = grab_local(generator, mem.size, .Qword);
            generate_instr2(generator, .mov, dst, src);

            return dst.mem_from_tmp(offset, size);
        },
        .Addr_T => |tmp| {
            return .{ .Tmp = .{
                .offset = tmp.offset + @as(i59, @intCast(offset)),
                .tag = tmp.tag,
                .size = size,
            } };
        },
        .Addr_M => |mem| {
            return .{ .Mem = .{
                .base = mem.base,
                .offset = mem.offset + offset,
                .size = size,
            } };
        },
        .Imm => |imm| {
            return .{ .Tmp = .{
                .offset = @intCast(imm + offset),
                .tag = .Absolute,
                .size = size,
            } };
        },
        .Label => {
            // Since labels are not yet resolved (we don't know absolute offset), we can't perform operations on it during IR generation.
            const dst = grab_local(generator, 8, .Qword);
            generate_instr3(generator, .uadd, dst, src, .{ .Imm = offset });
            return dst.mem_from_tmp(offset, size);
        },
    }
}

pub fn grab_local(generator: *IRGenerator, byte_size: u64, alignment: Alignment) IRE.Operand {
    const offset = nostd.align_up(@intCast(generator.next_local), alignment);
    generator.next_local = @intCast(offset + byte_size);
    generator.biggest_next_local = @max(generator.biggest_next_local, generator.next_local);
    return .{ .Tmp = .{
        .offset = @intCast(offset),
        .tag = .Relative,
        .size = byte_size,
    } };
}

pub fn grab_local_from_type(generator: *IRGenerator, data: *Ast.Type.SharedData) IRE.Operand {
    return grab_local(generator, data.byte_size, data.alignment);
}

pub fn grab_global(generator: *IRGenerator, byte_size: u64, alignment: Alignment) IRE.Operand {
    std.debug.assert(generator.next_global == generator.ir.globals.items.len);

    const old_offset = generator.next_global;
    const new_offset = nostd.align_up(old_offset, alignment);

    generator.next_global = new_offset + byte_size;
    generator.ir.globals.appendNTimes(0xAA, byte_size + (new_offset - old_offset)) catch {
        Compiler.exit(1);
    };

    return .{ .Tmp = .{
        .offset = @intCast(new_offset),
        .tag = .Global,
        .size = byte_size,
    } };
}

pub fn grab_global_from_type(generator: *IRGenerator, data: *Ast.Type.SharedData) IRE.Operand {
    return grab_global(generator, data.byte_size, data.alignment);
}

pub fn grab_label(generator: *IRGenerator) IRE.Label {
    return grab_many_labels(generator, 1);
}

pub fn grab_many_labels(generator: *IRGenerator, count: usize) IRE.Label {
    const label = generator.next_label;
    generator.next_label += count;
    return label;
}

pub fn grab_static_variable_storage(generator: *IRGenerator, variable: *Ast.Symbol.Variable) IR.Encoded.Operand {
    if (variable.storage) |storage| {
        return storage;
    } else {
        const storage = generator.grab_global_from_type(variable.typ.?.data);
        variable.storage = storage;
        return storage;
    }
}

pub fn grab_procedure_labels(generator: *IRGenerator, procedure: *Ast.Symbol.Procedure) Ast.Symbol.Procedure.LabelPair {
    if (procedure.labels) |labels| {
        return labels;
    } else {
        const labels = Ast.Symbol.Procedure.LabelPair{
            .start = generator.grab_label(),
            .end = generator.grab_label(),
        };
        procedure.labels = labels;
        return labels;
    }
}

const std = @import("std");
const nostd = @import("nostd.zig");
const Compiler = @import("Compiler.zig");
const Ast = @import("Ast.zig");
const IR = @import("IR.zig");
const Interpreter = @import("Interpreter.zig");

const IRE = IR.Encoded;
const IRD = IR.Decoded;
const Alignment = nostd.Alignment;

pub const LabelMap = std.ArrayList(u64);
