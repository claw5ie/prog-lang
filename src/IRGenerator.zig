labels: LabelMap,
next_local: u64,
biggest_next_local: u64,
next_global: u64,
next_label: IR.Label,
loop_condition_label: ?IR.Label,
loop_end_label: ?IR.Label,
return_label: ?IR.Label,
ast: *Ast,
ir: *IR,

const IRGenerator = @This();

pub fn init(ast: *Ast, ir: *IR) IRGenerator {
    var labels = IRGenerator.LabelMap.init(nostd.general_allocator);
    labels.resize(256) catch {
        exit(1);
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
    generate_instruction(generator, .exit, .{});
    remove_labels(generator);
}

pub fn remove_labels(generator: *IRGenerator) void {
    const vtable = Interpreter.init_vtable(generator.ir);
    var instructions = &generator.ir.instructions;
    var at: usize = 0;
    while (at < instructions.items.len) {
        var instruction, const count = IR.parse_instruction(instructions.items[at..]);

        var update_instruction = false;
        for (instruction.operands[0..instruction.operands_count]) |*op| {
            switch (op.*) {
                .Label => |label| {
                    op.* = .{ .Imm = vtable.start_instruction + generator.labels.items[label] };
                    update_instruction = true;
                },
                else => {},
            }
        }
        if (update_instruction) {
            write_instruction_at(generator, at, instruction);
        }

        at += count;
    }
}

fn generate_top_level(generator: *IRGenerator) void {
    {
        const labels = grab_procedure_labels(generator, &generator.ast.main.?.as.Procedure);
        generate_instruction(generator, .call, .{IR.BigOperand{ .Label = labels.start }});
        generate_instruction(generator, .exit, .{});
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

            var offset: IR.Tmp.OffsetType = -IR.first_param_offset;

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
            const start_proc_index = generator.ir.instructions.items.len;
            generate_instruction(generator, .start_proc, .{IR.BigOperand{ .Imm = 0 }});

            generate_statement_list(generator, Procedure.block);

            const stack_space_used = nostd.align_up(generator.biggest_next_local, .Qword);
            generate_label(generator, labels.end);
            generate_instruction(generator, .end_proc, .{IR.BigOperand{ .Imm = stack_space_used }});
            generate_instruction_at(generator, start_proc_index, .start_proc, .{IR.BigOperand{ .Imm = stack_space_used }});

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
            if (!(Variable.attributes.is_static or Variable.attributes.is_const)) {
                Variable.storage = grab_local_from_type(generator, Variable.typ.?.data);
            }

            if (Variable.value) |value| {
                _ = generate_expression(generator, Variable.storage, value);
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

fn generate_statement(generator: *IRGenerator, statement: *Ast.Statement) void {
    switch (statement.as) {
        .Print => |expression| {
            const src = generate_expression(generator, null, expression);
            switch (expression.typ.data.as) {
                .Enum => {
                    generate_instruction(
                        generator,
                        if (expression.typ.is_signed()) .printi else .printu,
                        .{src},
                    );
                },
                .Pointer => {
                    generate_instruction(generator, .printp, .{src});
                },
                .Integer => {
                    generate_instruction(
                        generator,
                        if (expression.typ.is_signed()) .printi else .printu,
                        .{src},
                    );
                },
                .Bool => {
                    generate_instruction(generator, .printb, .{src});
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
            generate_statement_list(generator, block);
            generator.next_local = old_next_local;
        },
        .If => |If| {
            if (If.false_branch) |false_branch| {
                const false_branch_label = grab_label(generator);
                const end_label = grab_label(generator);

                generate_jmpc(generator, If.condition, false_branch_label, false);

                _ = generate_statement(generator, If.true_branch);
                generate_instruction(generator, .jmp, .{IR.BigOperand{ .Label = end_label }});
                generate_label(generator, false_branch_label);
                _ = generate_statement(generator, false_branch);
                generate_label(generator, end_label);
            } else {
                const end_label = grab_label(generator);
                generate_jmpc(generator, If.condition, end_label, false);

                _ = generate_statement(generator, If.true_branch);
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

            if (statement.as == .While) {
                generate_instruction(generator, .jmp, .{IR.BigOperand{ .Label = condition_label }});
            }
            generate_label(generator, start_label);
            generate_statement(generator, While.body);
            generate_label(generator, condition_label);
            generate_jmpc(generator, While.condition, start_label, true);
            generate_label(generator, end_label);

            generator.loop_condition_label = old_loop_condition_label;
            generator.loop_end_label = old_loop_end_label;
        },
        .Break => {
            generate_instruction(generator, .jmp, .{IR.BigOperand{ .Label = generator.loop_end_label.? }});
        },
        .Continue => {
            generate_instruction(generator, .jmp, .{IR.BigOperand{ .Label = generator.loop_condition_label.? }});
        },
        .Switch => |Switch| {
            const condition = generate_expression(generator, null, Switch.condition);
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
                                const src0 = generate_expression(generator, null, Case.value);
                                generate_instruction(
                                    generator,
                                    if (is_signed) .ije else .uje,
                                    .{ condition, src0, IR.BigOperand{ .Label = label } },
                                );
                                case = Case.subcase;
                            },
                            .Statement => break,
                        }
                    }
                    label += 1;

                    it = node.next;
                }
            }

            if (Switch.default_case) |else_statement| {
                generate_statement(generator, else_statement);
            }

            generate_instruction(generator, .jmp, .{IR.BigOperand{ .Label = end_label }});

            {
                var label = first_label;
                var it = Switch.cases.first;
                while (it) |node| {
                    var case = node.data;
                    while (true) {
                        switch (case.*) {
                            .Case => |Case| case = Case.subcase,
                            .Statement => |substatement| {
                                generate_label(generator, label);
                                generate_statement(generator, substatement);
                                generate_instruction(generator, .jmp, .{IR.BigOperand{ .Label = end_label }});
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
        .Return => |has_expression| {
            if (has_expression) |expression| {
                const dst = IR.BigOperand{ .Mem = .{
                    .base = .{
                        .offset = -IR.first_param_offset,
                        .size_minus_one = 8 - 1,
                        .tag = .Relative,
                    },
                    .offset = 0,
                    .size = expression.typ.data.byte_size,
                } };
                // Can't move directly to 'dst', because of pointer aliasing. Example: src = foo(&src) may not generate correct code.
                const src = generate_expression(generator, null, expression);
                generate_instruction(generator, .mov, .{ dst, src });
            }

            generate_instruction(generator, .jmp, .{IR.BigOperand{ .Label = generator.return_label.? }});
        },
        .Symbol => |symbol| {
            generate_local_symbol(generator, symbol);
        },
        .Assign => |Assign| {
            const dst = generate_expression(generator, null, Assign.lhs);
            _ = generate_expression(generator, dst, Assign.rhs);
        },
        .Expression => |expression| {
            _ = generate_expression(generator, null, expression);
        },
    }
}

fn generate_statement_list(generator: *IRGenerator, list: Ast.StatementList) void {
    var it = list.first;
    while (it) |node| {
        generate_statement(generator, node.data);
        it = node.next;
    }
}

pub fn generate_expression(generator: *IRGenerator, has_dst: ?IR.BigOperand, expression: *Ast.Expression) IR.BigOperand {
    const State = struct {
        generator: *IRGenerator,
        has_dst: ?IR.BigOperand,
    };

    const fns = struct {
        pub fn maybe_grab_local_from_type(state: *State, data: *Ast.Type.SharedData) *IR.BigOperand {
            if (state.has_dst) |*dst| {
                return dst;
            } else {
                state.has_dst = grab_local_from_type(state.generator, data);
                return &state.has_dst.?;
            }
        }

        pub fn move_to_dst(state: *State, src: IR.BigOperand) void {
            if (state.has_dst) |dst| {
                generate_instruction(state.generator, .mov, .{ dst, src });
            } else {
                state.has_dst = src;
            }
        }
    };

    var state = State{
        .generator = generator,
        .has_dst = has_dst,
    };

    switch (expression.as) {
        .Binary_Op => |Binary_Op| {
            const src0 = generate_expression(generator, null, Binary_Op.lhs);
            const src1 = generate_expression(generator, null, Binary_Op.rhs);
            const dst = fns.maybe_grab_local_from_type(&state, expression.typ.data);

            var opcode: IR.Opcode = switch (Binary_Op.tag) {
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

            generate_instruction(generator, opcode, .{ dst.*, src0, src1 });
        },
        .Unary_Op => |Unary_Op| {
            const src = generate_expression(generator, null, Unary_Op.subexpression);

            switch (Unary_Op.tag) {
                .Pos => {
                    fns.move_to_dst(&state, src);
                },
                .Neg => {
                    const dst = fns.maybe_grab_local_from_type(&state, expression.typ.data);
                    generate_instruction(generator, .neg, .{ dst.*, src });
                },
                .Not => {
                    const dst = fns.maybe_grab_local_from_type(&state, expression.typ.data);
                    generate_instruction(generator, .not, .{ dst.*, src });
                },
            }
        },
        .Ref => |subexpression| {
            const src = generate_expression(generator, null, subexpression);
            const new_src = src.addr_of();
            fns.move_to_dst(&state, new_src);
        },
        .Deref => |subexpression| {
            const src = generate_expression(generator, null, subexpression);
            const new_src = deref(generator, src, 0, expression.typ.data.byte_size);
            fns.move_to_dst(&state, new_src);
        },
        .If => |If| {
            const false_branch_label = grab_label(generator);
            const end_label = grab_label(generator);

            generate_jmpc(generator, If.condition, false_branch_label, false);
            const dst = fns.maybe_grab_local_from_type(&state, expression.typ.data);

            _ = generate_expression(generator, dst.*, If.true_branch);
            generate_instruction(generator, .jmp, .{IR.BigOperand{ .Label = end_label }});
            generate_label(generator, false_branch_label);
            _ = generate_expression(generator, dst.*, If.false_branch);
            generate_label(generator, end_label);
        },
        .Field => |Field| {
            const offset: u64 = switch (Field.field.as.Symbol.as) {
                .Struct_Field, .Union_Field => |Struct_Field| Struct_Field.offset,
                else => unreachable,
            };

            const src = generate_expression(generator, null, Field.subexpression);
            const new_src: IR.BigOperand = if (Field.subexpression.typ.data.as != .Pointer)
                src.bump(offset, expression.typ.data.byte_size)
            else
                deref(generator, src, offset, expression.typ.data.byte_size);

            fns.move_to_dst(&state, new_src);
        },
        .Call => |Call| {
            const dst = fns.maybe_grab_local_from_type(&state, expression.typ.data);
            const old_next_local = generator.next_local;

            var bytes_pushed: u64 = 0;

            var it = Call.args.last;
            while (it) |node| {
                const arg = node.data.Expression;
                const src = generate_expression(generator, null, arg);
                generate_instruction(generator, .push, .{src});
                bytes_pushed += nostd.align_up(arg.typ.data.byte_size, .Qword);

                it = node.prev;
            }

            if (!expression.typ.equal(Ast.void_type)) {
                generate_instruction(generator, .push, .{dst.addr_of()});
                bytes_pushed += 8;
            }

            const src = generate_expression(generator, null, Call.subexpression);
            generate_instruction(generator, .call, .{src});

            if (bytes_pushed > 0) {
                generate_instruction(generator, .pop, .{IR.BigOperand{ .Imm = bytes_pushed }});
            }

            generator.next_local = old_next_local;
        },
        .Constructor => |Constructor| {
            switch (Constructor.typ.data.as) {
                .Struct, .Union => {
                    const dst = fns.maybe_grab_local_from_type(&state, expression.typ.data);

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
                        _ = generate_expression(generator, new_dst, Designator.rhs);

                        it = node.next;
                    }
                },
                .Array => |Array| {
                    const dst = fns.maybe_grab_local_from_type(&state, expression.typ.data);

                    var offset: u64 = 0;
                    var it = Constructor.args.first;
                    while (it) |node| {
                        const size = Array.subtype.data.byte_size;
                        const arg = node.data.Expression;

                        const new_dst = dst.bump(offset, size);
                        _ = generate_expression(generator, new_dst, arg);

                        offset += size;
                        it = node.next;
                    }
                },
                .Enum,
                .Proc,
                .Pointer,
                .Integer,
                .Bool,
                => {
                    const arg = Constructor.args.first.?.data.Expression;
                    const src = generate_expression(generator, null, arg);
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
            const subexpression = generate_expression(generator, null, Subscript.subexpression);
            const dst = grab_local(generator, 8, .Qword);

            {
                const index = generate_expression(generator, null, Subscript.index);
                const offset = expression.typ.data.byte_size;
                generate_instruction(generator, .umul, .{ dst, index, IR.BigOperand{ .Imm = offset } });
            }

            if (Subscript.subexpression.typ.data.as != .Pointer) {
                generate_instruction(generator, .uadd, .{ dst, dst, subexpression.addr_of() });
            } else {
                generate_instruction(generator, .uadd, .{ dst, dst, subexpression });
            }

            const src = dst.mem_from_tmp(0, expression.typ.data.byte_size);
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
                    const src = generate_expression(generator, null, Cast.expression);
                    fns.move_to_dst(&state, src);
                },
                .Enum => unreachable,
                .Integer => |dInteger| {
                    const src = generate_expression(generator, null, Cast.expression);

                    _ = src: {
                        switch (Cast.expression.typ.data.as) {
                            .Integer => |sInteger| {
                                if (dInteger.bits > sInteger.bits and dInteger.is_signed and sInteger.is_signed) {
                                    const dst = fns.maybe_grab_local_from_type(&state, expression.typ.data);
                                    generate_instruction(generator, .movsx, .{ dst.*, src, IR.BigOperand{ .Imm = sInteger.bits } });
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
                    const src = generate_expression(generator, null, Cast.expression);
                    const dst = fns.maybe_grab_local_from_type(&state, expression.typ.data);
                    generate_instruction(generator, .setnz, .{ dst.*, src });
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

fn generate_jmpc(generator: *IRGenerator, expression: *Ast.Expression, label: IR.Label, jump_if_true: bool) void {
    var src0: IR.BigOperand = undefined;
    var src1: IR.BigOperand = .{ .Imm = 0 };
    var opcode: IR.Opcode = .ujne;
    var is_signed = false;

    _ = fill: {
        switch (expression.as) {
            .Binary_Op => |Binary_Op| {
                switch (Binary_Op.tag) {
                    .Eq, .Neq, .Lt, .Leq, .Gt, .Geq => {
                        src0 = generate_expression(generator, null, Binary_Op.lhs);
                        src1 = generate_expression(generator, null, Binary_Op.rhs);

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
                        generate_jmpc(generator, Unary_Op.subexpression, label, !jump_if_true);
                        return;
                    },
                    else => {},
                }
            },
            else => {},
        }

        src0 = generate_expression(generator, null, expression);
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

    generate_instruction(generator, opcode, .{ src0, src1, IR.BigOperand{ .Label = label } });
}

fn generate_label(generator: *IRGenerator, label: IR.Label) void {
    var labels = &generator.labels;

    var capacity = labels.capacity;
    if (capacity < label) {
        capacity = label + capacity / 2 + 1;
        labels.resize(capacity) catch {
            exit(1);
        };
    }

    labels.items[label] = generator.ir.instructions.items.len;
}

pub fn generate_instruction(generator: *IRGenerator, opcode: IR.Opcode, args: anytype) void {
    generate_instruction_at(generator, generator.ir.instructions.items.len, opcode, args);
}

pub fn generate_instruction_at(generator: *IRGenerator, index: usize, opcode: IR.Opcode, args: anytype) void {
    const ArgsType = @TypeOf(args);
    const args_type_info = @typeInfo(ArgsType);

    if (args_type_info != .Struct) {
        @compileError("expected tuple or struct argument, found " ++ @typeName(ArgsType));
    }

    const fields_info = args_type_info.Struct.fields;

    if (fields_info.len > IR.Instruction.maximum_operands_count) {
        // TODO: convert 'maximum_operands_count' to string?
        @compileError("instruction can't take more than 3 arguments");
    }

    var new_opcode = opcode;
    var operands_count: u8 = fields_info.len;
    var operands: [IR.Instruction.maximum_operands_count]IR.BigOperand = undefined;

    inline for (fields_info, 0..) |field, i| {
        if (field.type != IR.BigOperand) {
            @compileError("operand must be of type " ++ @typeName(IR.BigOperand));
        }
        operands[i] = args[i];
    }

    std.debug.assert(is_valid_instruction: {
        if (operands_count != new_opcode.operand_count()) {
            break :is_valid_instruction false;
        }

        switch (new_opcode) {
            .exit,
            .printp,
            .printi,
            .printu,
            .printb,
            => break :is_valid_instruction true,
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
            => break :is_valid_instruction operands[0].is_lvalue(),
            .neg,
            .not,
            => break :is_valid_instruction operands[0].is_lvalue(),
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
            => break :is_valid_instruction true,
            .setnz,
            .mov,
            => break :is_valid_instruction operands[0].is_lvalue(),
            .mov_big => break :is_valid_instruction operands[0].is_lvalue() and operands[1].is_lvalue(),
            .movsx => break :is_valid_instruction operands[0].is_lvalue(),
            .call,
            .push,
            .pop,
            => break :is_valid_instruction true,
            .push_big => break :is_valid_instruction operands[0].is_lvalue(),
            .start_proc,
            .end_proc,
            => break :is_valid_instruction true,
        }
    });

    switch (new_opcode) {
        .mov => {
            const size = operands[0].grab_size();
            if (size > 8) {
                std.debug.assert(operands[1].grab_size() == size);
                new_opcode = .mov_big;
                operands[0] = operands[0].addr_of();
                operands[1] = operands[1].addr_of();
                operands[2] = .{ .Imm = size };
                operands_count = 3;
            }
        },
        .push => {
            const size = operands[0].grab_size_no_fail();
            if (size > 8) {
                new_opcode = .push_big;
                operands[0] = operands[0].addr_of();
                operands[1] = .{ .Imm = size };
                operands_count = 2;
            }
        },
        else => {},
    }

    var instruction = IR.Instruction{
        .opcode = new_opcode,
        .operands = undefined,
        .operands_count = operands_count,
    };

    for (operands[0..operands_count], 0..) |op, i| {
        instruction.operands[i] = op.to_operand();
    }

    write_instruction_at(generator, index, instruction);
}

fn write_instruction_at(generator: *IRGenerator, index: usize, instruction: IR.Instruction) void {
    // opcode:         1 byte
    // operands info:  3 bytes
    // operands:      48 bytes = 3 * (8 + 8)
    // total:         52 bytes
    const DataUnit = u32;
    var data = [1]DataUnit{0} ** ((1 + 3 + IR.Instruction.maximum_operands_count * (8 + 8)) / @sizeOf(DataUnit));
    var off: u5 = 0;

    data[0] |= @as(DataUnit, @intFromEnum(instruction.opcode)) << off;
    off += 8;

    var at: u8 = 1;
    for (instruction.operands[0..instruction.operands_count]) |op| {
        data[0] |= @as(DataUnit, @intFromEnum(op)) << off;
        off += @bitSizeOf(IR.Operand.Tag);

        switch (op) {
            .Tmp => |tmp| {
                const ptr: [*]const DataUnit = @ptrCast(&tmp);
                data[at + 0] = ptr[0];
                data[at + 1] = ptr[1];
                at += 2;
            },
            .Mem_B => |mem| {
                data[0] |= @as(DataUnit, mem.size_minus_one) << off;
                off += 3;

                const ptr: [*]const DataUnit = @ptrCast(&mem);
                data[at + 0] = ptr[0];
                data[at + 1] = ptr[1];
                at += 2;
            },
            .Mem_BO => |mem| {
                data[0] |= @as(DataUnit, mem.size_minus_one) << off;
                off += 3;

                const ptr: [*]const DataUnit = @ptrCast(&mem);
                data[at + 0] = ptr[0];
                data[at + 1] = ptr[1];
                data[at + 2] = ptr[2];
                data[at + 3] = ptr[3];
                at += 4;
            },
            .Addr_T => |tmp| {
                const ptr: [*]const DataUnit = @ptrCast(&tmp);
                data[at + 0] = ptr[0];
                data[at + 1] = ptr[1];
                at += 2;
            },
            .Addr_BO => |mem| {
                const ptr: [*]const DataUnit = @ptrCast(&mem);
                data[at + 0] = ptr[0];
                data[at + 1] = ptr[1];
                data[at + 2] = ptr[2];
                data[at + 3] = ptr[3];
                at += 4;
            },
            .Imm => |imm| {
                const ptr: [*]const DataUnit = @ptrCast(&imm);
                data[at + 0] = ptr[0];
                data[at + 1] = ptr[1];
                at += 2;
            },
            .Label => |label| {
                const ptr: [*]const DataUnit = @ptrCast(&label);
                data[at + 0] = ptr[0];
                data[at + 1] = ptr[1];
                at += 2;
            },
        }
    }

    {
        var bytes: []const u8 = undefined;
        bytes.ptr = @ptrCast(&data[0]);
        bytes.len = at * @sizeOf(DataUnit);

        var instructions = &generator.ir.instructions;

        if (index == instructions.items.len) {
            instructions.insertSlice(index, bytes) catch {
                exit(1);
            };
        } else {
            std.debug.assert(index + bytes.len <= generator.ir.instructions.items.len);
            instructions.replaceRange(index, bytes.len, bytes) catch {
                exit(1);
            };
        }
    }
}

fn deref(generator: *IRGenerator, src: IR.BigOperand, offset: u64, size: u64) IR.BigOperand {
    switch (src) {
        .Tmp => {
            return src.mem_from_tmp(offset, size);
        },
        .Mem => |mem| {
            std.debug.assert(mem.size <= 8);
            const dst = grab_local(generator, mem.size, .Qword);
            generate_instruction(generator, .mov, .{ dst, src });

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
            generate_instruction(generator, .uadd, .{ dst, src, IR.BigOperand{ .Imm = offset } });
            return dst.mem_from_tmp(offset, size);
        },
    }
}

pub fn grab_local(generator: *IRGenerator, byte_size: u64, alignment: Alignment) IR.BigOperand {
    const offset = nostd.align_up(@intCast(generator.next_local), alignment);
    generator.next_local = @intCast(offset + byte_size);
    generator.biggest_next_local = @max(generator.biggest_next_local, generator.next_local);
    return .{ .Tmp = .{
        .offset = @intCast(offset),
        .tag = .Relative,
        .size = byte_size,
    } };
}

pub fn grab_local_from_type(generator: *IRGenerator, data: *Ast.Type.SharedData) IR.BigOperand {
    return grab_local(generator, data.byte_size, data.alignment);
}

pub fn grab_global(generator: *IRGenerator, byte_size: u64, alignment: Alignment) IR.BigOperand {
    std.debug.assert(generator.next_global == generator.ir.globals.items.len);

    const old_offset = generator.next_global;
    const new_offset = nostd.align_up(old_offset, alignment);

    generator.next_global = new_offset + byte_size;
    generator.ir.globals.appendNTimes(0xAA, byte_size + (new_offset - old_offset)) catch {
        exit(1);
    };

    return .{ .Tmp = .{
        .offset = @intCast(new_offset),
        .tag = .Global,
        .size = byte_size,
    } };
}

pub fn grab_global_from_type(generator: *IRGenerator, data: *Ast.Type.SharedData) IR.BigOperand {
    return grab_global(generator, data.byte_size, data.alignment);
}

pub fn grab_label(generator: *IRGenerator) IR.Label {
    return grab_many_labels(generator, 1);
}

pub fn grab_many_labels(generator: *IRGenerator, count: usize) IR.Label {
    const label = generator.next_label;
    generator.next_label += count;
    return label;
}

pub fn grab_static_variable_storage(generator: *IRGenerator, variable: *Ast.Symbol.Variable) IR.BigOperand {
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

const exit = nostd.exit;

const std = @import("std");
const nostd = @import("nostd.zig");
const Compiler = @import("Compiler.zig");
const Ast = @import("Ast.zig");
const IR = @import("IR.zig");
const Interpreter = @import("Interpreter.zig");

const Alignment = nostd.Alignment;

pub const LabelMap = std.ArrayList(u64);
