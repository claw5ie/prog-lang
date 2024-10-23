const std = @import("std");
const utils = @import("utils.zig");
const Compiler = @import("compiler.zig");

const Ast = Compiler.Ast;
const IR = Compiler.IR;
const IRE = IR.Encoded;
const IRD = IR.Decoded;
const Alignment = utils.Alignment;

pub fn generate_ir(c: *Compiler) void {
    generate_ir_top_level(c);
    generate_ir_instr0(c, .exit);
    remove_labels_and_set_vtable(c);
}

pub fn remove_labels_and_set_vtable(c: *Compiler) void {
    c.init_vtable();

    var instrs = &c.ir.instrs;
    var at: usize = 0;
    while (at < instrs.items.len) {
        var instr, const count = IRD.decode_instr(instrs.items[at..]);

        var update_instr = false;
        for (instr.ops[0..instr.ops_count]) |*op| {
            switch (op.*) {
                .Label => |label| {
                    op.* = .{ .Imm = c.interp.vtable.start_instr + c.irgen.labels.items[label] };
                    update_instr = true;
                },
                else => {},
            }
        }
        if (update_instr) {
            write_instr_at(c, at, instr);
        }

        at += count;
    }
}

fn generate_ir_top_level(c: *Compiler) void {
    {
        const Procedure = &c.ast.main.?.as.Procedure;
        generate_ir_instr1(c, .call, .{ .Label = Procedure.start_label });
        generate_ir_instr0(c, .exit);
    }

    var it = c.ast.globals.first;
    while (it) |node| {
        generate_ir_global_symbol(c, node.data);
        it = node.next;
    }
}

fn generate_ir_global_symbol(c: *Compiler, symbol: *Ast.Symbol) void {
    switch (symbol.as) {
        .Variable => {},
        .Procedure => |*Procedure| {
            const Proc = &Procedure.typ.data.as.Proc;

            var offset: IRE.Tmp.OffsetType = -IRE.FIRST_PARAM_OFFSET;

            if (Proc.return_type.equal(Ast.void_type)) { // TODO: replace this condition with a flag.
                offset += 8; // There is no return pointer.
            }

            var it = Proc.params.first;
            while (it) |node| {
                const Parameter = &node.data.as.Parameter;

                const size = utils.align_u64(Parameter.typ.data.byte_size, .QWORD);
                offset -= @intCast(size);
                Parameter.storage = .{ .Tmp = .{
                    .offset = offset,
                    .tag = .Relative,
                    .size = size,
                } };

                it = node.next;
            }

            std.debug.assert(c.irgen.return_label == null and
                c.irgen.next_local == 0 and
                c.irgen.biggest_next_local == 0);
            c.irgen.return_label = Procedure.end_label;

            generate_ir_label(c, Procedure.start_label);
            const start_proc_index = c.ir.instrs.items.len;
            generate_ir_instr1(
                c,
                .start_proc,
                .{ .Imm = 0 },
            );

            generate_ir_stmt_list(c, Procedure.block);

            const stack_space_used = utils.align_u64(c.irgen.biggest_next_local, .QWORD);
            generate_ir_label(c, Procedure.end_label);
            generate_ir_instr1(
                c,
                .end_proc,
                .{ .Imm = stack_space_used },
            );
            generate_ir_instr_at1(
                c,
                start_proc_index,
                .start_proc,
                .{ .Imm = stack_space_used },
            );

            c.irgen.next_local = 0;
            c.irgen.biggest_next_local = 0;
            c.irgen.return_label = null;
        },
        .Type => |typ| {
            generate_ir_type(c, typ);
        },
        .Parameter,
        .Struct_Field,
        .Union_Field,
        .Enum_Field,
        => unreachable,
    }
}

fn generate_ir_local_symbol(c: *Compiler, symbol: *Ast.Symbol) void {
    switch (symbol.as) {
        .Variable => |*Variable| {
            if (!(symbol.attributes.is_static or symbol.attributes.is_const)) {
                Variable.storage = grab_local_from_type(c, Variable.typ.?.data);
            }

            if (Variable.value) |value| {
                _ = generate_ir_expr(c, Variable.storage, value);
            }
        },
        .Type => |typ| {
            generate_ir_type(c, typ);
        },
        .Procedure,
        .Parameter,
        .Struct_Field,
        .Union_Field,
        .Enum_Field,
        => unreachable,
    }
}

fn generate_ir_type(c: *Compiler, typ: *Ast.Type) void {
    const data = typ.data;
    switch (data.stages.ir_generation) {
        .None => data.stages.ir_generation = .Going,
        .Going => unreachable,
        .Done => return,
    }

    defer data.stages.ir_generation = .Done;

    switch (data.as) {
        .Struct, .Union => |Struct| {
            var it = Struct.rest.first;
            while (it) |node| {
                generate_ir_global_symbol(c, node.data);
                it = node.next;
            }
        },
        .Enum => |Enum| {
            var it = Enum.rest.first;
            while (it) |node| {
                generate_ir_global_symbol(c, node.data);
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

fn generate_ir_stmt(c: *Compiler, stmt: *Ast.Stmt) void {
    switch (stmt.as) {
        .Print => |expr| {
            const src = generate_ir_expr(c, null, expr);
            switch (expr.typ.data.as) {
                .Enum => {
                    generate_ir_instr1(
                        c,
                        if (expr.typ.is_signed()) .printi else .printu,
                        src,
                    );
                },
                .Pointer => {
                    generate_ir_instr1(c, .printp, src);
                },
                .Integer => {
                    generate_ir_instr1(
                        c,
                        if (expr.typ.is_signed()) .printi else .printu,
                        src,
                    );
                },
                .Bool => {
                    generate_ir_instr1(c, .printb, src);
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
            const old_next_local = c.irgen.next_local;
            generate_ir_stmt_list(c, block);
            c.irgen.next_local = old_next_local;
        },
        .If => |If| {
            if (If.false_branch) |false_branch| {
                const false_branch_label = grab_label(c);
                const end_label = grab_label(c);

                generate_ir_jmpc(c, If.condition, false_branch_label, false);

                _ = generate_ir_stmt(c, If.true_branch);
                generate_ir_instr1(c, .jmp, .{ .Label = end_label });
                generate_ir_label(c, false_branch_label);
                _ = generate_ir_stmt(c, false_branch);
                generate_ir_label(c, end_label);
            } else {
                const end_label = grab_label(c);
                generate_ir_jmpc(c, If.condition, end_label, false);

                _ = generate_ir_stmt(c, If.true_branch);
                generate_ir_label(c, end_label);
            }
        },
        .While, .Do_While => |While| {
            const start_label = grab_label(c);
            const condition_label = grab_label(c);
            const end_label = grab_label(c);

            const old_loop_condition_label = c.irgen.loop_condition_label;
            const old_loop_end_label = c.irgen.loop_end_label;

            c.irgen.loop_condition_label = condition_label;
            c.irgen.loop_end_label = end_label;

            if (stmt.as == .While) {
                generate_ir_instr1(c, .jmp, .{ .Label = condition_label });
            }
            generate_ir_label(c, start_label);
            generate_ir_stmt(c, While.body);
            generate_ir_label(c, condition_label);
            generate_ir_jmpc(c, While.condition, start_label, true);
            generate_ir_label(c, end_label);

            c.irgen.loop_condition_label = old_loop_condition_label;
            c.irgen.loop_end_label = old_loop_end_label;
        },
        .Break => {
            generate_ir_instr1(c, .jmp, .{ .Label = c.irgen.loop_end_label.? });
        },
        .Continue => {
            generate_ir_instr1(c, .jmp, .{ .Label = c.irgen.loop_condition_label.? });
        },
        .Switch => |Switch| {
            const condition = generate_ir_expr(c, null, Switch.condition);
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
                                const src0 = generate_ir_expr(c, null, Case.value);
                                generate_ir_instr3(
                                    c,
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
                generate_ir_stmt(c, else_stmt);
            }

            generate_ir_instr1(c, .jmp, .{ .Label = end_label });

            {
                var label = first_label;
                var it = Switch.cases.first;
                while (it) |node| {
                    var case = node.data;
                    while (true) {
                        switch (case.*) {
                            .Case => |Case| case = Case.subcase,
                            .Stmt => |substmt| {
                                generate_ir_label(c, label);
                                generate_ir_stmt(c, substmt);
                                generate_ir_instr1(c, .jmp, .{ .Label = end_label });
                                break;
                            },
                        }
                    }
                    label += 1;

                    it = node.next;
                }
            }

            generate_ir_label(c, end_label);
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
                const src = generate_ir_expr(c, null, expr);
                generate_ir_instr2(c, .mov, dst, src);
            }

            generate_ir_instr1(c, .jmp, .{ .Label = c.irgen.return_label.? });
        },
        .Symbol => |symbol| {
            generate_ir_local_symbol(c, symbol);
        },
        .Assign => |Assign| {
            const dst = generate_ir_expr(c, null, Assign.lhs);
            _ = generate_ir_expr(c, dst, Assign.rhs);
        },
        .Expr => |expr| {
            _ = generate_ir_expr(c, null, expr);
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

pub fn generate_ir_expr(c: *Compiler, has_dst: ?IRE.Operand, expr: *Ast.Expr) IRE.Operand {
    const State = struct {
        c: *Compiler,
        has_dst: ?IRE.Operand,
    };

    const fns = struct {
        pub fn maybe_grab_local_from_type(state: *State, data: *Ast.Type.SharedData) *IRE.Operand {
            if (state.has_dst) |*dst| {
                return dst;
            } else {
                state.has_dst = grab_local_from_type(state.c, data);
                return &state.has_dst.?;
            }
        }

        pub fn move_to_dst(state: *State, src: IRE.Operand) void {
            if (state.has_dst) |dst| {
                generate_ir_instr2(state.c, .mov, dst, src);
            } else {
                state.has_dst = src;
            }
        }
    };

    var state = State{
        .c = c,
        .has_dst = has_dst,
    };

    switch (expr.as) {
        .Binary_Op => |Binary_Op| {
            const src0 = generate_ir_expr(c, null, Binary_Op.lhs);
            const src1 = generate_ir_expr(c, null, Binary_Op.rhs);
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

            generate_ir_instr3(c, opcode, dst.*, src0, src1);
        },
        .Unary_Op => |Unary_Op| {
            const src = generate_ir_expr(c, null, Unary_Op.subexpr);

            switch (Unary_Op.tag) {
                .Pos => {
                    fns.move_to_dst(&state, src);
                },
                .Neg => {
                    const dst = fns.maybe_grab_local_from_type(&state, expr.typ.data);
                    generate_ir_instr2(c, .neg, dst.*, src);
                },
                .Not => {
                    const dst = fns.maybe_grab_local_from_type(&state, expr.typ.data);
                    generate_ir_instr2(c, .not, dst.*, src);
                },
            }
        },
        .Ref => |subexpr| {
            const src = generate_ir_expr(c, null, subexpr);
            const new_src = src.addr_of();
            fns.move_to_dst(&state, new_src);
        },
        .Deref => |subexpr| {
            const src = generate_ir_expr(c, null, subexpr);
            const new_src = deref(c, src, 0, expr.typ.data.byte_size);
            fns.move_to_dst(&state, new_src);
        },
        .If => |If| {
            const false_branch_label = grab_label(c);
            const end_label = grab_label(c);

            generate_ir_jmpc(c, If.condition, false_branch_label, false);
            const dst = fns.maybe_grab_local_from_type(&state, expr.typ.data);

            _ = generate_ir_expr(c, dst.*, If.true_branch);
            generate_ir_instr1(c, .jmp, .{ .Label = end_label });
            generate_ir_label(c, false_branch_label);
            _ = generate_ir_expr(c, dst.*, If.false_branch);
            generate_ir_label(c, end_label);
        },
        .Field => |Field| {
            const offset: u64 = switch (Field.field.as.Symbol.as) {
                .Struct_Field, .Union_Field => |Struct_Field| Struct_Field.offset,
                else => unreachable,
            };

            const src = generate_ir_expr(c, null, Field.subexpr);
            const new_src: IRE.Operand = if (Field.subexpr.typ.data.as != .Pointer)
                src.bump(offset, expr.typ.data.byte_size)
            else
                deref(c, src, offset, expr.typ.data.byte_size);

            fns.move_to_dst(&state, new_src);
        },
        .Call => |Call| {
            const dst = fns.maybe_grab_local_from_type(&state, expr.typ.data);
            const old_next_local = c.irgen.next_local;

            var bytes_pushed: u64 = 0;

            var it = Call.args.last;
            while (it) |node| {
                const arg = node.data.Expr;
                const src = generate_ir_expr(c, null, arg);
                generate_ir_instr1(c, .push, src);
                bytes_pushed += utils.align_u64(arg.typ.data.byte_size, .QWORD);

                it = node.prev;
            }

            if (!expr.typ.equal(Ast.void_type)) {
                generate_ir_instr1(c, .push, dst.addr_of());
                bytes_pushed += 8;
            }

            const src = generate_ir_expr(c, null, Call.subexpr);
            generate_ir_instr1(c, .call, src);

            if (bytes_pushed > 0) {
                generate_ir_instr1(c, .pop, .{ .Imm = bytes_pushed });
            }

            c.irgen.next_local = old_next_local;
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
                        _ = generate_ir_expr(c, new_dst, Designator.rhs);

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
                        _ = generate_ir_expr(c, new_dst, arg);

                        offset += size;
                        offset = utils.align_u64(offset, Array.subtype.data.alignment);
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
                    const src = generate_ir_expr(c, null, arg);
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
            const subexpr = generate_ir_expr(c, null, Subscript.subexpr);
            const dst = grab_local(c, 8, .QWORD);

            {
                const index = generate_ir_expr(c, null, Subscript.index);
                const offset = utils.align_u64(expr.typ.data.byte_size, expr.typ.data.alignment);
                generate_ir_instr3(c, .umul, dst, index, .{ .Imm = offset });
            }

            if (Subscript.subexpr.typ.data.as != .Pointer) {
                generate_ir_instr3(c, .uadd, dst, dst, subexpr.addr_of());
            } else {
                generate_ir_instr3(c, .uadd, dst, dst, subexpr);
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
                    const src = generate_ir_expr(c, null, Cast.expr);
                    fns.move_to_dst(&state, src);
                },
                .Enum => unreachable,
                .Integer => |dInteger| {
                    const src = generate_ir_expr(c, null, Cast.expr);

                    _ = src: {
                        switch (Cast.expr.typ.data.as) {
                            .Integer => |sInteger| {
                                if (dInteger.bits > sInteger.bits and dInteger.is_signed and sInteger.is_signed) {
                                    const dst = fns.maybe_grab_local_from_type(&state, expr.typ.data);
                                    generate_ir_instr3(c, .movsx, dst.*, src, .{ .Imm = sInteger.bits });
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
                    const src = generate_ir_expr(c, null, Cast.expr);
                    const dst = fns.maybe_grab_local_from_type(&state, expr.typ.data);
                    generate_ir_instr2(c, .setnz, dst.*, src);
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
                .Variable => |Variable| {
                    fns.move_to_dst(&state, Variable.storage);
                },
                .Parameter => |Parameter| {
                    fns.move_to_dst(&state, Parameter.storage);
                },
                .Procedure => |*Procedure| {
                    fns.move_to_dst(&state, .{ .Label = Procedure.start_label });
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

fn generate_ir_jmpc(c: *Compiler, expr: *Ast.Expr, label: IRE.Label, jump_if_true: bool) void {
    var src0: IRE.Operand = undefined;
    var src1: IRE.Operand = .{ .Imm = 0 };
    var opcode: IRE.Opcode = .ujne;
    var is_signed = false;

    _ = fill: {
        switch (expr.as) {
            .Binary_Op => |Binary_Op| {
                switch (Binary_Op.tag) {
                    .Eq, .Neq, .Lt, .Leq, .Gt, .Geq => {
                        src0 = generate_ir_expr(c, null, Binary_Op.lhs);
                        src1 = generate_ir_expr(c, null, Binary_Op.rhs);

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
                        generate_ir_jmpc(c, Unary_Op.subexpr, label, !jump_if_true);
                        return;
                    },
                    else => {},
                }
            },
            else => {},
        }

        src0 = generate_ir_expr(c, null, expr);
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

    generate_ir_instr3(c, opcode, src0, src1, .{ .Label = label });
}

fn generate_ir_label(c: *Compiler, label: IRE.Label) void {
    var labels = &c.irgen.labels;

    var capacity = labels.capacity;
    if (capacity < label) {
        capacity = label + capacity / 2 + 1;
        labels.resize(capacity) catch {
            Compiler.exit(1);
        };
    }

    labels.items[label] = c.ir.instrs.items.len;
}

pub fn generate_ir_instr0(c: *Compiler, opcode: IRE.Opcode) void {
    var params = IRE.Instr{
        .opcode = opcode,
        .ops = .{ undefined, undefined, undefined },
        .ops_count = 0,
    };
    generate_ir_instr(c, &params);
}

pub fn generate_ir_instr1(c: *Compiler, opcode: IRE.Opcode, op0: IRE.Operand) void {
    var params = IRE.Instr{
        .opcode = opcode,
        .ops = .{ op0, undefined, undefined },
        .ops_count = 1,
    };
    generate_ir_instr(c, &params);
}

pub fn generate_ir_instr2(c: *Compiler, opcode: IRE.Opcode, op0: IRE.Operand, op1: IRE.Operand) void {
    var params = IRE.Instr{
        .opcode = opcode,
        .ops = .{ op0, op1, undefined },
        .ops_count = 2,
    };
    generate_ir_instr(c, &params);
}

pub fn generate_ir_instr3(c: *Compiler, opcode: IRE.Opcode, op0: IRE.Operand, op1: IRE.Operand, op2: IRE.Operand) void {
    var params = IRE.Instr{
        .opcode = opcode,
        .ops = .{ op0, op1, op2 },
        .ops_count = 3,
    };
    generate_ir_instr(c, &params);
}

pub fn generate_ir_instr_at0(c: *Compiler, index: usize, opcode: IRE.Opcode) void {
    var params = IRE.Instr{
        .opcode = opcode,
        .ops = .{ undefined, undefined, undefined },
        .ops_count = 0,
    };
    generate_ir_instr_at(c, index, &params);
}

pub fn generate_ir_instr_at1(c: *Compiler, index: usize, opcode: IRE.Opcode, op0: IRE.Operand) void {
    var params = IRE.Instr{
        .opcode = opcode,
        .ops = .{ op0, undefined, undefined },
        .ops_count = 1,
    };
    generate_ir_instr_at(c, index, &params);
}

pub fn generate_ir_instr2_at(c: *Compiler, index: usize, opcode: IRE.Opcode, op0: IRE.Operand, op1: IRE.Operand) void {
    var params = IRE.Instr{
        .opcode = opcode,
        .ops = .{ op0, op1, undefined },
        .ops_count = 2,
    };
    generate_ir_instr_at(c, index, &params);
}

pub fn generate_ir_instr_at3(c: *Compiler, index: usize, opcode: IRE.Opcode, op0: IRE.Operand, op1: IRE.Operand, op2: IRE.Operand) void {
    var params = IRE.Instr{
        .opcode = opcode,
        .ops = .{ op0, op1, op2 },
        .ops_count = 3,
    };
    generate_ir_instr_at(c, index, &params);
}

fn generate_ir_instr(c: *Compiler, params: *IRE.Instr) void {
    generate_ir_instr_at(c, c.ir.instrs.items.len, params);
}

fn generate_ir_instr_at(c: *Compiler, index: usize, p: *IRE.Instr) void {
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

    write_instr_at(c, index, instr);
}

fn write_instr_at(c: *Compiler, index: usize, instr: IRD.Instr) void {
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
        write_instr_bytes_at(c, index, bytes);
    }
}

fn write_instr_bytes_at(c: *Compiler, index: usize, bytes: []const u8) void {
    if (index == c.ir.instrs.items.len) {
        c.ir.instrs.insertSlice(index, bytes) catch {
            Compiler.exit(1);
        };
    } else {
        std.debug.assert(index + bytes.len <= c.ir.instrs.items.len);
        c.ir.instrs.replaceRange(index, bytes.len, bytes) catch {
            Compiler.exit(1);
        };
    }
}

fn deref(c: *Compiler, src: IRE.Operand, offset: u64, size: u64) IRE.Operand {
    switch (src) {
        .Tmp => {
            return src.mem_from_tmp(offset, size);
        },
        .Mem => |mem| {
            std.debug.assert(mem.size <= 8);
            const dst = grab_local(c, mem.size, .QWORD);
            generate_ir_instr2(c, .mov, dst, src);

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
            const dst = grab_local(c, 8, .QWORD);
            generate_ir_instr3(c, .uadd, dst, src, .{ .Imm = offset });
            return dst.mem_from_tmp(offset, size);
        },
    }
}

pub fn grab_local(c: *Compiler, byte_size: u64, alignment: Alignment) IRE.Operand {
    const offset = utils.align_u64(@intCast(c.irgen.next_local), alignment);
    c.irgen.next_local = @intCast(offset + byte_size);
    c.irgen.biggest_next_local = @max(c.irgen.biggest_next_local, c.irgen.next_local);
    return .{ .Tmp = .{
        .offset = @intCast(offset),
        .tag = .Relative,
        .size = byte_size,
    } };
}

pub fn grab_local_from_type(c: *Compiler, data: *Ast.Type.SharedData) IRE.Operand {
    return grab_local(c, data.byte_size, data.alignment);
}

pub fn grab_global(c: *Compiler, byte_size: u64, alignment: Alignment) IRE.Operand {
    std.debug.assert(c.irgen.next_global == c.ir.globals.items.len);

    const old_offset = c.irgen.next_global;
    const new_offset = utils.align_u64(old_offset, alignment);

    c.irgen.next_global = new_offset + byte_size;
    c.ir.globals.appendNTimes(0xAA, byte_size + (new_offset - old_offset)) catch {
        Compiler.exit(1);
    };

    return .{ .Tmp = .{
        .offset = @intCast(new_offset),
        .tag = .Global,
        .size = byte_size,
    } };
}

pub fn grab_global_from_type(c: *Compiler, data: *Ast.Type.SharedData) IRE.Operand {
    return grab_global(c, data.byte_size, data.alignment);
}

pub fn grab_label(c: *Compiler) IRE.Label {
    return grab_many_labels(c, 1);
}

pub fn grab_many_labels(c: *Compiler, count: usize) IRE.Label {
    const label = c.irgen.next_label;
    c.irgen.next_label += count;
    return label;
}
