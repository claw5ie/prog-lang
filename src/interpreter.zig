irc: *IRC,

const std = @import("std");
const utils = @import("utils.zig");
const IRC = @import("ircode.zig");

const Interp = @This();

pub fn interpret(irc: *IRC) void {
    var interp = Interp{
        .irc = irc,
    };

    interpret_top_level(&interp);
    utils.oprint("\n", .{});
}

fn interpret_top_level(interp: *Interp) void {
    for (interp.irc.instrs.items) |item| {
        switch (item) {
            .Printb => |rvalue| {
                switch (rvalue) {
                    .Imm => |imm| {
                        utils.oprint("{s}\n", .{if (imm != 0) "true" else "false"});
                    },
                }
            },
            .Printi => |rvalue| {
                switch (rvalue) {
                    .Imm => |imm| {
                        utils.oprint("{}\n", .{@as(i64, @bitCast(imm))});
                    },
                }
            },
            .Printu => |rvalue| {
                switch (rvalue) {
                    .Imm => |imm| {
                        utils.oprint("{}\n", .{imm});
                    },
                }
            },
        }
    }
}
