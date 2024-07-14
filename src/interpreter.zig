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
}

fn interpret_top_level(interp: *Interp) void {
    for (interp.irc.instrs.items) |item| {
        switch (item) {
            .Print => |rvalue| {
                switch (rvalue) {
                    .Imm => |imm| {
                        utils.oprint("{}\n", .{imm});
                    },
                }
            },
        }
    }
}
