const std = @import("std");
const print = std.debug.print;

const chunks = @import("chunk.zig");
const Chunk = chunks.Chunk;
const Op = chunks.Op;

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) void {
    std.debug.print("== {s} ==\n", .{name});
    var i: usize = 0;
    while (i < chunk.code.items.len) {
        i = disassembleInstruction(chunk, i);
    }
}

pub fn disassembleInstruction(chunk: *Chunk, offset: usize) usize {
    print("{d:0>4} ", .{offset});
    const line = chunk.getLine(offset);
    if (offset > 0 and line == chunk.getLine(offset - 1)) {
        print("   | ", .{});
    } else {
        print("{d: >4} ", .{line});
    }
    const instruction: Op = @enumFromInt(chunk.code.items[offset]);
    switch (instruction) {
        .CONSTANT => return constantInstruction("OP_CONSTANT", chunk, offset),
        .NIL => return simpleInstruction("OP_NIL", offset),
        .TRUE => return simpleInstruction("OP_TRUE", offset),
        .FALSE => return simpleInstruction("OP_FALSE", offset),
        .ADD => return simpleInstruction("OP_ADD", offset),
        .SUBTRACT => return simpleInstruction("OP_SUBTRACT", offset),
        .MULTIPLY => return simpleInstruction("OP_MULTIPLY", offset),
        .DIVIDE => return simpleInstruction("OP_DIVIDE", offset),
        .NEGATE => return simpleInstruction("OP_NEGATE", offset),
        .RETURN => return simpleInstruction("OP_RETURN", offset),
        _ => {
            print("Unknown opcode {d}\n", .{@intFromEnum(instruction)});
            return offset + 1;
        },
    }
}

fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const constant = chunk.code.items[offset + 1];
    const value = chunk.constants.items[@as(usize, constant)];
    print("{s: <16} {d:>4} '", .{ name, constant });
    @import("value.zig").printValue(value);
    print("'\n", .{});
    return offset + 2;
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    print("{s}\n", .{name});
    return offset + 1;
}
