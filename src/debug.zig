const std = @import("std");
const print = std.debug.print;

const chunks = @import("chunk.zig");
const Chunk = chunks.Chunk;
const OpCode = chunks.OpCode;

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) void {
    std.debug.print("== {s} ==\n", .{name});
    var i: usize = 0;
    while (i < chunk.code.items.len) {
        i = disassembleInstruction(chunk, i);
    }
}

pub fn disassembleInstruction(chunk: *Chunk, offset: usize) usize {
    print("{d:0>4} ", .{offset});
    const instruction = @as(OpCode, @enumFromInt(chunk.code.items[offset]));
    switch (instruction) {
        .constant => return 0,
        .negate => return 0,
        .ret => return simpleInstruction("OP_RETURN\n", offset),
    }
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    print("{s}", .{name});
    return offset + 1;    
}
