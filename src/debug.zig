const std = @import("std");
const print = std.debug.print;

const ObjFunction = @import("object.zig").ObjFunction;

const chunks = @import("chunk.zig");
const Chunk = chunks.Chunk;
const Op = chunks.Op;
const printValue = @import("value.zig").printValue;

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) void {
    std.debug.print("== {s} ==\n", .{name});
    var i: usize = 0;
    var line: usize = 1;
    while (i < chunk.code.count) {
        i = disassembleInstruction(chunk, i, &line);
    }
}

pub fn disassembleInstruction(chunk: *Chunk, offset: usize, last_line: *usize) usize {
    print("{d:0>4} ", .{offset});
    // TODO: Fix this
    //const line = chunk.getLine(offset);
    const line = chunk.lines.items[offset];
    if (offset > 0 and line == last_line.*) {
        print("   | ", .{});
    } else {
        last_line.* = line;
        print("{d: >4} ", .{line});
    }
    //print("\n{any}\n", .{chunk.lines.items[0..line]});
    const instruction: Op = @enumFromInt(chunk.code.items[offset]);
    switch (instruction) {
        .CONSTANT => return constantInstruction("OP_CONSTANT", chunk, offset),
        .NIL => return simpleInstruction("OP_NIL", offset),
        .TRUE => return simpleInstruction("OP_TRUE", offset),
        .FALSE => return simpleInstruction("OP_FALSE", offset),
        .POP => return simpleInstruction("OP_POP", offset),
        .GET_LOCAL => return byteInstruction("OP_GET_LOCAL", chunk, offset),
        .SET_LOCAL => return byteInstruction("OP_SET_LOCAL", chunk, offset),
        .GET_GLOBAL => return constantInstruction("OP_GET_GLOBAL", chunk, offset),
        .DEFINE_GLOBAL => return constantInstruction("OP_DEFINE_GLOBAL", chunk, offset),
        .SET_GLOBAL => return constantInstruction("OP_SET_GLOBAL", chunk, offset),
        .GET_UPVALUE => return byteInstruction("OP_GET_UPVALUE", chunk, offset),
        .SET_UPVALUE => return byteInstruction("OP_SET_UPVALUE", chunk, offset),
        .EQUAL => return simpleInstruction("OP_EQUAL", offset),
        .GREATER => return simpleInstruction("OP_GREATER", offset),
        .LESS => return simpleInstruction("OP_LESS", offset),
        .ADD => return simpleInstruction("OP_ADD", offset),
        .SUBTRACT => return simpleInstruction("OP_SUBTRACT", offset),
        .MULTIPLY => return simpleInstruction("OP_MULTIPLY", offset),
        .DIVIDE => return simpleInstruction("OP_DIVIDE", offset),
        .NOT => return simpleInstruction("OP_NOT", offset),
        .NEGATE => return simpleInstruction("OP_NEGATE", offset),
        .PRINT => return simpleInstruction("OP_PRINT", offset),
        .JUMP => return jumpInstruction("OP_JUMP", 1, chunk, offset),
        .JUMP_IF_FALSE => return jumpInstruction("OP_JUMP_IF_FALSE", 1, chunk, offset),
        .LOOP => return jumpInstruction("OP_LOOP", -1, chunk, offset),
        .CALL => return byteInstruction("OP_CALL", chunk, offset),
        .CLOSE_UPVALUE => return simpleInstruction("OP_CLOSE_UPVALUE", offset),
        .CLOSURE => {
            var os = offset;
            os += 1;
            const constant = chunk.code.items[os];
            os += 1;
            print("{s: <16} {d:>4} ", .{ "OP_CLOSURE", constant });
            printValue(chunk.constants.items[constant]);
            print("\n", .{});

            const fun = @as(*ObjFunction, @ptrCast(chunk.constants.items[constant].obj));
            for (0..fun.upvalue_count) |_| {
                const is_local = chunk.code.items[os] == 1;
                os += 1;
                const index = chunk.code.items[os];
                os += 1;
                print("{d:0>4}      |                     {s} {d}\n", .{offset - 2, if (is_local) "local" else "upvalue", index});
            }

            return os;
        },
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

fn byteInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const slot = chunk.code.items[offset + 1];
    print("{s: <16} {d:>4}\n", .{ name, slot });
    return offset + 2;
}

fn jumpInstruction(name: []const u8, sign: isize, chunk: *Chunk, offset: usize) usize {
    const jump = std.mem.readPackedInt(u16, chunk.code.items[offset + 1 .. offset + 3], 0, .big);
    print("{s: <16} {d:>4} -> {d}\n", .{ name, offset, (@as(isize, @bitCast(offset)) + 3 + sign * jump) });
    return offset + 3;
}
