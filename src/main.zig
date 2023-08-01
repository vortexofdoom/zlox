const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const debug = @import("debug.zig");

pub fn main() !void {
    var gen_purpose = std.heap.GeneralPurposeAllocator(.{}){};
    var alloc = gen_purpose.allocator();

    defer _ = gen_purpose.deinit();

    var chunk = try Chunk.init(alloc);
    defer chunk.deinit();

    try chunk.write(@intFromEnum(OpCode.ret));
    debug.disassembleChunk(&chunk, "test chunk");

}
