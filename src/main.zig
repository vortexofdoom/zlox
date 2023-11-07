const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const debug = @import("debug.zig");
const Vm = @import("vm.zig");

pub fn main() !void {
    var gen_purpose = std.heap.GeneralPurposeAllocator(.{}){};
    var alloc = gen_purpose.allocator();

    defer _ = gen_purpose.deinit();

    var chunk = try Chunk.init(alloc);
    defer chunk.deinit();

    const constant = try chunk.addConstant(1.2);
    try chunk.write(@intFromEnum(OpCode.constant), 123);
    try chunk.write(constant, 123);
    try chunk.write(@intFromEnum(OpCode.negate), 123);
    try chunk.write(@intFromEnum(OpCode.ret), 123);
    var vm = try Vm.init(alloc);
    defer vm.deinit();
    try vm.interpret(&chunk);
}
