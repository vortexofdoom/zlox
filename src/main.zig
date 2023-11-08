const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const Op = @import("chunk.zig").Op;
const debug = @import("debug.zig");
const Vm = @import("vm.zig");

pub fn main() !void {
    var gen_purpose = std.heap.GeneralPurposeAllocator(.{}){};
    var alloc = gen_purpose.allocator();

    defer _ = gen_purpose.deinit();

    var chunk = try Chunk.init(alloc);
    defer chunk.deinit();

    var constant = try chunk.addConstant(1.2);
    try chunk.write(@intFromEnum(Op.constant), 123);
    try chunk.write(constant, 123);

    constant = try chunk.addConstant(3.4);
    try chunk.write(@intFromEnum(Op.constant), 123);
    try chunk.write(constant, 123);

    try chunk.write(@intFromEnum(Op.add), 123);

    constant = try chunk.addConstant(5.6);
    try chunk.write(@intFromEnum(Op.constant), 123);
    try chunk.write(constant, 123);

    try chunk.write(@intFromEnum(Op.div), 123);
    try chunk.write(@intFromEnum(Op.negate), 123);
    try chunk.write(@intFromEnum(Op.ret), 123);
    var vm = try Vm.init(alloc);

    defer vm.deinit();
    try vm.interpret(&chunk);
}
