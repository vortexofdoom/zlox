const std = @import("std");
const Allocator = std.mem.Allocator;
const chunk_ = @import("chunk.zig");
const Op = chunk_.Op;
const Chunk = chunk_.Chunk;
const value = @import("value.zig");
const Value = value.Value;
const printValue = value.printValue;
const print = std.debug.print;
const compile = @import("compiler.zig").compile;

const DEBUG_TRACE: bool = true;

const STACK_MAX = 256;

pub const InterpretError = error{
    CompileError,
    RuntimeError,
};

allocator: Allocator,
chunk: *Chunk,
ip: [*]u8,
stack: [STACK_MAX]Value,
sp: [*]Value,

const Self = @This();
pub var Vm: Self = .{
    .allocator = undefined,
    .chunk = undefined,
    .ip = undefined,
    .stack = [_]Value{Value{ .nil = {} }} ** STACK_MAX,
    .sp = undefined,
};

pub fn init(allocator: Allocator) !*Self {
    Vm.allocator = allocator;
    Vm.sp = Vm.stack[0..];
    return &Vm;
}

pub fn deinit() void {}

pub fn interpret(source: []const u8) InterpretError!void {
    var chunk = try Chunk.init(Vm.allocator);
    defer chunk.deinit();
    try compile(source, &chunk);
    Vm.chunk = &chunk;
    Vm.ip = Vm.chunk.code.items.ptr;

    try run();
}

inline fn readByte() u8 {
    const byte = Vm.ip[0];
    Vm.ip += 1;
    return byte;
}

inline fn push(val: Value) void {
    Vm.sp[0] = val;
    Vm.sp += 1;
}

inline fn pop() Value {
    Vm.sp -= 1;
    return Vm.sp[0];
}

fn readConstant() Value {
    return Vm.chunk.constants.items[readByte()];
}

inline fn binaryOp(comptime op: Op) !void {
    const r = switch (pop()) {
        .number => |n| n,
        else => return InterpretError.RuntimeError,
    };
    const l = switch (pop()) {
        .number => |n| n,
        else => return InterpretError.RuntimeError,
    };
    push(switch (op) {
        .ADD => Value{ .number = l + r },
        .SUBTRACT => Value{ .number = l - r },
        .MULTIPLY => Value{ .number = l * r },
        .DIVIDE => Value{ .number = l / r },
        else => unreachable,
    });
}

inline fn peek(distance: isize) Value {
    return (Vm.sp + distance - 1)[0];
}

pub fn run() InterpretError!void {
    var offset: usize = 0;
    while (true) {
        if (comptime DEBUG_TRACE) {
            std.debug.print("          ", .{});
            var slot: [*]Value = &Vm.stack;
            //for (self.stack[0..@intFromPtr(self.sp) - @intFromPtr(&self.stack)]) |slot| {
            while (@intFromPtr(slot) < @intFromPtr(Vm.sp)) : (slot += 1) {
                std.debug.print("[ ", .{});
                printValue(slot[0]);
                std.debug.print(" ]", .{});
            }
            std.debug.print("\n", .{});
            offset = @import("debug.zig").disassembleInstruction(Vm.chunk, offset);
        }

        const byte = @as(Op, @enumFromInt(readByte()));
        switch (byte) {
            .CONSTANT => {
                const constant = readConstant();
                push(constant);
            },
            .NIL => push(Value{ .nil = {} }),
            .TRUE => push(Value{ .bool = true }),
            .FALSE => push(Value{ .bool = false }),
            .ADD => try binaryOp(.ADD),
            .SUBTRACT => try binaryOp(.SUBTRACT),
            .MULTIPLY => try binaryOp(.MULTIPLY),
            .DIVIDE => try binaryOp(.DIVIDE),
            .NEGATE => {
                switch (peek(0)) {
                    .number => push(Value{ .number = -(pop().number) }),
                    else => return InterpretError.RuntimeError,
                }
            },
            .RETURN => {
                printValue(pop());
                std.debug.print("\n", .{});
                return;
            },
            else => return InterpretError.RuntimeError,
        }
    }
}
