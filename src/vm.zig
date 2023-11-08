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

pub const InterpretError = error {
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
    .stack = [1]Value{ 0.0 } ** STACK_MAX,
    .sp = undefined,
};

pub fn init(allocator: Allocator) !*Self {
    Vm.allocator = allocator;
    Vm.sp = Vm.stack[0..];
    return &Vm;
}

pub fn deinit() void {
    
}

pub fn interpret(source: []const u8) InterpretError!void {
    return compile(source);
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

fn readConstant(self: *Self) Value {
    return self.chunk.constants.items[self.readByte()];
}

inline fn binaryOp(self: *Self, comptime op: Op) !void {
    const b = self.pop();
    const a = self.pop();

    self.push(switch (op) {
        .add => a + b,
        .sub => a - b,
        .mul => a * b,
        .div => a / b,
        else => unreachable,
    });
}

pub fn run() InterpretError!void {
    var offset: usize = 0;
    while (true) {
        if (comptime DEBUG_TRACE) {
            std.debug.print("          ", .{});
            var slot: [*]Value = &Vm.stack;
            //for (self.stack[0..@intFromPtr(self.sp) - @intFromPtr(&self.stack)]) |slot| {
            while (@intFromPtr(slot) < @intFromPtr(Vm.sp)) : (slot += 1) {
                std.debug.print("[ {d} ]", .{slot[0]});
            }
            std.debug.print("\n", .{}); 
            offset = @import("debug.zig").disassembleInstruction(Vm.chunk, offset);
        }

        const byte = @as(Op, @enumFromInt(readByte()));
        switch (byte) {
            .constant => {
                const constant = readConstant();
                push(constant);
                continue;
            },
            .add => try binaryOp(.add),
            .sub => try binaryOp(.sub),
            .mul => try binaryOp(.mul),
            .div => try binaryOp(.div),
            .negate => push(-pop()),
            .ret => {
                printValue(pop());
                std.debug.print("\n", .{});
                return;
            },
            else => return InterpretError.RuntimeError,
        }
    }
}