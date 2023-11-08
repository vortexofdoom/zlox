const std = @import("std");
const Allocator = std.mem.Allocator;
const chunk_ = @import("chunk.zig");
const Op = chunk_.Op;
const Chunk = chunk_.Chunk;
const value = @import("value.zig");
const Value = value.Value;
const printValue = value.printValue;
const print = std.debug.print;


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

pub fn deinit(self: *Self) void {
    _ = self;
    
}

pub fn interpret(self: *Self, chunk: *Chunk) InterpretError!void {
    self.chunk = chunk;
    self.ip = chunk.code.items.ptr;
    return self.run();
}

fn readByte(self: *Self) u8 {
    const byte = self.ip[0];
    self.ip += 1;
    return byte;
}

fn push(self: *Self, val: Value) void {
    self.sp[0] = val;
    self.sp += 1;
}

fn pop(self: *Self) Value {
    self.sp -= 1;
    return self.sp[0];
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

pub fn run(self: *Self) InterpretError!void {
    var offset: usize = 0;
    while (true) {
        if (comptime DEBUG_TRACE) {
            std.debug.print("          ", .{});
            var slot: [*]Value = &self.stack;
            //for (self.stack[0..@intFromPtr(self.sp) - @intFromPtr(&self.stack)]) |slot| {
            while (@intFromPtr(slot) < @intFromPtr(self.sp)) : (slot += 1) {
                std.debug.print("[ {d} ]", .{slot[0]});
            }
            std.debug.print("\n", .{}); 
            offset = @import("debug.zig").disassembleInstruction(self.chunk, offset);
        }

        const byte = @as(Op, @enumFromInt(self.readByte()));
        switch (byte) {
            .constant => {
                const constant = self.readConstant();
                self.push(constant);
                continue;
            },
            .add => try self.binaryOp(.add),
            .sub => try self.binaryOp(.sub),
            .mul => try self.binaryOp(.mul),
            .div => try self.binaryOp(.div),
            .negate => self.push(-self.pop()),
            .ret => {
                printValue(self.pop());
                std.debug.print("\n", .{});
                return;
            },
            else => return InterpretError.RuntimeError,
        }
    }
}