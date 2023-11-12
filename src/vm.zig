const std = @import("std");
const Allocator = std.mem.Allocator;
const chunk_ = @import("chunk.zig");
const object = @import("object.zig");
const Obj = object.Obj;
const Op = chunk_.Op;
const Chunk = chunk_.Chunk;
const HashMap = @import("collections.zig").HashMap;
const value = @import("value.zig");
const Value = value.Value;
const printValue = value.printValue;
const print = std.debug.print;
const compile = @import("compiler.zig").compile;
const ObjFunction = object.ObjFunction;

const DEBUG_TRACE: bool = true;

const FRAMES_MAX = 64;
const STACK_MAX = 256 * FRAMES_MAX;

pub const InterpretError = error{
    CompileError,
    RuntimeError,
};

frames: [FRAMES_MAX]CallFrame,
frame_count: usize = 0,
allocator: Allocator,
chunk: *Chunk,
ip: [*]u8,
stack: [STACK_MAX]Value,
sp: [*]Value,
globals: HashMap,
strings: HashMap,
objects: ?*Obj,

const Self = @This();
pub var Vm: Self = .{
    .frames = [1]CallFrame{undefined} ** FRAMES_MAX,
    .allocator = undefined,
    .chunk = undefined,
    .ip = undefined,
    .stack = [_]Value{Value{ .nil = {} }} ** STACK_MAX,
    .sp = undefined,
    .strings = undefined,
    .globals = undefined,
    .objects = null,
};

const CallFrame = struct {
    function: *ObjFunction,
    ip: [*]u8,
    slots: [*]Value,

    inline fn readByte(frame: *CallFrame) u8 {
        const byte = frame.ip[0];
        frame.ip += 1;
        return byte;
    }

    inline fn readShort(frame: *CallFrame) u16 {
        frame.ip += 2;
        return std.mem.readInt(u16, (frame.ip - 2)[0..2], .big);
    }

    fn readConstant(frame: *CallFrame) Value {
        return frame.function.chunk.constants.items[frame.readByte()];
    }

    inline fn readString(frame: *CallFrame) *object.ObjString {
        return @ptrCast(frame.readConstant().obj);
    }
};

pub fn init(allocator: Allocator) !*Self {
    Vm.allocator = allocator;
    Vm.sp = Vm.stack[0..];
    Vm.strings.init();
    Vm.globals.init();
    return &Vm;
}

pub fn deinit() void {
    Vm.globals.free();
    Vm.strings.free();
    var obj = Vm.objects;
    while (obj) |o| {
        const next = o.next;
        o.free();
        obj = next;
    }
}

pub fn interpret(source: []const u8) InterpretError!void {
    const fun = compile(source) catch return InterpretError.CompileError;
    push(Value.obj(fun));
    var frame = &Vm.frames[Vm.frame_count];
    Vm.frame_count += 1;
    frame.function = fun;
    frame.ip = fun.chunk.code.items;
    frame.slots = &Vm.stack;

    try run();
}

inline fn push(val: Value) void {
    Vm.sp[0] = val;
    Vm.sp += 1;
}

inline fn pop() Value {
    Vm.sp -= 1;
    return Vm.sp[0];
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
        .GREATER => Value{ .bool = l > r },
        .LESS => Value{ .bool = l < r },
        else => unreachable,
    });
}

inline fn peek(distance: usize) Value {
    return (Vm.sp - distance - 1)[0];
}

fn concatenate() !void {
    const r = pop().obj.asString();
    const l = pop().obj.asString();
    const len = l.len + r.len;
    var chars = try Vm.allocator.alloc(u8, len);
    @memcpy(chars[0..l.len], l.ptr[0..l.len]);
    @memcpy(chars[l.len..len], r.ptr[0..r.len]);
    push(Value.obj(try object.takeString(chars, Vm.allocator)));
}

pub fn run() InterpretError!void {
    var frame = &Vm.frames[Vm.frame_count - 1];
    while (true) {
        if (comptime DEBUG_TRACE) {
            std.debug.print("          ", .{});
            var slot: [*]Value = frame.slots;
            //for (self.stack[0..@intFromPtr(self.sp) - @intFromPtr(&self.stack)]) |slot| {
            while (@intFromPtr(slot) < @intFromPtr(Vm.sp)) : (slot += 1) {
                std.debug.print("[ ", .{});
                printValue(slot[0]);
                std.debug.print(" ]", .{});
            }
            std.debug.print("\n", .{});
            _ = @import("debug.zig").disassembleInstruction(&frame.function.chunk, @intFromPtr(frame.ip - @intFromPtr(frame.function.chunk.code.items)));
        }

        const byte = @as(Op, @enumFromInt(frame.readByte()));
        switch (byte) {
            .CONSTANT => {
                const constant = frame.readConstant();
                push(constant);
            },
            .NIL => push(Value{ .nil = {} }),
            .TRUE => push(Value{ .bool = true }),
            .FALSE => push(Value{ .bool = false }),
            .POP => _ = pop(),
            .GET_LOCAL => {
                const slot = frame.readByte();
                push(frame.slots[slot]);
            },
            .SET_LOCAL => {
                const slot = frame.readByte();
                frame.slots[slot] = peek(0);
            },
            .GET_GLOBAL => {
                const name = frame.readString();
                const val = Vm.globals.get(name) orelse {
                    return InterpretError.RuntimeError;
                };
                push(val);
            },
            .DEFINE_GLOBAL => {
                _ = Vm.globals.insert(frame.readString(), peek(0)) catch return InterpretError.RuntimeError;
                _ = pop();
            },
            .SET_GLOBAL => {
                const name = frame.readString();
                if (Vm.globals.insert(name, peek(0)) catch return InterpretError.RuntimeError) {
                    _ = Vm.globals.delete(name);
                    // TODO: runtimeError()
                    return InterpretError.RuntimeError;
                }
            },
            .EQUAL => push(Value{ .bool = pop().equals(pop()) }),
            .GREATER => try binaryOp(.GREATER),
            .LESS => try binaryOp(.LESS),
            .ADD => {
                if (peek(0).valType() == peek(1).valType()) switch (peek(0)) {
                    .number => try binaryOp(.ADD),
                    .obj => |o| {
                        switch (o.*.type) {
                            .STRING => {
                                if (peek(1).objType() == .STRING) concatenate() catch {
                                    return InterpretError.RuntimeError;
                                };
                            },
                            else => return InterpretError.RuntimeError,
                        }
                        if (o.type == peek(1).objType()) {}
                    },
                    else => return InterpretError.RuntimeError,
                };
            },
            .SUBTRACT => try binaryOp(.SUBTRACT),
            .MULTIPLY => try binaryOp(.MULTIPLY),
            .DIVIDE => try binaryOp(.DIVIDE),
            .NOT => push(Value{ .bool = pop().isFalsey() }),
            .NEGATE => {
                switch (peek(0)) {
                    .number => push(Value{ .number = -(pop().number) }),
                    else => return InterpretError.RuntimeError,
                }
            },
            .PRINT => {
                printValue(pop());
                std.debug.print("\n", .{});
            },
            .JUMP => {
                const jump = frame.readShort();
                frame.ip += jump;
            },
            .JUMP_IF_FALSE => {
                const jump = frame.readShort();
                if (peek(0).isFalsey()) frame.ip += jump;
            },
            .LOOP => {
                const jump = frame.readShort();
                frame.ip -= jump;
            },
            .RETURN => {
                return;
            },
            else => return InterpretError.RuntimeError,
        }
    }
}
