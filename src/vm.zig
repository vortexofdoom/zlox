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
const NativeFn = object.NativeFn;
const ObjNative = object.ObjNative;
const copyString = object.copyString;

const DEBUG_TRACE: bool = false;

const FRAMES_MAX = 64;
const STACK_MAX = 256 * FRAMES_MAX;

pub const InterpretError = error{
    CompileError,
    RuntimeError,
};

var start: std.time.Instant = undefined;

fn clock(_: []Value) !Value {
    const now = try std.time.Instant.now();

    return Value{ .number = @as(f64, @floatFromInt(now.since(start))) / 1e9 };
}

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
pub var vm: Self = .{
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
    start = try std.time.Instant.now();
    vm.allocator = allocator;
    vm.sp = vm.stack[0..];
    vm.strings.init();
    vm.globals.init();
    try defineNative("clock", clock);
    return &vm;
}

pub fn deinit() void {
    vm.globals.free();
    vm.strings.free();
    var obj = vm.objects;
    while (obj) |o| {
        const next = o.next;
        o.free();
        obj = next;
    }
}

fn runtimeError(comptime fmt: []const u8, args: anytype) InterpretError {
    print(fmt, args);
    var i = vm.frame_count - 1;
    while (i < std.math.maxInt(usize)) : (i -%= 1) {
        const frame = &vm.frames[i];
        const function = frame.function;
        const instruction = @intFromPtr(frame.ip - @intFromPtr(function.chunk.code.items) - 1);
        std.debug.print("[line {d}] in ", .{ function.chunk.getLine(instruction) });
        if (function.name) |name| {
            print("{s}()\n", .{ name.ptr[0..name.len]});
        } else print("script\n", .{});
    }
    return InterpretError.RuntimeError;
}

fn defineNative(name: []const u8, function: NativeFn) !void {
    push(Value.obj(try copyString(name)));
    push(Value.obj(try ObjNative.new(function)));
    printValue(peek(1));
    printValue(peek(0));
    _ = try vm.globals.insert(@ptrCast(@alignCast(vm.stack[0].obj)), vm.stack[1]);
    _ = pop();
    _ = pop();
}

pub fn interpret(source: []const u8) InterpretError!void {
    const fun = compile(source) catch return InterpretError.CompileError;
    push(Value.obj(fun));
    try call(fun, 0);

    try run();
}

inline fn push(val: Value) void {
    vm.sp[0] = val;
    vm.sp += 1;
}

inline fn pop() Value {
    vm.sp -= 1;
    return vm.sp[0];
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
    return (vm.sp - distance - 1)[0];
}

fn concatenate() !void {
    const r = pop().obj.asString();
    const l = pop().obj.asString();
    const len = l.len + r.len;
    var chars = try vm.allocator.alloc(u8, len);
    @memcpy(chars[0..l.len], l.ptr[0..l.len]);
    @memcpy(chars[l.len..len], r.ptr[0..r.len]);
    push(Value.obj(try object.takeString(chars)));
}

fn call(function: *ObjFunction, arg_count: u8) InterpretError!void {
    if (arg_count != function.arity) {
        return runtimeError("Expected {d} arguments but got {d}", .{ function.arity, arg_count });
    }

    if (vm.frame_count == FRAMES_MAX) {
        return runtimeError("Stack overflow.", .{});
    }

    var frame = &vm.frames[vm.frame_count];
    vm.frame_count += 1;
    frame.function = function;
    frame.ip = function.chunk.code.items;
    frame.slots = vm.sp - arg_count - 1;
}

fn callValue(callee: Value, arg_count: u8) !void {
    switch (callee) {
        .obj => |o| {
            switch (o.type) {
                .FUNCTION => return call(@as(*ObjFunction, @ptrCast(o)), arg_count),
                .NATIVE => {
                    const native = @as(*ObjNative, @ptrCast(o)).function.?;
                    const result = try native((vm.sp - arg_count)[0..arg_count]);
                    vm.sp -= arg_count + 1;
                    push(result);
                    return;
                },
                else => {}
            }
        },
        else => {}
    }
    return runtimeError("Can only call functions and classes.", .{});
}

pub fn run() !void {
    var frame = &vm.frames[vm.frame_count - 1];
    while (true) {
        if (comptime DEBUG_TRACE) {
            std.debug.print("          ", .{});
            var slot: [*]Value = frame.slots;
            //for (self.stack[0..@intFromPtr(self.sp) - @intFromPtr(&self.stack)]) |slot| {
            while (@intFromPtr(slot) < @intFromPtr(vm.sp)) : (slot += 1) {
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
                const val = vm.globals.get(name) orelse {
                    return InterpretError.RuntimeError;
                };
                push(val);
            },
            .DEFINE_GLOBAL => {
                _ = vm.globals.insert(frame.readString(), peek(0)) catch return InterpretError.RuntimeError;
                _ = pop();
            },
            .SET_GLOBAL => {
                const name = frame.readString();
                if (vm.globals.insert(name, peek(0)) catch return InterpretError.RuntimeError) {
                    _ = vm.globals.delete(name);
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
            .CALL => {
                const arg_count = frame.readByte();
                callValue(peek(arg_count), arg_count) catch return InterpretError.RuntimeError;
                frame = &vm.frames[vm.frame_count - 1];
            },
            .RETURN => {
                const result = pop();
                vm.frame_count -= 1;
                if (vm.frame_count == 0) {
                    _ = pop();
                    return;
                }

                vm.sp = frame.slots;
                push(result);
                frame = &vm.frames[vm.frame_count - 1];
            },
            else => return InterpretError.RuntimeError,
        }
    }
}
