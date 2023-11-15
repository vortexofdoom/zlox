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
const ObjClosure = object.ObjClosure;
const ObjString = object.ObjString;
const ObjUpvalue = object.ObjUpvalue;
const ClosedUpvalue = object.ClosedUpvalue;
const copyString = object.copyString;
const stdout = std.io.getStdOut().writer();

const DEBUG_TRACE_EXECUTION: bool = true;

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

stack: [STACK_MAX]Value,
sp: [*]Value,

globals: HashMap,
strings: HashMap,

open_upvalues: ?*ObjUpvalue,
objects: ?*Obj,

const Self = @This();
pub var vm: Self = .{
    .frames = [1]CallFrame{undefined} ** FRAMES_MAX,
    .allocator = undefined,
    .stack = [_]Value{Value{ .nil = {} }} ** STACK_MAX,
    .sp = undefined,
    .strings = undefined,
    .globals = undefined,
    .open_upvalues = null,
    .objects = null,
};

const CallFrame = struct {
    closure: *ObjClosure,
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
        return frame.closure.function.chunk.constants.items[frame.readByte()];
    }

    inline fn readString(frame: *CallFrame) *object.ObjString {
        return @ptrCast(frame.readConstant().obj);
    }
};

pub fn resetStack() void {
    vm.sp = &vm.stack;
    vm.frame_count = 0;
    vm.open_upvalues = null;
}

pub fn init(allocator: Allocator) !*Self {
    start = try std.time.Instant.now();
    vm.allocator = allocator;
    resetStack();
    vm.objects = null;
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
        const function = frame.closure.function;
        const instruction = @intFromPtr(frame.ip - @intFromPtr(function.chunk.code.items) - 1);
        std.debug.print("[line {d}] in ", .{function.chunk.getLine(instruction)});
        if (function.name) |name| {
            print("{s}()\n", .{name.ptr[0..name.len]});
        } else print("script\n", .{});
    }
    resetStack();
    return InterpretError.RuntimeError;
}

fn defineNative(name: []const u8, function: NativeFn) !void {
    push(Value.obj(try copyString(name)));
    push(Value.obj(try ObjNative.new(function)));
    _ = try vm.globals.insert(@ptrCast(@alignCast(vm.stack[0].obj)), vm.stack[1]);
    _ = pop();
    _ = pop();
}

pub fn interpret(source: []const u8) !void {
    const fun = compile(source) catch return InterpretError.CompileError;

    push(Value.obj(fun));

    const closure = ObjClosure.new(fun) catch return runtimeError("Out of memory.", .{});
    _ = pop();
    push(Value.obj(closure));
    try call(closure, 0);

    run() catch {};
}

pub inline fn push(val: Value) void {
    vm.sp[0] = val;
    vm.sp += 1;
}

pub inline fn pop() Value {
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
    const r = peek(0).obj.asString();
    const l = peek(1).obj.asString();
    const len = l.len + r.len;
    var chars = try vm.allocator.alloc(u8, len);
    @memcpy(chars[0..l.len], l.ptr[0..l.len]);
    @memcpy(chars[l.len..len], r.ptr[0..r.len]);
    push(Value.obj(try object.takeString(chars)));
    _ = pop();
    _ = pop();
}

fn call(closure: *ObjClosure, arg_count: u8) InterpretError!void {
    if (arg_count != closure.function.arity) {
        return runtimeError("Expected {d} arguments but got {d}", .{ closure.function.arity, arg_count });
    }

    if (vm.frame_count == FRAMES_MAX) {
        return runtimeError("Stack overflow.", .{});
    }

    var frame = &vm.frames[vm.frame_count];
    vm.frame_count += 1;
    frame.closure = closure;
    frame.ip = closure.function.chunk.code.items;
    frame.slots = vm.sp - arg_count - 1;
}

fn callValue(callee: Value, arg_count: u8) !void {
    switch (callee) {
        .obj => |o| {
            switch (o.type) {
                .CLOSURE => return call(@as(*ObjClosure, @ptrCast(o)), arg_count),
                .NATIVE => {
                    const native = @as(*ObjNative, @ptrCast(o)).function.?;
                    const result = try native((vm.sp - arg_count)[0..arg_count]);
                    vm.sp -= arg_count + 1;
                    push(result);
                    return;
                },
                else => {},
            }
        },
        else => {},
    }
    return runtimeError("Can only call functions and classes.", .{});
}

fn captureUpvalue(local: *Value) !*ObjUpvalue {
    var prev: ?*ObjUpvalue = null;
    var upvalue = vm.open_upvalues;

    while (upvalue != null and @intFromPtr(upvalue.?.uv.value.open) > @intFromPtr(local)) {
        const curr = upvalue.?;
        if (@subWithOverflow(@intFromPtr(curr.uv.value.open), @intFromPtr(local))[1] == 1) break;
        prev = curr;
        upvalue = curr.next;
    }

    if (upvalue != null and @intFromPtr(upvalue.?.uv.value.open) == @intFromPtr(local)) {
        return upvalue.?;
    }

    var created = try ObjUpvalue.new(local);
    created.next = upvalue;
    if (prev) |uv| {
        uv.next = created;
    } else {
        vm.open_upvalues = created;
    }

    return created;
}

fn closeUpvalues(last: *Value) void {
    while (vm.open_upvalues) |curr| : (vm.open_upvalues = curr.next) {
        if (@intFromPtr(last) >= @intFromPtr(curr.uv.value.open)) break;
        const val = curr.get();
        curr.uv.closed = true;
        curr.set(val);
    }
}

pub fn run() !void {
    var frame = &vm.frames[vm.frame_count - 1];
    var offset: usize  = 0;
    while (true) {
        if (comptime DEBUG_TRACE_EXECUTION) {
            std.debug.print("          ", .{});
            var slot: [*]Value = &vm.stack;
            //for (self.stack[0..@intFromPtr(self.sp) - @intFromPtr(&self.stack)]) |slot| {
            while (@intFromPtr(slot) < @intFromPtr(vm.sp)) : (slot += 1) {
                std.debug.print("[ ", .{});
                try printValue(slot[0], std.io.getStdErr().writer());
                std.debug.print(" ]", .{});
            }
            std.debug.print("\n", .{});
            offset = @import("debug.zig").disassembleInstruction(&frame.closure.function.chunk, @intFromPtr(frame.ip - @intFromPtr(frame.closure.function.chunk.code.items)), offset);
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
                    return runtimeError("Undefined variable '{s}'.", .{name.ptr[0..name.len]});
                };
                push(val);
            },
            .DEFINE_GLOBAL => {
                const name = frame.readString();
                _ = vm.globals.insert(name, peek(0)) catch return InterpretError.RuntimeError;
                _ = pop();
            },
            .SET_GLOBAL => {
                const name = frame.readString();
                if (vm.globals.insert(name, peek(0)) catch return InterpretError.RuntimeError) {
                    _ = vm.globals.delete(name);
                    return runtimeError("Undefined variable '{s}'.", .{name.ptr[0..name.len]});
                }
            },
            .GET_UPVALUE => {
                const slot = frame.readByte();
                push(frame.closure.upvalues.items[slot].?.get());
            },
            .SET_UPVALUE => {
                const slot = frame.readByte();
                frame.closure.upvalues.items[slot].?.set(peek(0));
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
                printValue(pop(), stdout) catch {
                    return runtimeError("Error printing to stdout.", .{});
                };
                stdout.print("\n", .{}) catch {
                    return runtimeError("Error printing to stdout.", .{});
                };
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
            .CLOSURE => {
                const fun: *ObjFunction = @ptrCast(@alignCast(frame.readConstant().obj));
                const closure = try ObjClosure.new(fun);
                push(Value.obj(closure));
                for (0..closure.upvalues.count) |i| {
                    const is_local = frame.readByte();
                    const index = frame.readByte();
                    if (is_local == 1) {
                        closure.upvalues.items[i] = try captureUpvalue(@ptrCast(frame.slots + index));
                    } else {
                        closure.upvalues.items[i] = closure.upvalues.items[index];
                    }
                }
            },
            .CLOSE_UPVALUE => {
                closeUpvalues(@ptrCast(vm.sp - 1));
            },
            .RETURN => {
                const result = pop();
                closeUpvalues(@ptrCast(frame.slots));
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
