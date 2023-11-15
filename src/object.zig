const std = @import("std");
const Allocator = std.mem.Allocator;
const vm = @import("vm.zig");
const Chunk = @import("chunk.zig").Chunk;
const value = @import("value.zig");
const Value = value.Value;
const ValueType = value.ValueType;
const ArrayList = @import("collections.zig").ArrayList;
const memory = @import("memory.zig");
const DEBUG_LOG_GC = memory.DEBUG_LOG_GC;

pub const ObjType = enum(u8) {
    CLOSURE,
    FUNCTION,
    NATIVE,
    STRING,
    UPVALUE,
};

pub const Obj = extern struct {
    type: ObjType,
    is_marked: bool,
    next: ?*Obj,

    pub fn free(obj: *Obj) void {
        if (comptime DEBUG_LOG_GC) {
            //std.debug.print("free type {d}\n", .{@intFromEnum(obj.type)});
            std.debug.print("0x{x} free type {d}\n", .{@intFromPtr(obj), @intFromEnum(obj.type)});
        }
        const alloc = vm.vm.allocator;
        switch (obj.type) {
            .CLOSURE => {
                const closure: *ObjClosure = @ptrCast(@alignCast(obj));
                closure.upvalues.deinit();
                alloc.destroy(closure);
            },
            .FUNCTION => {
                var fun: *ObjFunction = @ptrCast(@alignCast(obj));
                fun.chunk.deinit();
                alloc.destroy(fun);
            },
            .NATIVE => {
                var native: *ObjNative = @ptrCast(@alignCast(obj));
                alloc.destroy(native);
            },
            .STRING => {
                var str: *ObjString = @ptrCast(@alignCast(obj));
                alloc.free(str.ptr[0..str.len]);
                alloc.destroy(str);
            },
            .UPVALUE => {
                const upvalue: *ObjUpvalue = @ptrCast(@alignCast(obj));
                alloc.destroy(upvalue);
            },
        }
    }

    pub inline fn asString(obj: *Obj) *ObjString {
        return @ptrCast(@alignCast(obj));
    }
};

pub const ObjString = extern struct {
    obj: Obj,
    len: usize,
    ptr: [*]u8,
    hash: u32,

    pub fn new(chars: []u8, hash: u32) !*ObjString {
        var str: *ObjString = @ptrCast(@alignCast(try allocateObject(ObjType.STRING, vm.vm.allocator)));
        str.ptr = chars.ptr;
        str.len = chars.len;
        str.hash = hash;
        vm.push(Value.obj(@ptrCast(str)));
        _ = try vm.vm.strings.insert(str, Value.Nil);
        _ = vm.pop();
        return str;
    }
};

pub const ObjFunction = extern struct {
    obj: Obj,
    arity: u8 = 0,
    upvalue_count: u16 = 0,
    chunk: Chunk,
    name: ?*ObjString = null,

    pub fn new() !*ObjFunction {
        var fun: *ObjFunction = @ptrCast(@alignCast(try allocateObject(.FUNCTION, vm.vm.allocator)));
        fun.name = null;
        fun.arity = 0;
        fun.upvalue_count = 0;
        fun.chunk.init(vm.vm.allocator);
        return fun;
    }
};

pub const NativeFn = ?*const fn ([]Value) anyerror!Value;

pub const ObjNative = extern struct {
    obj: Obj,
    function: NativeFn,

    pub fn new(fun: NativeFn) !*ObjNative {
        var native: *ObjNative = @ptrCast(@alignCast(try allocateObject(.NATIVE, vm.vm.allocator)));
        native.function = fun;
        return native;
    }
};

pub const ObjClosure = extern struct {
    obj: Obj,
    function: *ObjFunction,
    upvalues: ArrayList(?*ObjUpvalue),

    pub fn new(fun: *ObjFunction) !*ObjClosure {
        var upvalues = try ArrayList(?*ObjUpvalue).initCapacity(fun.upvalue_count, vm.vm.allocator);
        for (upvalues.items[0..fun.upvalue_count]) |*uv| {
            uv.* = null;
        }

        var closure: *ObjClosure = @ptrCast(@alignCast(try allocateObject(.CLOSURE, vm.vm.allocator)));
        closure.function = fun;
        closure.upvalues = upvalues;
        closure.upvalues.count = fun.upvalue_count;
        return closure;
    }
};

// Maybe someday
// pub const Upvalue = extern union(enum) {
//     open: *Value,
//     closed: Value,
// };

const Upvalue = extern struct {
    closed: bool,
    value: extern union {
        open: *Value,
        closed: ClosedUpvalue,
    },

    fn get(self: Upvalue) Value {
        return if (self.closed) self.value.closed.val() else self.value.open.*;
    }

    fn set(self: *Upvalue, val: Value) void {
        if (self.closed) {
            switch (val) {
                .bool => |b| {
                    self.value.closed.tag = .bool;
                    self.value.closed.as.bool = b;
                },
                .nil => {
                    self.value.closed.tag = .nil;
                    self.value.closed.as.nil = {};
                },
                .number => |n| {
                    self.value.closed.tag = .number;
                    self.value.closed.as.number = n;
                },
                .obj => |o| {
                    self.value.closed.tag = .obj;
                    self.value.closed.as.obj = o;
                },
            }
        } else {
            self.value.open.* = val;
        }
    } 
};

pub const ClosedUpvalue = extern struct {
    tag: ValueType,
    as: extern union {
        bool: bool,
        nil: void,
        number: f64,
        obj: *Obj,
    },

    // pub fn new(from: Value) ClosedUpvalue {
    //     switch (from) {
    //         .bool => |b| return ClosedUpvalue{
    //             .tag = from.valType(),
    //             .as = .{ .bool = b },
    //         },
    //         .nil => return ClosedUpvalue{
    //             .tag = from.valType(),
    //             .as = .{ .nil = {} },
    //         },
    //         .number => |n| return ClosedUpvalue{
    //             .tag = from.valType(),
    //             .as = .{ .number = n },
    //         },
    //         .obj => |o| return ClosedUpvalue{
    //             .tag = from.valType(),
    //             .as = .{ .obj = o },
    //         },
    //     }
    // }

    fn val(self: ClosedUpvalue) Value {
        return switch (self.tag) {
            .bool => Value{ .bool = self.as.bool },
            .nil => Value.Nil,
            .number => Value{ .number = self.as.number },
            .obj => Value{ .obj = self.as.obj },
        };
    }
};

pub const ObjUpvalue = extern struct {
    obj: Obj,
    uv: Upvalue,
    next: ?*ObjUpvalue,

    pub fn new(slot: *Value) !*ObjUpvalue {
        var uv: *ObjUpvalue = @ptrCast(@alignCast(try allocateObject(.UPVALUE, vm.vm.allocator)));
        uv.uv.closed = false;
        uv.uv.value.open = slot;
        uv.next = null;
        return uv;
    }

    pub inline fn closed(self: ObjUpvalue) bool {
        return self.uv.closed;
    }

    pub inline fn get(self: *ObjUpvalue) Value {
        return self.uv.get();
    }

    pub inline fn set(self: *ObjUpvalue, val: Value) void {
        self.uv.set(val);
    }
};

fn hashString(key: []const u8) u32 {
    var hash: u32 = 216613626;
    for (key) |char| {
        hash ^= @intCast(char);
        hash *%= 16777619;
    }
    return hash;
}

pub fn printObject(obj: *Obj, comptime writer: anytype) !void {
    switch (obj.type) {
        .CLOSURE => {
            try printObject(@ptrCast(@as(*ObjClosure, @ptrCast(@alignCast(obj))).function), writer);
        },
        .FUNCTION => {
            const fun = @as(*ObjFunction, @ptrCast(@alignCast(obj)));
            if (fun.name) |name| {
                try writer.print("<fn {s}>", .{name.ptr[0..name.len]});
            } else {
                try writer.print("<script>", .{});
            }
        },
        .NATIVE => try writer.print("<native fn>", .{}),
        .STRING => {
            const str = @as(*ObjString, @ptrCast(@alignCast(obj)));
            try writer.print("{s}", .{str.ptr[0..str.len]});
        },
        .UPVALUE => try writer.print("upvalue", .{}),
    }
}

pub fn takeString(chars: []u8) !*ObjString {
    const hash: u32 = hashString(chars);
    if (vm.vm.strings.findString(chars, hash)) |interned| {
        vm.vm.allocator.free(chars);
        return interned;
    } else return ObjString.new(chars, hash);
}

pub fn copyString(chars: []const u8) !*ObjString {
    const hash: u32 = hashString(chars);
    if (vm.vm.strings.findString(chars, hash)) |interned| return interned;
    var str = try vm.vm.allocator.alloc(u8, chars.len);
    @memcpy(str, chars);
    return ObjString.new(str, hash);
}

pub inline fn allocateObject(comptime obj_type: ObjType, alloc: Allocator) !*Obj {
    comptime var T: type = switch (obj_type) {
        .CLOSURE => ObjClosure,
        .FUNCTION => ObjFunction,
        .NATIVE => ObjNative,
        .STRING => ObjString,
        .UPVALUE => ObjUpvalue,
    };

    var mem = try alloc.create(T);
    var obj: *Obj = @ptrCast(@alignCast(mem));
    obj.type = obj_type;
    obj.is_marked = false;
    // Update the object linked list
    obj.next = vm.vm.objects;
    vm.vm.objects = obj;
    if (comptime DEBUG_LOG_GC) {
        comptime var name = switch (obj_type) {
            .CLOSURE => "ObjClosure",
            .FUNCTION => "ObjFunction",
            .NATIVE => "ObjNative",
            .STRING => "ObjString",
            .UPVALUE => "ObjUpvalue",
        };
        //std.debug.print("allocate {d} for {d}\n", .{@sizeOf(T), @intFromEnum(obj_type)});
        std.debug.print("0x{x} allocate {d} for {d} ({s})\n", .{@intFromPtr(obj), @sizeOf(T), @intFromEnum(obj_type), name});
    }
    return obj;
}
