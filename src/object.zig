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
const HashMap = @import("collections.zig").HashMap;

pub const ObjType = enum(u8) {
    BOUND_METHOD,
    CLASS,
    CLOSURE,
    FUNCTION,
    INSTANCE,
    NATIVE,
    STRING,
    UPVALUE,
};

// TODO: this could all be done with @fieldParentPtr instead of the janky casting currently in place.
// which I found out a little too late to do from the start.
pub const Obj = extern struct {
    type: ObjType,
    is_marked: bool,
    next: ?*Obj,

    pub fn new(comptime obj_type: ObjType, alloc: Allocator) !*Obj {
        comptime var T: type = switch (obj_type) {
            .BOUND_METHOD => ObjBoundMethod,
            .CLASS => ObjClass,
            .CLOSURE => ObjClosure,
            .FUNCTION => ObjFunction,
            .INSTANCE => ObjInstance,
            .NATIVE => ObjNative,
            .STRING => ObjString,
            .UPVALUE => ObjUpvalue,
        };

        var mem = try alloc.create(T);
        var obj: *Obj = &mem.obj;
        obj.type = obj_type;
        obj.is_marked = false;
        // Update the object linked list
        obj.next = vm.vm.objects;
        vm.vm.objects = obj;
        if (comptime DEBUG_LOG_GC) {
            comptime var name = switch (obj_type) {
                .BOUND_METHOD => "ObjBoundMethod",
                .CLASS => "ObjClass",
                .CLOSURE => "ObjClosure",
                .FUNCTION => "ObjFunction",
                .INSTANCE => "ObjInstance",
                .NATIVE => "ObjNative",
                .STRING => "ObjString",
                .UPVALUE => "ObjUpvalue",
            };
            //std.debug.print("allocate {d} for {d}\n", .{@sizeOf(T), @intFromEnum(obj_type)});
            std.debug.print("0x{x} allocate {d} for {d} ({s})\n", .{ @intFromPtr(obj), @sizeOf(T), @intFromEnum(obj_type), name });
        }
        return obj;
    }

    pub inline fn as(obj: *Obj, comptime T: type) *T {
        return @fieldParentPtr(T, "obj", obj);
    }

    pub fn free(obj: *Obj) void {
        if (comptime DEBUG_LOG_GC) {
            //std.debug.print("free type {d}\n", .{@intFromEnum(obj.type)});
            std.debug.print("0x{x} free type {d}\n", .{ @intFromPtr(obj), @intFromEnum(obj.type) });
        }
        const alloc = vm.vm.allocator;
        switch (obj.type) {
            .BOUND_METHOD => alloc.destroy(obj.as(ObjBoundMethod)), //@fieldParentPtr(ObjBoundMethod, "obj", obj)),
            .CLASS => {
                const class: *ObjClass = obj.as(ObjClass); // @ptrCast(@alignCast(obj));
                class.methods.free();
                alloc.destroy(class);
            },
            .CLOSURE => {
                const closure = obj.as(ObjClosure);
                closure.upvalues.deinit();
                alloc.destroy(closure);
            },
            .FUNCTION => {
                var fun = obj.as(ObjFunction);
                fun.chunk.deinit();
                alloc.destroy(fun);
            },
            .INSTANCE => {
                var instance = obj.as(ObjInstance);
                instance.fields.free();
                alloc.destroy(instance);
            },
            .NATIVE => {
                var native = obj.as(ObjNative);
                alloc.destroy(native);
            },
            .STRING => {
                var str = obj.as(ObjString);
                alloc.free(str.ptr[0..str.len]);
                alloc.destroy(str);
            },
            .UPVALUE => {
                const upvalue = obj.as(ObjUpvalue);
                alloc.destroy(upvalue);
            },
        }
    }
};

pub const ObjClass = extern struct {
    obj: Obj,
    name: *ObjString,
    methods: HashMap,

    pub fn new(name: *ObjString) !*ObjClass {
        const obj = try Obj.new(.CLASS, vm.vm.allocator);
        var class = obj.as(ObjClass);
        class.name = name;
        try class.methods.init();
        return class;
    }
};

pub const ObjClosure = extern struct {
    obj: Obj,
    function: *ObjFunction,
    upvalues: ArrayList(?*ObjUpvalue),

    pub fn new(fun: *ObjFunction) !*ObjClosure {
        var upvalues = try ArrayList(?*ObjUpvalue).initCapacity(fun.upvalue_count, vm.vm.allocator);
        // TODO: See if this actually needs to be initialized
        for (upvalues.items[0..fun.upvalue_count]) |*uv| {
            uv.* = null;
        }

        const obj = try Obj.new(.CLOSURE, vm.vm.allocator);
        var closure = obj.as(ObjClosure);
        closure.function = fun;
        closure.upvalues = upvalues;
        closure.upvalues.count = fun.upvalue_count;
        return closure;
    }
};

pub const ObjFunction = extern struct {
    obj: Obj,
    arity: u8 = 0,
    upvalue_count: u16 = 0,
    chunk: Chunk,
    name: ?*ObjString = null,

    pub fn new() !*ObjFunction {
        const obj = try Obj.new(.FUNCTION, vm.vm.allocator);
        var fun = obj.as(ObjFunction);
        fun.name = null;
        fun.arity = 0;
        fun.upvalue_count = 0;
        fun.chunk.init(vm.vm.allocator);
        return fun;
    }
};

pub const ObjInstance = extern struct {
    obj: Obj,
    class: *ObjClass,
    fields: HashMap,

    pub fn new(class: *ObjClass) !*ObjInstance {
        var instance: *ObjInstance = @ptrCast(@alignCast(try Obj.new(.INSTANCE, vm.vm.allocator)));
        try instance.fields.init();
        instance.class = class;
        return instance;
    }
};

pub const ObjBoundMethod = struct {
    obj: Obj,
    receiver: Value,
    method: *ObjClosure,

    pub fn new(receiver: Value, method: *ObjClosure) !*ObjBoundMethod {
        const obj = try Obj.new(.BOUND_METHOD, vm.vm.allocator);
        var bound = obj.as(ObjBoundMethod);
        bound.receiver = receiver;
        bound.method = method;
        return bound;
    }
};

// TODO: Ergonomics would benefit greatly from being made non-extern
pub const ObjString = extern struct {
    obj: Obj,
    len: usize,
    ptr: [*]u8,
    hash: u32,

    pub fn new(chars: []u8, hash: u32) !*ObjString {
        const obj = try Obj.new(ObjType.STRING, vm.vm.allocator);
        var str = obj.as(ObjString);
        str.ptr = chars.ptr;
        str.len = chars.len;
        str.hash = hash;
        vm.push(Value.obj(&str.obj));
        _ = try vm.vm.strings.insert(str, Value.Nil);
        _ = vm.pop();
        return str;
    }
};

pub const NativeFn = *const fn ([]Value) anyerror!Value;

pub const ObjNative = struct {
    obj: Obj,
    function: NativeFn,

    pub fn new(fun: NativeFn) !*ObjNative {
        const obj = try Obj.new(.NATIVE, vm.vm.allocator);
        var native: *ObjNative = obj.as(ObjNative);
        native.function = fun;
        return native;
    }
};

pub const ObjUpvalue = struct {
    obj: Obj,
    open: *Value,
    closed: Value = Value.Nil,
    next: ?*ObjUpvalue,

    pub fn new(slot: *Value) !*ObjUpvalue {
        const obj = try Obj.new(.UPVALUE, vm.vm.allocator);
        var uv: *ObjUpvalue = obj.as(ObjUpvalue);
        uv.open = slot;
        uv.next = null;
        return uv;
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
        .BOUND_METHOD => try printObject(&@fieldParentPtr(ObjBoundMethod, "obj", obj).method.function.obj, writer),
        .CLASS => {
            const class = obj.as(ObjClass);
            try writer.print("{s}", .{class.name.ptr[0..class.name.len]});
        },
        .CLOSURE => {
            try printObject(&obj.as(ObjClosure).function.obj, writer);
        },
        .FUNCTION => {
            const fun = obj.as(ObjFunction);
            if (fun.name) |name| {
                try writer.print("<fn {s}>", .{name.ptr[0..name.len]});
            } else {
                try writer.print("<script>", .{});
            }
        },
        .INSTANCE => {
            const instance = obj.as(ObjInstance);
            try writer.print("{s} instance", .{instance.class.name.ptr[0..instance.class.name.len]});
        },
        .NATIVE => try writer.print("<native fn>", .{}),
        .STRING => {
            const str = obj.as(ObjString);
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
