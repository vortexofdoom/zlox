const std = @import("std");
const Allocator = std.mem.Allocator;
const vm = @import("vm.zig");
const Chunk = @import("chunk.zig").Chunk;

pub const ObjType = enum(u8) {
    FUNCTION,
    STRING,
    _,
};

pub const Obj = extern struct {
    type: ObjType,
    next: ?*Obj,

    pub fn free(obj: *Obj) void {
        const alloc = vm.Vm.allocator;
        switch (obj.type) {
            .STRING => {
                var str: *ObjString = @ptrCast(@alignCast(obj));
                alloc.free(str.ptr[0..str.len]);
                alloc.destroy(str);
            },
            .FUNCTION => {
                var fun: *ObjFunction = @ptrCast(@alignCast(obj));
                fun.chunk.deinit();
                alloc.destroy(fun);
            },
            _ => {},
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

    pub fn new(chars: []u8, hash: u32, allocator: Allocator) !*ObjString {
        var str: *ObjString = @ptrCast(@alignCast(try allocateObject(ObjType.STRING, allocator)));
        const s: []u8 = @ptrCast(chars);
        str.ptr = s.ptr;
        str.len = s.len;
        str.hash = hash;
        _ = try vm.Vm.strings.insert(str, @import("value.zig").Value.nil);
        return str;
    }
};

pub const ObjFunction = extern struct {
    obj: Obj,
    arity: u8 = 0,
    chunk: Chunk,
    name: ?*ObjString = null,

    pub fn new(allocator: Allocator) !*ObjFunction {
        var fun: *ObjFunction = @ptrCast(@alignCast(try allocateObject(.FUNCTION, allocator)));
        fun.name = null;
        fun.arity = 0;
        fun.chunk.init(allocator);
        return fun;
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

pub fn printObject(obj: *Obj) void {
    switch (obj.type) {
        .FUNCTION => {
            const fun = @as(*ObjFunction, @ptrCast(@alignCast(obj)));
            if (fun.name) |name| {
                std.debug.print("<fn {s}>", .{ name.ptr[0..name.len]} );
            } else {
                std.debug.print("<script>", .{});
            }
        },
        .STRING => {
            const str = @as(*ObjString, @ptrCast(@alignCast(obj)));
            std.debug.print("{s}", .{ str.ptr[0..str.len] });
        },
        _ => {}
    }
}

pub fn takeString(chars: []u8, allocator: Allocator) !*ObjString {
    const hash: u32 = hashString(chars);
    if (vm.Vm.strings.findString(chars, hash)) |interned| {
        allocator.free(chars);
        return interned;
    } else return ObjString.new(chars, hash, allocator);
}

pub fn copyString(chars: []const u8, allocator: Allocator) !*ObjString {
    const hash: u32 = hashString(chars);
    if (vm.Vm.strings.findString(chars, hash)) |interned| return interned;
    var str = try allocator.alloc(u8, chars.len);
    @memcpy(str, chars);
    return ObjString.new(str, hash, allocator);
}

pub inline fn allocateObject(comptime obj_type: ObjType, allocator: Allocator) !*Obj {
    comptime var T: type = switch (obj_type) {
        .FUNCTION => ObjFunction,
        .STRING => ObjString,
        _ => unreachable,
    };
    var mem = try allocator.create(T);
    var obj: *Obj = @ptrCast(mem);
    obj.type = obj_type;
    // Update the object linked list
    obj.next = vm.Vm.objects;
    vm.Vm.objects = obj;
    return obj;
}
