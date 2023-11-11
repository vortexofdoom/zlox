const std = @import("std");
const Allocator = std.mem.Allocator;
const vm = @import("vm.zig");

pub const ObjType = enum(u8) {
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
};

fn hashString(key: []const u8) u32 {
    var hash: u32 = 216613626;
    for (key) |char| {
        hash ^= @intCast(char);
        hash *%= 16777619;
    }
    return hash;
}

pub fn allocateString(chars: []u8, hash: u32, allocator: Allocator) !*ObjString {
    var str: *ObjString = @ptrCast(@alignCast(try allocateObject(ObjType.STRING, allocator)));
    const s: []u8 = @ptrCast(chars);
    str.ptr = s.ptr;
    str.len = s.len;
    str.hash = hash;
    _ = try vm.Vm.strings.insert(str, @import("value.zig").Value.nil);
    return str;
}

pub fn takeString(chars: []u8, allocator: Allocator) !*ObjString {
    const hash: u32 = hashString(chars);
    if (vm.Vm.strings.findString(chars, hash)) |interned| {
        allocator.free(chars);
        return interned;
    } else return allocateString(chars, hash, allocator);
}

pub fn copyString(chars: []const u8, allocator: Allocator) !*ObjString {
    const hash: u32 = hashString(chars);
    if (vm.Vm.strings.findString(chars, hash)) |interned| return interned;
    var str = try allocator.alloc(u8, chars.len);
    @memcpy(str, chars);
    return allocateString(str, hash, allocator);
}

pub inline fn allocateObject(comptime obj_type: ObjType, allocator: Allocator) !*Obj {
    comptime var T: type = switch (obj_type) {
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
