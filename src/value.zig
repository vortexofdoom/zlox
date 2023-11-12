const std = @import("std");
const Allocator = std.mem.Allocator;
const object = @import("object.zig");
const Obj = object.Obj;
const ObjType = object.ObjType;
const ObjString = object.ObjString;

pub const ValueType = enum {
    bool,
    nil,
    number,
    obj,
};

pub const Value = union(ValueType) {
    bool: bool,
    nil: void,
    number: f64,
    obj: *Obj,

    pub const nil = Value{ .nil = {} };

    pub fn isFalsey(self: Value) bool {
        return switch (self) {
            .bool => |b| !b,
            .nil => true,
            else => false,
        };
    }

    pub inline fn valType(self: Value) ValueType {
        return switch (self) {
            .bool => .bool,
            .nil => .nil,
            .number => .number,
            .obj => .obj,
        };
    }

    pub inline fn objType(self: Value) ?ObjType {
        return switch (self) {
            .obj => |o| o.*.type,
            else => null,
        };
    }

    pub inline fn equals(lhs: Value, rhs: Value) bool {
        return lhs == rhs.valType() and switch (lhs) {
            .bool => |b| b == rhs.bool,
            .nil => true, // both nil
            .number => |n| n == rhs.number,
            .obj => lhs.obj == rhs.obj,
        };
    }

    pub inline fn obj(o: *anyopaque) Value {
        return Value{ .obj = @ptrCast(@alignCast(o)) };
    }
};

pub fn printValue(val: Value) void {
    switch (val) {
        .number => |n| std.debug.print("{d}", .{n}),
        .nil => std.debug.print("nil", .{}),
        .bool => |b| std.debug.print("{any}", .{b}),
        .obj => |o| object.printObject(o),
    }
}
