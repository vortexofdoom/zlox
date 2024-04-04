const std = @import("std");
const Allocator = std.mem.Allocator;
const object = @import("object.zig");
const Obj = object.Obj;
const ObjType = object.ObjType;
const ObjString = object.ObjString;

pub const ValueType = enum(u8) {
    bool,
    nil,
    number,
    obj,
};

const NAN_TAGGING = false;

pub const Value = if (NAN_TAGGING) NaNValue else UnionValue;

const UnionValue = union(ValueType) {
    bool: bool,
    nil: void,
    number: f64,
    obj: *Obj,

    pub const TRUE = UnionValue{ .bool = true };
    pub const FALSE = UnionValue{ .bool = false };
    pub const NIL = UnionValue{ .nil = {} };

    pub fn isFalsey(self: UnionValue) bool {
        return switch (self) {
            .bool => |b| !b,
            .nil => true,
            else => false,
        };
    }

    pub fn valType(self: UnionValue) ValueType {
        return switch (self) {
            .bool => .bool,
            .nil => .nil,
            .number => .number,
            .obj => .obj,
        };
    }

    pub fn number(n: f64) UnionValue {
        return UnionValue{ .number = n };
    }

    pub fn fromBool(b: bool) UnionValue {
        return UnionValue{ .bool = b };
    }

    pub fn obj(o: *Obj) UnionValue {
        return UnionValue{ .obj = o };
    }

    pub fn isBool(self: UnionValue) bool {
        return self == .bool;
    }

    pub fn isNumber(self: UnionValue) bool {
        return self == .number;
    }

    pub fn isObj(self: UnionValue) bool {
        return self == .obj;
    }

    pub fn asBool(self: UnionValue) bool {
        return self.bool;
        // return switch (self) {
        //     .bool => |b| b,
        //     else => null,
        // };
    }

    pub fn asNumber(self: UnionValue) f64 {
        return self.number;
        //return if (self == .number) self.number else null;
        // return switch (self) {
        //     .number => |n| n,
        //     else => null,  
        // };
    }

    pub fn asObj(self: UnionValue) *Obj {
        return self.obj;
        // return switch (self) {
        //     .obj => |o| o,
        //     else => null,
        // };
    }

    pub fn objType(self: UnionValue) ?ObjType {
        return switch (self) {
            .obj => |o| o.type,
            else => null,
        };
    }

    pub fn equals(lhs: UnionValue, rhs: UnionValue) bool {
        return std.meta.eql(lhs, rhs);
    }

    pub fn print(val: UnionValue, comptime writer: anytype) !void {
        switch (val) {
            .number => |n| try writer.print("{d}", .{n}),
            .nil => try writer.print("nil", .{}),
            .bool => |b| try writer.print("{any}", .{b}),
            .obj => |o| try object.printObject(o, writer),
        }
    }
};

const NaNValue = packed struct {
    data: u64,

    const SIGN_BIT: u64 = 0b10000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000;
    const QNAN: u64 = 0x7ffc000000000000;
    const TAG_OBJ = SIGN_BIT | QNAN;
    const TAG_NIL = 0b01;
    const TAG_FALSE = 0b10;
    const TAG_TRUE = 0b11;
    const ALL_TAGS = TAG_OBJ | TAG_TRUE;

    pub const NIL = NaNValue{ .data = QNAN | TAG_NIL };
    pub const TRUE = NaNValue{ .data = QNAN | TAG_TRUE };
    pub const FALSE = NaNValue{ .data = QNAN | TAG_FALSE };

    pub fn isFalsey(self: NaNValue) bool {
        return if (self.data == NIL.data) true else if (self.isBool()) !self.asBool() else false;
        // if (self == NIL) {
        //     return false;
        // } else if (self.asBool()) |b| {
        //     return b;
        // } else return true;
    }

    pub fn valType(self: NaNValue) ValueType {
        return switch (self.data & ALL_TAGS) {
            TAG_OBJ, TAG_OBJ | 0b10 => .obj,
            QNAN | TAG_TRUE, QNAN | TAG_FALSE => .bool,
            QNAN | TAG_NIL => .nil,
            else => .number,
        };
    }

    pub fn objType(self: NaNValue) ?ObjType {
        if (self.isObj()) return self.asObj().type;
        return null;
        // const o = self.asObj() orelse return null;
        // return o.type;
    }

    pub fn number(n: f64) NaNValue {
        return NaNValue{ .data = @bitCast(n) };
    }

    pub fn fromBool(b: bool) NaNValue {
        return NaNValue{ .data = FALSE.data | @intFromBool(b) };
    }

    pub fn isBool(self: NaNValue) bool {
        return (self.data | 1) == TRUE.data;
    }

    pub fn isNumber(self: NaNValue) bool {
        return (self.data & QNAN) != QNAN;
    }

    pub fn isObj(self: NaNValue) bool {
        return (self.data & TAG_OBJ) == TAG_OBJ;
    }

    pub fn asBool(self: NaNValue) bool {
        return (self.data & 1) == 1;//if ((self.data | 1) == TRUE.data) (self.data & 1) == 1 else null;
    }

    pub fn asNumber(self: NaNValue) f64 {
        return @bitCast(self.data);// if ((self.data & QNAN) != QNAN) @bitCast(self.data) else null;
    }

    pub fn asObj(self: NaNValue) *Obj {
        return @ptrFromInt(self.data & ~TAG_OBJ);// if ((TAG_OBJ & self.data) == TAG_OBJ) return @ptrFromInt(self.data & ~TAG_OBJ) else null;
    }

    pub fn obj(o: *Obj) NaNValue {
        return NaNValue{ .data = @intFromPtr(o) | TAG_OBJ };
    }

    pub fn print(val: NaNValue, comptime writer: anytype) !void {
        if (val.isNumber()) {
            try writer.print("{d}", .{val.asNumber()});
        } else if (val.isBool()) {
            try writer.print("{any}", .{val.asBool()});
        } else if (val.isObj()) {
            try object.printObject(val.asObj(), writer);
        } else {
            try writer.print("nil", .{});
        }
    }
};
