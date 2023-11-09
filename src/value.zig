const std = @import("std");

pub const ValueType = enum {
    bool,
    nil,
    number,
};

pub const Value = union(ValueType) {
    bool: bool,
    nil: void,
    number: f64,

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
        };
    }

    pub inline fn equals(self: Value, rhs: Value) bool {
        return self == rhs.valType() and switch (self) {
            .bool => |b| b == rhs.bool,
            .nil => true,   // both nil
            .number => |n| n == rhs.number,
        };
    }
};

pub fn printValue(val: Value) void {
    switch (val) {
        .number => |n| std.debug.print("{d}", .{n}),
        .nil => std.debug.print("nil", .{}),
        .bool => |b| std.debug.print("{any}", .{b}),
    }
}
