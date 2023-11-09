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
};

pub fn printValue(val: Value) void {
    switch (val) {
        .number => |n| std.debug.print("{d}", .{n}),
        .nil => std.debug.print("nil", .{}),
        .bool => |b| std.debug.print("{any}", .{b}),
    }
}
