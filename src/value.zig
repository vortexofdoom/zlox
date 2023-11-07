const std = @import("std");

pub const Value: type = f64;

pub fn printValue(val: Value) void {
    std.debug.print("{d}", .{val});
}