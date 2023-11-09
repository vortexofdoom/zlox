const std = @import("std");
const Value = @import("value.zig").Value;
const Error = @import("error.zig").Error;
const ArrayList = std.ArrayList;

pub const Op = enum(u8) {
    CONSTANT,
    NIL,
    TRUE,
    FALSE,
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    NEGATE,
    RETURN,
    _,
};

pub const Chunk = struct {
    const Self = @This();
    code: ArrayList(u8),
    lines: ArrayList(usize),
    constants: ArrayList(Value),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !Self {
        return .{
            .code = ArrayList(u8).init(allocator),
            .lines = ArrayList(usize).init(allocator),
            .constants = ArrayList(Value).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.code.deinit();
        self.lines.deinit();
        self.constants.deinit();
    }

    pub fn write(self: *Self, byte: u8, line: usize) !void {
        while (self.lines.items.len < line) {
            try self.lines.append(0);
        }
        if (self.lines.items.len > line) {
            self.lines.items[line] += 1;
        } else {
            try self.lines.append(1);
        }
        try self.code.append(byte);
    }

    pub fn getLine(self: *Self, offset: usize) usize {
        var curr: usize = 0;
        var total: usize = 0;
        for (self.lines.items) |line| {
            total += line;
            if (total > offset) {
                break;
            }
            curr += 1;
        }
        // may want to allow this to error if the offset is too big
        return curr;
    }

    pub fn addConstant(self: *Self, value: Value) !u8 {
        if (self.constants.items.len >= 256) {
            return Error.tooManyConstants;
        }
        try self.constants.append(value);
        return @intCast(self.constants.items.len - 1);
    }
};