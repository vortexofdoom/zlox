const std = @import("std");
const ArrayList = std.ArrayList;

pub const OpCode = enum(u8) {
    ret,
    negate,
    constant,
};

pub const Chunk = struct {
    const Self = @This();
    code: ArrayList(u8),
    lines: ArrayList(usize),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !Self {
        return .{
            .code = ArrayList(u8).init(allocator),
            .lines = ArrayList(usize).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.code.deinit();
        self.lines.deinit();
    }

    pub fn write(self: *Self, byte: u8) !void {
        try self.code.append(byte);
    }

    pub fn getLine(self: *Self, offset: usize) usize {
        var curr = 0;
        var total = 0;
        for (self.lines) |line| {
            curr += 1;
            total += line;
            if (total >= offset) {
                break;
            }
        }
        // may want to allow this to error if the offset is too big
        return curr;
    } 
};