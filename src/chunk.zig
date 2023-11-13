const std = @import("std");
const Value = @import("value.zig").Value;
const Error = @import("error.zig").CompileError;
const ArrayList = @import("collections.zig").ArrayList;

pub const Op = enum(u8) {
    CONSTANT,
    NIL,
    TRUE,
    FALSE,
    POP,
    GET_LOCAL,
    SET_LOCAL,
    GET_GLOBAL,
    DEFINE_GLOBAL,
    SET_GLOBAL,
    GET_UPVALUE,
    SET_UPVALUE,
    EQUAL,
    GREATER,
    LESS,
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    NOT,
    NEGATE,
    PRINT,
    JUMP,
    JUMP_IF_FALSE,
    LOOP,
    CALL,
    CLOSURE,
    CLOSE_UPVALUE,
    RETURN,
    _,
};

var alloc: std.mem.Allocator = undefined;

pub const Chunk = extern struct {
    const Self = @This();
    code: ArrayList(u8),
    lines: ArrayList(usize),
    constants: ArrayList(Value),

    pub inline fn count(self: *Chunk) usize {
        return self.code.count;
    }

    pub fn init(self: *Self, allocator: std.mem.Allocator) void {
        alloc = allocator;
        self.code = ArrayList(u8).init(alloc);
        self.lines = ArrayList(usize).init(alloc);
        self.constants = ArrayList(Value).init(alloc);
    }

    pub fn deinit(self: *Self) void {
        self.code.deinit();
        self.lines.deinit();
        self.constants.deinit();
    }

    pub fn write(self: *Self, byte: u8, line: usize) !void {
        // TODO: Fix this
        try self.lines.append(line);
        // while (self.lines.count < line - 1) {
        //     try self.lines.append(0);
        // }
        // if (self.count() >= line) {
        //     self.lines.items[line - 1] += 1;
        // } else {
        //     try self.lines.append(1);
        // }
        // std.debug.print("adding to line {d}: {any}\n", .{line, self.lines.items[0..self.lines.count]});
        
        try self.code.append(byte);
    }

    pub fn getLine(self: *Self, offset: usize) usize {
        var curr: usize = 0;
        var total: usize = 0;
        for (self.lines.items[0..self.lines.count]) |line| {
            // for (0..line) |_| {
            //     std.debug.print("{d}", .{line});
            // }
            // if (line != 0) std.debug.print("\n", .{});
            total += line;
            if (total > offset) {
                break;
            }
            curr += 1;
        }
        // may want to allow this to error if the offset is too big
        //std.debug.print("\n{any}\n", .{self.lines.items[0..curr + 1]});
        return curr + 1;
    }

    pub fn addConstant(self: *Self, value: Value) !u8 {
        if (self.constants.count >= 256) {
            return Error.tooManyConstants;
        }
        try self.constants.append(value);
        return @intCast(self.constants.count - 1);
    }
};
