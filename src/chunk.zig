const std = @import("std");
const Value = @import("value.zig").Value;
const Error = @import("error.zig").CompileError;
const ArrayList = @import("collections.zig").ArrayList;
const vm = @import("vm.zig");

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
    GET_PROPERTY,
    SET_PROPERTY,
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
    CLASS,
    _,
};

var alloc: std.mem.Allocator = undefined;

const Line = packed struct(u32) {
    line: u16,
    count: u16,
};

pub const Chunk = extern struct {
    const Self = @This();
    // TODO: Could maybe turn these back into std.ArrayListUnmanaged
    // given that all use the same allocator
    code: ArrayList(u8),
    lines: ArrayList(Line),
    constants: ArrayList(Value),

    pub inline fn count(self: *Chunk) usize {
        return self.code.count;
    }

    pub fn init(self: *Self, allocator: std.mem.Allocator) void {
        alloc = allocator;
        self.code = ArrayList(u8).init(alloc);
        self.lines = ArrayList(Line).init(alloc);
        self.constants = ArrayList(Value).init(alloc);
    }

    pub fn deinit(self: *Self) void {
        self.code.deinit();
        self.lines.deinit();
        self.constants.deinit();
    }

    pub fn write(self: *Self, byte: u8, line: usize) !void {
        if (self.lines.count > 0 and self.lines.items[self.lines.count - 1].line >= line) {
            for (0..self.lines.count) |i| {
                var entry = &self.lines.items[i];
                if (entry.line == line) {
                    entry.count += 1;
                    break;
                }
            }
        } else try self.lines.append(Line{ .line = @truncate(line), .count = 1 });
        
        // std.debug.print("Inserting byte at line {d}\n    {c} ", .{ line, '{' });
        // for (self.lines.items[0..self.lines.count], 0..) |entry, i| {
        //     std.debug.print("(line {d}: {d}){s}", .{entry.line, entry.count, if (i < self.lines.count - 1) ", " else " }\n" });
        // }

        try self.code.append(byte);
    }

    pub fn getLine(self: *Self, offset: usize) usize {
        if (self.lines.count == 0) return 0;
        var total: usize = 0;
        for (self.lines.items[0..self.lines.count]) |line| {
            total += line.count;
            if (total > offset) {
                return line.line;
            }
        } else return 0;
        // may want to allow this to error if the offset is too big
    }

    pub fn addConstant(self: *Self, value: Value) !u8 {
        if (self.constants.count >= 256) {
            return Error.tooManyConstants;
        }
        vm.push(value);
        try self.constants.append(value);
        _ = vm.pop();
        return @intCast(self.constants.count - 1);
    }
};
