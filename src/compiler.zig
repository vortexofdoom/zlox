const std = @import("std");
const Scanner = @import("scanner.zig");
const scanToken = Scanner.scanToken;

pub fn compile(source: []const u8) !void {
    Scanner.init(source);
    var line: usize = 0;
    
    while (try scanToken()) |token| {
        if (token.line != line) {
            line = token.line;
            std.debug.print("{d:>4} ", .{line});
        } else {
            std.debug.print("   | ", .{});
        }
        std.debug.print("{d:>2} '{s}'\n", .{@intFromEnum(token.type), token.str});
    }
}