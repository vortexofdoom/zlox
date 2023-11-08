const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const Op = @import("chunk.zig").Op;
const debug = @import("debug.zig");
const Vm = @import("vm.zig");
const InterpretError = Vm.InterpretError;

fn repl() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var line: [1024]u8 = undefined;

    try stdout.print("> ", .{});
    while (try stdin.readUntilDelimiterOrEof(&line, '\n')) |input| {
        if (input.len == 0) break;
        try stdout.print("> ", .{});
    }
    
}

fn readFile(path: []const u8, alloc: std.mem.Allocator) ![:0]u8 {
    var file = std.fs.cwd().openFile(path, .{}) catch {
        std.log.err("Could not open file \"{s}\".\n", .{path});
        std.os.exit(74);
    };

    defer file.close();
    
    return file.readToEndAllocOptions(alloc, 100_000_000, null, @alignOf(u8), 0) catch {
        std.log.err("Could not read file \"{s}\".\n", .{path});
        std.os.exit(74);
    };
}

fn runFile(path: []const u8, alloc: std.mem.Allocator) !void {
    const source: [:0]const u8 = try readFile(path, alloc);
    defer alloc.free(source);

    //std.debug.print("{s}", .{source});
    try Vm.interpret(source);
    // if (Vm.interpret(source)) {
    //     alloc.free(source);
    // } else |err| switch (err) {
    //     InterpretError.CompileError => std.os.exit(65),
    //     InterpretError.RuntimeError => std.os.exit(70),
    // }
}

pub fn main() !void {
    var gen_purpose = std.heap.GeneralPurposeAllocator(.{}){};
    var alloc = gen_purpose.allocator();

    defer _ = gen_purpose.deinit();

    _ = try Vm.init(alloc);
    defer Vm.deinit();

    var args = std.process.args();
    _ = args.skip();
    
    if (args.next()) |path| {
        try runFile(path, alloc);
    } else {
        try repl();
    }
}
