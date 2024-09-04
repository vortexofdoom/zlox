const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const Op = @import("chunk.zig").Op;
const debug = @import("debug.zig");
const Vm = @import("vm.zig");
const memory = @import("memory.zig");
const object = @import("object.zig");
const InterpretError = Vm.InterpretError;

fn readFile(path: []const u8, alloc: std.mem.Allocator) ![:0]u8 {
    var file = std.fs.cwd().openFile(path, .{}) catch {
        std.log.err("Could not open file \"{s}\".\n", .{path});
        std.process.exit(74);
    };

    defer file.close();

    return file.readToEndAllocOptions(alloc, 100_000_000, null, @alignOf(u8), 0) catch {
        std.log.err("Could not read file \"{s}\".\n", .{path});
        std.process.exit(74);
    };
}

fn runFile(path: []const u8, alloc: std.mem.Allocator) !void {
    const source: [:0]const u8 = try readFile(path, alloc);
    defer alloc.free(source);

    //std.debug.print("{s}\n", .{source});
    Vm.interpret(source) catch {};
    // Vm.interpret(source) catch |err| switch (err) {
    //     InterpretError.CompileError => std.os.exit(65),
    //     InterpretError.RuntimeError => std.os.exit(70),
    // };
}

pub fn main() !void {
    //var gen_purpose = std.heap.GeneralPurposeAllocator(.{}){};
    //var alloc = gen_purpose.allocator();
    const alloc = std.heap.c_allocator;
    var gc = memory.GcAllocator.init(alloc);

    const gc_alloc = gc.allocator();

    //defer _ = gen_purpose.deinit();

    _ = try Vm.init(gc.allocator());
    defer Vm.deinit();

    var args = try std.process.argsWithAllocator(alloc);
    _ = args.skip();

    if (args.next()) |path| {
        try runFile(path, gc_alloc);
        while (args.next()) |p| {
            try runFile(p, gc_alloc);
            gc.collect();
        }
    } else {
        const stdin = std.io.getStdIn().reader();
        const stdout = std.io.getStdOut().writer();
        var line: [1024]u8 = undefined;

        while (true) {
            try stdout.print("> ", .{});
            if (try stdin.readUntilDelimiterOrEof(&line, '\n')) |input| {
                if (input.len == 0) break;
                //try stdout.print("{s}\n", .{line[0..input.len]});
                line[input.len] = 0;
                try Vm.interpret(input);
            }
        }
    }
}
