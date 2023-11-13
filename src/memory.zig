const std = @import("std");
const object = @import("object.zig");
const value = @import("value.zig");
const collections = @import("collections.zig");
const compiler = @import("compiler.zig");
const HashMap = collections.HashMap;
const Obj = object.Obj;
const Value = value.Value;
const vm = @import("vm.zig");

pub var allocator = undefined;

pub const DEBUG_LOG_GC = false;

pub const GcAllocator = struct {
    const Self = @This();
    run_gc: bool = false,
    bytes_allocated: usize = 0,
    internal_allocator: std.mem.Allocator,


    pub fn init(alloc: std.mem.Allocator) Self {
        return .{
            .internal_allocator = alloc,
        };
    }

    pub fn allocator(self: *Self) std.mem.Allocator {
        return .{ .ptr = self, .vtable = comptime &std.mem.Allocator.VTable{
            .alloc = allocGC,
            .resize = resizeGC,
            .free = freeGC,
        } };
    }

    fn allocGC(ctx: *anyopaque, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
        const self: *GcAllocator = @ptrCast(@alignCast(ctx));
        if (self.run_gc) self.collect();
        self.run_gc = true;
        if (self.internal_allocator.rawAlloc(len, ptr_align, ret_addr)) |bytes| {
            self.bytes_allocated += len;
            return bytes;
        } else return null;
    }

    fn resizeGC(ctx: *anyopaque, buf: []u8, buf_align: u8, new_len: usize, ret_addr: usize) bool {
        const self: *Self = @ptrCast(@alignCast(ctx));

        if (self.run_gc) self.collect();
        const delta = new_len -% buf.len; // @as(isize, @bitCast(new_len)) - @as(isize, @bitCast(buf.len));
        self.bytes_allocated +%= delta;
        return self.internal_allocator.rawResize(buf, buf_align, new_len, ret_addr);
    }

    fn freeGC(ctx: *anyopaque, buf: []u8, buf_align: u8, ret_addr: usize) void {
        const self: *Self = @ptrCast(@alignCast(ctx));
        self.bytes_allocated -= buf.len;
        self.internal_allocator.rawFree(buf, buf_align, ret_addr);
    }

    fn collect(self: *Self) void {
        _ = self;
        if (comptime DEBUG_LOG_GC) {
            std.debug.print("-- gc begin\n", .{});
        }
        
        //markRoots();

        if (comptime DEBUG_LOG_GC) {
            std.debug.print("-- gc end\n", .{});
        }
    }

    fn markCompilerRoots() void {
        var comp = compiler.current;
        while (comp) |curr| : (comp = curr.enclosing) {
            markObject(@ptrCast(curr.function));
        }
    }

    fn markRoots() void {
        for (vm.vm.stack[0..@intFromPtr(vm.vm.sp) - @intFromPtr(&vm.vm.stack)]) |v| {
            markValue(v);
        }

        for (vm.vm.frames[0..vm.vm.frame_count]) |frame| {
            markObject(@ptrCast(frame.closure));
        }

        var upvalue = vm.vm.open_upvalues;
        while (upvalue) |curr| : (upvalue = curr.next) {
            markObject(@ptrCast(curr));
        }

        markTable(&vm.vm.globals);

        markCompilerRoots();
    }

    fn markTable(table: *HashMap) void {
        for (table.entries) |entry| {
            if (entry) |e| {
                markObject(@ptrCast(e.key));
                markValue(e.val);
            }
        }
    }

    fn markValue(val: Value) void {
        switch (val) {
            .obj => |o| markObject(o),
            else => return,
        }
    }

    fn markObject(obj: ?*Obj) void {
        if (obj) |o| o.is_marked = true;
    }
};
