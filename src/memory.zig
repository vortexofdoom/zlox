const std = @import("std");

pub var allocator = undefined;

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
    }
};
