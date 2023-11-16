const std = @import("std");
const object = @import("object.zig");
const value = @import("value.zig");
const collections = @import("collections.zig");
const compiler = @import("compiler.zig");
const HashMap = collections.HashMap;
const Obj = object.Obj;
const ObjType = object.ObjType;
const Value = value.Value;
const vm = @import("vm.zig");

pub var allocator = undefined;

pub const DEBUG_LOG_GC = false;
pub const DEBUG_STRESS_GC = false;
const GC_HEAP_GROW_FACTOR = 2;

pub const GcAllocator = struct {
    const Self = @This();
    bytes_allocated: usize = 0,
    next_gc: usize = 1024 * 1024,
    gray_stack: std.ArrayList(?*Obj),
    internal_allocator: std.mem.Allocator,

    pub fn init(alloc: std.mem.Allocator) Self {
        return .{
            .internal_allocator = alloc,
            // Using the std implementation to hold the internal allocator so the gray stack does not trigger a collection
            .gray_stack = undefined,
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
        if (comptime DEBUG_STRESS_GC) self.collect();
        if (self.bytes_allocated > self.next_gc) self.collect();
        if (self.internal_allocator.rawAlloc(len, ptr_align, ret_addr)) |bytes| {
            //std.debug.print("old: {d}, delta: {d} new: {d}\n", .{self.bytes_allocated, len, self.bytes_allocated +% len});
            self.bytes_allocated += len;
            return bytes;
        } else return null;
    }

    fn resizeGC(ctx: *anyopaque, buf: []u8, buf_align: u8, new_len: usize, ret_addr: usize) bool {
        const self: *Self = @ptrCast(@alignCast(ctx));
        if (comptime DEBUG_STRESS_GC) self.collect();
        if (self.bytes_allocated > self.next_gc) self.collect();
        const delta = new_len -% buf.len; // @as(isize, @bitCast(new_len)) - @as(isize, @bitCast(buf.len));
        //std.debug.print("old: {d}, delta: {d} new: {d}\n", .{self.bytes_allocated, delta, self.bytes_allocated +% delta});
        self.bytes_allocated +%= delta;
        return self.internal_allocator.rawResize(buf, buf_align, new_len, ret_addr);
    }

    fn freeGC(ctx: *anyopaque, buf: []u8, buf_align: u8, ret_addr: usize) void {
        const self: *Self = @ptrCast(@alignCast(ctx));
        self.bytes_allocated -= buf.len;
        //std.debug.print("old: {d}, delta: {d} new: {d}\n", .{self.bytes_allocated, buf.len, self.bytes_allocated - buf.len});
        self.internal_allocator.rawFree(buf, buf_align, ret_addr);
    }

    fn collect(self: *Self) void {
        if (comptime DEBUG_LOG_GC) {
            std.debug.print("-- gc begin\n", .{});
        }

        self.gray_stack = std.ArrayList(?*Obj).init(self.internal_allocator);

        // if (comptime DEBUG_LOG_GC) {
        //     std.debug.print("  ...mark roots...\n", .{});
        // }
        self.markRoots();

        // if (comptime DEBUG_LOG_GC) {
        //     std.debug.print("  ...trace references...\n", .{});
        // }
        while (self.gray_stack.popOrNull()) |obj| {
            if (obj) |o| {
                self.blackenObject(o);
            }
        }
        // if (comptime DEBUG_LOG_GC) {
        //     std.debug.print("  ...clear interned strings...\n", .{});
        // }
        vm.vm.strings.clearUnmarked();

        // if (comptime DEBUG_LOG_GC) {
        //     std.debug.print("  ...sweep unmarked...\n", .{});
        // }
        Self.sweep();

        self.gray_stack.deinit();
        self.next_gc = self.bytes_allocated * GC_HEAP_GROW_FACTOR;
        if (comptime DEBUG_LOG_GC) {
            std.debug.print("-- gc end\n", .{});
        }
    }

    fn markCompilerRoots(self: *GcAllocator) void {
        var comp = compiler.current;
        while (comp) |curr| : (comp = curr.enclosing) {
            self.markObject(@ptrCast(curr.function));
        }
    }

    fn markRoots(self: *GcAllocator) void {
        for (vm.vm.stack[0 .. @intFromPtr(vm.vm.sp) - @intFromPtr(&vm.vm.stack)]) |v| {
            self.markValue(v);
        }

        for (vm.vm.frames[0..vm.vm.frame_count]) |frame| {
            self.markObject(@ptrCast(frame.closure));
        }

        var upvalue = vm.vm.open_upvalues;
        while (upvalue) |curr| : (upvalue = curr.next) {
            self.markObject(@ptrCast(curr));
        }

        self.markTable(&vm.vm.globals);
        self.markCompilerRoots();
        if (vm.vm.init_string) |init_str| self.markObject(&init_str.obj);
    }

    fn markTable(self: *GcAllocator, table: *HashMap) void {
        for (table.entries()) |entry| {
            if (entry) |e| {
                self.markObject(@ptrCast(e.key));
                self.markValue(e.val);
            }
        }
    }

    fn markValue(self: *GcAllocator, val: Value) void {
        switch (val) {
            .obj => |o| self.markObject(o),
            else => return,
        }
    }

    fn blackenObject(self: *GcAllocator, obj: *Obj) void {
        if (comptime DEBUG_LOG_GC) {
            //std.debug.print("0x{x} blacken ", .{ @intFromPtr(obj) });
            std.debug.print("blacken ", .{});
            value.printValue(Value.obj(obj), std.io.getStdErr().writer()) catch {};
            std.debug.print("\n", .{});
        }

        switch (obj.type) {
            .BOUND_METHOD => {
                const bound = @fieldParentPtr(object.ObjBoundMethod, "obj", obj);
                self.markValue(bound.receiver);
                self.markObject(&bound.method.obj);
            },
            .CLASS => {
                const class: *object.ObjClass = @ptrCast(obj);
                self.markObject(@ptrCast(class.name));
                self.markTable(&class.methods);
            },
            .CLOSURE => {
                const closure: *object.ObjClosure = @ptrCast(obj);
                self.markObject(@ptrCast(closure.function));
                for (closure.upvalues.toSlice()) |uv| {
                    self.markObject(@ptrCast(uv));
                }
            },
            .FUNCTION => {
                const fun: *object.ObjFunction = @ptrCast(obj);
                self.markObject(@ptrCast(fun.name));
                for (fun.chunk.constants.toSlice()) |val| {
                    self.markValue(val);
                }
            },
            .INSTANCE => {
                const instance: *object.ObjInstance = @ptrCast(obj);
                self.markTable(&instance.fields);
            },
            .UPVALUE => {
                const uv: *object.ObjUpvalue = @ptrCast(@alignCast(obj));
                self.markValue(uv.open.*);
            },
            else => {},
        }
    }

    fn markObject(self: *GcAllocator, obj: ?*Obj) void {
        if (obj) |o| {
            if (!o.is_marked) {
                if (comptime DEBUG_LOG_GC) {
                    //std.debug.print("mark ", .{ });
                    std.debug.print("0x{x} mark ", .{@intFromPtr(o)});
                    value.printValue(Value.obj(o), std.io.getStdErr().writer()) catch {};
                    std.debug.print("\n", .{});
                }
                o.is_marked = true;
                self.gray_stack.append(o) catch {
                    std.os.exit(1);
                };
                // switch (o.type) {
                //     .NATIVE, .STRING => {},
                //     else => self.gray_stack.append(o) catch {
                //         std.os.exit(1);
                //     },
                // }
            }
        }
    }

    fn sweep() void {
        var prev: ?*Obj = null;
        var obj: ?*Obj = vm.vm.objects;
        while (obj) |curr| {
            if (curr.is_marked) {
                curr.is_marked = false;
                prev = curr;
                obj = curr.next;
            } else {
                obj = curr.next;
                if (prev) |p| {
                    p.next = obj;
                } else {
                    vm.vm.objects = obj;
                }
                curr.free();
            }
        }
    }
};
