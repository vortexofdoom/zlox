const std = @import("std");
const Allocator = std.mem.Allocator;

const value = @import("value.zig");
const Value = value.Value;
const vm = @import("vm.zig");
const object = @import("object.zig");
const ObjString = object.ObjString;

const TABLE_MAX_LOAD = 0.75;

inline fn growCapacity(cap: usize) usize {
    return if (cap < 8) 8 else cap << 1;
}

var allocator: std.mem.Allocator = undefined;

pub fn ArrayList(comptime T: type) type {
    return extern struct {
        const Self = @This();
        items: [*]T = &[_]T{},
        count: usize = 0,
        capacity: usize = 0,

        pub fn init(alloc: Allocator) Self {
            allocator = alloc; 
            return Self{
                .capacity = 0,
                .count = 0,
            };
        }

        pub fn append(self: *Self, item: T) !void {
            if (self.capacity < self.count + 1) {
                const old_cap = self.capacity;
                self.capacity = growCapacity(self.capacity);
                self.items = (try allocator.realloc(self.items[0..old_cap], self.capacity)).ptr;
                //std.debug.print("reallocating {any} array: ", .{T});
            }

            self.items[self.count] = item;
            self.count += 1;
            //std.debug.print("{any}\n", .{self.items[0..self.count]});
        }

        pub fn deinit(self: Self) void {
            if (@sizeOf(T) > 0) {
                allocator.free(self.items[0..self.capacity]);
            }
        }
    };
}

const Entry = struct {
    key: ?*ObjString,
    val: Value,
};

pub const HashMap = struct {
    count: usize,
    allocator: Allocator,
    entries: []?Entry,

    pub fn init(self: *HashMap) void {
        self.allocator = vm.Vm.allocator;
        self.count = 0;
        self.entries = &.{};
    }

    pub fn free(self: *HashMap) void {
        self.allocator.free(self.entries);
        self.init();
    }

    pub fn get(self: *HashMap, key: *ObjString) ?Value {
        if (self.count == 0) return null;
        if (findEntry(self.entries, key).*) |entry| {
            if (entry.key) |_| return entry.val;
        }
        return null;
    }

    pub fn insert(self: *HashMap, key: *ObjString, val: Value) !bool {
        if (@as(f64, @floatFromInt(self.count + 1)) > @as(f64, @floatFromInt(self.entries.len)) * TABLE_MAX_LOAD) {
            try self.adjustCapacity(growCapacity(self.entries.len));
        }

        var entry = findEntry(self.entries, key);
        const is_new_key = entry.* == null;
        if (is_new_key) self.count += 1;

        entry.* = .{
            .key = key,
            .val = val,
        };

        return is_new_key;
    }

    pub fn delete(self: *HashMap, key: *ObjString) bool {
        var entry = findEntry(self.entries, key);
        if (entry.* != null) {
            entry.*.?.key = null;
            entry.*.?.val = Value{ .bool = true };
            return true;
        } else return false;
    }

    pub fn copyFrom(self: *HashMap, from: *HashMap) !void {
        for (from.entries) |entry| {
            if (entry) |e| {
                try self.insert(e.key, e.val);
            }
        }
    }

    pub fn adjustCapacity(self: *HashMap, capacity: usize) !void {
        var entries: []?Entry = try self.allocator.alloc(?Entry, capacity);
        for (entries) |*entry| {
            entry.* = null;
        }

        self.count = 0;
        for (self.entries) |entry| {
            if (entry) |e| {
                if (e.key) |k| {
                    self.count += 1;
                    findEntry(entries, k).* = .{
                        .key = e.key,
                        .val = e.val,
                    };
                }
            }
        }

        self.allocator.free(self.entries);

        self.entries = entries;
    }

    pub fn findString(self: *HashMap, chars: []const u8, hash: u32) ?*ObjString {
        if (self.count == 0) return null;
        var idx = hash % self.entries.len;
        while (true) : (idx = (idx + 1) % self.entries.len) {
            const entry = &self.entries[idx];
            if (entry.*) |e| {
                if (e.key) |k| {
                    if (k.len == chars.len and k.hash == hash and std.mem.order(u8, k.ptr[0..k.len], chars) == .eq) {
                        return k;
                    }
                }
            } else return null;
        }
    }
};

fn findEntry(entries: []?Entry, key: *ObjString) *?Entry {
    var idx: usize = key.hash % entries.len;
    var tombstone: ?*?Entry = null;
    while (true) : (idx = (idx + 1) % entries.len) {
        const entry = &entries[idx];

        if (entry.*) |e| {
            if (e.key) |k| {
                if (k == key) return entry;
            } else {
                tombstone = entry;
            }
        } else return tombstone orelse entry;
    }
}
