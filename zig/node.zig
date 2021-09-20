const std = @import("std");

pub const Node = union(enum) {
    string: []const u8,
    number: f64,
    boolean: bool,
    null_: void,
    array: []const Node,
    object: std.StringHashMap(Node),

    pub fn deinit(self: *Node, allocator: *std.mem.Allocator) void {
        switch (self.*) {
            .string => |str| allocator.free(str),
            .array => |nodes| {
                var i: usize = 0;
                while (i < nodes.len) {
                    var n = nodes[i];
                    n.deinit(allocator);
                    i += 1;
                }
                allocator.free(nodes);
            },
            .object => |*map| {
                var iter = map.iterator();
                while (iter.next()) |kv| {
                    allocator.free(kv.key_ptr.*);
                    kv.value_ptr.deinit(allocator);
                }
                map.deinit();
            },
            else => {},
        }
    }
};
