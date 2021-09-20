const std = @import("std");

pub const Node = union(enum) {
    string: []const u8,
    number: f64,
    boolean: bool,
    null_: void,
    array: []const Node,
    object: std.StringHashMap(Node),

    pub fn deinit(self: Node, allocator: *std.mem.Allocator) void {
        switch (self) {
            .string => |str| allocator.free(str),
            .array => |nodes| {
                for (nodes) |child| {
                    child.deinit(allocator);
                }
                allocator.free(nodes);
            },
            else => {},
        }
    }
};
