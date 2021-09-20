const std = @import("std");

pub const NodeKind = enum {
    string,
    number,
    boolean,
    null_,
    array,
    object,
};

pub const Node = union(NodeKind) {
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

    pub fn eql(self: Node, other: Node) bool {
        return switch (self) {
            .string => |a| switch (other) {
                .string => |b| std.mem.eql(u8, a, b),
                else => false,
            },
            .array => |a| switch (other) {
                .array => |b| {
                    if (a.len != b.len) {
                        return false;
                    }
                    var i: usize = 0;
                    while (i < a.len) {
                        if (!a[i].eql(b[i])) {
                            return false;
                        }
                        i += 1;
                    }
                    return true;
                },
                else => false,
            },
            .object => |a| {
                var iter = a.iterator();

                return switch (other) {
                    .object => |b| {
                        while (iter.next()) |kv| {
                            const found = b.get(kv.key_ptr.*);
                            if (found == null or !kv.value_ptr.eql(found.?)) {
                                return false;
                            }
                        }
                        return true;
                    },
                    else => false,
                };
            },
            .boolean => |a| switch (other) {
                .boolean => |b| a == b,
                else => false,
            },
            .null_ => |a| switch (other) {
                .null_ => true,
                else => false,
            },
            .number => |a| switch (other) {
                .number => |b| a == b,
                else => return false,
            },
        };
    }
};
