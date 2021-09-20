const std = @import("std");

pub const Node = union(enum) {
    string: []const u8,
    number: f64,
    boolean: bool,
    null_: void,
    array: []const Node,
    object: std.StringHashMap(Node),
};
