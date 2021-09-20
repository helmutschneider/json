const std = @import("std");
const Parser = @import("parser.zig").Parser;
const Node = @import("node.zig").Node;

fn println(thing: anytype) void {
    std.io.getStdOut().writer().print("{s}\n", .{thing}) catch unreachable;
}

pub fn main() !void {
    const str =
        \\ ["a", "b", ["c"]]
    ;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;
    const parser = Parser.init(allocator);
    const n = parser.parse(str);
    println(n);
}
