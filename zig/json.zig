const std = @import("std");
const Parser = @import("parser.zig").Parser;
const Node = @import("node.zig").Node;
const t = std.testing;

const str =
    \\ ["a", "b", ["c"]]
;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;
    const parser = Parser.init(allocator);
    const n = parser.parse(str);
    const stdout = std.io.getStdOut().writer();
    try stdout.print("{s}\n", .{n});
}

const E = error{
    TestUnexpectedError,
};

fn expectEqualNodes(expected: Node, actual: Node) E!void {
    switch (expected) {
        .string => |a| {
            switch (actual) {
                .string => |b| t.expectEqualStrings(a, b) catch return E.TestUnexpectedError,
                else => return E.TestUnexpectedError,
            }
        },
        .array => |a| {
            switch (actual) {
                .array => |b| {
                    t.expectEqual(a.len, b.len) catch return E.TestUnexpectedError;
                    var i: usize = 0;
                    while (i < a.len) {
                        try expectEqualNodes(a[i], b[i]);
                        i += 1;
                    }
                },
                else => return E.TestUnexpectedError,
            }
        },
        .boolean => |a| {
            switch (actual) {
                .boolean => |b| {
                    t.expectEqual(a, b) catch return E.TestUnexpectedError;
                },
                else => return E.TestUnexpectedError,
            }
        },
        .null_ => |a| {
            switch (actual) {
                .null_ => return,
                else => return E.TestUnexpectedError,
            }
        },
        else => return E.TestUnexpectedError,
    }
}

test "Read string" {
    const parser = Parser.init(t.allocator);
    const node = parser.parse("\"yee\"");
    defer node.deinit(t.allocator);
    try expectEqualNodes(Node{ .string = "yee" }, node);
}

test "Read string with escaped character" {
    const parser = Parser.init(t.allocator);
    const node = parser.parse("\"\\\"yee\"");
    defer node.deinit(t.allocator);
    try expectEqualNodes(Node{ .string = "\"yee" }, node);
}

test "Read array of strings" {
    const parser = Parser.init(t.allocator);
    const node = parser.parse("[ \"a\" , \"b\" ]");
    defer node.deinit(t.allocator);
    try expectEqualNodes(Node{ .array = &[_]Node{ Node{ .string = "a" }, Node{ .string = "b" } } }, node);
}

test "Read booleans" {
    const parser = Parser.init(t.allocator);
    const a = parser.parse("true");
    const b = parser.parse("false");
    try expectEqualNodes(Node{ .boolean = true }, a);
    try expectEqualNodes(Node{ .boolean = false }, b);
}

test "Read null" {
    const parser = Parser.init(t.allocator);
    const a = parser.parse("null");
    try expectEqualNodes(Node.null_, a);
}
