const std = @import("std");
const Parser = @import("parser.zig").Parser;
const Node = @import("node.zig").Node;
const t = std.testing;

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
        .object => |a| {
            var iter = a.iterator();

            switch (actual) {
                .object => |b| {
                    while (iter.next()) |kv| {
                        const found = b.get(kv.key_ptr.*);
                        t.expect(found != null) catch return E.TestUnexpectedError;
                        try expectEqualNodes(kv.value_ptr.*, found.?);
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
        .number => |a| {
            switch (actual) {
                .number => |b| t.expectEqual(a, b) catch return E.TestUnexpectedError,
                else => return E.TestUnexpectedError,
            }
        },
    }
}

test "Parse string" {
    const parser = Parser.init(t.allocator);
    var node = parser.parse("\"yee\"");
    defer node.deinit(t.allocator);
    try expectEqualNodes(Node{ .string = "yee" }, node);
}

test "Parse string with escaped character" {
    const parser = Parser.init(t.allocator);
    var node = parser.parse("\"\\\"yee\"");
    defer node.deinit(t.allocator);
    try expectEqualNodes(Node{ .string = "\"yee" }, node);
}

test "Parse array of strings" {
    const parser = Parser.init(t.allocator);
    var node = parser.parse("[ \"a\" , \"b\" ]");
    defer node.deinit(t.allocator);
    try expectEqualNodes(Node{ .array = &[_]Node{ Node{ .string = "a" }, Node{ .string = "b" } } }, node);
}

test "Parse booleans" {
    const parser = Parser.init(t.allocator);
    const a = parser.parse("true");
    const b = parser.parse("false");
    try expectEqualNodes(Node{ .boolean = true }, a);
    try expectEqualNodes(Node{ .boolean = false }, b);
}

test "Parse null" {
    const parser = Parser.init(t.allocator);
    const a = parser.parse("null");
    try expectEqualNodes(Node.null_, a);
}

test "Parse object with single key" {
    const parser = Parser.init(t.allocator);
    var node = parser.parse("{ \"a\": null }");
    defer node.deinit(t.allocator);
    var map = std.StringHashMap(Node).init(t.allocator);
    map.put("a", Node.null_) catch unreachable;
    defer map.deinit();
    try expectEqualNodes(Node{ .object = map }, node);
}

test "Parse object with multiple keys" {
    const parser = Parser.init(t.allocator);
    var node = parser.parse("{ \"a\": null, \"b\": [null, null] }");
    defer node.deinit(t.allocator);
    var map = std.StringHashMap(Node).init(t.allocator);
    map.put("a", Node.null_) catch unreachable;
    map.put("b", Node{ .array = &[_]Node{ Node.null_, Node.null_ } }) catch unreachable;
    defer map.deinit();
    try expectEqualNodes(Node{ .object = map }, node);
}

test "Parse object with nested stuff" {
    const parser = Parser.init(t.allocator);
    var node = parser.parse("{ \"a\": { \"b\": [] } }");
    defer node.deinit(t.allocator);
    var second = std.StringHashMap(Node).init(t.allocator);
    defer second.deinit();
    second.put("b", Node{ .array = &.{} }) catch unreachable;

    var first = std.StringHashMap(Node).init(t.allocator);
    defer first.deinit();
    first.put("a", Node{ .object = second }) catch unreachable;

    try expectEqualNodes(Node{ .object = first }, node);
}

test "Parse float" {
    const parser = Parser.init(t.allocator);
    var node = parser.parse("123.5");
    defer node.deinit(t.allocator);
    try expectEqualNodes(Node{ .number = 123.5 }, node);
}

test "Parse int" {
    const parser = Parser.init(t.allocator);
    var node = parser.parse("456");
    defer node.deinit(t.allocator);
    try expectEqualNodes(Node{ .number = 456 }, node);
}

test "Parse large thing" {
    const str =
        \\    {
        \\        "firstName": "John",
        \\        "lastName": "Smith",
        \\        "isAlive": true,
        \\        "age": 27,
        \\        "address": {
        \\          "streetAddress": "21 2nd Street",
        \\          "city": "New York",
        \\          "state": "NY",
        \\          "postalCode": "10021-3100"
        \\        },
        \\        "phoneNumbers": [
        \\          {
        \\            "type": "home",
        \\            "number": "212 555-1234"
        \\          },
        \\          {
        \\            "type": "office",
        \\            "number": "646 555-4567"
        \\          }
        \\        ],
        \\        "children": [],
        \\        "spouse": null
        \\    }
    ;
    const parser = Parser.init(t.allocator);
    var node = parser.parse(str);
    defer node.deinit(t.allocator);

    println(node);

    try t.expect(true);

    // println(node);
}
