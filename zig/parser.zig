const std = @import("std");
const n = @import("node.zig");
const Node = n.Node;
const NodeKind = n.NodeKind;
const t = std.testing;

fn isWhitespace(chr: u8) bool {
    return chr == ' ' or chr == '\t' or chr == '\n' or chr == '\r';
}

fn isNumberLike(chr: u8) bool {
    return chr == '0' or chr == '1' or chr == '2' or chr == '3' or chr == '4' or chr == '5' or chr == '6' or chr == '7' or chr == '8' or chr == '9' or chr == '.';
}

const State = struct {
    allocator: *std.mem.Allocator,
    index: usize,
    buffer: []const u8,

    const Self = @This();

    fn readNode(self: *Self) Node {
        self.skipWhitespace();

        if (isNumberLike(self.peek())) {
            return Node{ .number = self.readNumber() };
        }

        return switch (self.peek()) {
            '"' => Node{ .string = self.readString() },
            '[' => Node{ .array = self.readArray() },
            '{' => Node{ .object = self.readObject() },
            't' => {
                self.index += 4;
                return Node{ .boolean = true };
            },
            'f' => {
                self.index += 5;
                return Node{ .boolean = false };
            },
            'n' => {
                self.index += 4;
                return Node.null_;
            },
            else => unreachable,
        };
    }

    fn peek(self: *Self) u8 {
        return self.buffer[self.index];
    }

    fn expect(self: *Self, chr: u8) void {
        if (self.peek() != chr) {
            unreachable;
        }
    }

    fn moveNext(self: *Self) void {
        self.index += 1;
    }

    fn skipWhitespace(self: *Self) void {
        while (isWhitespace(self.peek())) {
            self.moveNext();
        }
    }

    fn readString(self: *Self) []const u8 {
        self.expect('"');
        self.moveNext();

        var buffer = std.ArrayList(u8).init(self.allocator);

        while (self.peek() != '"') {
            const isReadingEscapedCharacter = self.peek() == '\\';

            if (isReadingEscapedCharacter) {
                self.moveNext();
                buffer.append(self.peek()) catch unreachable;
            } else {
                buffer.append(self.peek()) catch unreachable;
            }

            self.moveNext();
        }

        self.moveNext();

        return buffer.toOwnedSlice();
    }

    fn readArray(self: *Self) []const Node {
        self.expect('[');
        self.moveNext();

        var buffer = std.ArrayList(Node).init(self.allocator);

        while (self.peek() != ']') {
            const node = self.readNode();
            buffer.append(node) catch unreachable;
            self.skipWhitespace();

            if (self.peek() == ',') {
                self.moveNext();
            }
        }

        self.moveNext();

        return buffer.toOwnedSlice();
    }

    fn readObject(self: *Self) std.StringHashMap(Node) {
        self.expect('{');
        self.moveNext();

        var map = std.StringHashMap(Node).init(self.allocator);

        while (self.peek() != '}') {
            self.skipWhitespace();
            const key = self.readString();
            self.skipWhitespace();
            self.expect(':');
            self.moveNext();
            const value = self.readNode();
            map.put(key, value) catch unreachable;
            self.skipWhitespace();

            if (self.peek() == ',') {
                self.moveNext();
            }
        }
        self.moveNext();

        return map;
    }

    fn readNumber(self: *Self) f64 {
        var buffer: [64]u8 = undefined;
        var len: usize = 0;
        while (self.index < self.buffer.len and isNumberLike(self.peek())) {
            buffer[len] = self.peek();
            self.moveNext();
            len += 1;
        }
        return std.fmt.parseFloat(f64, buffer[0..len]) catch unreachable;
    }
};

pub const Parser = struct {
    allocator: *std.mem.Allocator,

    pub fn init(
        allocator: *std.mem.Allocator,
    ) Parser {
        return .{ .allocator = allocator };
    }

    pub fn parse(self: Parser, str: []const u8) Node {
        var state = State{
            .allocator = self.allocator,
            .index = 0,
            .buffer = str,
        };
        return state.readNode();
    }
};

const E = error{
    TestUnexpectedError,
};

fn expectEqualNodes(expected: Node, actual: Node) !void {
    if (!expected.eql(actual)) {
        return E.TestUnexpectedError;
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

    try t.expectEqual(NodeKind.object, node);
}
