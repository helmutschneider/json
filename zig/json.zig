const std = @import("std");
const t = std.testing;

const writer = std.io.getStdOut().writer();

fn println(str: []const u8) void {
    writer.print("{s}\n", .{str}) catch unreachable;
}

const Str = struct {
    buffer: [256]u8 = [_]u8{0} ** 256,
    len: usize = 0,

    pub fn init(data: []const u8) Str {
        var str = Str{};
        var i: usize = 0;
        while (i < str.len) {
            str.buffer[i] = data[i];
            i += 1;
        }
        str.len = i + 1;
        return str;
    }

    pub fn toU8Slice(self: Str) []const u8 {
        return self.buffer[0..self.len];
    }
};
const Node = union(enum) {
    string: Str,
    number: f64,
    boolean: bool,
    null_: void,
    array: []const Node,
    object: std.StringHashMap(Node),
};

fn isWhitespace(chr: u8) bool {
    return chr == ' ' or chr == '\t' or chr == '\n' or chr == '\r';
}

const State = struct {
    index: usize,
    buffer: []const u8,

    pub fn readNode(self: *State) Node {
        self.skipWhitespace();

        return switch (self.peek()) {
            '"' => {
                const str = self.readString();
                return Node{ .string = str };
            },
            else => unreachable,
        };
    }

    fn peek(self: *State) u8 {
        return self.buffer[self.index];
    }

    fn expect(self: *State, chr: u8) void {
        if (self.peek() != chr) {
            unreachable;
        }
    }

    fn moveNext(self: *State) void {
        self.index += 1;
    }

    fn skipWhitespace(self: *State) void {
        while (isWhitespace(self.peek())) {
            self.moveNext();
        }
    }

    fn readString(self: *State) Str {
        self.expect('"');
        self.moveNext();

        var buffer = [_]u8{0} ** 128;
        var index: usize = 0;

        while (self.peek() != '"') {
            const isReadingEscapedCharacter = self.peek() == '\\';

            var arr = [_]u8{self.peek()};

            if (isReadingEscapedCharacter) {
                self.moveNext();
                buffer[index] = self.peek();
            } else {
                buffer[index] = self.peek();
            }

            index += 1;

            self.moveNext();
        }

        self.moveNext();

        return Str.init(buffer[0..(index + 1)]);
    }
};

const Parser = struct {
    pub fn init() Parser {
        return .{};
    }

    pub fn parse(self: Parser, str: []const u8) Node {
        var state = State{
            .index = 0,
            .buffer = str,
        };
        return state.readNode();
    }
};

const parser = Parser.init();

pub fn main() !void {
    const n = parser.parse("\"yee\"");
    const stdout = std.io.getStdOut().writer();
    try stdout.print("{s}\n", .{n});
}

test "Read string" {
    const node = parser.parse("\"yee\"");
    switch (node) {
        .string => |n| {
            try t.expectEqual(Str.init("yee"), n);
        },
        else => unreachable,
    }
}

test "Read string with escaped character" {
    const node = parser.parse("\"\\\"yee\"");
    switch (node) {
        .string => |n| {
            try t.expectEqual(Str.init("\"yee"), n);
        },
        else => unreachable,
    }
}
