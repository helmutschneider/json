const std = @import("std");
const t = std.testing;

const writer = std.io.getStdOut().writer();

fn println(str: []const u8) void {
    writer.print("{s}\n", .{str}) catch unreachable;
}

fn BufferedArray(comptime T: type, comptime N: usize) type {
    return struct {
        buffer: [N]T,
        len: usize,

        pub const BUFFER_SIZE = N;

        pub fn init(data: []const T) @This() {
            var thing = @This(){
                .buffer = [_]u8{0} ** N,
                .len = data.len,
            };
            var i: usize = 0;
            while (i < data.len) {
                thing.buffer[i] = data[i];
                i += 1;
            }
            return thing;
        }

        pub fn toSlice(self: @This()) []const T {
            return self.buffer[0..self.len];
        }
    };
}

const Str = BufferedArray(u8, 128);
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

    pub fn readNode(self: *@This()) Node {
        self.skipWhitespace();

        return switch (self.peek()) {
            '"' => {
                const str = self.readString();
                return Node{ .string = str };
            },
            else => unreachable,
        };
    }

    fn peek(self: *@This()) u8 {
        return self.buffer[self.index];
    }

    fn expect(self: *@This(), chr: u8) void {
        if (self.peek() != chr) {
            unreachable;
        }
    }

    fn moveNext(self: *@This()) void {
        self.index += 1;
    }

    fn skipWhitespace(self: *@This()) void {
        while (isWhitespace(self.peek())) {
            self.moveNext();
        }
    }

    fn readString(self: *@This()) Str {
        self.expect('"');
        self.moveNext();

        var buffer = [_]u8{0} ** Str.BUFFER_SIZE;
        var len: usize = 0;

        while (self.peek() != '"') {
            const isReadingEscapedCharacter = self.peek() == '\\';

            if (isReadingEscapedCharacter) {
                self.moveNext();
                buffer[len] = self.peek();
            } else {
                buffer[len] = self.peek();
            }

            len += 1;

            self.moveNext();
        }

        self.moveNext();

        return Str.init(buffer[0..len]);
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

test "Initialize the string type" {
    const str = Str.init("Hello");

    try t.expectEqual(@as(usize, 5), str.len);
    try t.expectEqualStrings("Hello", str.toSlice());
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
