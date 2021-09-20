const std = @import("std");
const Node = @import("node.zig").Node;

fn isWhitespace(chr: u8) bool {
    return chr == ' ' or chr == '\t' or chr == '\n' or chr == '\r';
}

const State = struct {
    allocator: *std.mem.Allocator,
    index: usize,
    buffer: []const u8,

    const Self = @This();

    pub fn readNode(self: *Self) Node {
        self.skipWhitespace();

        return switch (self.peek()) {
            '"' => Node{ .string = self.readString() },
            '[' => Node{ .array = self.readArray() },
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
