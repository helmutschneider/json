use crate::node::JsonNode;

#[derive(Debug)]
struct ParserState {
    buffer: Vec<char>,
    index: usize,
}

fn is_whitespace(chr: char) -> bool {
    return chr == ' ' || chr == '\r' || chr == '\n' || chr == '\t';
}

impl ParserState {
    fn new(data: &str) -> Self {
        return Self {
            buffer: data.chars().collect::<Vec<char>>(),
            index: 0,
        };
    }

    fn move_next(&mut self) {
        self.index += 1;
    }

    fn peek(&self) -> char {
        return self.buffer[self.index];
    }

    fn skip_whitespace(&mut self) {
        while is_whitespace(self.peek()) {
            self.move_next();
        }
    }

    fn read_node(&mut self) -> JsonNode {
        self.skip_whitespace();

        let node: JsonNode = match self.peek() {
            '"' => JsonNode::String(self.read_string()),
            '[' => JsonNode::Array(self.read_array()),
            _ => JsonNode::Null,
        };

        self.move_next();

        return node;
    }

    fn read_string(&mut self) -> String {
        assert_eq!('"', self.peek());

        let mut is_reading_escaped_string = false;
        self.move_next();

        let mut buffer = String::new();

        while self.peek() != '"' || is_reading_escaped_string {
            buffer.push(self.peek());
            self.move_next();

            is_reading_escaped_string = self.peek() == '\\';

            // move past the escape character. it should not
            // be included in the parsed string.
            if is_reading_escaped_string {
                self.move_next();
            }
        }

        return buffer;
    }

    fn read_array(&mut self) -> Vec<JsonNode> {
        assert_eq!('[', self.peek());

        let mut out: Vec<JsonNode> = Vec::new();
        self.move_next();

        while self.peek() != ']' {
            out.push(self.read_node());
            self.skip_whitespace();

            if self.peek() == ',' {
                self.move_next();
            }
        }

        return out;
    }
}

pub struct Parser;

impl Parser {
    pub fn new() -> Self {
        return Self {};
    }

    pub fn parse(&self, data: &str) -> JsonNode {
        return ParserState::new(data).read_node();
    }
}
