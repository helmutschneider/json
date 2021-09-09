use std::{collections::HashMap, ops::Add};

#[derive(Debug, PartialEq)]
enum JsonNode {
    String(String),
    Number(f64),
    Null,
    Boolean(bool),
    Array(Vec<JsonNode>),
    Object(HashMap<String, JsonNode>),
}

#[derive(Debug)]
struct ParserState {
    buffer: Vec<char>,
    index: usize,
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
}

fn is_whitespace(chr: char) -> bool {
    return chr == ' ' || chr == '\r' || chr == '\n' || chr == '\t';
}

struct Parser {}

impl Parser {
    const fn new() -> Self {
        return Self {};
    }

    fn parse(&self, data: &str) -> JsonNode {
        let mut state = ParserState::new(data);

        while is_whitespace(state.peek()) {
            state.move_next();
        }

        let node: JsonNode = match state.peek() {
            '"' => JsonNode::String(state.read_string()),
            _ => JsonNode::Null,
        };

        return node;
    }
}

fn main() {
    let parser = Parser::new();
}

mod tests {
    use crate::{JsonNode, Parser};

    #[test]
    fn parse_string() {
        let parser = Parser::new();
        let result = parser.parse("\"bruh\"");
        assert_eq!(JsonNode::String("bruh".to_string()), result);
    }

    #[test]
    fn parse_string_with_escaped_double_quote() {
        let parser = Parser::new();
        let result = parser.parse("\"swagger\\\"jagger\"");
        assert_eq!(JsonNode::String("swagger\"jagger".to_string()), result);
    }
}
