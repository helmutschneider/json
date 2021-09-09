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

fn is_whitespace(chr: char) -> bool {
    return chr == ' ' || chr == '\r' || chr == '\n' || chr == '\t';
}

struct Parser {}

impl Parser {
    const fn new() -> Self {
        return Self {};
    }

    fn parse(&self, data: &str) -> JsonNode {
        return ParserState::new(data).read_node();
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

    #[test]
    fn parse_empty_array() {
        let parser = Parser::new();
        let result = parser.parse("[]");
        assert_eq!(JsonNode::Array(vec![]), result);
    }

    #[test]
    fn parse_array_of_strings() {
        let parser = Parser::new();
        let result = parser.parse("[\"yee\", \"boi\"]");
        assert_eq!(
            JsonNode::Array(vec![
                JsonNode::String("yee".to_string()),
                JsonNode::String("boi".to_string())
            ]),
            result
        );
    }

    #[test]
    fn parse_array_with_whitespace() {
        let parser = Parser::new();
        let result = parser.parse("[   \"boi\"  \n    ]");
        assert_eq!(
            JsonNode::Array(vec![JsonNode::String("boi".to_string())]),
            result
        );
    }
}
