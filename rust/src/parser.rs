use core::panic;
use std::collections::HashMap;

use crate::node::JsonNode;

#[derive(Debug)]
struct JsonParserState {
    buffer: Vec<char>,
    index: usize,
}

fn is_whitespace(chr: char) -> bool {
    return chr == ' ' || chr == '\r' || chr == '\n' || chr == '\t';
}

fn is_number_like(ch: char) -> bool {
    return ('0'..='9').contains(&ch) || ch == '.';
}

impl JsonParserState {
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

    // FIXME: this is unnecessary. peek() should return an optional instead.
    fn is_eof(&self) -> bool {
        return self.buffer.get(self.index).is_none();
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
            '{' => JsonNode::Object(self.read_object()),
            ('0'..='9') => JsonNode::Number(self.read_number()),
            'n' => {
                self.index += 4;
                JsonNode::Null
            }
            't' => {
                self.index += 4;
                JsonNode::Boolean(true)
            }
            'f' => {
                self.index += 5;
                JsonNode::Boolean(false)
            }
            _ => JsonNode::Null,
        };

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

        self.move_next();

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

        self.move_next();

        return out;
    }

    fn read_object(&mut self) -> HashMap<String, JsonNode> {
        assert_eq!('{', self.peek());

        let mut out: HashMap<String, JsonNode> = HashMap::new();
        self.move_next();

        while self.peek() != '}' {
            self.skip_whitespace();
            let key = self.read_string();
            self.skip_whitespace();
            assert_eq!(':', self.peek());
            self.move_next();
            let value = self.read_node();
            out.insert(key, value);
            self.skip_whitespace();

            if self.peek() == ',' {
                self.move_next();
            }
        }

        self.move_next();

        return out;
    }

    fn read_number(&mut self) -> f64 {
        let mut buf = String::new();

        while !self.is_eof() && is_number_like(self.peek()) {
            buf.push(self.peek());
            self.move_next();
        }

        return match buf.parse::<f64>() {
            Result::Ok(value) => value,
            Result::Err(e) => panic!("{:?}", e),
        };
    }
}

pub struct JsonParser;

impl JsonParser {
    pub fn new() -> Self {
        return Self {};
    }

    pub fn parse(&self, data: &str) -> JsonNode {
        return JsonParserState::new(data).read_node();
    }
}
