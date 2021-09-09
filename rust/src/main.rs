mod node;
mod parser;

use crate::parser::Parser;

fn main() {
    let parser = Parser::new();
    let node = parser.parse("[\"yee\", \"boi\"]");

    println!("{:?}", node);
}

mod tests {
    use crate::node::JsonNode;
    use crate::parser::Parser;

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
