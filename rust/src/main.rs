mod node;
mod parser;

use crate::parser::JsonParser;

fn main() {
    let parser = JsonParser::new();
    let node = parser.parse("[\"yee\", \"boi\"]");

    println!("{:?}", node);
}

mod tests {
    use std::collections::HashMap;

    use crate::node::JsonNode;
    use crate::parser::JsonParser;

    #[test]
    fn parse_string() {
        let parser = JsonParser::new();
        let result = parser.parse("\"bruh\"");
        assert_eq!(JsonNode::String("bruh".to_string()), result);
    }

    #[test]
    fn parse_string_with_escaped_double_quote() {
        let parser = JsonParser::new();
        let result = parser.parse("\"swagger\\\"jagger\"");
        assert_eq!(JsonNode::String("swagger\"jagger".to_string()), result);
    }

    #[test]
    fn parse_empty_array() {
        let parser = JsonParser::new();
        let result = parser.parse("[]");
        assert_eq!(JsonNode::Array(vec![]), result);
    }

    #[test]
    fn parse_array_of_strings() {
        let parser = JsonParser::new();
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
        let parser = JsonParser::new();
        let result = parser.parse("[   \"boi\"  \n    ]");
        assert_eq!(
            JsonNode::Array(vec![JsonNode::String("boi".to_string())]),
            result
        );
    }

    #[test]
    fn parse_object_with_single_property() {
        let parser = JsonParser::new();
        let result = parser.parse("{ \"a\": \"b\" }");
        let mut map = HashMap::<String, JsonNode>::new();
        map.insert("a".to_string(), JsonNode::String("b".to_string()));
        assert_eq!(JsonNode::Object(map), result);
    }

    #[test]
    fn parse_object_with_multiple_properties() {
        let parser = JsonParser::new();
        let result = parser.parse("{ \"a\": \"b\", \"c\": \"d\" }");
        let mut map = HashMap::<String, JsonNode>::new();
        map.insert("a".to_string(), JsonNode::String("b".to_string()));
        map.insert("c".to_string(), JsonNode::String("d".to_string()));
        assert_eq!(JsonNode::Object(map), result);
    }

    #[test]
    fn parse_free_integer() {
        let parser = JsonParser::new();
        let result = parser.parse("123");
        assert_eq!(JsonNode::Number(123.0), result);
    }

    #[test]
    fn parse_free_float() {
        let parser = JsonParser::new();
        let result = parser.parse("123.5");
        assert_eq!(JsonNode::Number(123.5), result);
    }
}
