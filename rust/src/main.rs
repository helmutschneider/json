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
        let obj = JsonNode::object_of([("a", JsonNode::String("b".to_string()))]);
        assert_eq!(obj, result);
    }

    #[test]
    fn parse_object_with_multiple_properties() {
        let parser = JsonParser::new();
        let result = parser.parse("{ \"a\": \"b\", \"c\": \"d\" }");
        let obj = JsonNode::object_of([
            ("a", JsonNode::String("b".to_string())),
            ("c", JsonNode::String("d".to_string())),
        ]);
        assert_eq!(obj, result);
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

    #[test]
    fn parse_null() {
        let parser = JsonParser::new();
        let result = parser.parse("null");
        assert_eq!(JsonNode::Null, result);
    }

    #[test]
    fn parse_object_with_null() {
        let parser = JsonParser::new();
        let result = parser.parse("{ \"a\": null }");
        let obj = JsonNode::object_of([("a", JsonNode::Null)]);
        assert_eq!(obj, result);
    }

    #[test]
    fn parse_object_with_booleans() {
        let parser = JsonParser::new();
        let result = parser.parse("{ \"a\": true, \"b\": false }");
        let obj = JsonNode::object_of([
            ("a", JsonNode::Boolean(true)),
            ("b", JsonNode::Boolean(false)),
        ]);
        assert_eq!(obj, result);
    }

    #[test]
    fn parse_large_thing() {
        // https://en.wikipedia.org/wiki/JSON#Syntax
        let thing = r#"
        {
            "firstName": "John",
            "lastName": "Smith",
            "isAlive": true,
            "age": 27,
            "address": {
              "streetAddress": "21 2nd Street",
              "city": "New York",
              "state": "NY",
              "postalCode": "10021-3100"
            },
            "phoneNumbers": [
              {
                "type": "home",
                "number": "212 555-1234"
              },
              {
                "type": "office",
                "number": "646 555-4567"
              }
            ],
            "children": [],
            "spouse": null
          }
        "#;

        let parsed = JsonParser::new().parse(thing);

        assert!(matches!(parsed, JsonNode::Object(_)));
        assert_eq!(
            "John",
            parsed.as_object().get("firstName").unwrap().as_str()
        );
        assert_eq!(
            "646 555-4567",
            parsed.as_object().get("phoneNumbers").unwrap().as_array()[1]
                .as_object()
                .get("number")
                .unwrap()
                .as_str()
        )
    }
}
