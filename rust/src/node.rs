use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum JsonNode {
    String(String),
    Number(f64),
    Null,
    Boolean(bool),
    Array(Vec<JsonNode>),
    Object(HashMap<String, JsonNode>),
}

impl JsonNode {
    pub fn as_str(&self) -> &str {
        if let JsonNode::String(x) = self {
            return x;
        }
        panic!("Expected string, found {:?}.", self);
    }

    pub fn as_object(&self) -> &HashMap<String, JsonNode> {
        if let JsonNode::Object(x) = self {
            return x;
        }
        panic!("Expected object, found {:?}.", self);
    }

    pub fn as_array(&self) -> &[JsonNode] {
        if let JsonNode::Array(x) = self {
            return x;
        }
        panic!("Expected array, found {:?}.", self);
    }

    pub fn object_of<const N: usize>(entries: [(&str, JsonNode); N]) -> JsonNode {
        let mut map = HashMap::<String, JsonNode>::new();
        for e in entries {
            map.insert(e.0.to_string(), e.1.clone());
        }
        return JsonNode::Object(map);
    }
}
