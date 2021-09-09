use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub enum JsonNode {
    String(String),
    Number(f64),
    Null,
    Boolean(bool),
    Array(Vec<JsonNode>),
    Object(HashMap<String, JsonNode>),
}
