![Build status](https://github.com/helmutschneider/json/workflows/build/badge.svg)

# JSON parser implementations
This repository is a collection of JSON parser implementations in various
languages. They are absolutely not production ready; I am just fooling around
with languages that I find interesting.

All implementations should roughly follow this interface:

```
type JsonNode =
    | String
    | Number
    | Null
    | Boolean
    | Map<String, JsonNode>
    | Array<JsonNode>

type JsonParser =
    fun parse(data: String) -> JsonNode
```

## Usage

| Name   | Command          |
|--------|------------------|
| C      | `make tests`     |
| Golang | `go test`        |
| Rust   | `cargo test`     |
| OCaml  | `dune runtest`   |
