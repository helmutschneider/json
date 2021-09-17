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

## Languages

| Name   | Test command     | Review                                                                                                         | 
|--------|------------------|----------------------------------------------------------------------------------------------------------------|
| C      | `make tests`     | Better than expected. Lacking in type safety but the preprocessor & union types make up for some of it. 6/10 |
| Golang | `go test`        | Programming in Go honestly feels like a worse C. The type system is so weak that I get angry. No true enums, no unions, no generics, shitty structural typing, `nil` pointers, `interface {}` for polymorphism, the ability to instantiate incomplete structs... the list goes on and on. 2/10 |
| Rust   | `cargo test`     | Absolutely fantastic. Great ecosystem with low barrier of entry. Strong type system. Built-in testing framework. Generics, traits & algebraic data types makes working in Rust a breeze. 9/10 |
| OCaml  | `dune runtest`   | Good FP language with a strong type system. The docs & ecosystem are quite lacking though. Setting up a test case took several hours. 7/10 |
