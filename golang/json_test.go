package main

import (
	"reflect"
	"testing"
)

func assertEq(t *testing.T, expected Node, actual Node) {
	if !reflect.DeepEqual(expected, actual) {
		t.Errorf("Expected %+v, received %+v\n", expected, actual)
	}
}

func TestParseString(t *testing.T) {
	parser := newParser()
	node := parser.parse("\"yee!\"")

	assertEq(t, StringNode{str: "yee!"}, node)
}

func TestParseArrayWithSingleElement(t *testing.T) {
	parser := newParser()
	node := parser.parse("[ \"a\" ]")

	assertEq(t, ArrayNode{array: []Node{StringNode{str: "a"}}}, node)
}

func TestParseArrayWithMultipleElements(t *testing.T) {
	parser := newParser()
	node := parser.parse("[ \"a\", [ \"b\" ], \"c\" ]")
	expected := ArrayNode{
		array: []Node{
			StringNode{str: "a"},
			ArrayNode{array: []Node{
				StringNode{str: "b"},
			}},
			StringNode{str: "c"},
		},
	}

	assertEq(t, expected, node)
}

func TestParseBooleans(t *testing.T) {
	parser := newParser()
	a := parser.parse("true")
	b := parser.parse("false")

	assertEq(t, BooleanNode{boolean: true}, a)
	assertEq(t, BooleanNode{boolean: false}, b)
}

func TestParseNull(t *testing.T) {
	parser := newParser()
	node := parser.parse("null")

	assertEq(t, NullNode{}, node)
}

func TestParseNumber(t *testing.T) {
	parser := newParser()
	a := parser.parse("123.5")
	b := parser.parse("123")

	assertEq(t, NumberNode{number: 123.5}, a)
	assertEq(t, NumberNode{number: 123.0}, b)
}

func TestParseObject(t *testing.T) {
	parser := newParser()
	node := parser.parse("{ \"a\": \"b\", \"c\": [\"d\"] }")
	assertEq(t, ObjectNode{entries: map[string]Node{
		"a": StringNode{str: "b"},
		"c": ArrayNode{array: []Node{
			StringNode{str: "d"},
		}},
	}}, node)
}

func TestParseLargeThing(t *testing.T) {
	parser := newParser()
	str := `
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
	`
	node := parser.parse(str)

	if node.kind() != NodeKindObject {
		t.Errorf("bad!bad!")
	}
}

func TestAcceptsTrailingCommas(t *testing.T) {
	parser := newParser()
	a := parser.parse("[ 1.5, ]")

	assertEq(t, ArrayNode{array: []Node{NumberNode{number: 1.5}}}, a)

	b := parser.parse("{ \"a\": 1.5, }")

	assertEq(t, ObjectNode{
		entries: map[string]Node{
			"a": NumberNode{number: 1.5},
		},
	}, b)
}
