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
