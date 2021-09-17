package main

type NodeKind int

const (
	NodeKindString NodeKind = iota
	NodeKindNumber
	NodeKindNull
	NodeKindBoolean
	NodeKindArray
	NodeKindObject
)

type Node interface {
	kind() NodeKind
}
type StringNode struct {
	str string
}
type NumberNode struct {
	number float64
}
type NullNode struct {
}
type BooleanNode struct {
	boolean bool
}
type ArrayNode struct {
	array []Node
}
type ObjectNode struct {
	entries map[string]Node
}

func (n StringNode) kind() NodeKind {
	return NodeKindString
}

func (n NumberNode) kind() NodeKind {
	return NodeKindNumber
}

func (n NullNode) kind() NodeKind {
	return NodeKindNull
}

func (n BooleanNode) kind() NodeKind {
	return NodeKindBoolean
}

func (n ArrayNode) kind() NodeKind {
	return NodeKindArray
}

func (n ObjectNode) kind() NodeKind {
	return NodeKindObject
}
