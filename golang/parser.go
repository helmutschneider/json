package main

import "fmt"

type Parser struct {
	buffer []rune
	index  int
}

func newParser() Parser {
	return Parser{
		buffer: []rune{},
		index:  0,
	}
}

func isWhitespace(chr rune) bool {
	return chr == ' ' || chr == '\n' || chr == '\t' || chr == '\r'
}

func (p *Parser) skipWhitespace() {
	for isWhitespace(p.peek()) {
		p.moveNext()
	}
}

func (p *Parser) expectRune(ch rune) {
	if p.peek() != ch {
		str := fmt.Sprintf("Expected rune %c, received %c\n", ch, p.peek())
		panic(str)
	}
}

func (p *Parser) readNode() Node {
	p.skipWhitespace()
	chr := p.peek()
	var node Node = NullNode{}

	switch chr {
	case '"':
		node = StringNode{
			str: p.readString(),
		}
		break
	case '[':
		node = ArrayNode{
			array: p.readArray(),
		}
		break
	}

	return node
}

func (p *Parser) readString() string {
	p.expectRune('"')
	p.moveNext()

	carry := ""
	is_reading_escaped_char := false

	for p.peek() != '"' {
		is_reading_escaped_char = p.peek() == '\\'

		if is_reading_escaped_char {
			p.moveNext()
		}

		carry += string(p.peek())
		p.moveNext()
	}

	p.moveNext()

	return carry
}

func (p *Parser) readArray() []Node {
	p.expectRune('[')
	p.moveNext()
	stuff := []Node{}

	for p.peek() != ']' {
		stuff = append(stuff, p.readNode())
		p.skipWhitespace()
		if p.peek() == ',' {
			p.moveNext()
		}
	}

	p.moveNext()

	return stuff
}

func (p *Parser) peek() rune {
	return p.buffer[p.index]
}

func (p *Parser) moveNext() {
	p.index += 1
}

func (p *Parser) parse(str string) Node {
	p.buffer = []rune(str)
	p.index = 0
	return p.readNode()
}
