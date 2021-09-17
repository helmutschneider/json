package main

import (
	"fmt"
	"strconv"
)

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

func isNumberLike(chr rune) bool {
	return chr == '0' || chr == '1' || chr == '2' || chr == '3' || chr == '4' || chr == '5' || chr == '6' || chr == '7' || chr == '8' || chr == '9' || chr == '.'
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

	if isNumberLike(chr) {
		return NumberNode{
			number: p.readNumber(),
		}
	}

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
	case '{':
		node = ObjectNode{
			entries: p.readObject(),
		}
		break
	case 't':
		node = BooleanNode{
			boolean: true,
		}
		p.index += 4
		break
	case 'f':
		node = BooleanNode{
			boolean: false,
		}
		p.index += 5
		break
	case 'n':
		node = NullNode{}
		p.index += 4
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
			p.skipWhitespace()
		}
	}

	p.moveNext()

	return stuff
}

func (p *Parser) readObject() map[string]Node {
	p.expectRune('{')
	p.moveNext()
	out := map[string]Node{}
	for p.peek() != '}' {
		p.skipWhitespace()
		key := p.readString()
		p.skipWhitespace()
		p.expectRune(':')
		p.moveNext()
		value := p.readNode()

		out[key] = value

		p.skipWhitespace()
		if p.peek() == ',' {
			p.moveNext()
			p.skipWhitespace()
		}
	}
	p.moveNext()
	return out
}

func (p *Parser) readNumber() float64 {
	carry := ""
	for !p.isEof() && isNumberLike(p.peek()) {
		carry += string(p.peek())
		p.moveNext()
	}
	num, e := strconv.ParseFloat(carry, 64)
	if e != nil {
		return 0
	}
	return num
}

func (p *Parser) isEof() bool {
	return p.index >= len(p.buffer)
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
