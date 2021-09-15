#include <stdbool.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include "node.h"
#include "parser.h"

static const node_t DEFAULT_NODE = {
    .kind = NODE_KIND_NULL,
};

parser_t parser_make()
{
    parser_t thing = {
        .index = 0,
        .length = 0,
        .str = NULL,
    };
    return thing;
}

static bool parser_is_eof(const parser_t *self)
{
    return self->index >= self->length;
}

static bool parser_is_whitespace(const parser_t *self)
{
    char chr = self->str[self->index];
    return chr == ' ' || chr == '\r' || chr == '\n' || chr == '\t';
}

static bool parser_is_number_like(const parser_t *self)
{
    char chr = self->str[self->index];
    return chr == '0' || chr == '1' || chr == '2' || chr == '3' || chr == '4' || chr == '5' || chr == '6' || chr == '7' || chr == '8' || chr == '9' || chr == '.';
}

static char parser_peek(const parser_t *self)
{
    return self->str[self->index];
}

static void parser_skip_whitespace(parser_t *self)
{
    while (!parser_is_eof(self) && parser_is_whitespace(self))
    {
        self->index += 1;
    }
}

static vector_char_t parser_read_string(parser_t *self)
{
    assert(parser_peek(self) == '"');

    self->index += 1;

    vector_char_t out = vector_char_make();
    bool is_reading_escaped_string = false;

    while ((parser_peek(self) != '"' || is_reading_escaped_string))
    {
        vector_char_push(&out, parser_peek(self));
        self->index += 1;
        is_reading_escaped_string = parser_peek(self) == '\\';

        if (is_reading_escaped_string)
        {
            self->index += 1;
        }
    }

    self->index += 1;

    return out;
}

static vector_node_t parser_read_array(parser_t *self)
{
    assert(parser_peek(self) == '[');

    self->index += 1;
    vector_node_t out = vector_node_make();

    while (parser_peek(self) != ']')
    {
        node_t node = parser_read_node(self);
        vector_node_push(&out, node);
        parser_skip_whitespace(self);

        if (parser_peek(self) == ',')
        {
            self->index += 1;
        }
    }

    self->index += 1;

    return out;
}

static vector_entry_t parser_read_object(parser_t *self)
{
    assert(parser_peek(self) == '{');
    self->index += 1;
    vector_entry_t out = vector_entry_make();

    while (parser_peek(self) != '}')
    {
        parser_skip_whitespace(self);
        vector_char_t key = parser_read_string(self);
        parser_skip_whitespace(self);
        assert(parser_peek(self) == ':');
        self->index += 1;
        node_t value = parser_read_node(self);
        entry_t entry = {
            .key = key,
            .value = value,
        };
        vector_entry_push(&out, entry);
        parser_skip_whitespace(self);

        if (parser_peek(self) == ',')
        {
            self->index += 1;
        }
    }

    self->index += 1;
    return out;
}

static double parser_read_number(parser_t *self)
{
    size_t index = 0;
    char buf[32];

    while (parser_is_number_like(self))
    {
        buf[index] = parser_peek(self);
        index += 1;
        self->index += 1;
    }

    self->index += 1;

    return strtod(buf, NULL);
}

node_t parser_read_node(parser_t *self)
{
    parser_skip_whitespace(self);

    node_t node = DEFAULT_NODE;

    if (parser_is_number_like(self))
    {
        node.kind = NODE_KIND_NUMBER;
        node.number = parser_read_number(self);
        return node;
    }

    const char thing = parser_peek(self);

    switch (thing)
    {
    case '"':
    {
        node.kind = NODE_KIND_STRING;
        node.string = parser_read_string(self);
        break;
    }
    case '[':
    {
        node.kind = NODE_KIND_ARRAY;
        node.array = parser_read_array(self);
        break;
    }
    case '{':
    {
        node.kind = NODE_KIND_OBJECT;
        node.object = parser_read_object(self);
        break;
    }
    case 'n':
    {
        node.kind = NODE_KIND_NULL;
        self->index += 4;
        break;
    }
    case 't':
    {
        node.kind = NODE_KIND_BOOLEAN;
        node.boolean = true;
        self->index += 4;
        break;
    }
    case 'f':
    {
        node.kind = NODE_KIND_BOOLEAN;
        node.boolean = false;
        self->index += 5;
        break;
    }
    default:
        break;
    }

    return node;
}

node_t parser_parse(parser_t *self, const char *str)
{
    self->index = 0;
    self->str = str;
    self->length = strlen(str);

    return parser_read_node(self);
}
