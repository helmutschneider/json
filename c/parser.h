#ifndef PARSER_H
#define PARSER_H

#include "node.h"

typedef struct
{
    size_t index;
    size_t length;
    const char *str;
} parser_t;

parser_t parser_make();
node_t parser_parse(parser_t *self, const char *str);
node_t parser_read_node(parser_t *self);

#endif
