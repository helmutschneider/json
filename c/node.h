#ifndef NODE_H
#define NODE_H

#include <stdio.h>
#include <stdbool.h>
#include "vector.h"

VECTOR_DECL(vector_char, char)

// we cannot use the typedef because it's recursive. kind of.
VECTOR_DECL(vector_node, struct node)
VECTOR_DECL(vector_entry, struct entry)

typedef enum
{
    NODE_KIND_STRING,
    NODE_KIND_NUMBER,
    NODE_KIND_NULL,
    NODE_KIND_BOOLEAN,
    NODE_KIND_ARRAY,
    NODE_KIND_OBJECT,
} node_kind_t;

typedef struct node
{
    node_kind_t kind;
    union
    {
        vector_char_t string;
        double number;
        bool boolean;
        vector_node_t array;
        vector_entry_t object;
    };
} node_t;

typedef struct entry
{
    vector_char_t key;
    node_t value;
} entry_t;

void print_node(node_t node);

#endif
