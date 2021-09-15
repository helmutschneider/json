#include "node.h"
#include "vector.h"

VECTOR_IMPL(vector_char, char)
VECTOR_IMPL(vector_node, node_t)
VECTOR_IMPL(vector_entry, entry_t)

static const char *INDENTS[] = {
    [0] = "",
    [1] = "  ",
    [2] = "    ",
    [3] = "      ",
    [4] = "        ",
    [5] = "          ",
    [6] = "            ",
    [7] = "              ",
    [8] = "                ",
    [9] = "                  ",
};

static const char *NODE_KIND_NAMES[] = {
    [NODE_KIND_STRING] = "NODE_KIND_STRING",
    [NODE_KIND_NUMBER] = "NODE_KIND_NUMBER",
    [NODE_KIND_NULL] = "NODE_KIND_NULL",
    [NODE_KIND_BOOLEAN] = "NODE_KIND_BOOLEAN",
    [NODE_KIND_ARRAY] = "NODE_KIND_ARRAY",
    [NODE_KIND_OBJECT] = "NODE_KIND_OBJECT",
};

static void print_vector_char(vector_char_t vec)
{
    for (size_t i = 0; i < vec.length; ++i)
    {
        printf("%c", vec.data[i]);
    }
}

static void print_node_with_depth(node_t node, uint64_t depth)
{
    const char *name = NODE_KIND_NAMES[node.kind];
    const char *indent = INDENTS[depth];

    switch (node.kind)
    {
    case NODE_KIND_STRING:
        printf("%s%s: ", indent, name);
        print_vector_char(node.string);
        printf("\n");
        break;
    case NODE_KIND_ARRAY:
        printf("%s%s:\n", indent, name);
        for (size_t i = 0; i < node.array.length; ++i)
        {
            print_node_with_depth(node.array.data[i], depth + 1);
        }
        break;
    case NODE_KIND_OBJECT:
        printf("%s%s:\n", indent, name);
        for (size_t i = 0; i < node.object.length; ++i)
        {
            entry_t entry = node.object.data[i];
            printf("%s", INDENTS[depth + 1]);
            print_vector_char(entry.key);
            printf(":\n");
            print_node_with_depth(entry.value, depth + 2);
        }
        break;
    case NODE_KIND_NULL:
        printf("%s%s\n", indent, name);
        break;
    case NODE_KIND_BOOLEAN:
        printf("%s%s: %s\n", indent, name, node.boolean ? "true" : "false");
        break;
    case NODE_KIND_NUMBER:
        printf("%s%s: %f\n", indent, name, node.number);
        break;
    }
}

void print_node(node_t node)
{
    print_node_with_depth(node, 0);
}
