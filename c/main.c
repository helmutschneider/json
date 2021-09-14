#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "parser.h"

int main()
{
    parser_t parser = parser_make();
    node_t node = parser_parse(&parser, "{ \"yee\": \"boi\", \"swag\": true }");

    print_node(node);
}
