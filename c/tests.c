#include <stdio.h>
#include "parser.h"
#include "tests.h"

static parser_t p = {
    .index = 0,
    .length = 0,
    .str = NULL,
};

static test_result_t parse_string()
{
    node_t node = parser_parse(&p, "\"Hello!\"");

    ASSERT_EQ(NODE_KIND_STRING, node.kind);
    ASSERT_STR_EQ("Hello!", node.string.data);

    return TEST_PASS;
}

static test_result_t parse_escaped_string()
{
    node_t node = parser_parse(&p, "\"a\\\"b\"");

    ASSERT_EQ(NODE_KIND_STRING, node.kind);
    ASSERT_STR_EQ("a\"b", node.string.data);

    return TEST_PASS;
}

static test_result_t parse_empty_array()
{
    node_t node = parser_parse(&p, "[]");

    ASSERT_EQ(NODE_KIND_ARRAY, node.kind);
    ASSERT_EQ(0, node.array.length);

    return TEST_PASS;
}

static test_result_t parse_array_of_strings()
{
    node_t node = parser_parse(&p, "[\"yee\", \"boi\"]");

    ASSERT_EQ(NODE_KIND_ARRAY, node.kind);
    ASSERT_EQ(2, node.array.length);
    ASSERT_STR_EQ("yee", node.array.data[0].string.data);
    ASSERT_STR_EQ("boi", node.array.data[1].string.data);

    return TEST_PASS;
}

static test_result_t parse_array_with_whitespace()
{
    node_t node = parser_parse(&p, "[    \"boi\"    \n  ]");
    ASSERT_EQ(NODE_KIND_ARRAY, node.kind);
    ASSERT_EQ(1, node.array.length);
    ASSERT_STR_EQ("boi", node.array.data[0].string.data);

    return TEST_PASS;
}

static test_result_t parse_object_with_single_property()
{
    node_t node = parser_parse(&p, "{ \"a\": \"b\" }");
    ASSERT_EQ(NODE_KIND_OBJECT, node.kind);
    ASSERT_EQ(1, node.object.length);
    ASSERT_STR_EQ("a", node.object.data[0].key.data);
    ASSERT_STR_EQ("b", node.object.data[0].value.string.data);

    return TEST_PASS;
}

int main()
{
    TEST(parse_string);
    TEST(parse_escaped_string);
    TEST(parse_empty_array);
    TEST(parse_array_of_strings);
    TEST(parse_array_with_whitespace);
    TEST(parse_object_with_single_property);

    int res = 0;

    for (size_t i = 0; i < SUITE.length; ++i)
    {
        test_t test = SUITE.tests[i];
        test_result_t ok = test.fn();

        printf("[%s] %s\n", ok == TEST_PASS ? "OK" : "FAIL", test.name);

        res += ok;
    }

    return res;
}
