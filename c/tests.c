#include <stdio.h>
#include "parser.h"
#include "tests.h"

VECTOR_IMPL(vec_u8, uint8_t)

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

static test_result_t parse_object_with_multiple_properties()
{
    node_t node = parser_parse(&p, "{ \"a\": \"b\", \"c\": \"d\"} ");

    ASSERT_EQ(NODE_KIND_OBJECT, node.kind);
    ASSERT_EQ(2, node.object.length);
    ASSERT_STR_EQ("a", node.object.data[0].key.data);
    ASSERT_STR_EQ("b", node.object.data[0].value.string.data);
    ASSERT_STR_EQ("c", node.object.data[1].key.data);
    ASSERT_STR_EQ("d", node.object.data[1].value.string.data);

    return TEST_PASS;
}

static test_result_t parse_free_integer()
{
    node_t node = parser_parse(&p, "123");
    ASSERT_EQ(NODE_KIND_NUMBER, node.kind);
    ASSERT_EQ(123.0, node.number);

    return TEST_PASS;
}

static test_result_t parse_free_float()
{
    node_t node = parser_parse(&p, "123.5");
    ASSERT_EQ(NODE_KIND_NUMBER, node.kind);
    ASSERT_EQ(123.5, node.number);

    return TEST_PASS;
}

static test_result_t parse_null()
{
    node_t node = parser_parse(&p, "null");
    ASSERT_EQ(NODE_KIND_NULL, node.kind);

    return TEST_PASS;
}

static test_result_t parse_object_with_null()
{
    node_t node = parser_parse(&p, "{ \"a\": null }");
    ASSERT_EQ(NODE_KIND_OBJECT, node.kind);
    ASSERT_EQ(1, node.object.length);
    ASSERT_EQ(NODE_KIND_NULL, node.object.data[0].value.kind);

    return TEST_PASS;
}

static test_result_t parse_object_with_booleans()
{
    node_t node = parser_parse(&p, "{ \"a\": true, \"b\": false }");
    ASSERT_EQ(NODE_KIND_OBJECT, node.kind);
    ASSERT_EQ(true, node.object.data[0].value.boolean);
    ASSERT_EQ(false, node.object.data[1].value.boolean);
    return TEST_PASS;
}

static test_result_t parse_large_thing()
{
    const char *large_thing = "{ \
        \"firstName\": \"John\", \
        \"lastName\": \"Smith\", \
        \"isAlive\": true, \
        \"age\": 27, \
        \"address\": { \
          \"streetAddress\": \"21 2nd Street\", \
          \"city\": \"New York\", \
          \"state\": \"NY\", \
          \"postalCode\": \"10021-3100\" \
        }, \
        \"phoneNumbers\": [ \
          { \
            \"type\": \"home\", \
            \"number\": \"212 555-1234\" \
          }, \
          { \
            \"type\": \"office\", \
            \"number\": \"646 555-4567\" \
          } \
        ], \
        \"children\": [], \
        \"spouse\": null \
} \
                              ";
    node_t node = parser_parse(&p, large_thing);

    ASSERT_EQ(NODE_KIND_OBJECT, node.kind);
    ASSERT_STR_EQ("John", node.object.data[0].value.string.data);
    ASSERT_STR_EQ("646 555-4567", node.object.data[5].value.array.data[1].object.data[1].value.string.data);

    return TEST_PASS;
}

static test_result_t vec_make()
{
    vec_u8_t vec = vec_u8_make();
    ASSERT_EQ(0, vec.length);

    vec_u8_free(&vec);

    return TEST_PASS;
}

static test_result_t vec_push()
{
    vec_u8_t vec = vec_u8_make();
    vec_u8_push(&vec, 42);
    vec_u8_push(&vec, 13);

    ASSERT_EQ(2, vec.length);
    ASSERT_EQ(42, vec.data[0]);
    ASSERT_EQ(13, vec.data[1]);

    vec_u8_free(&vec);

    return TEST_PASS;
}

static test_result_t vec_grow()
{
    vec_u8_t vec = vec_u8_make();
    size_t old_capacity = vec.capacity;

    for (size_t i = 0; i < VECTOR_DEFAULT_CAPACITY + 1; ++i)
    {
        vec_u8_push(&vec, 42);
    }

    ASSERT_NOT_EQ(old_capacity, vec.capacity);

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
    TEST(parse_object_with_multiple_properties);
    TEST(parse_free_integer);
    TEST(parse_free_float);
    TEST(parse_null);
    TEST(parse_object_with_null);
    TEST(parse_object_with_booleans);
    TEST(parse_large_thing);
    TEST(vec_make);
    TEST(vec_push);
    TEST(vec_grow);

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
