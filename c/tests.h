#ifndef TESTS_H
#define TESTS_H

#include <string.h>
#include "vector.h"

typedef enum
{
    TEST_PASS,
    TEST_FAIL,
} test_result_t;

typedef struct
{
    const test_result_t result;
    const char *message;
} test_output_t;

typedef test_output_t (*test_fn_t)(void);

typedef struct
{
    test_fn_t fn;
    const char *name;
} test_t;

typedef struct
{
    size_t length;
    test_t tests[64];
} test_suite_t;

static const test_output_t TEST_OUTPUT_PASS = {
    .result = TEST_PASS,
    .message = "",
};

#define FAIL(MESSAGE)        \
    test_output_t out = {    \
        .result = TEST_FAIL, \
        .message = #MESSAGE, \
    };                       \
    return out;

#define PASS() return TEST_OUTPUT_PASS

#define ADD_TEST(SUITE, NAME) \
    test_t test_##NAME = {    \
        .fn = &NAME,          \
        .name = #NAME,        \
    };                        \
    SUITE.tests[SUITE.length++] = test_##NAME;

#define ASSERT_EQ(EXPECTED, ACTUAL)                          \
    if (EXPECTED != ACTUAL)                                  \
    {                                                        \
        FAIL("Assertion failed: " #EXPECTED " == " #ACTUAL); \
    }

#define ASSERT_NOT_EQ(EXPECTED, ACTUAL)                      \
    if (EXPECTED == ACTUAL)                                  \
    {                                                        \
        FAIL("Assertion failed: " #EXPECTED " != " #ACTUAL); \
    }

#define ASSERT_STR_EQ(EXPECTED, ACTUAL)                      \
    if (strcmp(EXPECTED, ACTUAL) != 0)                       \
    {                                                        \
        FAIL("Assertion failed: " #EXPECTED " == " #ACTUAL); \
    }

#define RUN_TESTS(SUITE)                                                         \
    uint64_t res = 0;                                                            \
    for (size_t i = 0; i < SUITE.length; ++i)                                    \
    {                                                                            \
        test_t test = SUITE.tests[i];                                            \
        test_output_t out = test.fn();                                           \
        printf("[%s] %s\n", out.result == TEST_PASS ? "OK" : "FAIL", test.name); \
        if (out.result == TEST_FAIL)                                             \
        {                                                                        \
            printf("  %s\n", out.message);                                       \
        }                                                                        \
        res += (out.result == TEST_FAIL ? 1 : 0);                                \
    }                                                                            \
    return res;

VECTOR_DECL(vec_u8, uint8_t)

#endif
