#include <string.h>

typedef enum
{
    TEST_PASS,
    TEST_FAIL,
} test_result_t;

typedef test_result_t (*test_fn_t)(void);

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

static test_suite_t SUITE = {
    .length = 0,
};

#define TEST(NAME)         \
    test_t test_##NAME = { \
        .fn = &NAME,       \
        .name = #NAME,     \
    };                     \
    SUITE.tests[SUITE.length++] = test_##NAME;

#define ASSERT_EQ(EXPECTED, ACTUAL) \
    if (EXPECTED != ACTUAL)         \
    {                               \
        return TEST_FAIL;           \
    }

#define ASSERT_STR_EQ(EXPECTED, ACTUAL) \
    if (strcmp(EXPECTED, ACTUAL) != 0)  \
    {                                   \
        return TEST_FAIL;               \
    }
