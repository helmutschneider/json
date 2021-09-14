#ifndef VECTOR_H
#define VECTOR_H

#include <stdlib.h>
#include <stdint.h>

#define VECTOR_DEFAULT_CAPACITY 16

#define VECTOR_DECL(TYPE_NAME, TYPE_ARG) \
    typedef struct TYPE_NAME             \
    {                                    \
        size_t capacity;                 \
        size_t length;                   \
        TYPE_ARG *data;                  \
    } TYPE_NAME##_t;                     \
                                         \
    struct TYPE_NAME TYPE_NAME##_make(); \
                                         \
    void TYPE_NAME##_push(struct TYPE_NAME *self, TYPE_ARG item);

#define VECTOR_IMPL(TYPE_NAME, TYPE_ARG)                                        \
    struct TYPE_NAME TYPE_NAME##_make()                                         \
    {                                                                           \
        size_t capacity = VECTOR_DEFAULT_CAPACITY;                              \
        struct TYPE_NAME thing = {                                              \
            .capacity = capacity,                                               \
            .length = 0,                                                        \
            .data = malloc(sizeof(TYPE_ARG) * capacity),                        \
        };                                                                      \
        return thing;                                                           \
    }                                                                           \
                                                                                \
    void TYPE_NAME##_push(struct TYPE_NAME *self, TYPE_ARG item)                \
    {                                                                           \
        if (self->capacity == self->length)                                     \
        {                                                                       \
            size_t next_capacity = self->capacity * 2;                          \
            self->data = realloc(self->data, sizeof(TYPE_ARG) * next_capacity); \
            self->capacity = next_capacity;                                     \
        }                                                                       \
        self->data[self->length++] = item;                                      \
    }

#endif
