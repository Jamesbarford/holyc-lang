#ifndef UTIL_H
#define UTIL_H

#include <stdio.h>
#include <stdlib.h>

#define static_size(x) (sizeof((x)) / sizeof((x[0])))

#define loggerDebug(...)                                             \
    do {                                                             \
        fprintf(stdout, "%s:%d:%s\t", __FILE__, __LINE__, __func__); \
        fprintf(stdout, __VA_ARGS__);                                \
    } while (0)

#define loggerPanic(...)                                                   \
    do {                                                                   \
        fprintf(stderr, "\033[0;31m%s:%d:%s\t\033[0m", __FILE__, __LINE__, \
                __func__);                                                 \
        fprintf(stderr, __VA_ARGS__);                                      \
        exit(EXIT_FAILURE);                                                \
    } while (0)

#define loggerWarning(...)                                                 \
    do {                                                                   \
        fprintf(stderr, "\033[0;35m%s:%d:%s\t\033[0m", __FILE__, __LINE__, \
                __func__);                                                 \
        fprintf(stderr, __VA_ARGS__);                                      \
    } while (0)

#endif // !UTIL_H
