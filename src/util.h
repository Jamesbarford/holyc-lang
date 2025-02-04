#ifndef UTIL_H
#define UTIL_H

#include <stdio.h>
#include <stdlib.h>

#define static_size(x) (sizeof((x)) / sizeof((x[0])))
#define cast(type, value) (((type)(value)))
#define str_lit(s) s,sizeof(s)-1

#define ESC_GREEN  "\033[0;32m"
#define ESC_BLACK  "\033[0;30m"
#define ESC_RED    "\033[0;31m"
#define ESC_BOLD_RED "\033[1;91m"
#define ESC_GREEN  "\033[0;32m"
#define ESC_YELLOW "\033[0;33m"
#define ESC_BOLD_YELLOW "\033[1;33m"
#define ESC_BLUE   "\033[0;34m"
#define ESC_PURPLE "\x1b[38;5;171m"
#define ESC_CYAN   "\033[0;36m"
#define ESC_BOLD_CYAN   "\033[1;36m"
#define ESC_WHITE  "\033[0;37m"
#define ESC_RESET  "\033[0m"
#define ESC_BOLD   "\x1b[1m"
#define ESC_CLEAR_BOLD "\x1b[0m"

#define min(x,y) (((x) < (y) ? (x) : (y)))

extern int is_terminal;

#ifndef DEBUG
#define loggerDebug(...)

#define loggerPanic(...)                             \
    do {                                             \
        fprintf(stderr, "\033[0;31mERROR: \033[0m"); \
        fprintf(stderr, __VA_ARGS__);                \
        exit(EXIT_FAILURE);                          \
    } while (0)

#define loggerWarning(...)                             \
    do {                                               \
        fprintf(stderr, "\033[0;33mWARNING: \033[0m"); \
        fprintf(stderr, __VA_ARGS__);                  \
    } while (0)

#else
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

#endif


#endif // !UTIL_H
