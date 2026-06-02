#ifndef HCC_UTIL_H
#define HCC_UTIL_H

#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#define UNUSED(x) ((void)(x))

void *xmalloc(size_t n);
void *xcalloc(size_t n, size_t sz);
void *xrealloc(void *p, size_t n);
char *xstrdup(const char *s);
char *xstrndup(const char *s, size_t n);

/* Read entire file. Returns owned buffer with trailing NULL.
 * *out_len excludes NULL. */
char *slurp_file(const char *path, size_t *_out_len);

/* Path helpers. All return owned strings. */
char *path_join(const char *dir, const char *name);
char *path_dir(const char *path);

#endif
