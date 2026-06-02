/* These are kind of hacky and should be phased out but this asm module and
 * holyc CANNOT share code */
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "tasm_util.h"

void
die(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    fputs("hcc: ", stderr);
    vfprintf(stderr, fmt, ap);
    fputc('\n', stderr);
    va_end(ap);
    exit(1);
}

void *
xmalloc(size_t n)
{
    void *p = malloc(n);
    if (!p && n) die("out of memory (malloc %zu)", n);
    return p;
}

void *
xcalloc(size_t n, size_t sz)
{
    void *p = calloc(n ? n : 1, sz ? sz : 1);
    if (!p) die("out of memory (calloc %zu*%zu)", n, sz);
    return p;
}

void *
xrealloc(void *p, size_t n)
{
    void *q = realloc(p, n);
    if (!q && n) die("out of memory (realloc %zu)", n);
    return q;
}

char *
xstrdup(const char *s)
{
    size_t n = strlen(s);
    char *p = xmalloc(n + 1);
    memcpy(p, s, n + 1);
    return p;
}

char *
xstrndup(const char *s, size_t n)
{
    char *p = xmalloc(n + 1);
    memcpy(p, s, n);
    p[n] = '\0';
    return p;
}

char *
slurp_file(const char *path, size_t *out_len)
{
    FILE *f = fopen(path, "rb");
    if (!f) die("cannot open %s: %s", path, strerror(errno));
    if (fseek(f, 0, SEEK_END) != 0) die("fseek %s: %s", path, strerror(errno));
    long sz = ftell(f);
    if (sz < 0) die("ftell %s: %s", path, strerror(errno));
    rewind(f);
    char *buf = xmalloc((size_t)sz + 1);
    size_t got = fread(buf, 1, (size_t)sz, f);
    fclose(f);
    if (got != (size_t)sz) die("short read on %s", path);
    buf[sz] = '\0';
    if (out_len) *out_len = (size_t)sz;
    return buf;
}

char *
path_join(const char *dir, const char *name)
{
    if (!dir || !*dir) return xstrdup(name);
    if (name[0] == '/') return xstrdup(name);
    size_t a = strlen(dir), b = strlen(name);
    int needs_slash = (a > 0 && dir[a - 1] != '/');
    char *r = xmalloc(a + b + 2);
    memcpy(r, dir, a);
    if (needs_slash) r[a++] = '/';
    memcpy(r + a, name, b + 1);
    return r;
}

char *
path_dir(const char *path)
{
    const char *slash = strrchr(path, '/');
    if (!slash) return xstrdup(".");
    if (slash == path) return xstrdup("/");
    return xstrndup(path, (size_t)(slash - path));
}
