#ifndef JSON_H__
#define JSON_H__

#include <stddef.h>

#include "aostr.h"
#include "arena.h"
#include "types.h"

/* Small hand-rolled JSON reader for the LSP (see lsp.c). Scoped to
 * what LSP traffic actually contains: objects, arrays, strings with
 * full escape/\uXXXX handling, 64-bit ints, doubles, bools, null. No
 * comments, no NaN/Inf. Every node and string is allocated from the
 * caller's Arena, so an entire message tree is freed by clearing the
 * arena - there is no per-node release.
 *
 * Emission needs no counterpart: responses are built with
 * aoStrCatPrintf plus jsonEscapeInto for string payloads. */

typedef enum JsonKind {
    JSON_NULL,
    JSON_BOOL,
    JSON_INT,
    JSON_FLOAT,
    JSON_STRING,
    JSON_ARRAY,
    JSON_OBJECT,
} JsonKind;

typedef struct Json Json;
typedef struct JsonPair JsonPair;

/* Object member or array element; arena-allocated singly linked
 * chain. `key` is NULL for array elements. */
struct JsonPair {
    char *key;
    Json *value;
    JsonPair *next;
};

struct Json {
    JsonKind kind;
    union {
        int boolean;
        s64 i;
        double f;
        struct {
            char *str; /* unescaped, NUL-terminated, arena-owned */
            u32 str_len;
        };
        struct {
            JsonPair *head; /* object members / array elements */
            u32 size;
        };
    } as;
};

/* Parse `len` bytes of JSON. Returns NULL on malformed input, with
 * *err (when non-NULL) pointing at a static description. */
Json *jsonParse(Arena *arena, const char *s, size_t len, const char **err);

/* NULL when `obj` isn't an object or lacks `key`. Linear scan - LSP
 * objects are small. */
Json *jsonObjGet(Json *obj, const char *key);

/* NULL when `arr` isn't an array or idx is out of range. */
Json *jsonArrayAt(Json *arr, u32 idx);

/* Typed conveniences over jsonObjGet; the default is returned on any
 * shape mismatch. The string is arena-owned - copy it to keep it
 * beyond the message. */
const char *jsonStrOr(Json *obj, const char *key, const char *dflt);
s64 jsonIntOr(Json *obj, const char *key, s64 dflt);
int jsonBoolOr(Json *obj, const char *key, int dflt);

/* Append `len` bytes of `s` onto `buf`, JSON-escaped (quotes NOT
 * added by this helper). */
void jsonEscapeInto(AoStr *buf, const char *s, size_t len);

#endif
