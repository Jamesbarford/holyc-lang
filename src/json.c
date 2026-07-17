/* Hand-rolled JSON reader - see json.h for scope and the arena
 * ownership model. Recursive descent with a depth cap so hostile
 * nesting can't blow the C stack. */

#include <string.h>
#include <stdlib.h>

#include "aostr.h"
#include "arena.h"
#include "json.h"

#define JSON_MAX_DEPTH 128

typedef struct JsonParser {
    const char *s;
    size_t len;
    size_t pos;
    Arena *arena;
    const char *err;
    int depth;
} JsonParser;

static Json *jsonValue(JsonParser *p);

static int jsonAtEnd(JsonParser *p) { return p->pos >= p->len; }
static char jsonPeek(JsonParser *p) {
    return jsonAtEnd(p) ? '\0' : p->s[p->pos];
}

static void jsonSkipWs(JsonParser *p) {
    while (!jsonAtEnd(p)) {
        char c = p->s[p->pos];
        if (c == ' ' || c == '\t' || c == '\n' || c == '\r') p->pos++;
        else break;
    }
}

static Json *jsonNode(JsonParser *p, JsonKind kind) {
    Json *j = (Json *)arenaAlloc(p->arena, sizeof(Json));
    memset(j, 0, sizeof(*j));
    j->kind = kind;
    return j;
}

static int jsonLiteral(JsonParser *p, const char *lit) {
    size_t n = strlen(lit);
    if (p->pos + n > p->len) return 0;
    if (memcmp(p->s + p->pos, lit, n) != 0) return 0;
    p->pos += n;
    return 1;
}

static int jsonHex4(JsonParser *p, u32 *out) {
    if (p->pos + 4 > p->len) return 0;
    u32 v = 0;
    for (int i = 0; i < 4; ++i) {
        char c = p->s[p->pos + i];
        v <<= 4;
        if      (c >= '0' && c <= '9') v |= (u32)(c - '0');
        else if (c >= 'a' && c <= 'f') v |= (u32)(c - 'a' + 10);
        else if (c >= 'A' && c <= 'F') v |= (u32)(c - 'A' + 10);
        else return 0;
    }
    p->pos += 4;
    *out = v;
    return 1;
}

static void jsonUtf8Encode(char *dst, u32 cp, u32 *n) {
    if (cp < 0x80) {
        dst[(*n)++] = (char)cp;
    } else if (cp < 0x800) {
        dst[(*n)++] = (char)(0xC0 | (cp >> 6));
        dst[(*n)++] = (char)(0x80 | (cp & 0x3F));
    } else if (cp < 0x10000) {
        dst[(*n)++] = (char)(0xE0 | (cp >> 12));
        dst[(*n)++] = (char)(0x80 | ((cp >> 6) & 0x3F));
        dst[(*n)++] = (char)(0x80 | (cp & 0x3F));
    } else {
        dst[(*n)++] = (char)(0xF0 | (cp >> 18));
        dst[(*n)++] = (char)(0x80 | ((cp >> 12) & 0x3F));
        dst[(*n)++] = (char)(0x80 | ((cp >> 6) & 0x3F));
        dst[(*n)++] = (char)(0x80 | (cp & 0x3F));
    }
}

/* Consumes the opening quote onward. The unescaped form is never
 * longer than the raw span (\uXXXX: 6 bytes -> at most 4), so one
 * arena allocation of the raw length always fits. */
static int jsonString(JsonParser *p, char **out, u32 *out_len) {
    p->pos++; /* '"' */
    size_t start = p->pos;
    /* Find the closing quote to size the buffer, honouring escape
     * parity (`\\"` closes, `\"` doesn't). */
    size_t end = start;
    while (end < p->len && p->s[end] != '"') {
        if (p->s[end] == '\\') end++; /* skip the escaped byte */
        end++;
    }
    if (end >= p->len) {
        p->err = "unterminated string";
        return 0;
    }

    char *buf = (char *)arenaAlloc(p->arena, (u32)(end - start) + 1);
    u32 n = 0;
    while (!jsonAtEnd(p)) {
        char c = p->s[p->pos];
        if (c == '"') {
            p->pos++;
            buf[n] = '\0';
            *out = buf;
            *out_len = n;
            return 1;
        }
        if (c != '\\') {
            buf[n++] = c;
            p->pos++;
            continue;
        }
        p->pos++; /* '\' */
        if (jsonAtEnd(p)) break;
        char e = p->s[p->pos++];
        switch (e) {
            case '"':  buf[n++] = '"';  break;
            case '\\': buf[n++] = '\\'; break;
            case '/':  buf[n++] = '/';  break;
            case 'b':  buf[n++] = '\b'; break;
            case 'f':  buf[n++] = '\f'; break;
            case 'n':  buf[n++] = '\n'; break;
            case 'r':  buf[n++] = '\r'; break;
            case 't':  buf[n++] = '\t'; break;
            case 'u': {
                u32 cp;
                if (!jsonHex4(p, &cp)) {
                    p->err = "bad \\u escape";
                    return 0;
                }
                /* Surrogate pair: high followed by \uDC00-\uDFFF. */
                if (cp >= 0xD800 && cp <= 0xDBFF &&
                    p->pos + 6 <= p->len &&
                    p->s[p->pos] == '\\' && p->s[p->pos + 1] == 'u')
                {
                    p->pos += 2;
                    u32 lo;
                    if (!jsonHex4(p, &lo) || lo < 0xDC00 || lo > 0xDFFF) {
                        p->err = "bad surrogate pair";
                        return 0;
                    }
                    cp = 0x10000 + ((cp - 0xD800) << 10) + (lo - 0xDC00);
                }
                jsonUtf8Encode(buf, cp, &n);
                break;
            }
            default:
                p->err = "bad escape";
                return 0;
        }
    }
    p->err = "unterminated string";
    return 0;
}

static Json *jsonNumber(JsonParser *p) {
    size_t start = p->pos;
    int is_float = 0;
    if (jsonPeek(p) == '-') p->pos++;
    while (!jsonAtEnd(p)) {
        char c = p->s[p->pos];
        if (c >= '0' && c <= '9') {
            p->pos++;
            continue;
        }
        if (c == '.' || c == 'e' || c == 'E' || c == '+' || c == '-') {
            is_float = 1;
            p->pos++;
            continue;
        }
        break;
    }
    if (p->pos == start || (p->pos == start + 1 && p->s[start] == '-')) {
        p->err = "bad number";
        return NULL;
    }
    /* strtoll/strtod stop at the first non-numeric byte, so pointing
     * them at the slice start (NUL-free) is safe. */
    Json *j;
    if (is_float) {
        j = jsonNode(p, JSON_FLOAT);
        j->as.f = strtod(p->s + start, NULL);
    } else {
        j = jsonNode(p, JSON_INT);
        j->as.i = (s64)strtoll(p->s + start, NULL, 10);
    }
    return j;
}

static Json *jsonArray(JsonParser *p) {
    p->pos++; /* '[' */
    Json *j = jsonNode(p, JSON_ARRAY);
    JsonPair *tail = NULL;
    jsonSkipWs(p);
    if (jsonPeek(p) == ']') {
        p->pos++;
        return j;
    }
    for (;;) {
        Json *v = jsonValue(p);
        if (v == NULL) return NULL;
        JsonPair *pair = (JsonPair *)arenaAlloc(p->arena, sizeof(JsonPair));
        pair->key = NULL;
        pair->value = v;
        pair->next = NULL;
        if (tail) tail->next = pair;
        else      j->as.head = pair;
        tail = pair;
        j->as.size++;

        jsonSkipWs(p);
        char c = jsonPeek(p);
        if (c == ',') { p->pos++; continue; }
        if (c == ']') { p->pos++; return j; }
        p->err = "expected ',' or ']'";
        return NULL;
    }
}

static Json *jsonObject(JsonParser *p) {
    p->pos++; /* '{' */
    Json *j = jsonNode(p, JSON_OBJECT);
    JsonPair *tail = NULL;
    jsonSkipWs(p);
    if (jsonPeek(p) == '}') {
        p->pos++;
        return j;
    }
    for (;;) {
        jsonSkipWs(p);
        if (jsonPeek(p) != '"') {
            p->err = "expected object key";
            return NULL;
        }
        char *key;
        u32 key_len;
        if (!jsonString(p, &key, &key_len)) return NULL;
        jsonSkipWs(p);
        if (jsonPeek(p) != ':') {
            p->err = "expected ':'";
            return NULL;
        }
        p->pos++;
        Json *v = jsonValue(p);
        if (v == NULL) return NULL;

        JsonPair *pair = (JsonPair *)arenaAlloc(p->arena, sizeof(JsonPair));
        pair->key = key;
        pair->value = v;
        pair->next = NULL;
        if (tail) tail->next = pair;
        else      j->as.head = pair;
        tail = pair;
        j->as.size++;

        jsonSkipWs(p);
        char c = jsonPeek(p);
        if (c == ',') { p->pos++; continue; }
        if (c == '}') { p->pos++; return j; }
        p->err = "expected ',' or '}'";
        return NULL;
    }
}

static Json *jsonValue(JsonParser *p) {
    if (++p->depth > JSON_MAX_DEPTH) {
        p->err = "nesting too deep";
        return NULL;
    }
    jsonSkipWs(p);
    Json *j = NULL;
    char c = jsonPeek(p);
    if (c == '{') {
        j = jsonObject(p);
    } else if (c == '[') {
        j = jsonArray(p);
    } else if (c == '"') {
        char *s;
        u32 n;
        if (jsonString(p, &s, &n)) {
            j = jsonNode(p, JSON_STRING);
            j->as.str = s;
            j->as.str_len = n;
        }
    } else if (c == 't') {
        if (jsonLiteral(p, "true")) {
            j = jsonNode(p, JSON_BOOL);
            j->as.boolean = 1;
        } else p->err = "bad literal";
    } else if (c == 'f') {
        if (jsonLiteral(p, "false")) {
            j = jsonNode(p, JSON_BOOL);
            j->as.boolean = 0;
        } else p->err = "bad literal";
    } else if (c == 'n') {
        if (jsonLiteral(p, "null")) j = jsonNode(p, JSON_NULL);
        else p->err = "bad literal";
    } else if (c == '-' || (c >= '0' && c <= '9')) {
        j = jsonNumber(p);
    } else {
        p->err = jsonAtEnd(p) ? "unexpected end of input"
                              : "unexpected character";
    }
    p->depth--;
    return j;
}

Json *jsonParse(Arena *arena, const char *s, size_t len, const char **err) {
    JsonParser p = { .s = s, .len = len, .pos = 0, .arena = arena,
                     .err = NULL, .depth = 0 };
    Json *j = jsonValue(&p);
    if (j) {
        jsonSkipWs(&p);
        if (!jsonAtEnd(&p)) {
            p.err = "trailing bytes after value";
            j = NULL;
        }
    }
    if (err) *err = p.err;
    return j;
}

Json *jsonObjGet(Json *obj, const char *key) {
    if (obj == NULL || obj->kind != JSON_OBJECT) return NULL;
    for (JsonPair *p = obj->as.head; p; p = p->next) {
        if (p->key && strcmp(p->key, key) == 0) return p->value;
    }
    return NULL;
}

Json *jsonArrayAt(Json *arr, u32 idx) {
    if (arr == NULL || arr->kind != JSON_ARRAY || idx >= arr->as.size)
        return NULL;
    JsonPair *p = arr->as.head;
    while (idx--) p = p->next;
    return p->value;
}

const char *jsonStrOr(Json *obj, const char *key, const char *dflt) {
    Json *v = jsonObjGet(obj, key);
    return (v && v->kind == JSON_STRING) ? v->as.str : dflt;
}

s64 jsonIntOr(Json *obj, const char *key, s64 dflt) {
    Json *v = jsonObjGet(obj, key);
    return (v && v->kind == JSON_INT) ? v->as.i : dflt;
}

int jsonBoolOr(Json *obj, const char *key, int dflt) {
    Json *v = jsonObjGet(obj, key);
    return (v && v->kind == JSON_BOOL) ? v->as.boolean : dflt;
}

void jsonEscapeInto(AoStr *buf, const char *s, size_t len) {
    for (size_t i = 0; i < len; ++i) {
        unsigned char c = (unsigned char)s[i];
        switch (c) {
            case '"':  aoStrCatLen(buf, "\\\"", 2); break;
            case '\\': aoStrCatLen(buf, "\\\\", 2); break;
            case '\b': aoStrCatLen(buf, "\\b", 2);  break;
            case '\f': aoStrCatLen(buf, "\\f", 2);  break;
            case '\n': aoStrCatLen(buf, "\\n", 2);  break;
            case '\r': aoStrCatLen(buf, "\\r", 2);  break;
            case '\t': aoStrCatLen(buf, "\\t", 2);  break;
            default:
                if (c < 0x20) aoStrCatPrintf(buf, "\\u%04x", c);
                else          aoStrPutChar(buf, (char)c);
        }
    }
}
