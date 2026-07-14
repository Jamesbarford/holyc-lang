/* Unit tests for the hand-rolled JSON reader (json.c). Standalone:
 *   cc -o json-test json-test.c json.c aostr.c arena.c memory.c mempool.c \
 *      containers.c list.c util.c  (see the build line in the repo Makefile
 *   comments if this drifts)
 * Prints one PASSED/FAILED line per case and exits non-zero on failure. */

#include <stdio.h>
#include <string.h>

#include "arena.h"
#include "aostr.h"
#include "json.h"

/* aostr.c/containers.c reference these from the wider compiler; stub
 * them so the test links standalone. */
int is_terminal = 0;
void *astLValueToAoStr(void *ast, unsigned long flags) {
    (void)ast; (void)flags;
    return NULL;
}

static int g_failures = 0;

static void check(int ok, const char *name) {
    printf("%s: %s\n", ok ? "PASSED" : "FAILED", name);
    if (!ok) g_failures++;
}

static Json *parse(Arena *a, const char *s) {
    return jsonParse(a, s, strlen(s), NULL);
}

int main(void) {
    Arena *a = arenaNew(1 << 16);

    /* scalars */
    Json *j = parse(a, "42");
    check(j && j->kind == JSON_INT && j->as.i == 42, "int");
    j = parse(a, "-7");
    check(j && j->kind == JSON_INT && j->as.i == -7, "negative int");
    j = parse(a, "3.5");
    check(j && j->kind == JSON_FLOAT && j->as.f == 3.5, "float");
    j = parse(a, "2e3");
    check(j && j->kind == JSON_FLOAT && j->as.f == 2000.0, "exponent");
    j = parse(a, "true");
    check(j && j->kind == JSON_BOOL && j->as.boolean == 1, "true");
    j = parse(a, "false");
    check(j && j->kind == JSON_BOOL && j->as.boolean == 0, "false");
    j = parse(a, "null");
    check(j && j->kind == JSON_NULL, "null");

    /* strings + escapes */
    j = parse(a, "\"hi\"");
    check(j && j->kind == JSON_STRING && strcmp(j->as.str, "hi") == 0,
          "plain string");
    j = parse(a, "\"a\\\"b\\\\c\\nd\\te\"");
    check(j && strcmp(j->as.str, "a\"b\\c\nd\te") == 0, "escapes");
    j = parse(a, "\"\\u0041\\u00e9\"");
    check(j && strcmp(j->as.str, "A\xc3\xa9") == 0, "unicode escapes");
    j = parse(a, "\"\\ud83d\\ude00\"");
    check(j && strcmp(j->as.str, "\xf0\x9f\x98\x80") == 0,
          "surrogate pair (emoji)");
    j = parse(a, "\"tail\\\\\"");
    check(j && strcmp(j->as.str, "tail\\") == 0,
          "backslash before closing quote");

    /* containers */
    j = parse(a, "[1, 2, 3]");
    check(j && j->kind == JSON_ARRAY && j->as.size == 3 &&
          jsonArrayAt(j, 2) && jsonArrayAt(j, 2)->as.i == 3, "array");
    j = parse(a, "[]");
    check(j && j->kind == JSON_ARRAY && j->as.size == 0, "empty array");
    j = parse(a, "{}");
    check(j && j->kind == JSON_OBJECT && j->as.size == 0, "empty object");
    j = parse(a, "{\"a\": 1, \"b\": \"x\"}");
    check(j && jsonIntOr(j, "a", -1) == 1 &&
          strcmp(jsonStrOr(j, "b", "?"), "x") == 0, "object accessors");
    check(jsonObjGet(j, "missing") == NULL, "missing key");

    /* an LSP-shaped message */
    const char *msg =
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"textDocument/didOpen\","
        "\"params\":{\"textDocument\":{\"uri\":\"file:///a.HC\","
        "\"languageId\":\"holyc\",\"version\":1,\"text\":\"U0 Main(){}\\n\"}}}";
    j = parse(a, msg);
    Json *td = jsonObjGet(jsonObjGet(j, "params"), "textDocument");
    check(j && strcmp(jsonStrOr(j, "method", "?"),
                      "textDocument/didOpen") == 0 &&
          strcmp(jsonStrOr(td, "uri", "?"), "file:///a.HC") == 0 &&
          strcmp(jsonStrOr(td, "text", "?"), "U0 Main(){}\n") == 0 &&
          jsonIntOr(j, "id", -1) == 1,
          "LSP didOpen shape");

    /* malformed input must fail, not crash */
    const char *bad[] = {
        "", "{", "[1,", "{\"a\"}", "{\"a\":}", "\"unterminated",
        "tru", "01x", "{\"a\":1}garbage", "\"\\u12g4\"", "-",
    };
    int all_bad_rejected = 1;
    for (size_t i = 0; i < sizeof(bad) / sizeof(bad[0]); ++i) {
        if (parse(a, bad[i]) != NULL) {
            printf("  accepted malformed: %s\n", bad[i]);
            all_bad_rejected = 0;
        }
    }
    check(all_bad_rejected, "malformed inputs rejected");

    /* deep nesting must be refused, not overflow the stack */
    AoStr *deep = aoStrNew();
    for (int i = 0; i < 4096; ++i) aoStrPutChar(deep, '[');
    check(jsonParse(a, deep->data, deep->len, NULL) == NULL,
          "hostile nesting refused");
    aoStrRelease(deep);

    /* escaping round-trip */
    AoStr *esc = aoStrNew();
    jsonEscapeInto(esc, "a\"b\\c\nd\x01", 8);
    check(strcmp(esc->data, "a\\\"b\\\\c\\nd\\u0001") == 0, "jsonEscapeInto");
    aoStrRelease(esc);

    arenaRelease(a);
    printf("%s\n", g_failures ? "FAILURES" : "ALL PASSED");
    return g_failures ? 1 : 0;
}
