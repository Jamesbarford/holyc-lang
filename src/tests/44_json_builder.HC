#include "testhelper.HC"

I32 Main()
{
  "Test - JSON Builder & Serializer:\n";
  I32 tests = 12, correct = 0;
  U8 *str;

  /* AC-1: Empty object -> {} */
  auto obj = JsonCreateObject();
  str = JsonToString(obj);
  if (!StrCmp(str, "{}")) correct++;
  else "\033[0;31mAC-1 FAILED\033[0;0m: got '%s'\n", str;
  Free(str);
  JsonRelease(obj);

  /* AC-2: Empty array -> [] */
  auto arr = JsonCreateArray();
  str = JsonToString(arr);
  if (!StrCmp(str, "[]")) correct++;
  else "\033[0;31mAC-2 FAILED\033[0;0m: got '%s'\n", str;
  Free(str);
  JsonRelease(arr);

  /* AC-3: Object with scalar types (string, int, bool, null) */
  obj = JsonCreateObject();
  JsonObjectSetString(obj, "name", "Alice");
  JsonObjectSetInt(obj, "age", 30);
  JsonObjectSetBool(obj, "active", TRUE);
  JsonObjectSetNull(obj, "data");
  str = JsonToString(obj);
  auto parsed = JsonParse(str);
  auto sel = JsonSelect(parsed, ".name:s");
  if (sel && !StrCmp(sel->str, "Alice")) {
    sel = JsonSelect(parsed, ".age:i");
    if (sel && sel->i64 == 30) {
      sel = JsonSelect(parsed, ".active:b");
      if (sel && sel->boolean == TRUE) {
        sel = JsonSelect(parsed, ".data");
        if (sel && sel->type == JSON_NULL) {
          correct++;
        }
      }
    }
  }
  if (correct < 3) "\033[0;31mAC-3 FAILED\033[0;0m: got '%s'\n", str;
  Free(str);
  JsonRelease(parsed);
  JsonRelease(obj);

  /* AC-4: Array with scalar types */
  arr = JsonCreateArray();
  JsonArrayAddString(arr, "hello");
  JsonArrayAddInt(arr, 42);
  JsonArrayAddBool(arr, FALSE);
  JsonArrayAddNull(arr);
  str = JsonToString(arr);
  parsed = JsonParse(str);
  sel = JsonSelect(parsed, "[0]:s");
  if (sel && !StrCmp(sel->str, "hello")) {
    sel = JsonSelect(parsed, "[1]:i");
    if (sel && sel->i64 == 42) {
      sel = JsonSelect(parsed, "[2]:b");
      if (sel && sel->boolean == FALSE) {
        sel = JsonSelect(parsed, "[3]");
        if (sel && sel->type == JSON_NULL) {
          correct++;
        }
      }
    }
  }
  if (correct < 4) "\033[0;31mAC-4 FAILED\033[0;0m: got '%s'\n", str;
  Free(str);
  JsonRelease(parsed);
  JsonRelease(arr);

  /* AC-5: Nested object in object */
  obj = JsonCreateObject();
  auto inner = JsonCreateObject();
  JsonObjectSetString(inner, "city", "Berlin");
  JsonObjectSetObject(obj, "address", inner);
  str = JsonToString(obj);
  parsed = JsonParse(str);
  sel = JsonSelect(parsed, ".address.city:s");
  if (sel && !StrCmp(sel->str, "Berlin")) correct++;
  else "\033[0;31mAC-5 FAILED\033[0;0m: got '%s'\n", str;
  Free(str);
  JsonRelease(parsed);
  JsonRelease(obj);

  /* AC-6: Array in object, object in array */
  obj = JsonCreateObject();
  arr = JsonCreateArray();
  JsonArrayAddInt(arr, 1);
  JsonArrayAddInt(arr, 2);
  JsonArrayAddInt(arr, 3);
  JsonObjectSetArray(obj, "nums", arr);
  auto arr2 = JsonCreateArray();
  auto item = JsonCreateObject();
  JsonObjectSetString(item, "id", "x");
  JsonArrayAddObject(arr2, item);
  JsonObjectSetArray(obj, "items", arr2);
  str = JsonToString(obj);
  parsed = JsonParse(str);
  sel = JsonSelect(parsed, ".nums[1]:i");
  if (sel && sel->i64 == 2) {
    sel = JsonSelect(parsed, ".items[0].id:s");
    if (sel && !StrCmp(sel->str, "x")) {
      correct++;
    }
  }
  if (correct < 6) "\033[0;31mAC-6 FAILED\033[0;0m: got '%s'\n", str;
  Free(str);
  JsonRelease(parsed);
  JsonRelease(obj);

  /* AC-7: Builder output produces correct string */
  obj = JsonCreateObject();
  JsonObjectSetString(obj, "key", "value");
  JsonObjectSetInt(obj, "num", 99);
  auto builder_str = JsonToString(obj);
  /* Verify the output contains expected content */
  if (strstr(builder_str, "\"key\"") != NULL &&
      strstr(builder_str, "\"value\"") != NULL &&
      strstr(builder_str, "99") != NULL) {
    correct++;
  } else {
    "\033[0;31mAC-7 FAILED\033[0;0m: got '%s'\n", builder_str;
  }
  Free(builder_str);
  JsonRelease(obj);

  /* AC-8/9: Pretty print */
  obj = JsonCreateObject();
  JsonObjectSetString(obj, "name", "Bob");
  JsonObjectSetInt(obj, "age", 25);
  str = JsonToPrettyString(obj, 2);
  /* Check for newlines (formatted output) and indented content */
  if (strchr(str, '\n') != NULL && strstr(str, "name") != NULL) {
    correct++;
  }
  if (correct < 8) "\033[0;31mAC-8/9 FAILED\033[0;0m: got '%s'\n", str;
  Free(str);
  JsonRelease(obj);

  /* AC-10: String escaping */
  obj = JsonCreateObject();
  JsonObjectSetString(obj, "msg", "line1\nline2\ttab");
  str = JsonToString(obj);
  parsed = JsonParse(str);
  sel = JsonSelect(parsed, ".msg:s");
  if (sel && strstr(sel->str, "line1") != NULL) correct++;
  else "\033[0;31mAC-10 FAILED\033[0;0m: got '%s'\n", str;
  Free(str);
  JsonRelease(parsed);
  JsonRelease(obj);

  /* AC-11: Round-trip */
  U8 *original = "{\"a\":1,\"b\":[2,3],\"c\":{\"d\":\"e\"}}";
  parsed = JsonParse(original);
  auto str1 = JsonToString(parsed);
  auto parsed2 = JsonParse(str1);
  auto str2 = JsonToString(parsed2);
  if (!StrCmp(str1, str2)) correct++;
  else "\033[0;31mAC-11 FAILED\033[0;0m: '%s' != '%s'\n", str1, str2;
  Free(str1);
  Free(str2);
  JsonRelease(parsed);
  JsonRelease(parsed2);

  /* AC-12: Pretty-print with nested object */
  obj = JsonCreateObject();
  JsonObjectSetString(obj, "name", "Test");
  auto nested = JsonCreateObject();
  JsonObjectSetInt(nested, "x", 1);
  JsonObjectSetObject(obj, "inner", nested);
  str = JsonToPrettyString(obj, 2);
  if (strchr(str, '\n') != NULL && strstr(str, "name") != NULL) {
    correct++;
  } else {
    "\033[0;31mAC-12 FAILED\033[0;0m\n";
  }
  Free(str);
  JsonRelease(obj);

  /* AC-13: JsonRelease (no crash = pass) */
  obj = JsonCreateObject();
  arr = JsonCreateArray();
  JsonArrayAddString(arr, "test");
  JsonObjectSetArray(obj, "arr", arr);
  inner = JsonCreateObject();
  JsonObjectSetInt(inner, "x", 1);
  JsonObjectSetObject(obj, "nested", inner);
  JsonRelease(obj);
  correct++;

  PrintResult(correct, tests);
  "====\n";
  return 0;
}
