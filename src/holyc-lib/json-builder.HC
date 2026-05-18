/* JSON Builder API
 * Programmatic construction of JSON objects and arrays.
 * Uses the same Json data model as json.HC. */

static U8 *JsonSafeKey(U8 *key)
{
  if (key) return StrNew(key);
  return StrNew("");
}

static U8 *JsonSafeStr(U8 *value)
{
  if (value) return StrNew(value);
  return StrNew("");
}

static Json *JsonBuilderAppendToObject(Json *parent, Json *node)
{
  if (parent->object == NULL) {
    parent->object = node;
  } else {
    auto cur = parent->object;
    while (cur->next) {
      cur = cur->next;
    }
    cur->next = node;
  }
  return node;
}

static Json *JsonBuilderAppendToArray(Json *parent, Json *node)
{
  if (parent->array == NULL) {
    parent->array = node;
  } else {
    auto cur = parent->array;
    while (cur->next) {
      cur = cur->next;
    }
    cur->next = node;
  }
  return node;
}

/* ---- Constructors ---- */

public Json *JsonCreateObject()
{
  Json *j = JsonNew();
  j->type = JSON_OBJECT;
  j->object = NULL;
  return j;
}

public Json *JsonCreateArray()
{
  Json *j = JsonNew();
  j->type = JSON_ARRAY;
  j->array = NULL;
  return j;
}

/* ---- Object Setters ---- */

public Json *JsonObjectSetString(Json *obj, U8 *key, U8 *value)
{
  if (!obj || obj->type != JSON_OBJECT) return NULL;
  Json *node = JsonNew();
  node->type = JSON_STRING;
  node->key = JsonSafeKey(key);
  node->str = JsonSafeStr(value);
  return JsonBuilderAppendToObject(obj, node);
}

public Json *JsonObjectSetInt(Json *obj, U8 *key, I64 value)
{
  if (!obj || obj->type != JSON_OBJECT) return NULL;
  Json *node = JsonNew();
  node->type = JSON_INT;
  node->key = JsonSafeKey(key);
  node->i64 = value;
  return JsonBuilderAppendToObject(obj, node);
}

public Json *JsonObjectSetBool(Json *obj, U8 *key, Bool value)
{
  if (!obj || obj->type != JSON_OBJECT) return NULL;
  Json *node = JsonNew();
  node->type = JSON_BOOL;
  node->key = JsonSafeKey(key);
  node->boolean = value;
  return JsonBuilderAppendToObject(obj, node);
}

public Json *JsonObjectSetNull(Json *obj, U8 *key)
{
  if (!obj || obj->type != JSON_OBJECT) return NULL;
  Json *node = JsonNew();
  node->type = JSON_NULL;
  node->key = JsonSafeKey(key);
  return JsonBuilderAppendToObject(obj, node);
}

public Json *JsonObjectSetObject(Json *obj, U8 *key, Json *child)
{
  if (!obj || obj->type != JSON_OBJECT || !child) return NULL;
  child->key = JsonSafeKey(key);
  return JsonBuilderAppendToObject(obj, child);
}

public Json *JsonObjectSetArray(Json *obj, U8 *key, Json *child)
{
  if (!obj || obj->type != JSON_OBJECT || !child) return NULL;
  child->key = JsonSafeKey(key);
  return JsonBuilderAppendToObject(obj, child);
}

/* ---- Array Adders ---- */

public Json *JsonArrayAddString(Json *arr, U8 *value)
{
  if (!arr || arr->type != JSON_ARRAY) return NULL;
  Json *node = JsonNew();
  node->type = JSON_STRING;
  node->str = JsonSafeStr(value);
  return JsonBuilderAppendToArray(arr, node);
}

public Json *JsonArrayAddInt(Json *arr, I64 value)
{
  if (!arr || arr->type != JSON_ARRAY) return NULL;
  Json *node = JsonNew();
  node->type = JSON_INT;
  node->i64 = value;
  return JsonBuilderAppendToArray(arr, node);
}

public Json *JsonArrayAddBool(Json *arr, Bool value)
{
  if (!arr || arr->type != JSON_ARRAY) return NULL;
  Json *node = JsonNew();
  node->type = JSON_BOOL;
  node->boolean = value;
  return JsonBuilderAppendToArray(arr, node);
}

public Json *JsonArrayAddNull(Json *arr)
{
  if (!arr || arr->type != JSON_ARRAY) return NULL;
  Json *node = JsonNew();
  node->type = JSON_NULL;
  return JsonBuilderAppendToArray(arr, node);
}

public Json *JsonArrayAddObject(Json *arr, Json *child)
{
  if (!arr || arr->type != JSON_ARRAY || !child) return NULL;
  return JsonBuilderAppendToArray(arr, child);
}

public Json *JsonArrayAddArray(Json *arr, Json *child)
{
  if (!arr || arr->type != JSON_ARRAY || !child) return NULL;
  return JsonBuilderAppendToArray(arr, child);
}

/* ---- Pretty Print ---- */

static I64 _pp_indent;
static I64 _pp_depth;
static JsonStringBuilder *_pp_sb;

static U0 JsonPPIndent()
{
  I64 i, j;
  for (i = 0; i < _pp_depth; i++) {
    for (j = 0; j < _pp_indent; j++) {
      _pp_sb->buf = CatLenPrint(_pp_sb->buf, &_pp_sb->len, " ");
    }
  }
}

static U0 JsonPPInternal(Json *J)
{
  if (J == NULL) return;
  I64 saved = _pp_depth;

  while (J) {
    JsonPPIndent();
    if (J->key) _pp_sb->buf = CatLenPrint(_pp_sb->buf, &_pp_sb->len, "\"%Q\": ", J->key);

    if (J->type == JSON_INT) {
      _pp_sb->buf = CatLenPrint(_pp_sb->buf, &_pp_sb->len, "%d", J->i64);
    } else if (J->type == JSON_STRNUM) {
      _pp_sb->buf = CatLenPrint(_pp_sb->buf, &_pp_sb->len, "%s", J->strnum);
    } else if (J->type == JSON_STRING) {
      _pp_sb->buf = CatLenPrint(_pp_sb->buf, &_pp_sb->len, "\"%Q\"", J->str);
    } else if (J->type == JSON_OBJECT) {
      if (J->object == NULL) {
        _pp_sb->buf = CatLenPrint(_pp_sb->buf, &_pp_sb->len, "{}");
      } else {
        _pp_sb->buf = CatLenPrint(_pp_sb->buf, &_pp_sb->len, "{\n");
        _pp_depth = saved + 1;
        JsonPPInternal(J->object);
        _pp_depth = saved;
        _pp_sb->buf = CatLenPrint(_pp_sb->buf, &_pp_sb->len, "\n");
        JsonPPIndent();
        _pp_sb->buf = CatLenPrint(_pp_sb->buf, &_pp_sb->len, "}");
      }
    } else if (J->type == JSON_ARRAY) {
      if (J->array == NULL) {
        _pp_sb->buf = CatLenPrint(_pp_sb->buf, &_pp_sb->len, "[]");
      } else {
        _pp_sb->buf = CatLenPrint(_pp_sb->buf, &_pp_sb->len, "[\n");
        _pp_depth = saved + 1;
        JsonPPInternal(J->array);
        _pp_depth = saved;
        _pp_sb->buf = CatLenPrint(_pp_sb->buf, &_pp_sb->len, "\n");
        JsonPPIndent();
        _pp_sb->buf = CatLenPrint(_pp_sb->buf, &_pp_sb->len, "]");
      }
    } else if (J->type == JSON_BOOL) {
      if (J->boolean == TRUE) {
        _pp_sb->buf = CatLenPrint(_pp_sb->buf, &_pp_sb->len, "true");
      } else {
        _pp_sb->buf = CatLenPrint(_pp_sb->buf, &_pp_sb->len, "false");
      }
    } else if (J->type == JSON_NULL) {
      _pp_sb->buf = CatLenPrint(_pp_sb->buf, &_pp_sb->len, "null");
    }

    if (J->next) {
      _pp_sb->buf = CatLenPrint(_pp_sb->buf, &_pp_sb->len, ",\n");
    }
    J = J->next;
  }
}

public U8 *JsonToPrettyString(Json *json, I64 indent)
{
  U8 *buf = MAlloc(1024);
  JsonStringBuilder sb;
  sb.buf = buf;
  sb.len = 0;
  _pp_indent = indent;
  _pp_depth = 0;
  _pp_sb = &sb;

  if (json == NULL) {
    sb.buf = CatLenPrint(sb.buf, &sb.len, "null");
  } else if (json->type == JSON_OBJECT) {
    if (json->object == NULL) {
      sb.buf = CatLenPrint(sb.buf, &sb.len, "{}");
    } else {
      sb.buf = CatLenPrint(sb.buf, &sb.len, "{\n");
      _pp_depth = 1;
      JsonPPInternal(json->object);
      sb.buf = CatLenPrint(sb.buf, &sb.len, "\n}");
    }
  } else if (json->type == JSON_ARRAY) {
    if (json->array == NULL) {
      sb.buf = CatLenPrint(sb.buf, &sb.len, "[]");
    } else {
      sb.buf = CatLenPrint(sb.buf, &sb.len, "[\n");
      _pp_depth = 1;
      JsonPPInternal(json->array);
      sb.buf = CatLenPrint(sb.buf, &sb.len, "\n]");
    }
  }

  sb.buf[sb.len] = '\0';
  _pp_sb = NULL;
  return sb.buf;
}
