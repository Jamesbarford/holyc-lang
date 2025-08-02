/* Source code Heavily inspired from: 
 * https://github.com/Jamesbarford/easy-json - A json parser I wrote a while ago 
 */
#define JSON_NO_FLAGS     (0x0)

/* Do not parse numbers, treat them as strings */
#define JSON_STRNUM_FLAG (0x1)

#define JSON_STRING 0
#define JSON_FLOAT  1
#define JSON_INT    2
#define JSON_STRNUM 3
#define JSON_ARRAY  4
#define JSON_OBJECT 5
#define JSON_BOOL   6
#define JSON_NULL   7

#define JSON_OK 0
#define JSON_INVALID_UTF16 1
#define JSON_INVALID_UTF16_SURROGATE 2
#define JSON_INVALID_HEX 3
#define JSON_INVALID_STRING_NOT_TERMINATED 4
#define JSON_INVALID_NUMBER 5
#define JSON_INVALID_DECIMAL 6
#define JSON_INVALID_SIGN 7
#define JSON_INVALID_JSON_TYPE_CHAR 8
#define JSON_INVALID_BOOL 9
#define JSON_INVALID_TYPE 10
#define JSON_CANNOT_ADVANCE 11
#define JSON_CANNOT_START_PARSE 12
#define JSON_INVALID_KEY_TERMINATOR_CHARACTER 13
#define JSON_INVALID_KEY_VALUE_SEPARATOR 14
#define JSON_INVALID_ARRAY_CHARACTER 15
#define JSON_INVALID_ESCAPE_CHARACTER 16
#define JSON_EOF 17

#define JSON_PARSER_STRING  0
#define JSON_PARSER_NUMERIC 1
#define JSON_PARSER_ARRAY   2
#define JSON_PARSER_OBJECT  3
#define JSON_PARSER_BOOL    4
#define JSON_PARSER_NULL    5

#define JSON_SEL_INVALD    (0)
#define JSON_SEL_OBJ       (1)
#define JSON_SEL_ARRAY     (2)
#define JSON_SEL_TYPECHECK (3)
#define JSON_SEL_MAX_BUF   (256)

class JsonState
{
  U8 ch;
  I64 offset;
  I64 error;
};

class Json;
class Json
{
  JsonState *state;
  Json *next;
  I64 type;
  U8 *key;
  union
  {
    Json *array;
    Json *object;
    U8 *str;
    U8 *strnum;
    F64 f64;
    I64 i64;
    Bool boolean;
  };
};

class JsonParser 
{
  I32 type;
  U8 *buffer;
  I64 offset;
  I64 buflen;
  I32 error;
  Json *J;
  Json *ptr;
  I32 flags;
  JsonState *state;
};

class JsonStringBuilder
{
  U8 *buf;
  I64 len;
};

public U8 *JsonToString(Json *j);
Bool JsonParseValue(JsonParser *p);

static Json *JsonNew()
{
  Json *J = MAlloc(sizeof(Json));
  J->type = JSON_NULL;
  J->key = NULL;
  J->next = NULL;
  J->state = NULL;
  return J;
}

static JsonState *JsonStateNew()
{
  JsonState *json_state = MAlloc(sizeof(JsonState));
  json_state->error = JSON_OK;
  json_state->ch = '\0';
  json_state->offset = 0;
  return json_state;
}

I64 GetNextNonWhitespaceIdx(U8 *ptr)
{
  U8 *start = ptr;
  while (IsSpace(*ptr)) {
    ++ptr;
  }
  I64 val = ptr - start;
  return val;
}

static U0 JsonUnsafeAdvanceBy(JsonParser *p, I64 jmp)
{
  p->offset += jmp;
}

static Bool JsonCanAdvanceBy(JsonParser *p, I64 jmp)
{
  return (p->offset + jmp) < p->buflen;
}

static U0 JsonAdvance(JsonParser *p)
{
  if (JsonCanAdvanceBy(p,1)) {
    ++p->offset;
    return;
  }
  p->error = JSON_EOF;
}

static Bool JsonAdvanceToError(JsonParser *p, I64 jmp, I32 error_code)
{
  p->error = error_code;
  JsonUnsafeAdvanceBy(p, jmp);
  return 0;
}

static U8 JsonPeek(JsonParser *p)
{
  return p->buffer[p->offset];
}

static U8 JsonUnsafePeekAt(JsonParser *p, I64 idx)
{
  return p->buffer[idx];
}

static U0 JsonAdvanceWhitespace(JsonParser *p)
{
  I64 val = GetNextNonWhitespaceIdx(p->buffer+p->offset);
  p->offset += val;
}

static U0 JsonAdvanceToTerminator(JsonParser *p, U8 terminator)
{
  while (JsonCanAdvanceBy(p, 1) && JsonPeek(p) != terminator) {
    ++p->offset;
  }
}

static I64 CountNumberLen(JsonParser *p, U8 *ptr, Bool *is_hex)
{
  U8 *start = ptr;
  I32 seen_e = 0, seen_dec = 0, seen_X = 0;

  while (*ptr != ',' && *ptr != ']' && *ptr != '\0' && *ptr != '\n' && *ptr != ' ') {
    switch (*ptr) {
      case 'e':
      case 'E':
        if (seen_e) {
          return JsonAdvanceToError(p, ptr - start, JSON_INVALID_NUMBER);
        }
        seen_e = 1;
        break;
      case 'x':
      case 'X':
        if (seen_X) {
          return JsonAdvanceToError(p, ptr - start, JSON_INVALID_NUMBER);
        }
        *is_hex = TRUE;
        seen_X = 1;
        break;
      case '-':
      case '+':
        break;
      case '.':
        if (seen_dec) {
          return JsonAdvanceToError(p, ptr - start, JSON_INVALID_NUMBER);
        }
        seen_dec = 1;
        break;
      /* Anything else is invalid */
      default:
        if (!(((*ptr) >= '0' && (*ptr) <= '9') ||
            *ptr >= 'a' && *ptr <= 'f' || *ptr >= 'A' && *ptr <= 'F')) {
          return JsonAdvanceToError(p, ptr - start, JSON_INVALID_NUMBER);
        }
        break;
      }
    ptr++;
  }

  /* Floating point hex does not exist */
  if (seen_dec && seen_X) {
    return JsonAdvanceToError(p, ptr - start, JSON_INVALID_NUMBER);
  }

  /* Exponent hex does not exist */
  if (seen_X && seen_e) {
    return JsonAdvanceToError(p, ptr - start, JSON_INVALID_NUMBER);
  }

  return ptr - start;
}

/* Takes a string pointer and a pointer to an integer as input, and returns the
 * number of decimal numeric characters in the string, and the position of the
 * decimal point if present.
 */
static I32 CountMantissa(U8 *ptr, I32 *decidx)
{
  I32 mantissa = 0;
  *decidx = -1;

  for (;; ++mantissa) {
    U8 ch = *ptr;
    /* If the character is not a decimal numeric character, or if
     * the decimal point has already been found exit the loop
     */
    if (!('0'<=ch<='9')) {
      if ((ch != '.') || *decidx >= 0) {
        break;
      }
      *decidx = mantissa;
    }
    ptr++;
  }
  return mantissa;
}

static U0 JsonParseNumber(JsonParser *p)
{
  auto current = p->ptr;
  auto ptr = p->buffer + p->offset;
  I64 res;
  U8 *endptr = NULL;
  Bool is_hex = FALSE;
  I32 neg = 0, dec_idx = -1, mantissa = 0;
  I32 num_len = CountNumberLen(p, ptr,&is_hex);

  if (*ptr == '-') {
    neg = 1;
    ptr++;
  } else if (*ptr == '+') {
    neg = 0;
    ptr++;
  } else if ((!((*ptr) >= '0' && (*ptr) <= '9') && *ptr != '.') ||
               (num_len == 0)) {
    JsonAdvanceToError(p, 0, JSON_INVALID_NUMBER);
    return;
  }

  if (num_len == 1) {
    current->i64 = JsonPeek(p)-'0';
    current->type = JSON_INT;
    JsonAdvance(p);
    return;
  }

  mantissa = CountMantissa(ptr, &dec_idx);

  if (dec_idx == -1) {
    if (neg) --ptr; /* let strtoll do the hard work */
    if (is_hex) {
      res = strtoll(ptr,&endptr,16);
    } else {
      res = strtoll(ptr,&endptr);
    }
    current->i64 = res;
    current->type = JSON_INT;
    JsonUnsafeAdvanceBy(p, num_len);
    return;
  }

  auto parsed = strtod(ptr, &endptr);
  if (neg) {
    current->f64 = -parsed;
  } else {
    current->f64 = parsed;
  }
  current->type = JSON_FLOAT;
  JsonUnsafeAdvanceBy(p, num_len);
}

/**
 * Parse json boolean
 * 1: true
 * 0: false
 * -1: error
 */
static Bool JsonParseBool(JsonParser *p)
{
  U8 peek = JsonPeek(p);
  Bool retval = -1;

  /* Check if the next character in the buffer is 't' for true */
  if (peek == 't') {
    if (!JsonCanAdvanceBy(p, 4)) {
      goto error;
    }
    /* check next 3 characters for 'rue' */
    if (!StrNCmp(p->buffer+p->offset,"true",4)) {
      retval = TRUE;
    }
    JsonUnsafeAdvanceBy(p, 4);
  }
  /* Check if the next character in the buffer is 'f' for false */
  else if (peek == 'f') {
    if (!JsonCanAdvanceBy(p, 5)) {
      goto error;
    }

    /* check next 4 characters for 'alse' */
    if (!StrNCmp(p->buffer+p->offset,"false",5)) {
      retval = FALSE;
    }
    JsonUnsafeAdvanceBy(p, 5);
  }

error:
  /* Failed to parse boolean */
  if (retval == -1) {
    p->error = JSON_CANNOT_ADVANCE;
  }
  return retval;
}

/**
 * Parse json null
 * 1: success
 * -1: error
 */
static I32 JsonParseNull(JsonParser *p)
{
  I32 retval = -1;
  if (!JsonCanAdvanceBy(p, 4)) {
    p->error = JSON_CANNOT_ADVANCE;
    return -1;
  }
  if (!StrNCmp(p->buffer + p->offset,"null",4)) {
    retval = 1;
  }
  JsonUnsafeAdvanceBy(p, 4);
  return retval;
}

static Bool JsonSetExpectedType(JsonParser *p)
{
  auto peek = JsonPeek(p);
  switch (peek) {
    case '{': p->type = JSON_PARSER_OBJECT; break;
    case '[': p->type = JSON_PARSER_ARRAY; break;
    case 'n': p->type = JSON_PARSER_NULL; break;
    case 't':
    case 'f': p->type = JSON_PARSER_BOOL; break;
    case '"': p->type = JSON_PARSER_STRING; break;
    default: {
      if (('0'<=peek<='9') || peek == '-' || peek == '+' || peek == '.') {
        p->type = JSON_PARSER_NUMERIC;
        break;
      } else {
        p->error = JSON_INVALID_JSON_TYPE_CHAR;
        return FALSE;
      }
    }
  }
  return TRUE;
}

static U8 *JsonParseString(JsonParser *p)
{
  Bool run = TRUE;
  I64 start = p->offset;
  I64 end = p->offset;

  if (JsonPeek(p) == '"') {
    JsonAdvance(p);
    end++;
  }

  /* approximate string length */
  while (JsonUnsafePeekAt(p, end) != '\0') {
    U8 cur = JsonUnsafePeekAt(p, end);
    if (cur == '"') {
      if (JsonUnsafePeekAt(p, end - 1) != '\\') {
        break;
      }
    } else if (cur == '\\') {
      // escape sequence so add another byte
      if (JsonUnsafePeekAt(p, end + 1) == '\0') {
        break;
      }
      end++;
    }
    end++;
  }

  if (JsonUnsafePeekAt(p, end) == '\0') {
    p->error = JSON_EOF;
    return NULL;
  }

  I64 len = 0;
  U8 *str = MAlloc(sizeof(U8) * end - start);

  while (run && JsonPeek(p) != '\0') {
    switch (JsonPeek(p)) {
      case '\\':
        JsonAdvance(p);
        switch (JsonPeek(p)) {
          case '\\':
          case '"':
          case '/': str[len++] = JsonPeek(p); break;
          case 'b': str[len++] = '\b'; break;
          case 'f': str[len++] = '\f'; break;
          case 'n': str[len++] = '\n'; break;
          case 'r': str[len++] = '\r'; break;
          case 't': str[len++] = '\t'; break;
          case 'u': {
            /* XXX: handle unicode */
            p->error = JSON_INVALID_ESCAPE_CHARACTER;
            goto err;
          }
          default:
            p->error = JSON_INVALID_ESCAPE_CHARACTER;
            goto err;
        }
        break;
      case '"':
        run = 0;
        break;
      default:
        str[len++] = JsonPeek(p);
        break;
    }
  
    JsonAdvance(p);
  }
  if (JsonPeek(p) == '"') {
    p->error = JSON_INVALID_STRING_NOT_TERMINATED;
    goto err;
  }

  str[len] = '\0';
  return str;

err:
  if (str) {
    Free(str);
  }
  return NULL;
}

static U0 JsonToStringInternal(Json *J, JsonStringBuilder *js)
{
  if (J == NULL) {
    js->buf = CatLenPrint(js->buf,&js->len,"(nil)");
    return;
  }

  while (J) {
    if (J->key) js->buf = CatLenPrint(js->buf,&js->len,"\"%Q\": ",J->key);
    switch (J->type) {
      case JSON_INT:
        js->buf = CatLenPrint(js->buf,&js->len,"%d",J->i64);
        break;
      case JSON_FLOAT:
        js->buf = CatLenPrint(js->buf,&js->len,"%f",J->f64);
        break;
      case JSON_STRNUM:
        js->buf = CatLenPrint(js->buf,&js->len,"%s",J->strnum);
        break;
      case JSON_STRING:
        js->buf = CatLenPrint(js->buf,&js->len,"\"%Q\"",J->str);
        break;
      case JSON_ARRAY:
        js->buf = CatLenPrint(js->buf,&js->len,"[");
        JsonToStringInternal(J->array,js);
        js->buf = CatLenPrint(js->buf,&js->len,"]");
        break;
      case JSON_OBJECT:
        js->buf = CatLenPrint(js->buf,&js->len,"{");
        JsonToStringInternal(J->object,js);
        js->buf = CatLenPrint(js->buf,&js->len,"}");
        break;
      case JSON_BOOL:
        if (J->boolean == TRUE) {
          js->buf = CatLenPrint(js->buf,&js->len,"true");
        } else {
          js->buf = CatLenPrint(js->buf,&js->len,"false");
        }
        break;
      case JSON_NULL:
        js->buf = CatLenPrint(js->buf,&js->len,"null");
        break;
      default:
        break;
    }
    if (J->next) {
      js->buf = CatLenPrint(js->buf,&js->len,",");
    }
    J = J->next;
  }
}

public U8 *JsonToString(Json *json)
{// Json to string
  U8 *buf = MAlloc(sizeof(U8)*1<<10);
  I64 len = 0;
  JsonStringBuilder sb;
  sb.buf = buf;
  sb.len = len;
  JsonToStringInternal(json,&sb);
  sb.buf[sb.len] = '\0';
  return sb.buf;
}

/**
 * Recursively frees whole JSON object
 */
public U0 JsonRelease(Json *J)
{
  if (J == NULL) {
    return;
  }
  Json *ptr = J;
  Json *next = NULL;

  while (ptr) {
    next = ptr->next;
    if (ptr->key) {
      Free(ptr->key);
    }

    switch (ptr->type) {
      case JSON_STRNUM:
        if (ptr->strnum) {
          Free(ptr->strnum);
        }
        break;

      case JSON_STRING:
        if (ptr->str) {
          Free(ptr->str);
        }
        break;

      case JSON_ARRAY:
        JsonRelease(ptr->array);
        break;

      case JSON_OBJECT:
        JsonRelease(ptr->object);
        break;

      case JSON_FLOAT:
      case JSON_INT:
      case JSON_BOOL:
      case JSON_NULL:
        break;
    }

    Free(ptr);
    ptr = next;
  }
}

/**
 * Very similar to parsing an object where starting char is '[',
 * if array is empty return NULL
 */
static Json *JsonParseArray(JsonParser *p)
{
  /* move past '[' */
  JsonAdvance(p);
  JsonAdvanceWhitespace(p);

  /* array empty we can skip */
  if (JsonPeek(p) == ']') {
    JsonAdvance(p);
    return NULL;
  }

  U8 ch = '\0';
  I32 can_advance = 0;
  Json *J;
  Json *val = JsonNew();
  p->ptr = val;

  while (TRUE) {
    J = p->ptr;
    JsonAdvanceWhitespace(p);

    if (!JsonSetExpectedType(p) || !JsonParseValue(p)) {
      break;
    }


    JsonAdvanceWhitespace(p);
    ch = JsonPeek(p);
    if (ch != ',') {
      can_advance = JsonCanAdvanceBy(p, 1);
      if (ch == ']' && can_advance) {
        JsonAdvance(p);
        break;
      } else if (ch == ']' && !can_advance) {
        break;
      } else {
        Free(val);
        p->error = JSON_INVALID_ARRAY_CHARACTER;
        return NULL;
      }
    }
    JsonAdvance(p);
    J->next = JsonNew();
    p->ptr = J->next;
  }

  p->ptr = val;
  return val;
}

Json *JsonParseObject(JsonParser *p)
{
  JsonAdvance(p);
  JsonAdvanceWhitespace(p);
  if (JsonPeek(p) == '}') {
    JsonAdvance(p);
    return NULL;
  }
  U8 ch = '\0';
  Bool can_advance;
  Json *J,*val;
  val = JsonNew;
  p->ptr = val;

  while (TRUE) {
    J = p->ptr;
    JsonAdvanceWhitespace(p);

    if (JsonPeek(p) != '"') {
      Free(val);
      p->error = JSON_INVALID_KEY_TERMINATOR_CHARACTER;
      return NULL;
    }

    J->key = JsonParseString(p);
    JsonAdvanceToTerminator(p, ':');
    if (JsonPeek(p) != ':') {
      Free(val);
      p->error = JSON_INVALID_KEY_TERMINATOR_CHARACTER;
      return NULL;
    }

    JsonAdvance(p);
    JsonAdvanceWhitespace(p);

    if (!JsonSetExpectedType(p) || !JsonParseValue(p)) {
      break;
    }

    JsonAdvanceWhitespace(p);
    ch = JsonPeek(p);
    if (ch != ',') {
      can_advance = JsonCanAdvanceBy(p, 1);
      if (ch == '}' && can_advance) {
        JsonAdvance(p);
        break;
      } else if (ch == '}' && !can_advance) {
        break;
      } else {
        Free(val);
        p->error = JSON_INVALID_JSON_TYPE_CHAR;
        return NULL;
      }
    }

    JsonAdvance(p);
    J->next = JsonNew();
    p->ptr = J->next;
  }

  return val;
}

/**
 * Selects which parser to use depending on the type which has been set on the
 * previous parse
 */
static Bool JsonParseValue(JsonParser *p)
{
  auto J = p->ptr;
  switch (p->type) {
    case JSON_PARSER_NUMERIC: {
      if (p->flags & JSON_STRNUM_FLAG) {
          J->type = JSON_STRNUM;
          J->strnum = JsonParseString(p);
      } else {
          JsonParseNumber(p);
      }
      break;
    }

    case JSON_PARSER_STRING:
      J->type = JSON_STRING;
      J->str = JsonParseString(p);
      break;

    case JSON_PARSER_NULL:
      if (JsonParseNull(p)) {
          J->type = JSON_NULL;
      }
      break;

    case JSON_PARSER_OBJECT:
      J->type = JSON_OBJECT;
      J->object = JsonParseObject(p);
      break;

    case JSON_PARSER_BOOL:
      J->type = JSON_BOOL;
      J->boolean = JsonParseBool(p);
      break;

    case JSON_PARSER_ARRAY:
      J->type = JSON_ARRAY;
      J->array = JsonParseArray(p);
      break;
  }

  return p->error == JSON_OK;
}


Json *JsonParseWithLenAndFlags(U8 *raw, I64 len, I32 flags)
{
  auto J = JsonNew();
  JsonParser p;
  p.flags = flags;
  p.offset = 0;
  p.buffer = raw;
  p.buflen = len;
  p.type = -1;
  p.J = NULL;
  p.ptr = NULL;
  p.error = JSON_OK; 
  p.J = J;

  JsonAdvanceWhitespace(&p);

  U8 peek = JsonPeek(&p);

  if (peek == '{') {
    J->type = JSON_OBJECT;
    J->object = JsonParseObject(&p);
  } else if (peek = '[') {
    J->type = JSON_ARRAY;
    J->array = JsonParseArray(&p);
  } else {
    p.error = JSON_CANNOT_START_PARSE;
  }

  J->state = JsonStateNew();
  J->state->error = p.error;
  J->state->ch = p.buffer[p.offset];
  J->state->offset = p.offset;
  return J;
}

public Json *JsonParse(U8 *raw, I64 len=-1, I64 flags=0)
{
  if (len == -1) len = StrLen(raw);
  return JsonParseWithLenAndFlags(raw,len,0);
}

static U8 *JsonGetStrerrorInternal(I32 error, U8 ch, I64 offset)
{
  switch (error) {
    case JSON_OK:
      return StrPrint(NULL, "Success");

    case JSON_INVALID_UTF16:
      return StrPrint(NULL,
          "Unexpected UTF16 character '%c' while parsing UTF16 at position: %d",
          ch, offset);

    case JSON_INVALID_UTF16_SURROGATE:
      return StrPrint(NULL,
          "Unexpected UTF16 surrogate character '%c' while parsing UTF16 at position: %d",
          ch, offset);

    case JSON_INVALID_HEX:
      return StrPrint(NULL,
          "Unexpected hex '%c' while parsing UTF16 at position: %d",
          ch, offset);

    case JSON_INVALID_STRING_NOT_TERMINATED:
      return StrPrint(NULL,
          "Expected '\"' to terminate string recieved '%c' at position: %d",
          ch, offset);

    case JSON_INVALID_NUMBER:
      return StrPrint(NULL,
          "Unexpected numeric character '%c' while parsing number at position: %d",
          ch, offset);

    case JSON_INVALID_DECIMAL:
      return StrPrint(NULL,
          "Unexpected decimal character '%c' while parsing number at position: %d",
          ch, offset);

    case JSON_INVALID_SIGN:
      return StrPrint(NULL,
          "Unexpected sign character '%c' while parsing number at position: %d",
          ch, offset);

    case JSON_INVALID_KEY_TERMINATOR_CHARACTER:
      return StrPrint(NULL,
          "Unexpected character '%c' while parsing object key at position: %d",
          ch, offset);

    case JSON_INVALID_KEY_VALUE_SEPARATOR:
      return StrPrint(NULL,
          "Unexpected character '%c' while parsing key value separator at position: %d",
          ch, offset);

    case JSON_INVALID_ARRAY_CHARACTER:
      return StrPrint(NULL,
          "Unexpected character '%c' while parsing array at position: %d",
          ch, offset);

    case JSON_INVALID_ESCAPE_CHARACTER:
      return StrPrint(NULL,
          "Invalid JSON escape character '%c' at position: %d",
          ch, offset);

    case JSON_INVALID_BOOL:
      return StrPrint(NULL,
          "Unexpected character '%c' while parsing boolean at position: %d",
          ch, offset);

    case JSON_INVALID_JSON_TYPE_CHAR:
    case JSON_INVALID_TYPE: {
      if (ch == '\0') {
        return StrPrint(NULL,
            "Unexpected NULL terminator (possible EOF) at position: %d\n",
            offset);
      } else {
        return StrPrint(NULL,
            "Unexpected character '%c' while seeking next type to parse at position: %d",
            ch, offset);
      }
    }
    case JSON_CANNOT_START_PARSE:
      return StrPrint(NULL, "JSON must start with '[' or '{', at position: %d",
          offset);
    case JSON_CANNOT_ADVANCE:
    case JSON_EOF:
      return StrPrint(NULL, "Unexpected end of json buffer at position: %d",
          offset);
    }
  return NULL;
}

/* If the json has state this will return the error as a human readible string
 * must be freed by the caller (allows it to be thread safe). It would be
 * advisable to use this for debugging purposes only as it calls malloc */
U8 *JsonGetStrerror(Json *json)
{
  if (!json) {
    return StrPrint(NULL, "Json is NULL");
  }
  if (json->state->error != JSON_OK) {
    U8 *str = JsonGetStrerrorInternal(json->state->error,json->state->ch,
        json->state->offset);
    return str;
  }
  return NULL;
}

/* Print the error to stdout */
U0 JsonPrintError(Json *json)
{
  U8 *str_error = JsonGetStrerror(json);
  if (str_error) {
    "%s\n", str_error;
    Free(str_error);
  } else {
    "No errors\n";
  }
}

public Bool JsonOk(Json *j)
{
  if (!j) return FALSE;
  return j->state && j->state->error == JSON_OK;
}

Bool JsonIsObject(Json *j)
{
  if (!j) return FALSE;
  return j->type == JSON_OBJECT;
}

Bool JsonIsArray(Json *j)
{
  if (!j) return FALSE;
  return j->type == JSON_ARRAY;
}

Bool JsonIsNull(Json *j)
{
  if (!j) return FALSE;
  return j->type == JSON_NULL;
}

Bool JsonIsBool(Json *j)
{
  if (!j) return FALSE;
  return j->type == JSON_BOOL;
}

Bool JsonIsString(Json *j)
{
  if (!j) return FALSE;
  return j->type == JSON_STRING;
}

Bool JsonIsInt(Json *j)
{
  if (!j) return FALSE;
  return j->type == JSON_INT;
}

Bool JsonIsFloat(Json *j)
{
  if (!j) return FALSE;
  return j->type == JSON_FLOAT;
}

static Bool JsonTypeCheck(Json *j, U8 tk)
{
  switch (tk) {
    case 's':
      if (!JsonIsString(j)) {
        return FALSE;
      }
      return TRUE;
    case 'i':
      if (!JsonIsInt(j)) {
        return FALSE;
      }
      return TRUE;
    case 'f':
      if (!JsonIsFloat(j)) {
        return FALSE;
      }
      return TRUE;
    case 'o':
      if (!JsonIsObject(j)) {
        return FALSE;
      }
      return TRUE;
    case 'a':
      if (!JsonIsArray(j)) {
        return FALSE;
      }
      return TRUE;
    case 'b':
      if (!JsonIsBool(j)) {
        return FALSE;
      }
      return TRUE;
    case '!':
      if (!JsonIsNull(j)) {
        return FALSE;
      }
      return TRUE;
    default:
      return FALSE;
  }
}

/**
 * Get item from an array of json or return null
 */
Json *JsonArrayAt(Json *j, I64 idx) {
  if (!JsonIsArray(j) || idx < 0) {
    return NULL;
  }

  auto node = j->array;
  auto i = 0;

  while (node && i < idx) {
    node = node->next;
    ++i;
  }

  if (!node) return NULL;
  /* The idx was passed the number of elements in the list */
  if (i != idx) return NULL;

  return node;
}

static Json *JsonObjectAt(Json *j, U8 *name)
{
  if (!JsonIsObject(j)) {
    return NULL;
  }
  if (name == NULL) {
    return NULL;
  }

  auto el = j->object;
  while ((el != NULL) && (el->key != NULL) && StrCmp(name,el->key) != 0) {
    el = el->next;
  }

  if ((el == NULL) || (el->key == NULL)) {
    return NULL;
  }

  return el;
}

/**
 * Allows selections like:
 *
 * Json *J = JsonParse(myJson_string);
 * Json *width = JsonSelect(J,".foo.bar[*].baz",4);
 * Json *height = JsonSelect(J,".tv.type[4].*","name");
 * Json *price = JsonSelect(J,".clothes.shirt[4].price_*", <some_variable>);
 *
 * You can also include a :<type> specifier, typically at the end, to verify the
 * type of the final JSON object selected. If the type doesn't match, the
 * function will return NULL. For instance, the specifier .foo.bar:s will return
 * NULL unless the root object has a 'foo' field, which is an object with a
 * 'bar' cfield that contains a string. Here is a comprehensive list of
 * selectors:
 *
 *    ".field" selects the "field" of the current object.
 *    "[1234]" selects the specified index of the current array.
 *    ":<type>" checks if the currently selected type is of the specified type,
 *              where the type can be a single letter representing:
 *      "s" -> string
 *      "f" -> float
 *      "i" -> int
 *      "a" -> array
 *      "o" -> object
 *      "b" -> boolean
 *      "!" -> null
 */
public Json *JsonSelect(Json *j, U8 *fmt, ...)
{
  /**
   * Heavily inspired by:
   * https://github.com/antirez/stonky/blob/main/stonky.c
   *
   * There was an issue in CJSON that rejected this contribution.
   * https://github.com/DaveGamble/cJSON/issues/553
   *
   * The fmt string is parsed, the JSON path is built and the
   * appropriate action is performed depending on the type of the selector
   * (Array, Object or Type Check). If a wildcard "*" is found, the next
   * argument from the variable arguments list is used.
   *
   * If any error occurs during the process (for example, the path length
   * exceeds the maximum buffer length, or an invalid type check character is
   * encountered), the function will stop processing and return NULL. If
   * everything goes smoothly, the function will return the selected JSON
   * object.
   */
  I64 next = JSON_SEL_INVALD, arg = 0;
  U8 path[JSON_SEL_MAX_BUF + 1], buf[64], *s = NULL, *end;
  I64 path_len = 0, len = 0, idx = -1;
  U8 *ptr = fmt;

  if (*ptr != '.' && *ptr != '[') {
    return NULL;
  }

  while (TRUE) {
    if (path_len && (*ptr == '\0' || strchr(".[]:", *ptr))) {
      path[path_len] = '\0';
      switch (next) {
        case JSON_SEL_ARRAY:
          idx = strtoll(path, &end)(I64);
          if ((idx == 0 && *end != '\0') || idx < 0) {
            goto fail;
          }

          j = JsonArrayAt(j, idx);
          if (!j) {
            goto fail;
          }
          break;
        case JSON_SEL_OBJ:
          j = JsonObjectAt(j, path);
          if (!j) {
            goto fail;
          }
          break;
        case JSON_SEL_TYPECHECK: {
          if (!JsonTypeCheck(j, path[0])) {
            goto fail;
          }
          break;
        }

        case JSON_SEL_INVALD:
         goto fail;
      }
    } else if (next != JSON_SEL_INVALD) {
      if (*ptr != '*') {
        path[path_len] = *ptr++;
        path_len++;
        if (path_len > JSON_SEL_MAX_BUF) {
          goto fail;
        }
        continue;
      } else {
        if (next == JSON_SEL_ARRAY) {
          I64 idx = argv[arg++](I64);
          len = snprintf(buf, sizeof(buf), "%d", idx);
          buf[len] = '\0';
          s = buf;
        } else if (next == JSON_SEL_OBJ) {
          s = argv[arg++](U8*);
          len = StrLen(s);
        } else {
          goto fail;
        }
        if (path_len + len > JSON_SEL_MAX_BUF) {
          goto fail;
        }

        MemCpy(path + path_len, buf, len);
        path_len += len;
        ptr++;
        continue;
      }
    }

    if (*ptr == ']') {
      ptr++;
    }
    if (*ptr == '\0') {
      break;
    } else if (*ptr == '[') {
      next = JSON_SEL_ARRAY;
    } else if (*ptr == '.') {
      next = JSON_SEL_OBJ;
    } else if (*ptr == ':') {
      next = JSON_SEL_TYPECHECK;
    } else {
      goto fail;
    }
    path_len = 0;
    ptr++;
  }

  return j;

fail:
  return NULL;
}
