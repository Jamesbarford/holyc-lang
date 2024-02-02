#include <defs.HH>
#include <memory.HC>
#include <system.HC>

/* C declarations that we can use freely */
extern "c" i64 strlen(u8 *s);
extern "c" i64 strnlen(u8 *s, i64 size);
extern "c" u0 *memchr(u0 *__s, i32 __c, u64 __n);
extern "c" i32 memcmp(u0 *__s1, u0 *__s2, u64 __n);
extern "c" u0 *memcpy(u0 *__dst, u0 *__src, u64 __n);
extern "c" u0 *memmove(u0 *__dst, u0 *__src, u64 __len);
extern "c" u0 *memset(u0 *__b, i32 __c, u64 __len);
extern "c" u8 *strcat(u8 *__s1, u8 *__s2);
extern "c" u8 *strchr(u8 *__s, i32 __c);
extern "c" i32 strcmp(u8 *__s1, u8 *__s2);
extern "c" i32 strcoll(u8 *__s1, u8 *__s2);
extern "c" u8 *strcpy(u8 *__dst, u8 *__src);
extern "c" u64 strcspn(u8 *__s, u8 *__u8set);
extern "c" u8 *strerror(i32 __errnum);
extern "c" u64 strlen(u8 *__s);
extern "c" u8 *strncat(u8 *__s1, u8 *__s2, u64 __n);
extern "c" i32 strncmp(u8 *__s1, u8 *__s2, u64 __n);
extern "c" u8 *strncpy(u8 *__dst, u8 *__src, u64 __n);
extern "c" u8 *strpbrk(u8 *__s, u8 *__u8set);
extern "c" u8 *strrchr(u8 *__s, i32 __c);
extern "c" u64 strspn(u8 *__s, u8 *__u8set);
extern "c" u8 *strstr(u8 *__big, u8 *__little);
extern "c" u8 *strtok(u8 *__str, u8 *__sep);
extern "c" u64 strxfrm(u8 *__s1, u8 *__s2, u64 __n);

#define STR_DEFAULT_LEN 512

class str {
  u64 len;
  u64 size;
  u8 *buf;
};

str *strnew(u64 size=STR_DEFAULT_LEN) {
  /* 2 * u64's + 1 u8* ptr + the pointer to the class. This means that the 
   * class' memory is one block, great for the cache. not sure how to then just
   * get at buf */
  str *s = MAlloc(sizeof(str));
  s->len = 0;
  s->size = size;
  s->buf = MAlloc(sizeof(u8)*size);
  return s;
}

u0 strFree(str *s) {
  Free(s);
}

str *strCopy(str *s) {
  str *copy = strnew(s->size);
  MemCpy(copy->buf,s->buf,s->len);
  copy->buf[copy->len] = '\0';
  return copy;
}

bool strShouldResize(str *s, u64 size) {
  if (s->len + size >= s->size) {
    return TRUE;
  }
  return FALSE;
}

str *strResize(str *s, u64 additional) {
  if (strShouldResize(s,additional)) {
    u64 new_size = (s->len + s->size) * 2 + additional;
    str *newstr = strnew(new_size);
    newstr->len = s->len;
    newstr->size = new_size;
    MemCpy(newstr->buf,s->buf,s->len);
    strFree(s);
    return newstr;
  }
  return s;
}

bool strMatchU8(str *s, u8 *buf) {
  u64 len = strlen(buf);
  return s->len == len && !strncmp(s->buf,buf,len);
}

bool strMatchStr(str *s1, str *s2) {
  return s1->len == s2->len &&
    !strncmp(s1->buf,s2->buf,s1->len);
}


u8 charToLower(u8 ch) {
  if ('A' <= ch <= 'Z') return ch + 32;
  return ch;
}

u0 strLowerCase(str *s) {
  for (i64 i = 0; i < s->len; ++i) {
    s->buf[i] = charToLower(s->buf[i]);
  }
}

u8 charToUpper(u8 ch) {
  if ('a' <= ch <= 'z') return ch - 32;
  return ch;
}

u0 strUpperCase(str *s) {
  for (i64 i = 0; i < s->len; ++i) {
    s->buf[i] = charToUpper(s->buf[i]);
  }
}

u8 strUnPutChar(str *s) {
  if (s->len == 0) {
    return '\0';
  } else if (s->len - 1 == 0) {
    s->len = 0;
    return s->buf[0];
  }
  u8 ch = s->buf[s->len - 2];
  s->len--;
  s->buf[s->len] = '\0';
  return ch;
}

u0 strClear(str *s) {
  MemSet(s->buf,'\0',s->len);
  s->len = 0;
}

u0 u8BufReverse(u8 *buf, u64 len) {
  u64 start = 0;
  u64 end = len - 1;
  u8 *tmp;

  while (start < end) {
    tmp = buf[start];
    buf[start] = buf[end];
    buf[end] = tmp;
    start++;
    end--;
  }
}

u0 strReverse(str *s) {
  return u8BufReverse(s->buf,s->len);
}

/* Donald knuth hashing function */
u64 strHash(str *s) {
  u64 hash = 0;
  u8 *ptr = s->buf;
  while (*ptr) {
    hash = ((hash << 5) + hash) + *(ptr++);
  }
  return hash;
}

str *strCatLen(str *s, u8 *buf, u64 buflen) {
  s = strResize(s,buflen);
  MemCpy(s->buf+s->len,buf,buflen);
  s->len += buflen;
  s->buf[s->len] = '\0';
}

str *strCatStr(str *s1, str *s2) {
  return strCatLen(s1,s2->buf,s2->len);
}

str *strPutChar(str *s, u8 ch) {
  s = strResize(s,1);
  s->buf[s->len++] = ch;
  s->buf[s->len] = '\0';
}

u64 stru64ToBuf(u8 *buf, u64 num) {
  u8 *ptr, ch, tmp;
  i64 len = 0, retval = 0;

  /* create a string out of the number, however it is put into the string 
   * backwards */
  ptr = buf;
  while (num) {
    *(ptr++) = '0'+(num % 10);
    num /= 10;
  }

  len = ptr - buf;
  *ptr = '\0';
  u8BufReverse(buf,len);
  return len;
}

i64 stri64ToBuf(u8 *buf, i64 num) {
  u8 *ptr, tmp, ch;
  u64 v;
  i64 len = 0;

  if (num < 0) {
    if (v != I64_MIN) {
      v = -num;
    } else {
      v = cast<u64>(I64_MAX)+1;
    }
  } else {
    v = num;
  }

  ptr = buf;
  while (v) {
    *(ptr++) = '0'+(v % 10);
    v /= 10;
  }

  if (num < 0) *(ptr++) = '-';
  len = ptr - buf;
  *ptr = '\0';
  u8BufReverse(buf,len);
  return len;
}

i64 strHexToBuf(u8 *buf, i64 num) {
  u8 *hex_str="0123456789ABCDEF";
  i64 i = (sizeof(u64) * 2) + 3, len = i;
  buf[i-1] = '\0';
  while (i >= 2) {
    buf[i] = hex_str[num & 0xF];
    num >>= 4;
    i--;
  }
  buf[i] = 'x';
  buf[i-1] = '0';
  return len;
}

str *strCatVaPrintf(str *s, U8 *fmt, I64 argc, I64 *argv) {
  u8 ibuf[32];
  u8 ch = '\0';
  u8 char_arg = '\0';
  i64 sp = 0, s_num;
  u64 u_num = 0, arg_len;
  u8 *str_arg = NULL;
  str *_s;

  while (ch = *fmt++) {
    switch (ch) {
      case '$': { /* Terminal colours */
        ch = *fmt++;
        switch (ch) {
          case '$': { /* normal dollar sign */
            strPutChar(s,ch);
            break;
          }
          /**
           * ANSI terminal colour escape sequences
           * \033[30m - Black
           * \033[31m - Red
           * \033[32m - Green
           * \033[33m - Yellow
           * \033[34m - Blue
           * \033[35m - Magenta
           * \033[36m - Cyan
           * \033[37m - White
           */
          case 'W': {
            if (*(fmt) == 'H' && 
                *(fmt + 1) == 'I' && 
                *(fmt + 2) == 'T' &&
                *(fmt + 3) == 'E' &&
                *(fmt + 4) == '$') 
            {
              fmt += 4;
              strCatLen(s,"\\033[37m", 5);
            } else {
              strCatLen(s,"$W",2);
            }
            break;
          }

          case 'C': {
            if (*(fmt) == 'Y' && 
                *(fmt + 1) == 'A' && 
                *(fmt + 2) == 'N' &&
                *(fmt + 3) == '$') 
            {
              fmt += 3;
              strCatLen(s,"\\033[36m", 5);
            } else {
              strCatLen(s,"$C",2);
            }
            break;
          }

          case 'M': {
            if (*(fmt) == 'A' && 
                *(fmt + 1) == 'G' && 
                *(fmt + 2) == 'E' &&
                *(fmt + 3) == 'N' &&
                *(fmt + 4) == 'T' &&
                *(fmt + 5) == 'A' &&
                *(fmt + 6) == '$') 
            {
              fmt += 6;
              strCatLen(s,"\\033[35m", 5);
            } else {
              strCatLen(s,"$M",2);
            }
            break;
          }

          case 'Y': {
            if (*(fmt) == 'E' && 
                *(fmt + 1) == 'L' && 
                *(fmt + 2) == 'L' &&
                *(fmt + 3) == 'O' &&
                *(fmt + 4) == 'W' &&
                *(fmt + 5) == '$') 
            {
              fmt += 5;
              strCatLen(s,"\\033[33m", 5);
            } else {
              strCatLen(s,"$Y",2);
            }
            break;
          }

          case 'R': {
            if (*fmt == 'E' && 
                *(fmt + 1) == 'D' && 
                *(fmt + 2) == '$') 
            {
              fmt += 2;
              strCatLen(s,"\\033[31m", 5);
            } else {
              strCatLen(s,"$R",2);
            }
            break;
          }

          case 'G': {
            if (*(fmt) == 'R' &&
                *(fmt + 1) == 'E' && 
                *(fmt + 2) == 'E' &&
                *(fmt + 3) == 'N' &&
                *(fmt + 4) == '$')
            {
              fmt += 4;
              strCatLen(s,"\\033[32m", 5);
            } else {
              strCatLen(s,"$G",2);
            }
            break;
          }

          case 'B': {
            if (*(fmt) == 'O' && 
                *(fmt + 1) == 'L' && 
                *(fmt + 2) == 'D' &&
                *(fmt + 3) == '$') 
            {
              fmt += 3;
              strCatLen(s,"\\033[1m", 4);
            } else if (*(fmt) == 'L' && 
                       *(fmt + 1) == 'U' && 
                       *(fmt + 2) == 'E' &&
                       *(fmt + 3) == '$') 
            {
              fmt += 3;
              strCatLen(s,"\\033[34m", 5);
            } else if (*(fmt) == 'L' && 
                       *(fmt + 1) == 'A' && 
                       *(fmt + 2) == 'C' &&
                       *(fmt + 3) == 'K' &&
                       *(fmt + 4) == '$') 
            {
              fmt += 4;
              strCatLen(s,"\\033[30m", 5);
            } else  {
              strCatLen(s,"$B",2);
            }
            break;
          }

          case 'F': {
            if (*(fmt) == 'G' && 
                *(fmt + 1) == '$')
            {
              fmt++;
              strCatLen(s,"\\033[0;0m",6);
            } else {
              strCatLen(s,"$F",2);
            }
            break;
          }
          default:
            strPutChar(s,ch);
            break;
        }
        break;
      }

      case '%': {
        /* See what format we are going to be using */
        ch = *fmt++;

        switch (ch) {
          case 's': { /* u8 string */
            str_arg = cast<U8 *>(argv[sp++]);
            strCatLen(s,str_arg,strlen(str_arg));
            break;
          }

          case 'S': { /* class str */
            _s = cast<str *>(argv[sp++]);
            strCatLen(s,_s->buf,_s->len);
            break;
          }

          case 'c': { /* char */
            char_arg = cast<u8>(argv[sp++]);
            strPutChar(s,char_arg);
            break;
          }

          case '%': { /* the % */
            strPutChar(s,ch);
            break;
          }

          case 'd': { /* signed number */
            s_num = cast<i64>(argv[sp++]);
            arg_len = stri64ToBuf(ibuf,s_num);
            strCatLen(s,ibuf,arg_len);
            break;
          }

          case 'u': { /* unsigned number */
            u_num = cast<u64>(argv[sp++]);
            arg_len = stru64ToBuf(ibuf,u_num);
            strCatLen(s,ibuf,arg_len);
            break;
          }

          case 'X':
          case 'x': { /* hex number */
            u_num = cast<u64>(argv[sp++]);
            arg_len = strHexToBuf(ibuf,u_num);
            strCatLen(s,ibuf,arg_len);
            break;
          }

          case 'p': { /* memory */
            u_num = cast<u64>(&(argv[sp++]));
            arg_len = strHexToBuf(ibuf,u_num);
            strCatLen(s,ibuf,arg_len);
            break;
          }

          default:
            strPutChar(s,ch);
            break;
        }
        break;
      }
      default:
        strPutChar(s,ch);
        break;
    }
  }
  return s;
}

str *strCatPrintf(str *s, u8 *fmt, ...) {
  return strCatVaPrintf(s,fmt,argc,argv);
}
