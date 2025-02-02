#include "./math.HC"
#include "./defs.HH"
#include "./date.HC"
#include "./memory.HC"
#include "./io.HC"

extern "c" F64 strtod(U8 *nptr, U8 **endptr);
extern "c" I64 strtoll(U8 *nptr, U8 **endptr, I64 base=10);
extern "c" U64 strtoul(U8 *nptr, U8 **endptr, I64 base=10);
extern "c" I64 snprintf(I8 *nptr, U64 len, I8 *fmt,...);
extern "c" I64 strlen(U8 *s);
extern "c" I64 strnlen(U8 *s, I64 size);
extern "c" U8 *strcat(U8 *__s1, U8 *__s2);
extern "c" U8 *strchr(U8 *__s, I32 __c);
extern "c" I32 strcmp(U8 *__s1, U8 *__s2);
extern "c" I32 strcoll(U8 *__s1, U8 *__s2);
extern "c" U8 *strcpy(U8 *__dst, U8 *__src);
extern "c" U64 strcspn(U8 *__s, U8 *__u8set);
extern "c" U8 *strerror(I32 __errnum);
extern "c" U64 strlen(U8 *__s);
extern "c" U8 *strncat(U8 *__s1, U8 *__s2, U64 __n);
extern "c" I32 strncmp(U8 *__s1, U8 *__s2, U64 __n);
extern "c" U8 *strncpy(U8 *__dst, U8 *__src, U64 __n);
extern "c" U8 *strpbrk(U8 *__s, U8 *__u8set);
extern "c" U8 *strrchr(U8 *__s, I32 __c);
extern "c" U64 strspn(U8 *__s, U8 *__u8set);
extern "c" U8 *strstr(U8 *__big, U8 *__little);
extern "c" U8 *strtok(U8 *__str, U8 *__sep);
extern "c" U64 strxfrm(U8 *__s1, U8 *__s2, U64 __n);

public U8 *StrPrint(U8 *dst,U8 *fmt,...);
public U8 *StrPrintJoin(U8 *_dst, U8 *fmt,I64 argc,I64 *argv);
public U8 *MPrintDate(CDate cdt);
public U8 *MPrintTime(CDate cdt);
public U8 *MPrintTimestampNow();
public I64 StrOcc(U8 *src, I64 ch);
public Bool WildMatch(U8 *test_str, U8 *wild_str);

#define STR_LEN 256

#define PRTF_PAD_ZERO        0x001
#define PRTF_LEFT_JUSTIFY	 0x002
#define PRTF_TRUNCATE		 0x004
#define PRTF_COMMA		     0x008
#define PRTF_DOLLAR          0x010
#define PRTF_SLASH		     0x020
#define PRTF_QUESTION		 0x040
#define PRTF_AUX_FMT_NUM	 0x080
#define PRTF_DECIMAL		 0x100
#define PRTF_NEG		     0x200
#define PRTF_NEG_E           0x400
#define PRTF_NEG_AUX_FMT_NUM 0x800

#define CH_BACKSPACE   0x08
#define CH_ESC         0x1B
#define CH_SHIFT_ESC   0x1C
#define CH_SHIFT_SPACE 0x1F
#define CH_SPACE	   0x20

#define TIMESTAMP_FMT "%d-%02d-%02d %02d:%02d:%02d"

asm {
// RDI=s1, RSI=s2, RDX=N
// 'a'=97, 'z'=122
// 'A'=65, 'Z'=90
_STRNICMP::
    PUSHQ  RBP
    MOVQ   RBP, RSP
    MOVQ   RCX, RDX
@@3:
    TEST   RCX, RCX
    JZ     @@2
    DECQ   RCX
    LODSB
    TEST   AL,AL
    JZ     @@6
    CMP    AL, 97
    JB     @@22
    CMP    AL, 122
    JA     @@22
    SUB    AL, 32
@@22:
    MOV    BL, [RDI]
    INCQ   RDI
    TEST   BL,BL
    JZ     @@6
    CMP    BL, 97
    JB     @@23
    CMP    BL, 122
    JA     @@23
    SUB    BL, 32
@@23:
    CMP    AL, BL
    JE     @@3
@@6:
    SUBQ   RSI, RDI
    MOVQ   RAX, RSI
    LEAVE
    RET
@@2:
    XORQ   RAX, RAX
    LEAVE
    RET

_STRNCMP::
    PUSHQ   RBP
    MOVQ    RBP, RSP
    MOVQ    RCX, RDX
    TEST    RCX, RCX
    JZ      @@2
    REPE    CMPSB
    JE      @@1
    MOVZBQ  RAX,-1[RDI]
    MOVZBQ  RDX,-1[RSI]
    SUBQ    RAX,RDX
    LEAVE
    RET
@@1:
    XORQ    RAX, RAX
@@2:
    LEAVE
    RET

_STRCMP::
    PUSH  RBP
    MOV   RBP,RSP
@@2:
    LODSB
    SCASB
    JNE    @@1
    TEST   AL, AL
    JNZ    @@2
    XOR    RAX, RAX
    JMP    @@3
@@1:
    SUB     RDI, 1
    MOVZX   RAX, AL
    MOVZBQ  RDX, [RDI]
    SUB     RAX, RDX
@@3:
    LEAVE
    RET

_STRLEN_FAST::
    MOVQ  RSI, RDI
    XOR   RAX, RAX
    MOVQ  RCX, -1
    REPNZ SCASB   // decrements RCX
    NOT   RCX     // Reverse RCX
    DEC   RCX
    MOVQ  RAX, RCX
    RET

// @TempleOS - only slight tweaks
_STRCPY:: 
    PUSH  RBP
    MOVQ  RBP,RSP
    TEST  RDI,RDI
    JZ    @@15
    TEST  RSI, RSI
    JNZ   @@05
    XOR   RAX,RAX
    JMP   @@10
@@05:
    LODSB
@@10:
    STOSB
    TEST  AL,AL
    JNZ	  @@05
@@15:
    LEAVE
    RET

// RDI= char to convert
_TOUPPER::
    PUSH  RBP
    MOVQ  RBP,RSP
    MOVQ  RAX,RDI
    CMP   AL, 'a'
    JB    @@1
    CMP   AL, 'z'
    JA    @@1
    SUB   AL, 32
@@1:
    LEAVE
    RET

// RDI= char to convert
_TOLOWER::
    PUSH  RBP
    MOVQ  RBP,RSP
    MOVQ  RAX,RDI
    CMP   AL, 'A'
    JB    @@1
    CMP   AL, 'Z'
    JA    @@1
    ADD   AL, 32
@@1:
    LEAVE
    RET

/* RDI= U8 character */
_ISSPACE::
    MOVQ    RAX, RDI
    CMP     AL, ' '
    JE      @@5
    CMP     AL, '\t'
    JE      @@5
    CMP     AL, '\n'
    JE      @@5
    CMP     AL, '\v'
    JE      @@5
    CMP     AL, '\f'
    JE      @@5
    CMP     AL, '\r'
    JE      @@5
    JMP     @@10
@@5:
    MOVQ    RAX, 1
    JMP     @@15
@@10:
    MOVQ    RAX, 0
@@15:
    RET

/* RDI: U8 * string
 * RET U64 hashed string */
_STRHASH::
    MOVZBL ECX, [RDI]
    LEAQ   RSI, 1[RDI]
    MOVSBL EAX, 1[RDI]
    TEST   AL, AL
    JE     @@10
@@5:
    MOVQ   RDX, RCX
    SALQ   RDX, 5
    SUB    RDX,RCX
    MOVZBL EAX, AL
    MOVQ   RCX,RAX
    ADD    RCX, RDX
    ADD    RSI, 1
    MOVZBL EAX, [RSI]
    TEST   AL, AL
    JNE    @@5
@@10:
    MOV    RAX, RCX
    RET

_ATOI::
    PUSH    RBP
    MOV     RBP, RSP
    MOV     RAX, 0
    XOR     R8,  R8
    CMPB    [RDI], '-'
    JNE     @@01
    ADD     RDI, 1
    MOV     R8,  1 // mark as being negative
@@01:
    CMPB    [RDI], '0'
    JL      @@02
    CMPB    [RDI], '9'
    JG      @@02
    MOVB    BL, [RDI]
    SUBB    BL, '0'
    MOVZBQ  RBX, BL
    IMUL    RAX, 10
    ADD     RAX, RBX
    ADD     RDI, 1
    JMP     @@01
@@02:
    TEST    R8, R8
    JZ      @@03
    NEG     RAX
@@03:
    LEAVE
    RET
}

public _extern _STRLEN_FAST U64 StrLen(U8 *buf);
public _extern _STRNCMP I64 StrNCmp(U8 *s1, U8 *s2, I64 len);
public _extern _STRNICMP I64 StrNICmp(U8 *s1, U8 *s2, I64 len);
public _extern _STRCMP I64 StrCmp(U8 *s1, U8 *s2);
public _extern _STRCPY U0 StrCpy(U8 *dst, U8 *src);
public _extern _TOLOWER U8 ToLower(U8 ch);
public _extern _TOUPPER U8 ToUpper(U8 ch);
public _extern _ISSPACE Bool IsSpace(U8 ch);
public _extern _STRHASH U64 StrHash(U8 *str);
public _extern _ATOI I64 Atoi(U8 *str);

public U8 *StrNCpy(U8 *buf, I64 len)
{
  U8 *str = MAlloc(sizeof(U8)*(len+1));
  if (len == 0) return str;
  MemCpy(str,buf,len);
  str[len] = '\0';
  return str;
}

U8 *StrNew(U8 *buf)
{ // @TempleOS
  U8 *res;
  I64 size;
  if (buf) {
    size = StrLen(buf)+1;
    res = MAlloc(size);
    MemCpy(res,buf,size);
    res[size-1] = '\0';
  } else {
    res=MAlloc(1);
    *res=0;
  }
  return res;
}

public U8 *FileExtDot(U8 *src)
{//@TempleOS - Find dot char in name.
  I64 ch;
  while (ch=*src++)
    if (ch=='.' && *src!='/' && *src!='.')
      return src-1;
  return NULL;
}

public U8 *FileExtRem(U8 *src,U8 *dst=NULL)
{//@TempleOS - Remove filename extension from str.
  U8 *ptr;
  if (ptr=FileExtDot(src)) {
    if (dst)
      StrCpy(dst,ptr+1);
    *ptr=0;
  } else if (dst)
    *dst=0;
  return dst;
}

public Bool IsDotZ(U8 *filename)
{//@TempleOS - Does name end in .Z?
  I64 i=StrLen(filename);
  if (StrOcc(filename,'.')>1 && filename[i-1]=='Z' && filename[i-2]=='.')
    return TRUE;
  else
    return FALSE;
}

public Bool IsDotC(U8 *filename)
{//@TempleOS - Does name end in .C?
  I64 i=StrLen(filename);
  if (StrOcc(filename,'.')>1 && filename[i-1]=='C' && filename[i-2]=='.')
    return TRUE;
  else
    return FALSE;
}

public U0 SPutChar(U8 **_dst,U8 ch,U8 **_buf)
{ // @TempleOS
  I64 i;
  U8 *dst=*_dst,*buf;
  if (_buf) {
    buf=*_buf;
    i=dst-buf;
    if (i>=MSize(buf)) {
      buf=MAlloc(i<<1+1);
      MemCpy(buf,*_buf,i);
      Free(*_buf);
      dst=buf+i;
      *_buf=buf;
    }
  }
  *dst++=ch;
  *dst='\0';
  *_dst=dst;
}

public U0 OutStr(U8 *instr,U8 **_buf,U8 **_dst,I64 len,I64 flags)
{// @TempleOS
 // instr -> in string
 // _buf  -> the underlying buffer 
 // _dst  -> where the string will go
  I64 i,j,size,dst_len;
  U8 *buf,*dst;
  if (!instr)
    i=0;
  else
    i=StrLen(instr);
  dst = *_dst;
  buf = *_buf;
  dst_len = dst-buf;
  size = MSize(buf);

  if (i >= (size - dst_len)) {
    buf = ReAlloc(buf,((size*10)+i)*sizeof(U8));
    *_buf = buf;
    *_dst = buf + dst_len;
    dst = *_dst;
  }
  if (flags&PRTF_TRUNCATE && i>len)
    i=len;
  if (flags&PRTF_LEFT_JUSTIFY) {
    for (j=0;j<i;j++)
      SPutChar(_dst,*instr++,_buf);
    for (j=0;j<len-i;j++)
      SPutChar(_dst,CH_SPACE,_buf);
  } else {
    for (j=0;j<len-i;j++)
      SPutChar(&dst,CH_SPACE,_buf);
    for (j=len-i;j<len;j++)
      SPutChar(_dst,*instr++,_buf);
  }
}

public I64 StrOcc(U8 *src, I64 ch)
{// @TempleOS - Count occurrences of a char.
  I64 i=0;
  if (!src) return 0;
  while (*src)
    if (*src++==ch)
      i++;
  return i;
}

public U8 *StrFirstOcc(U8 *src,U8 *marker)
{//@TempleOS - Point to 1st occurrence of marker set in str.
  I64 ch;
  while ((ch=*src++) && !StrOcc(marker,ch));
  if (ch)
    return src-1;
  else
    return NULL;
}

U8 *StrFirstRem(U8 *src,U8 *marker,U8 *dst=NULL)
{//@TempleOS - Remove first str segment and place in dst buf or NULL.
  I64 ch;
  U8 *ptr=src,*res=dst;
  if (dst) {
    while ((ch=*ptr++) && !StrOcc(marker,ch))
      *dst++=ch;
    *dst=0;
  } else
    while ((ch=*ptr++) && !StrOcc(marker,ch));
  if (ch)
    StrCpy(src,ptr);
  else
    *src=0;
  return res;
}

public U8 *StrLastOcc(U8 *src,U8 *marker)
{// @TempleOS - Point to last occurrence of market set in str.
  I64 ch;
  U8 *res=NULL;
  while (ch=*src++)
    if (StrOcc(marker,ch))
      res=src-1;
  return res;
}

public U8 *StrLastRem(U8 *src,U8 *marker,U8 *dst=NULL)
{// @TempleOS - Remove last str segment and place in dst buf or NULL.
  U8 *ptr;
  if ((ptr=StrLastOcc(src,marker)) != NULL) {
    if (dst)
      StrCpy(dst,ptr+1);
    *ptr=0;
  } else {
    if (dst)
      StrCpy(dst,src);
    *src=0;
  }
  return dst;
}

public U8 *StrTrimStart(U8 *str, I64 str_len=-1, I64 *_new_len=NULL)
{ // Mutates the string, removes whitespace at the start
  // of the string, MSize calls are still safe
  I64 new_len,len,j;

  len = StrLen(str);
  j = 0;

  while (TRUE) {
    if (IsSpace(str[j])) ++j;
    else break;
  }

  new_len = len-j;
  if (_new_len) {
    *_new_len = new_len;
  }
  if (new_len == len) {
    return str;
  }
  memmove(str,str+j,new_len);
  str[new_len] = '\0';
  return str;
}

public U8 *StrTrimEnd(U8 *str, I64 str_len=-1, I64 *_new_len=NULL)
{ // Mutates the string, removes whitespace at the end 
  // of the string, MSize calls are still safe
  I64 new_len,len,i;

  if (str_len == -1) {
    len = StrLen(str);
  } else {
    len = str_len;
  }
  i = len-1;

  while (TRUE) {
    if (IsSpace(str[i])) --i;
    else break;
  }
  i++;

  new_len = i;
  if (_new_len) {
    *_new_len = new_len;
  }
  if (new_len == len) {
    return str;
  }
  memmove(str,str,new_len);
  str[new_len] = '\0';
  return str;
}

public U0 StrReverse(U8 *buf, I64 length)
{ // Reverse a string inplace
  I64 start = 0, end = length - 1;
  while (start < end) {
    U8 temp = buf[start];
    buf[start] = buf[end];
    buf[end] = temp;
    start++;
    end--;
  }
}

public I64 I64ToStr(U8 *buf, I64 num, Bool *_is_neg=NULL, I64 _len=5000)
{ // puts num in the buf, returns the length of the number
  I64 i = 0;

  if (num == 0) {
    buf[i++] = '0';
    buf[i] = '\0';
    return 1;
  }

  if (_is_neg) {
    *_is_neg = FALSE;
  }
  // Check for negative numbers
  if (num < 0) {
    if (_is_neg) {
      *_is_neg = TRUE;
    }
    num = -num; // absolute
  }

  // Process individual digits
  while (num != 0 && _len != 0) {
    I64 rem = num % 10;
    if (rem > 9) {
      buf[i++] = (rem - 10) + 'a';
    } else {
      buf[i++] = rem + '0';
    }
    num = num / 10;
    _len--;
  }

  buf[i] = '\0';
  //StrReverse(dst, i);
  return i;
}

public U64 U64ToStr(U8 *dst, U64 num, I64 len=5000)
{
  I64 i = 0;
  if (num == 0) {
    dst[i++] = '0';
    dst[i] = '\0';
    return 1;
  }

  while (num != 0 && len != 0) {
    I64 rem = num%10;
    if (rem > 9) {
      dst[i++] = (rem - 10) + 'a';
    } else {
      dst[i++] = rem + '0';
    }
    num = num / 10;
    len--;
  }

  dst[i] = '\0';
  //StrReverse(dst, i);
  return i;
}

I64 StrIntToHex(U8 *dst, I64 num)
{
  U8 *hex_str="0123456789ABCDEF";
  I64 i = (sizeof(U64) * 2) + 3, len = 2;
  dst[i-1] = '\0';
  while (i >= 2) {
    len++;
    dst[i] = hex_str[num & 0xF];
    num >>= 4;
    i--;
  }
  dst[i] = 'x';
  dst[i-1] = '0';
  return len;
}

U8 *MPrintQ(U8 *ptr,I64 flags = 0)
{ // @TempleOS
  U8 **_buf,*buf,**_dst,*dst,buf2[8],*ptr2;
  I64 ch;
  buf=MAlloc(STR_LEN);
  _buf=&buf;
  dst=buf;
  _dst=&dst;
  if (ptr)
    while (ch=*ptr++) {
      /* XXX: '$$' creates a ridiculously high number which makes the jump table
       * occupy over 10k lines in assembly as the table is not optimised to 
       * if's. Thus it is split out. */
      if (ch == '$$') {
        if (flags&PRTF_DOLLAR) {
          SPutChar(_dst,'\\',_buf);
          SPutChar(_dst,'d',_buf);
        } else {
          SPutChar(_dst,ch,_buf);
          SPutChar(_dst,ch,_buf);
        }
      } else {
        switch (ch) {
          case '%':
            SPutChar(_dst,ch,_buf);
            if (flags&PRTF_SLASH)
              SPutChar(_dst,ch,_buf);
            break;
          case '\n':
            SPutChar(_dst,'\\',_buf);
            SPutChar(_dst,'n',_buf);
            break;
          case '\r':
            SPutChar(_dst,'\\',_buf);
            SPutChar(_dst,'r',_buf);
            break;
          case '\t':
            SPutChar(_dst,'\\',_buf);
            SPutChar(_dst,'t',_buf);
            break;
          case '\v':
            SPutChar(_dst,'\\',_buf);
            SPutChar(_dst,'v',_buf);
            break;
          case '\f':
            SPutChar(_dst,'\\',_buf);
            SPutChar(_dst,'f',_buf);
            break;
          case '"':
          case '\\':
            SPutChar(_dst,'\\',_buf);
            SPutChar(_dst,ch,_buf);
            break;
          default:
            if (ch>=CH_SHIFT_SPACE && ch!=0x7F)
              SPutChar(_dst,ch,_buf);
            else {
              StrPrint(buf2,"x%02X",ch);
              ptr2=buf2;
              while (*ptr2)
                SPutChar(_dst,*ptr2++,_buf);
            }
        }
      }
    }
  SPutChar(_dst,0,_buf);
  return buf;
}

U8 *StrPrintJoin(U8 *_dst, U8 *fmt,I64 argc,I64 *argv)
{// @TempleOS - Not a 1:1 yet
  if (!fmt) return NULL;
  U64 uintarg, curlen = 0, memsize;
  F64 farg;
  I64 ch, sp = 0, intarg, U8arg, len, flags, tlen,dec_len,aux_fmt_num;
  U8 *ptr, *strarg, *tmp, *dst, buf[256];
  CDate cdt;

  if (_dst == NULL) {
    dst = MAlloc(STR_LEN);
    memsize = STR_LEN;
  } else {
    memsize = MSize(_dst);
    dst = _dst;
  }

  ptr = dst;
  while (ch = *fmt++) {
    if (ch == '%') {
      flags = 0;
      if (*fmt == '-') {
        flags|= PRTF_LEFT_JUSTIFY;
        fmt++;
      }
      if (*fmt == '0') {
        flags|=PRTF_PAD_ZERO;
        fmt++;
      }
      tlen = 0;
      while ('0'<=*fmt<='9') {
        tlen = tlen * 10 + *fmt++ -'0';
      }
      if (*fmt == '*') {
        fmt++;
        tlen = argv[sp++](U64);
      }
      dec_len = -1;
      if (*fmt == '.') {
        fmt++;
        while ('0'<=*fmt<='9') {
          dec_len = dec_len * 10 + *fmt++ -'0';
        }
        if (*fmt == '*') {
          fmt++;
          dec_len=argv[sp++];
          flags|=PRTF_TRUNCATE;
        }
        flags|=PRTF_DECIMAL;
      }
      aux_fmt_num = 0;

      switch (*fmt++) {
        case '%': {
          SPutChar(&ptr,'%',&dst);
          break;
        }

        case 'Q': {
          strarg = argv[sp++](U8*);
          strarg = MPrintQ(strarg,flags);
          OutStr(strarg,&dst,&ptr,tlen,flags);
          Free(strarg);
          break;
        }

        case 'T': {
          cdt = argv[sp++](CDate);
          strarg = MPrintTime(cdt);
          OutStr(strarg,&dst,&ptr,tlen,flags);
          Free(strarg);
          break;
        }

        case 'D': {
          cdt = argv[sp++](CDate);
          strarg = MPrintDate(cdt);
          OutStr(strarg,&dst,&ptr,tlen,flags);
          Free(strarg);
          break;
        }

        /* Presently there is only F64 */
        case 'f': {
          "Floats not supported\n";
          Exit(1);
          F64 n;
          U64 m;
          I64 k = 0, idx = 0;
          farg = argv[sp++](F64); // XXX: this should require a cast?
          if (farg < 0) {
            flags |= PRTF_NEG;
            farg=-farg;
          }

          if (dec_len < 0) {
            dec_len = 0;
          }
          idx = dec_len;
          // F64 m;
          n = log10(farg);

          if (n > 17) {
            n -= 17;
            farg *=pow(10.0,-n);
          } else {
            n = 0;
          }
          m = round(farg)(U64);
          if (dec_len) {
            buf[k++]='.';
          }

          while (idx-- && k < sizeof(buf)-8) {
            if (n) {
              n--;
              buf[k++] = '0';
            } else {
              buf[k++]= (m % 10) + '0';
            }
          }
          if (dec_len) {
            buf[k++] = '.';
          }

          do {
            if (n) {
              n--;
              buf[k++] = '0';
            } else {
              buf[k++] = (m % 10) + '0';
            }
          } while (k<sizeof(buf)-8);

          if (flags & PRTF_NEG) {
            SPutChar(&ptr,'-',&dst);
          }
          for (I64 i = k-1;i>=0;--i) {
            SPutChar(&ptr,buf[i],&dst);
          }
          break;
        }

        case 's': {
          strarg = argv[sp++](U8 *);
          OutStr(strarg,&dst,&ptr,dec_len,flags);
          break;
        }
        
        case 'F': { // a file
          tmp = argv[sp++](U8 *);
          strarg = FileRead(tmp,&len);
          OutStr(strarg,&dst,&ptr,dec_len,flags);
          Free(strarg);
          break;
        }

        case 'c': {
          uintarg = argv[sp++](U64);
          for (I64 i = 0; i < 8; ++i) {
            buf[i] = uintarg >> ((i*8) & 0xFF);
          }
          buf[8] = '\0';
          OutStr(buf,&dst,&ptr,tlen,flags);
          break;
        }

        case 'C': {
          uintarg = argv[sp++](U64);
          for (I64 i = 0; i < 8; ++i) {
            buf[i] = ToUpper(uintarg >> ((i*8) & 0xFF));
          }
          buf[8] = '\0';
          OutStr(buf,&dst,&ptr,tlen,flags);
          break;
        }

        case 'u':
        case 'd': {
          uintarg = argv[sp++](U64);
          I64 idx = 0; /* Bit of a hack as I64ToStr will set this to TRUE which
                        * is 1. */
          if (*(fmt-1) == 'd') {
            intarg = uintarg(I64);
            if (intarg < 0) {
              idx = 1;
            }
            len = I64ToStr(buf,intarg);
          } else {
            len = U64ToStr(buf,uintarg);
          }

          if (idx) {
            flags |= PRTF_NEG;
          }

          if (flags & PRTF_TRUNCATE && len + idx>tlen) {
            len = tlen-idx;
          }

          if (flags & PRTF_PAD_ZERO) {
            if (flags & PRTF_NEG) {
              SPutChar(&ptr,'-',&dst);
            }
            for (;idx < tlen-len; ++idx) {
              SPutChar(&ptr,'0',&dst);
            }
          } else {
            for (;idx < tlen-len; ++idx) {
              SPutChar(&ptr,CH_SPACE,&dst);
            }
            if (flags&PRTF_NEG) {
              SPutChar(&ptr,'-',&dst);
            }
          }
          for (idx = len - 1; idx >= 0; --idx) {
            SPutChar(&ptr,buf[idx],&dst);
          }
          break;
        }

        case 'x':
          U8arg = 'a';
          goto place_hex;
        case 'X': {
          U8arg = 'A';
place_hex:
          uintarg = argv[sp++](U64);
          len = 0;

          do {
            buf[len] = (uintarg & 15) + '0';
            if (buf[len] > '9') {
               buf[len] += U8arg - 0x3A;
            }
            len++;
            uintarg >>= 4;
          } while (uintarg);

          if (flags & PRTF_TRUNCATE && len > tlen) {
            len = tlen;
          }

          if (flags & PRTF_PAD_ZERO) {
            for (I64 idx = 0; idx < tlen-len; ++idx) {
              SPutChar(&ptr,'0',&dst);
            }
          } else {
            for (I64 idx = 0; idx < tlen-len; ++idx) {
              SPutChar(&ptr,CH_SPACE,&dst);
            }
          }
          for (I64 idx = len - 1; idx >= 0; --idx) {
            SPutChar(&ptr,buf[idx],&dst);
          }
          break;
        }

        case 'p': {
          uintarg = argv[sp++](U64);
          len = StrIntToHex(buf,uintarg);
          OutStr(buf,&dst,&ptr,tlen,flags);
          break;
        }

        default:
          "Invalid format char: '%c'\n",*fmt;
          SPutChar(&ptr,ch,&dst);
          break;
      }
    } else {
      SPutChar(&ptr,ch,&dst);
    }
  }
  return dst;
}

public U8 *StrMergeJoin(I64 argc, U64 *argv)
{
  U8 *buf = MAlloc(256), *dst = buf;
  for (I64 i = 0; i < argc; ++i) {
    OutStr(argv[i](U8*),&buf,&dst,0,0);
  }
  return buf;
}

public U8 *StrMerge(...)
{
  return StrMergeJoin(argc,argv);
}

U8 *StrPrint(U8 *dst=NULL,U8 *fmt,...)
{// @TempleOS
  return StrPrintJoin(dst,fmt,argc,argv);
}

U8 *MStrPrint(U8 *fmt,...)
{// @TempleOS - MAlloc StrPrint.
  U8 *res,*buf=StrPrintJoin(NULL,fmt,argc,argv);
  res=StrNew(buf);
  Free(buf);
  return res;
}

U8 *MPrintTime(CDate cdt)
{// @TempleOS
  CDateStruct ds;
  Date2Struct(&ds,cdt);
  return MStrPrint("%02d:%02d:%02d",ds.hour,ds.min,ds.sec);
}

U8 *MPrintDate(CDate cdt)
{// @TempleOS
  CDateStruct ds;
  Date2Struct(&ds,cdt);
  return MStrPrint("%02d/%02d/%04d",ds.mon,ds.day_of_mon,ds.year);
}


U8 *MPrintTimestampNow()
{// Get a timestamp
  I64 seconds = NowMilliseconds() / 1000; 
  U8 *timestamp = MAlloc(sizeof(U8)*32);
  auto ptm = localtime(&seconds);
  auto len = snprintf(timestamp,sizeof(U8)*32,TIMESTAMP_FMT,
          ptm->tm_year+1900,ptm->tm_mon+1,ptm->tm_mday,
          ptm->tm_hour,ptm->tm_min,ptm->tm_sec);
  timestamp[len] = '\0';
  return timestamp;
}

U8 *CatPrint(U8 *_dst,U8 *fmt,...)
{// @TempleOS
  U8 *s2 = StrPrintJoin(NULL,fmt,argc,argv);
  U8 *s = StrPrint(NULL,"%s%s",_dst,s2);
  Free(s2);
  return s;
}

U8 *CatLenPrint(U8 *_dst, I64 *_len, U8 *fmt, ...)
{
  I64 len = *_len;
  U8 *s2 = StrPrintJoin(NULL,fmt,argc,argv);
  I64 s2len = StrLen(s2);
  I64 size = MSize(_dst);
  if (len + s2len >= size) {
    _dst = ReAlloc(_dst,(size*2)+s2len);
  }
  MemCpy(_dst+len,s2,s2len);
  len+=s2len;
  _dst[len] = '\0';
  *_len = len;
  return _dst;
}

I64 days[] =
{ 'sun', 'mon', 'tue', 'wed', 'thu', 'fri', 'sat' };
I64 months[] =
{ 'jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec' };
/* similar to strptime  */
Bool Str2CDateStruct(U8 *strdate, U8 *format, CDateStruct *t)
{
  /* initialise the date struct's field to -1, error state */
  MemSet(t, -1, sizeof(CDateStruct));
  /* This is for when we pass the string to strtoll */
  U8 *endptr = strdate;
  I64 day,tmp_day,mon,tmp_mon;
  t->sec10000 = 0;
  t->sec100 = 0;
  t->min = 0;
  t->hour = 0;

  while (*strdate && *format) {
    if (*format == '%') {
      format++;
      switch (*format) {
        case 'a':
          tmp_day = 0;
          tmp_day = ToLower((strdate[0] << 16)) |
                    ToLower((strdate[1] <<  8)) |
                    ToLower(strdate[2]);
          for (I64 i = 0; i < 7; ++i) {
            if (tmp_day == days[i]) {
              strdate += 3;
              t->day_of_week = i;
              break;
            }
          }

          // full name of day
          if (strdate[0] == 'd' || strdate[0] == 'D') {
            strdate += 2;
          }
          if (t->day_of_week == -1) {
            return FALSE;
          }
          break;

        case 'b': /* abbreviation of month string */
          tmp_mon = ToLower((strdate[0] << 16)) |
                    ToLower((strdate[1] <<  8)) |
                    ToLower(strdate[2]);
          for (I64 i = 0; i < 12; ++i) {
            if (tmp_mon == months[i]) {
              strdate += 2;
              t->mon = i;
              break;
            }
          }
          if (t->mon == -1) {
            return 0;
          }
          break;

        case 'd': /* day of the month */
          t->day_of_mon = strtoll(strdate, &endptr);
          if (strdate == endptr) {
            return 0;
          }
          strdate = endptr;
          strdate += 1;
          break;

        case 'm': /* month as a number */
          t->mon = strtoll(strdate, &endptr);
          if (strdate == endptr) {
            return 0;
          }
          strdate = endptr;
          strdate += 1;
          break;

        case 'Y': /* Year */
          t->year = strtoll(strdate, &endptr);
          if (strdate == endptr) {
            return 0;
          }
          strdate = endptr;
          strdate += 1;
          break;

        case 'H': /* Hour */
          t->hour = strtoll(strdate, &endptr);
          if (strdate == endptr) {
            return 0;
          }
          strdate = endptr;
          strdate += 1;
          break;

        case 'M': /* Minutes */
          t->min = strtoll(strdate, &endptr);
          if (strdate == endptr) {
            return 0;
          }
          strdate = endptr;
          strdate += 1;
          break;

        case 'p': /* am or pm */
          if (StrNICmp(strdate, "AM", 2) == 0) {
            if (t->hour >= 12) {
              t->hour -= 12;
            }
          } else if (StrNICmp(strdate, "PM", 2) == 0) {
            t->hour += 12;
          }
          if (strdate == endptr) {
            return 0;
          }
          strdate += 1;
          break;

        case 's': /* seconds */
          t->sec = strtoll(strdate, &endptr);
          if (strdate == endptr) {
            return 0;
          }
          strdate = endptr;
          strdate += 1;
          break;

        case 'z': /* timezone difference */
          t->zone_diff = strtoll(strdate, &endptr);
          if (strdate == endptr) {
            return 0;
          }
          strdate = endptr;
          strdate += 1;
          break;

        default:
          break;
      }
    }

    /* This means we parsed a number and it was the end of the string
     * That can be considered a successful parse for us. We can always
     * check if the field was populated by ensuring it is not -1 */
    if (*endptr == '\0') {
      break;
    }

    if (*format == *strdate) {
      strdate++;
    }
    format++;
  }
  /* We did it! */
  return TRUE;
}

CDate StrToCDate(U8 *_str, U8 *fmt)
{
  CDateStruct ds;
  CDate cdt;
  Bool ok;
  if ((ok = Str2CDateStruct(_str,fmt,&ds)) == FALSE) {
    return FALSE;
  }
  cdt = Struct2Date(&ds);
  return cdt;
}

Bool WildMatch(U8 *test_str, U8 *wild_str)
{// @TempleOS Wildcard match with '*' and '?'.
  I64 ch1, ch2;
  U8 *fall_back_src = NULL, *fall_back_wild = NULL;
  while (TRUE) {
    if (!(ch1 = *test_str++)) {
      if (*wild_str && *wild_str != '*')
        return FALSE;
      else
        return TRUE;
    } else {
      if (!(ch2 = *wild_str++))
        return FALSE;
      else {
        if (ch2 == '*') {
          fall_back_wild = wild_str - 1;
          fall_back_src = test_str;
          if (!(ch2 = *wild_str++)) {
            return TRUE;
          }
          while (ch2 != ch1) {
            if (!(ch1 = *test_str++)) {
              return FALSE;
            }
          }
        } else if (ch2 != '?' && ch1 != ch2) {
          if (fall_back_wild) {
            wild_str = fall_back_wild;
            test_str = fall_back_src;
            fall_back_wild = NULL;
            fall_back_src = NULL;
          } else
            return FALSE;
        }
      }
    }
  }
}
