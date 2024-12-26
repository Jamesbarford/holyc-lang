/* All prototypes and classes for all functions and classes provided
 * by the library */
/* XXX: now we have a JSON parser it should be easier to generate this code, 
 * JSON might be easier to maintain than this ever growing header file? */
#define SF_ARG1 8
#define SF_ARG2 16
#define SF_ARG3 24
#define SF_ARG4 32
#define SF_ARG5 40
#define SF_ARG6 48
#define SF_ARG7 52
#define SF_ARG8 56

#define TRUE  1
#define FALSE 0
#define NULL  0

#define STDOUT 0
#define STDERR 1
#define STDIN  2

#define __BUFSIZ__ 2048

#define EXIT_FAIL 1
#define EXIT_OK   0

#define I64_MIN 0x8000000000000000
#define I64_MAX 0x7fffffffffffffff
#define U64_MAX 0xffffffffffffffff
#define U8_MAX  0xFF
#define I8_MAX  0x7F
#define I8_MIN  0xFFFFFF80

public class List;
public class List
{
  U0 *value;
  List *next, *prev;
};

public U0 ListInit(List *l);
public List *ListNew();
public U0 ListPrepend(List *head, U0 *value);
public Bool ListEmpty(List *l);
public U0 ListAppend(List *head, U0 *value);
public U0 *ListDeque(List *l);
public U0 ListInsertBefore(List *ll, List *new_node);
public U0 ListInsertValueBefore(List *ll, U0 *value);
public U0 *ListPop(List *l);
public U0 ListRelease(List *ll, U0 (*FreeValue)(U0 *arg));
public I64 ListCount(List *ll);
public U0 ListMergePrepend(List *l1, List *l2);
public U0 ListMergeAppend(List *l1, List *l2);

#define CSV_EOF   0
#define CSV_ROW   1
#define CSV_FIELD 2

public extern "c" I32 open(U8 *__name, I32 flags, ...);
public extern "c" I32 creat(U8 *__name, U16 __flags);
public extern "c" I32 fcntl(I32 __fd, I32 __flags, ...);
public extern "c" I32 close(I32 __fd);
public extern "c" I64 read(I32 __fd, U0 *__buf, U64 __nbyte);
public extern "c" I64 write(I32 __fd, U0 *__buf, U64 __nbyte);
public extern "c" I64 lseek(I32 __fd, I64 __offset, I32 __whence);
public extern "c" I32 chdir(U8 *path);

public class CsvParser {
  U8 *csv;
  U64 len, off, row_off, nrows, nfields;
  I64 state;
};

public class CsvSlice {
  U64 idx, off, len;
};
public U0 CsvParserInit(CsvParser *p, U8 *csv, U64 len);
public I64 CsvParse(CsvParser *p, CsvSlice *s);

#define CDATE_YEAR_DAYS        365.24225
#define CDATE_YEAR_DAYS_INT    36524225
#define CDATE_BASE_DAY_OF_WEEK 0

public class timeval
{ // c compatability
  I64 tv_sec;        /* seconds since Jan. 1, 1970 */
  I32 tv_usec;         /* and microseconds */
};

public class tm 
{     // c compatability
  I32 tm_sec;  /* seconds after the minute [0-60] */
  I32 tm_min;  /* minutes after the hour [0-59] */
  I32 tm_hour;   /* hours since midnight [0-23] */
  I32 tm_mday;   /* day of the month [1-31] */
  I32 tm_mon;  /* months since January [0-11] */
  I32 tm_year;   /* years since 1900 */
  I32 tm_wday;   /* days since Sunday [0-6] */
  I32 tm_yday;   /* days since January 1 [0-365] */
  I32 tm_isdst;  /* Daylight Savings Time flag */
  I64 tm_gmtoff; /* offset from UTC in seconds */
  I8 *tm_zone;   /* timezone abbreviation */
};

public extern "c" tm *localtime(I64 *clock);
public extern "c" I32 gettimeofday(timeval *tp, U0 *tzp);

public I64 class CDate {
  U32 time;
  I32 date;
};

public class CDateStruct {
  U64 sec10000, sec100, sec, min, hour, day_of_week, day_of_mon, mon;
  I32 year;
  I64 zone_diff;
};

public U64 NowMilliseconds();
public U0 NowDateTimeStruct(CDateStruct *_ds);
public I64 YearStartDate(I64 year);
public I64 DayOfWeek(I64 i);
public U0 Date2Struct(CDateStruct *_ds, CDate cdt);
public CDate Struct2Date(CDateStruct *_ds);
public I32 FirstDayOfMon(I64 i);
public I64 LastDayOfMon(I64 i);
public I32 FirstDayOfYear(I64 i);
public I64 LastDayOfYear(I64 i);
public CDate Now();

#define HT_LOAD  0.60
#define HT_DELETED 0x7fffffffffffffff
#define HT_PROBE_1 1
#define HT_PROBE_3 3

public class IntVec
{
  I64 size;
  I64 *entries;
};

public class PtrVec
{
  I64 size;
  U0 **entries;
};

public class FloatVec 
{
  I64 size;
  F64 *entries;
};

U0 QSortGeneric(U0 **arr, I64 high, I64 low, 
    I64 (*_compare_fn)(U0 *_arg1, U0 *_arg2));

public FloatVec *FloatVecNew(I64 size = 32);
public U64 FloatVecCapacity(IntVec *vec);
public U0 FloatVecPush(FloatVec *vec, F64 value);
public F64 FloatVecPop(FloatVec *vec, Bool *_ok);
public F64 FloatVecGet(FloatVec *vec, I64 idx);
public U0 FloatVecClear(FloatVec *vec);
public U0 FloatVecRelease(FloatVec *vec);
public U0 FloatVecSort(FloatVec *vec);

public IntVec *IntVecNew(I64 size = 32);
public U64 IntVecCapacity(IntVec *vec);
public U0 IntVecPush(IntVec *vec, I64 value);
public I64 IntVecPop(IntVec *vec, Bool *_ok);
public I64 IntVecGet(IntVec *vec, I64 idx);
public I64 IntVecClear(IntVec *vec);
public I64 IntVecRelease(IntVec *vec);
public U0 IntVecSort(IntVec *vec);

public PtrVec *PtrVecNew(I64 size = 32);
public U64 PtrVecCapacity(IntVec *vec);
public U0 PtrVecPush(PtrVec *vec, U0 *value);
public U0 *PtrVecPop(PtrVec *vec, Bool *_ok);
public U0 *PtrVecGet(PtrVec *vec, I64 idx);
public I64 PtrVecClear(PtrVec *vec);
public I64 PtrVecRelease(PtrVec *vec, U0 (*_free_value)(U0 *_val) = NULL);
public U0 PtrVecSort(PtrVec *vec,
                     I64 (*_compare_fn)(U0 *_arg1, U0 *_arg2))

public class IntMapNode
{
  I64 key;
  U0 *value;
};

public class StrMapNode
{
  U8 *key;
  I64 key_len;
  U0 *value;
};

public class IntMap
{
  U64 size;     /* How many entries are in the hashtable */
  U64 capacity; /* How much capacity we have in the entries array */
  U64 mask;     /* Used for hashing, as the capacity is always a power of 2
                 * we can use fast modulo of `<int> & capacity-1`. */
  IntVec *indexes; /* Where all of the values are in the entries array, in 
                 * insertion order. Means we can iterate over the HashTable 
                 * quickly at the cost of memory */
  U64 threashold; /* rebuild threashold */
  U0 (*_free_value)(U0 *value); /* User defined callback for freeing values */
  IntMapNode **entries; /* All of the entries, XXX: could this be IntMapNode *entries? */
};


public class StrMap
{
  U64 size;     /* How many entries are in the hashtable */
  U64 capacity; /* How much capacity we have in the entries array */
  U64 mask;     /* Used for hashing, as the capacity is always a power of 2
                 * we can use fast modulo of `<int> & capacity-1`. */
  IntVec *indexes; /* Where all of the values are in the entries array, in 
                 * insertion order. Means we can iterate over the HashTable 
                 * quickly at the cost of memory */
  U64 threashold; /* rebuild threashold */
  U0 (*_free_value)(U0 *_value); /* User defined callback for freeing values */
U0 (*_free_key)(U0 *_key); /* User defined callback for freeing keys */
  StrMapNode **entries; /* All of the entries, XXX: could this be IntMapNode *entries? */
};

public StrMap *StrMapNew(U64 capacity = 32);
public Bool StrMapSet(StrMap *map, U8 *key, U0 *value);
public Bool StrMapHasLen(StrMap *map, U8 *key, I64 key_len);
public Bool StrMapHas(StrMap *map, U8 *key);
public U0 *StrMapGetLen(StrMap *map, U8 *key, I64 key_len);
public U0 *StrMapGet(StrMap *map, U8 *key);
public U0 StrMapSetFreeValue(StrMap *map, U0 (*_free_value)(U0 *value));
public U0 StrMapSetFreeKey(StrMap *map, U0 (*_free_key)(U0 *key));
public U0 StrMapRelease(StrMap *map);
public Bool StrMapResize(StrMap *map, U64 size);
public Bool StrMapIter(StrMap *map, I64 *_idx, StrMapNode **_node);
public Bool StrMapValueIter(StrMap *map, I64 *_idx, U0 **_value);
public Bool StrMapKeyIter(StrMap *map, I64 *_idx, U8 **_key);
public U8 *StrMapToString(StrMap *map, U8 *(*stringify_value)(U0 *));
public U8 *StrMapKeysToString(StrMap *map);


public Bool IntMapSet(IntMap *map, I64 key, U0 *value);
public Bool IntMapHas(IntMap *map, I64 key);
public U0 *IntMapGet(IntMap *map, I64 key);
public IntMap *IntMapNew(U64 capacity = 32);
public U0 IntMapSetFreeValue(IntMap *map, U0 (*_free_value)(U0 *value));
public U0 IntMapRelease(IntMap *map);
public Bool IntMapResize(IntMap *map, U64 size);
public Bool IntMapIter(IntMap *map, I64 *_idx, IntMapNode **_node);
public Bool IntMapValueIter(IntMap *map, I64 *_idx, U0 **_value);
public Bool IntMapKeyIter(IntMap *map, I64 *_idx, I64 *_key);
public U8 *IntMapToString(IntMap *map, U8 *(*stringify_value)(U0 *));
public U8 *IntMapKeysToString(IntMap *map);

/* THREADS ==========*/
#ifdef IS_MACOS
#define __MUTEX_LOCK_PADDING__   56
#define __MUTEX_LOCK_ATTR_SIZE__ 8

#define __THREAD_COND_SIZE__     40
#define __THREAD_CONDATTR_SIZE__ 8
#define __THREAD_SIZE__          8176
#define __THREAD_ATTR_SIZE__     56

/* MACOS definitions */
class pthread_mutex_t 
{
  I64 __sig;
  I8 __opaque[__MUTEX_LOCK_PADDING__];
};

class pthread_mutexattr_t
{
  I64 __sig;
  I8 __opaque[__MUTEX_LOCK_ATTR_SIZE__];
};

class pthread_cond_t
{
  I64 __sig;
  I8 __opaque[__THREAD_COND_SIZE__];
};

class pthread_condattr_t
{
  I64 __sig;
  I8 __opaque[__THREAD_CONDATTR_SIZE__];
};

class pthread_handler_rec;
class pthread_handler_rec
{
  U0 (*__routine)(U0 *__arg); // Routine to call
  U0 *__arg;                  // Argument to pass
  pthread_handler_rec *__next;
};

class pthread_attr_t
{  
  I64 __sig;
  I8 __opaque[__THREAD_ATTR_SIZE__];
};

class pthread_t 
{
  I64 __sig;
  pthread_handler_rec  *__cleanup_stack;
  I8 __opaque[__THREAD_SIZE__];
};

#elifdef IS_LINUX /* Linux definitions, this needs to be auto generated */

#define __SIZEOF_PTHREAD_MUTEX_T   40
#define __SIZEOF_PTHREAD_ATTR_T    56
#define __SIZEOF_PTHREAD_RWLOCK_T  56
#define __SIZEOF_PTHREAD_BARRIER_T 32

#define __SIZEOF_PTHREAD_MUTEXATTR_T   4
#define __SIZEOF_PTHREAD_COND_T        48
#define __SIZEOF_PTHREAD_CONDATTR_T    4
#define __SIZEOF_PTHREAD_RWLOCKATTR_T  8
#define __SIZEOF_PTHREAD_BARRIERATTR_T 4

union pthread_mutexattr_t
{
  I8  __size[__SIZEOF_PTHREAD_MUTEXATTR_T];
  I32 __align;
};

union pthread_condattr_t
{
  I8  __size[__SIZEOF_PTHREAD_CONDATTR_T];
  I32 __align;
};

union pthread_attr_t
{
  I8  __size[__SIZEOF_PTHREAD_ATTR_T];
  I64 __align;
};


/* this is the spiritual equivilent of typedef  */ 
public class pthread_t {
  U64 __padding;
};

class __pthread_list_t;
class __pthread_list_t
{
  __pthread_list_t *__prev;
  __pthread_list_t *__next;
};

class __pthread_mutex_s
{
  I32 __lock;
  U32 __count;
  I32 __owner;
  U32 __nusers;
  I32 __kind;
  I16 __spins;
  I16 __elision;
  __pthread_list_t __list;
};

union pthread_mutex_t 
{
  __pthread_mutex_s __data;
  I32 __size[__SIZEOF_PTHREAD_MUTEX_T];
  I64 __align;
};

union __atomic_wide_counter
{
  U64 __value64;
  class
  {
    U32 __low;
    U32 __high;
  };
};

class __pthread_cond_s
{
  __atomic_wide_counter __wseg;
  __atomic_wide_counter __g1_start;
  U32 __g_regs[2];
  U32 __g_size[2];
  U32 __g1_orig_size;
  U32 __wrefs;
  U32 __g_signals[2];
};

union pthread_cond_t
{
  __pthread_cond_s __data;
  I8 __size[__SIZEOF_PTHREAD_COND_T];
  I64 __align;
};
#endif

class ThreadWorker
{
  pthread_t *th;
  I32 id;
};

class ThreadSemaphore
{
  pthread_cond_t cond;
  pthread_mutex_t lk;
  I32 val;
}; 

public class ThreadWorkerJob;
public class ThreadWorkerJob {
  U0 *(*callback)(U0 *priv_data, U0 *argv);
  U0 *argv;
  ThreadWorkerJob *next;
};

public class ThreadPool {
  U64 worker_count, alive_threads, active_threads;
  Bool run;
  pthread_cond_t has_work, no_work;
  pthread_mutex_t lock, qlock;
  ThreadSemaphore *sem;
  ThreadWorker *workers;
  List *jobs;
  U0 *priv_data;
};

public extern "c" I64 pthread_create(pthread_t **th, U0 *thread_attr = NULL,
                U0 *(*function_ptr)(U0 *_argv), U0 *argv = NULL);
public extern "c" I32 pthread_mutex_init(pthread_mutex_t *lk,
                  pthread_mutexattr_t *mutex_attr = NULL);
public extern "c" I32 pthread_mutex_destroy(pthread_mutex_t *lk);
public extern "c" I32 pthread_mutex_lock(pthread_mutex_t *lk);
public extern "c" I32 pthread_mutex_unlock(pthread_mutex_t *lk);
public extern "c" I32 pthread_join(pthread_t *th, U0 *ptr = NULL);
public extern "c" I32 pthread_cond_broadcast(pthread_cond_t *cond);
public extern "c" I32 pthread_cond_destroy(pthread_cond_t *cond);
public extern "c" I32 pthread_cond_init(pthread_cond_t *cond, pthread_condattr_t *attr = NULL);
public extern "c" I32 pthread_cond_signal(pthread_cond_t *cond);
public extern "c" I32 pthread_cond_wait(pthread_cond_t *cond, pthread_mutex_t *lk);
public extern "c" I32 pthread_exit(U0 *data = NULL);
public extern "c" I32 pthread_detach(pthread_t *th);
public extern "c" I32 pthread_attr_setstacksize(pthread_attr_t *attr, U64 stacksize);
public extern "c" I32 pthread_attr_init(pthread_attr_t *attr);

ThreadPool *ThreadPoolNew(I64 worker_count);
public U0 ThreadPoolEnqueue(ThreadPool *pool, U0 *argv,
           U0 (*callback)(U0 *priv_data, U0 *argv));
public U0 ThreadPoolWait(ThreadPool *pool);
public ThreadPool *ThreadPoolNew(I64 worker_count);
public U0 ThreadPoolStop(ThreadPool *pool);
public U0 ThreadPoolRelease(ThreadPool *pool);

public extern "c" I32 wait(I32 *statloc);
public extern "c" I32 waitpid(I32 pid, I32 *statlock, I32 options);
public extern "c" I32 fork();
public _extern _EXIT U0 Exit(I64 exit_code = EXIT_FAIL);
public _extern _WRITE I64 Write(I64 fd, U8 *buf, I64 len);
public _extern _SYSTEM I64 System(U8 *command);

public extern "c" F64 acos(F64 f1);
public extern "c" F64 asin(F64 f1);
public extern "c" F64 atan(F64 f1);
public extern "c" F64 atan2(F64 f1, F64 f2);
public extern "c" F64 cos(F64 f1);
public extern "c" F64 sin(F64 f1);
public extern "c" F64 tan(F64 f1);
public extern "c" F64 acosh(F64 f1);
public extern "c" F64 asinh(F64 f1);
public extern "c" F64 atanh(F64 f1);
public extern "c" F64 cosh(F64 f1);
public extern "c" F64 sinh(F64 f1);
public extern "c" F64 tanh(F64 f1);
public extern "c" F64 exp(F64 f1);
public extern "c" F64 exp2(F64 f1);
public extern "c" F64 expm1(F64 f1);
public extern "c" F64 log(F64 f1);
public extern "c" F64 log10(F64 f1);
public extern "c" F64 log2(F64 f1);
public extern "c" F64 log1p(F64 f1);
public extern "c" F64 logb(F64 f1);
public extern "c" F64 modf(F64 f1, F64 *fptr);
public extern "c" F64 ldexp(F64 f1, I32 i);
public extern "c" F64 frexp(F64 f1, I32 *i);
public extern "c" I32 ilogb(F64 f1);
public extern "c" F64 scalbn(F64 f1, I32 i);
public extern "c" F64 scalbln(F64 f1, I64 i);
public extern "c" F64 fabs(F64 f1);
public extern "c" F64 cbrt(F64 f1);
public extern "c" F64 hypot(F64 f1, F64 f2);
public extern "c" F64 pow(F64 f1, F64 f2);
public extern "c" F64 sqrt(F64 f1);
public extern "c" F64 erf(F64 f1);
public extern "c" F64 erfc(F64 f1);
public extern "c" F64 lgamma(F64 f1);
public extern "c" F64 tgamma(F64 f1);
public extern "c" F64 ceil(F64 f1);
public extern "c" F64 floor(F64 f1);
public extern "c" F64 nearbyint(F64 f1);
public extern "c" F64 rint(F64 f1);
public extern "c" I64 lrint(F64 f1);
public extern "c" F64 round(F64 f1);
public extern "c" I64 lround(F64 f1);
public extern "c" F64 trunc(F64 f1);
public extern "c" F64 fmod(F64 f1, F64 f2);
public extern "c" F64 remainder(F64 f1, F64 f2);
public extern "c" F64 remquo(F64 f1, F64 f2, I32 *ptr);
public extern "c" F64 copysign(F64 f1, F64 f2);
public extern "c" F64 nan(U8 *s);
public extern "c" F64 nextafter(F64 f1, F64 f2);
public extern "c" F64 fdim(F64 f1, F64 f2);
public extern "c" F64 fmax(F64 f1, F64 f2);
public extern "c" F64 fmin(F64 f1, F64 f2);
public extern "c" F64 fma(F64 f1, F64 f2, F64 f3);
public _extern _ABS U64 Abs(I64 num);
public _extern _GCD U64 Gcd(I64 a, I64 b);
public F64 Pow(F64 a, I64 b);
public U64 ModU64(U64 *k, U64 m);

public extern "c" U0 *memchr(U0 *__s, I32 __c, U64 __n);
public extern "c" I32 memcmp(U0 *__s1, U0 *__s2, U64 __n);
public extern "c" U0 *memmove(U0 *__dst, U0 *__src, U64 __len);
public extern "c" U0 free(U0 *ptr);
public extern "c" U0 *malloc(U64 size);
public _extern _MALLOC U0 *MAlloc(U64 size);
public _extern _FREE U0 Free(U0 *ptr);
public _extern _REALLOC U0 *ReAlloc(U0 *ptr, U64 new_size);
public _extern _CALLOC U0 *CAlloc(U64 new_size);
public _extern _MSIZE U64 MSize(U0 *ptr);
public _extern _MEMCPY U0 *MemCpy(U0 *dst, U0 *src, U64 len);
public _extern _MEMSET U0 *MemSet(U0 *dst, I32 ch, U64 len);

#define STR_LEN 256

#define PRTF_PAD_ZERO        0x001
#define PRTF_LEFT_JUSTIFY    0x002
#define PRTF_TRUNCATE        0x004
#define PRTF_COMMA           0x008
#define PRTF_DOLLAR          0x010
#define PRTF_SLASH           0x020
#define PRTF_QUESTION        0x040
#define PRTF_AUX_FMT_NUM     0x080
#define PRTF_DECIMAL         0x100
#define PRTF_NEG             0x200
#define PRTF_NEG_E           0x400
#define PRTF_NEG_AUX_FMT_NUM 0x800

#define CH_BACKSPACE   0x08
#define CH_ESC         0x1B
#define CH_SHIFT_ESC   0x1C
#define CH_SHIFT_SPACE 0x1F
#define CH_SPACE       0x20

extern "c" F64 strtod(U8 *nptr, U8 **endptr);
extern "c" I64 strtoll(U8 *nptr, U8 **endptr, I64 base = 10);
extern "c" U64 strtoul(U8 *nptr, U8 **endptr, I64 base = 10);
extern "c" I64 snprintf(I8 *nptr, U64 len, I8 *fmt, ...);
extern "c" I64 printf(I8 *fmt, ...);
extern "c" I64 strlen(U8 *s);
extern "c" I64 strnlen(U8 *s, I64 size);
extern "c" U8 *strcat(U8 *__s1, U8 *__s2);
extern "c" U8 *strchr(U8 *__s, I32 __c);
extern "c" I32 strcmp(U8 *__s1, U8 *__s2);
extern "c" I32 strcasecmp(U8 *__s1, U8 *__s2);
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
extern "c" U8 *strdup(U8 *__s);
extern "c" U64 strspn(U8 *__s, U8 *__u8set);
extern "c" U8 *strstr(U8 *__big, U8 *__little);
extern "c" U8 *strtok(U8 *__str, U8 *__sep);
extern "c" U64 strxfrm(U8 *__s1, U8 *__s2, U64 __n);

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

public U8 *StrNCpy(U8 *buf, I64 len);
public U8 *FileExtDot(U8 *src);
public U8 *FileExtRem(U8 *src, U8 *dst = NULL);
public U8 *StrTrimStart(U8 *str, I64 str_len=-1, I64 *_new_len=NULL);
public U8 *StrTrimEnd(U8 *str, I64 str_len=-1, I64 *_new_len=NULL);
public Bool IsDotZ(U8 *filename);
public Bool IsDotC(U8 *filename);
public U0 SPutChar(U8 **_dst, U8 ch, U8 **_buf);
public U0 OutStr(U8 *instr, U8 **_buf, U8 **_dst, I64 len, I64 flags);
public I64 StrOcc(U8 *src, I64 ch);
public U8 *StrFirstOcc(U8 *src, U8 *marker);
public U8 *StrLastOcc(U8 *src, U8 *marker);
public U8 *StrLastRem(U8 *src, U8 *marker, U8 *dst = NULL);
public U0 StrReverse(U8 *buf, I64 length);
public I64 I64ToStr(U8 *buf, I64 num, Bool *_is_neg = NULL, I64 _len = 5000);
public U64 U64ToStr(U8 *dst, U64 num, I64 len = 5000);
public U8 *StrMergeJoin(I64 argc, U64 *argv);
public U8 *StrMerge(...);
public U8 *StrPrint(U8 *dst=NULL, U8 *fmt, ...);
public U8 *StrPrintJoin(U8 *_dst, U8 *fmt, I64 argc, I64 *argv);
public U8 *MPrintDate(CDate cdt);
public U8 *MPrintTime(CDate cdt);
public U8 *MPrintTimestampNow();
public U8 *StrNew(U8 *buf);
public U8 *StrFirstRem(U8 *src, U8 *marker, U8 *dst = NULL);
public I64 StrIntToHex(U8 *dst, I64 num);
public U8 *MPrintQ(U8 *ptr, I64 flags = 0);
public U8 *MStrPrint(U8 *fmt, ...);
public U8 *CatPrint(U8 *_dst, U8 *fmt, ...);
public U8 *CatLenPrint(U8 *_dst, I64 *_len, U8 *fmt, ...);
public Bool Str2CDateStruct(U8 *strdate, U8 *format, CDateStruct *t);
public Bool WildMatch(U8 *test_str, U8 *wild_str);
public CDate StrToCDate(U8 *_str, U8 *fmt);

#define F_DUPFD 0 /* Duplicate a file descriptor */
#define F_GETFD 1 /* Retrieve file descriptor flags */
#define F_SETFD 2 /* Assign flags to file descriptor */
#define F_GETFL 3 /* Get file status flags */
#define F_SETFL 4 /* Assign file status flags */

#define F_FLUSH_DATA  40 /* Commit data to disk */
#define F_CHKCLEAN    41 /* Verify file is not modified */
#define F_PREALLOCATE 42 /* Allocate space for file in advance */
#define F_SETSIZE     43 /* Adjust file size */
#define F_RDADVISE    44 /* Suggest read behavior */
#define F_RDAHEAD     45 /* Enable read ahead */

#define FD_CLOEXEC 1  /* Close file descriptor on exec */

#ifdef IS_LINUX
  #define F_GETOWN 0x9 /* Retrieve SIGIO/SIGURG process/group */
  #define F_SETOWN 0x8 /* Set SIGIO/SIGURG process/group */
  #define F_GETLK  0x5 /* Query record locking info */
  #define F_SETLK  0x6 /* Set record locking info without waiting */
  #define F_SETLKW 0x7 /* Set record locking info with waiting */
#else
  #define F_GETOWN 0x5
  #define F_SETOWN 0x6
  #define F_GETLK  0x7
  #define F_SETLK  0x8
  #define F_SETLKW 0x9
#endif

/* record locking flags */
#ifdef IS_LINUX
  #define F_RDLCK 0 /* Establish a read lock */
  #define F_WRLCK 1 /* Remove a lock */
  #define F_UNLCK 2 /* Establish a write lock */
#else
  #define F_RDLCK 1
  #define F_UNLCK 2
  #define F_WRLCK 3
#endif

#define O_RDONLY  0x0000 /* Open exclusively for reading */
#define O_WRONLY  0x0001 /* Open exclusively for writing */
#define O_RDWR    0x0002 /* Open for reading and writing */
#define O_ACCMODE 0x0003 /* Mask to extract access modes */

/* Record locking flags */
#ifdef IS_LINUX
  #define O_CREAT    0x040 /* Create file if it doesn't exist */
  #define O_TRUNC    0x200 /* Truncate existing file to zero length */
  #define O_EXCL     0x080 /* Error if file already exists */
  #define O_NONBLOCK 0x800 /* Non-blocking mode */
  #define O_APPEND   0x400 /* Append data to the end of the file */
#else
  #define O_CREAT    0x00000200
  #define O_TRUNC    0x00000400
  #define O_EXCL     0x00000800
  #define O_NONBLOCK 0x00000004
  #define O_APPEND   0x00000008
#endif

#define SEEK_SET 0 /* Set offset to specified value */
#define SEEK_CUR 1 /* Set offset to current position plus value */
#define SEEK_END 2 /* Set offset to size of file plus value */

/*
 * The iovec class represents a buffer in memory, with a base address and a length.
 * It's typically used for scatter-gather I/O operations to manage multiple buffers.
 */
class iovec
{
  U0 *iov_base;  /* Pointer to data buffer */
  U64 iov_len;   /* Length of the buffer */
};

public extern "c" I32 open(U8 *__name, I32 flags, ...);
public extern "c" I32 creat(U8 *__name, U16 __flags);
public extern "c" I32 fcntl(I32 __fd, I32 __flags, ...);
public extern "c" I32 close(I32 __fd);
public extern "c" I64 read(I32 __fd, U0 *__buf, U64 __nbyte);
public extern "c" I64 write(I32 __fd, U0 *__buf, U64 __nbyte);
public extern "c" I64 lseek(I32 __fd, I64 __offset, I32 __whence);
public extern "c" I32 chdir(U8 *path);

public U8 *FileRead(U8 *path, I64 *_size = NULL);
public Bool FileWrite(U8 *filename, U8 *buf, I64 size, I64 flags = O_CREAT | O_RDWR);
public I32 Cd(U8 *path);

#define DT_UNKNOWN 0  /* Type not determined */
#define DT_FIFO    1  /* FIFO/pipe */
#define DT_CHR     2  /* Character device */
#define DT_DIR     4  /* Directory */
#define DT_BLK     6  /* Block device */
#define DT_REG     8  /* Regular file */
#define DT_LNK    10  /* Symbolic link */
#define DT_SOCK   12  /* Socket */
#define DT_WHT    14  /* Whiteout */

class cDIR;

#ifdef IS_MACOS
#define DIR_MAXPATHLEN (1024)

class Dirent {
  U64 ino;
  U64 off;
  U16 reclen;        /* length of this record */
  U16 namlen;        /* length of string in d_name */
  U8 type;         /* file type, see below */
  I8 name[DIR_MAXPATHLEN]; /* entry name (up to MAXPATHLEN bytes) */
};
public _extern _opendir$INODE64 cDIR *opendir(U8 *dirname);
public _extern _readdir$INODE64 Dirent *readdir(cDIR *dir);

#elifdef IS_LINUX
#define DIR_MAXPATHLEN (256)

class Dirent {
  U64 ino;         /* file number of entry */
  U64 seekoff;       /* seek offset (optional, used by servers) */
  U16 reclen;        /* length of this record */
  U8 type;         /* file type, see below */
  I8 name[DIR_MAXPATHLEN]; /* entry name (up to MAXPATHLEN bytes) */
};
public cDIR *opendir(U8 *dirname);
public Dirent *readdir(cDIR *dir);
#endif

public extern "c" I32 closedir(cDIR *dir);
public extern "c" U0 rewinddir(cDIR *dir);
public extern "c" U0 seekdir(cDIR *dir, I64 pos);
public extern "c" I64 telldir(cDIR *dir);
/* XXX: Normalise stat ? */
public class cTimespec
{
  I64 tv_sec;
  I64 tv_nsec;
};

#ifdef IS_MACOS
public class cStat {
  I32 st_dev;
  U16 st_mode;
  U16 st_nlink;
  U64 st_ino;
  U32 st_uid;
  U32 st_gid;
  I32 st_rdev;
  cTimespec st_atimespec;
  cTimespec st_mtimespec;
  cTimespec st_ctimespec;
  cTimespec st_birthtimespec;
  I64 st_size;
  I64 st_blocks;
  I32 st_blksize;
  U32 st_flags;
  U32 st_gen;
  I32 st_lspare;
  I64 st_qspare[2];
};
public _extern _stat$INODE64 I32 stat(U8 *pathname, cStat *st);
public _extern _lstat$INODE64 I32 lstat(U8 *pathname, cStat *st);
public _extern _fstat$INODE64 I32 fstat(I32 fd, cStat *st);
#elifdef IS_LINUX
class cStat
{
  U64 st_dev;
  U64 st_ino;
  U64 st_nlink;
  U32 st_mode;
  U32 st_uid;
  U32 st_gid;
  I32 __pad0;
  U64 st_rdev;
  I64 st_size;
  I64 st_blksize;
  I64 st_blocks;
  cTimespec st_atim;
  cTimespec st_mtim;
  cTimespec st_ctim;
  I64 __glibc_reserved[3];
};

public extern "c" I32 stat(U8 *pathname, cStat *st);
public extern "c" I32 lstat(U8 *pathname, cStat *st);
public extern "c" I32 fstat(I32 fd, cStat *st);
#endif

public extern "c" I32 mkdir(U8 *dirname, I32 mode = 438);
public extern "c" I32 unlink(U8 *pathname);
public extern "c" I32 rmdir(U8 *dirname);

#define S_IFMT      0xF000  /* Bit mask for the file type bit fields */
#define S_IFIFO     0x1000  /* FIFO or pipe */
#define S_IFCHR     0x2000  /* Character device */
#define S_IFDIR     0x4000  /* Directory */
#define S_IFBLK     0x6000  /* Block device */
#define S_IFREG     0x8000  /* Regular file */
#define S_IFLNK     0xA000  /* Symbolic link */
#define S_IFSOCK    0xC000  /* Socket */

#define S_IRWXU 0x1C0  /* Permission: user (owner) can read, write, execute */
#define S_IRUSR 0x100  /* Permission: user (owner) can read */
#define S_IWUSR 0x80   /* Permission: user (owner) can write */
#define S_IXUSR 0x40   /* Permission: user (owner) can execute */

/* Permissions: group can read, write, execute */
#define S_IRWXG 0x38
#define S_IRGRP 0x20   /* Permission: group can read */
#define S_IWGRP 0x10   /* Permission: group can write */
#define S_IXGRP 0x8    /* Permission: group can execute */

/* Permissions: others can read, write, execute */
#define S_IRWXO 0x7
#define S_IROTH 0x4    /* Permission: others can read */
#define S_IWOTH 0x2    /* Permission: others can write */
#define S_IXOTH 0x1    /* Permission: others can execute */

#define S_ISUID 0x800  /* Set user ID on execution */
#define S_ISGID 0x400  /* Set group ID on execution */
#define S_ISVTX 0x200  /* Save swapped text after use (sticky) */
#define MKDIR_DEFAULT (S_ISUID|S_IRWXU|S_IRWXG|S_IROTH|S_IXOTH) /* Default mkdir permission */

#define RM_RECURSE  0x1  /* Remove directories and their contents recursively */
#define RM_VERBOSE  0x2  /* Verbosely list files processed */
#define RM_FORCE    0x4  /* Force removal, ignore nonexistent files and arguments */

public I32 RmDir(U8 *path, Bool flags=0);
public I32 Rm(U8 *path, Bool flags=0)
public Bool MkDir(U8 *path, I32 mode=MKDIR_DEFAULT, Bool recurse=FALSE);

#define FZF_CASE_SMART   0
#define FZF_CASE_IGNORE  1
#define FZF_CASE_RESPECT 2

public class FzFPosition
{
  I64 *data;
  I64 size;
  I64 cap;
  I64 score;
};

public FzFPosition *FzF(U8 *text, U8 *pattern,I32 case_mode=FZF_CASE_SMART,
    Bool fuzzy=TRUE);
public U0 FzFPositionRelease(FzFPosition *pos);

#define JSON_STRNUM_FLAG (0x1)

#define JSON_STRING 0
#define JSON_FLOAT  1
#define JSON_INT    2
#define JSON_STRNUM 3
#define JSON_ARRAY  4
#define JSON_OBJECT 5
#define JSON_BOOL   6
#define JSON_NULL   7

class JsonState
{
  U8 ch;
  I64 offset;
  I64 error;
};

public class Json;
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
    Bool boolean;
    U8 *strnum;
    F64 f64;
    I64 i64;
  };
};
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

/* Raw string, with option length of the string and 
 * flags */
public Json *JsonParse(U8 *raw, I64 len=-1, I64 flags=0);
public Bool JsonOk(Json *j);
public U0 JsonRelease(Json *J);
public U8 *JsonToString(Json *json);
public U8 *JsonGetStrerror(Json *json);
public U0 JsonPrintError(Json *J);
public Bool JsonIsObject(Json *j);
public Bool JsonIsArray(Json *j);
public Bool JsonIsNull(Json *j);
public Bool JsonIsBool(Json *j);
public Bool JsonIsString(Json *j);
public Bool JsonIsInt(Json *j);
public Bool JsonIsFloat(Json *j);
public Json *JsonSelect(Json *j, U8 *fmt, ...);

#ifdef __HCC_LINK_SQLITE3__ 

#define SQLITE_OK           0   /* Successful result */
/* beginning-of-error-codes */
#define SQLITE_ERROR        1   /* Generic error */
#define SQLITE_INTERNAL     2   /* Internal logic error in SQLite */
#define SQLITE_PERM         3   /* Access permission denied */
#define SQLITE_ABORT        4   /* Callback routine requested an abort */
#define SQLITE_BUSY         5   /* The database file is locked */
#define SQLITE_LOCKED       6   /* A table in the database is locked */
#define SQLITE_NOMEM        7   /* A malloc() failed */
#define SQLITE_READONLY     8   /* Attempt to write a readonly database */
#define SQLITE_INTERRUPT    9   /* Operation terminated by sqlite3_interrupt()*/
#define SQLITE_IOERR       10   /* Some kind of disk I/O error occurred */
#define SQLITE_CORRUPT     11   /* The database disk image is malformed */
#define SQLITE_NOTFOUND    12   /* Unknown opcode in sqlite3_file_control() */
#define SQLITE_FULL        13   /* Insertion failed because database is full */
#define SQLITE_CANTOPEN    14   /* Unable to open the database file */
#define SQLITE_PROTOCOL    15   /* Database lock protocol error */
#define SQLITE_EMPTY       16   /* Internal use only */
#define SQLITE_SCHEMA      17   /* The database schema changed */
#define SQLITE_TOOBIG      18   /* String or BLOB exceeds size limit */
#define SQLITE_CONSTRAINT  19   /* Abort due to constraint violation */
#define SQLITE_MISMATCH    20   /* Data type mismatch */
#define SQLITE_MISUSE      21   /* Library used incorrectly */
#define SQLITE_NOLFS       22   /* Uses OS features not supported on host */
#define SQLITE_AUTH        23   /* Authorization denied */
#define SQLITE_FORMAT      24   /* Not used */
#define SQLITE_RANGE       25   /* 2nd parameter to sqlite3_bind out of range */
#define SQLITE_NOTADB      26   /* File opened that is not a database file */
#define SQLITE_NOTICE      27   /* Notifications from sqlite3_log() */
#define SQLITE_WARNING     28   /* Warnings from sqlite3_log() */
#define SQLITE_ROW         100  /* sqlite3_step() has another row ready */
#define SQLITE_DONE        101  /* sqlite3_step() has finished executing */

#define SQLITE_INTEGER  1
#define SQLITE_FLOAT    2
#define SQLITE_BLOB     4
#define SQLITE_NULL     5
#define SQLITE_TEXT     3
#define SQLITE3_TEXT    3

class sqlite3_stmt {};
class sqlite3 {};
class sqlite3_value {};

class SqlParam
{
  I64 type;
  union 
  {
    I64 i64;
    F64 f64;
    U8 *str;
    class 
    {
      U0 *blob;
      I64 blob_len;
    };
  };
};

class SqlPreparedStmt
{
  sqlite3_stmt *stmt;
  I64  params_count;
  I64 *params_type;
};

class SqlColumn
{
  I64 len;
  I64 type;
  union
  {
    I64 i64;
    U8 *str;
    U0 *blob;
    F64 f64;
  };
};

class SqlRow
{
  sqlite3_stmt *stmt;
  I64 cols;
  SqlColumn *col;
};

class SqlCtx
{
  sqlite3 *conn;
  U8 *dbname;
};

public extern "c" I32 sqlite3_prepare_v2(
  sqlite3 *db,            /* Database handle */
  U8 *zSql,               /* SQL statement, UTF-8 encoded */
  I32 nByte,              /* Maximum length of zSql in bytes. */
  sqlite3_stmt **ppStmt,  /* OUT: Statement handle */
  U8 **pzTail             /* OUT: Pointer to unused portion of zSql */
);
public extern "c" I32 sqlite3_bind_parameter_count(sqlite3_stmt *stmt);
public extern "c" I32 sqlite3_close(sqlite3 *db);
public extern "c" I32 sqlite3_clear_bindings(sqlite3_stmt *stmt);
public extern "c" I32 sqlite3_reset(sqlite3_stmt *pStmt);
public extern "c" I32 sqlite3_finalize(sqlite3_stmt *pStmt);
public extern "c" I32 sqlite3_exec(
  sqlite3 *db,                               /* An open database */
  U8 *sql,                                  /* SQL to be evaluated */
  I32 (*callback)(U0 *_priv, I32 argc, U8 **ptr, U8 **ptr2),  /* Callback function */
  U8 *priv,                                    /* 1st argument to callback */
  U8 **errmsg                              /* Error msg written here */
);
public extern "c" I32 sqlite3_bind_blob(sqlite3_stmt *stmt, I32 __i, U0 *__ptr, I32 n,
    U0 *(*_bind)(U0 *param));
public extern "c" I32 sqlite3_bind_blob64(sqlite3_stmt *stmt, I32 __i, U0 *__ptr, I64 __i2,
    U0 *(*_bind)(U0 *param));
public extern "c" I32 sqlite3_bind_double(sqlite3_stmt *stmt, I32 __i, F64 __f64);
public extern "c" I32 sqlite3_bind_int(sqlite3_stmt *stmt, I32 __i, I32 __int);
public extern "c" I32 sqlite3_bind_int64(sqlite3_stmt *stmt, I32 __i, I64 __int64);
public extern "c" I32 sqlite3_bind_null(sqlite3_stmt *stmt, I32 __i);
public extern "c" I32 sqlite3_bind_text(sqlite3_stmt *stmt, I32 __i, U8 *__buf, I32 __i2, 
    U0 *(*_bind)(U0 *param));
public extern "c" I32 sqlite3_bind_text16(sqlite3_stmt *stmt, I32 __i, U0 *__buf, I32 __i2, 
    U0 *(*_bind)(U0 *param));
public extern "c" I32 sqlite3_bind_text64(sqlite3_stmt *stmt, I32 __i, U8 *__buf, I64 __int64,
    U0 *(*_bind)(U0 *param), U8 encoding);
public extern "c" I32 sqlite3_bind_value(sqlite3_stmt *stmt, I32 __i, sqlite3_value *__val);

public extern "c" I32 sqlite3_step(sqlite3_stmt *stmt);
public extern "c" I32 sqlite3_data_count(sqlite3_stmt *pStmt);

public extern "c" I32 sqlite3_column_bytes(sqlite3_stmt *stmt, I32 iCol);
public extern "c" U0 *sqlite3_column_blob(sqlite3_stmt *stmt, I32 iCol);
public extern "c" F64 sqlite3_column_double(sqlite3_stmt *stmt, I32 iCol);
public extern "c" I32 sqlite3_column_int(sqlite3_stmt *stmt, I32 iCol);
public extern "c" I64 sqlite3_column_int64(sqlite3_stmt *stmt, I32 iCol);
public extern "c" U8 *sqlite3_column_text(sqlite3_stmt *stmt, I32 iCol);
public extern "c" U0 *sqlite3_column_text16(sqlite3_stmt *stmt, I32 iCol);
public extern "c" sqlite3_value *sqlite3_column_value(sqlite3_stmt *stmt, I32 iCol);
public extern "c" I32 sqlite3_column_bytes(sqlite3_stmt *stmt, I32 iCol);
public extern "c" I32 sqlite3_column_bytes16(sqlite3_stmt *stmt, I32 iCol);
public extern "c" I32 sqlite3_column_type(sqlite3_stmt *stmt, I32 iCol);
public extern "c" Bool sqlite3_open(U8 *dbname, sqlite3 **db);
public extern "c" U8 *sqlite3_errmsg(sqlite3 *db);

public SqlCtx *SqlCtxNew(U8 *dbname, Bool printerror=FALSE);
public U0 SqlCtxRelease(SqlCtx *ctx);
public SqlPreparedStmt *SqlPrepare(SqlCtx *ctx, U8 *Sql);
public U0 SqlFinalizePrepared(SqlPreparedStmt *pstmt);
/* Unsafe */ 
public U8 *SqlExecRaw(SqlCtx *ctx, U8 *sql);
public I32 SqlExecPrepared(SqlPreparedStmt *pstmt, SqlRow *row,
    SqlParam *params);
public Bool SqlSelect(SqlCtx *ctx, SqlRow *row, U8 *sql,
    SqlParam *params=NULL, I32 param_count=0);
public Bool SqlQuery(SqlCtx *ctx, U8 *sql, SqlParam *params=NULL,
    I32 param_count=0);
public I32 SqlIter(SqlRow *row);
public I64 SqlIterPrepared(SqlRow *row);
public U0 SqlRowRelease(SqlRow *row);

#endif /* End of SQLite */

/* Socket options level */
#define SOL_SOCKET      0xffff          /* Socket-level options */

/*
 * Address families.
 */
#define AF_UNSPEC       0               /* Unspecified */
#define AF_UNIX         1               /* Local communication (unix pipes) */
#define AF_INET         2               /* Internet IPv4 */
#define AF_INET6        30              /* Internet IPv6 */

/* Socket types */
#define SOCK_STREAM     1               /* Stream (connection-oriented) socket */
#define SOCK_DGRAM      2               /* Datagram (connectionless) socket */
#define SOCK_RAW        3               /* Raw protocol interface */
#define SOCK_SEQPACKET  5               /* Sequenced packet stream */

/*
 * Per-socket option flags.
 */
#define SO_DEBUG        0x0001          /* Enable debugging information */
#define SO_ACCEPTCONN   0x0002          /* Socket has had listen() applied */
#define SO_REUSEADDR    0x0004          /* Allow reuse of local addresses */
#define SO_KEEPALIVE    0x0008          /* Keep connections active */
#define SO_DONTROUTE    0x0010          /* Route packets through local interfaces */
#define SO_BROADCAST    0x0020          /* Allow broadcast messages */
#define SO_LINGER       0x1080          /* Linger on close if data is present */
#define SO_OOBINLINE    0x0100          /* Leave out-of-band data inline */
#define SO_REUSEPORT    0x0200          /* Allow reuse of local ports */
#define SO_TIMESTAMP    0x0400          /* Timestamp incoming datagrams */
#define SO_TIMESTAMP_MONOTONIC  0x0800  /* Use monotonic timestamps for datagrams */

/* Protocols */
#define IPPROTO_IP              0               /* Placeholder for IP */
#define IPPROTO_IPV6            41              /* IPv6 protocol */

/* TCP options */
#define TCP_NODELAY             0x01    /* Disable Nagle's algorithm */
#define TCP_MAXSEG              0x02    /* Set maximum segment size */
#define TCP_NOPUSH              0x04    /* Disable push on final write */
#define TCP_NOOPT               0x08    /* Disable TCP options */
#define TCP_KEEPALIVE           0x10    /* Set keepalive idle time */
#define TCP_CONNECTIONTIMEOUT   0x20    /* Set connection timeout */
#define PERSIST_TIMEOUT         0x40    /* Timeout for persist state */
#define TCP_RXT_CONNDROPTIME    0x80    /* Connection drop time for retransmissions */
#define TCP_RXT_FINDROP         0x100   /* Drop connection after retransmitting FIN */
#define TCP_KEEPINTVL           0x101   /* Interval between keepalives */
#define TCP_KEEPCNT             0x102   /* Number of keepalives before close */
#define TCP_SENDMOREACKS        0x103   /* Acknowledge every other packet */
#define TCP_ENABLE_ECN          0x104   /* Enable Explicit Congestion Notification */
#define TCP_FASTOPEN            0x105   /* TCP Fast Open */
#define TCP_CONNECTION_INFO     0x106   /* TCP connection state info */

#define INET6_ADDRSTRLEN        46      /* Length of string form of IPv6 address */

/* Address info flags */
#define AI_PASSIVE     0x00000001 /* Suitable for bind() */
#define AI_CANONNAME   0x00000002 /* Request canonical name */
#define AI_NUMERICHOST 0x00000004 /* Numeric host address string */
#define AI_NUMERICSERV 0x00001000 /* Numeric service address string */
#define AI_UNUSABLE    0x10000000 /* Include unusable addresses */

/*
 * The hostent class represents an entry from the database of host names and addresses.
 * It includes the official name of the host, aliases, the type of address, the length of the address,
 * and a list of addresses for the host.
 */
public class hostent
{
  U8	*h_name;	/* Official name of the host */
  U8	**h_aliases;	/* Null-terminated array of alternate names for the host */
  I32 h_addrtype;	/* Type of address; typically AF_INET */
  I32 h_length;		/* Length of each address in bytes */
  U8	**h_addr_list;	/* Null-terminated array of addresses for the host */
};

/*
 * The netent class provides details about a network including its name, aliases, address type,
 * and the network number. The assumption is that a network number can fit in an unsigned long.
 */
public class netent
{
  U8  *n_name;      /* Official name of the network */
  U8  **n_aliases;  /* Null-terminated array of alternate names for the network */
  I32 n_addrtype;   /* Type of address for the network */
  U32 n_net;        /* Network number */
};

/*
 * The servent class encapsulates information about a service provided over the network,
 * including its official name, aliases, port number, and the protocol used.
 */
public class servent
{
  U8 *s_name;	  /* Official name of the service */
  U8 **s_aliases; /* Null-terminated array of alternate names for the service */
  I32 s_port;     /* Port number associated with the service */
  U8 *s_proto;    /* Name of the protocol to use with this service */
};

/*
 * The protoent class contains information about a given protocol, such as its name,
 * aliases, and the protocol number.
 */
public class protoent
{
  U8  *p_name;     /* Official name of the protocol */
  U8  **p_aliases; /* Null-terminated array of alternate names for the protocol */
  I32 p_proto;     /* Protocol number */
};

/*
 * The sockaddr class is a generic descriptor for socket address information,
 * including length, family, and actual address data.
 */
public class sockaddr
{
  U8 sa_len;       /* Length of the structure */
  U8 sa_family;    /* Address family */
  U8 sa_data[14];  /* Actual address information */
};

/*
 * The in_addr class represents an IPv4 Internet address,
 * stored as an unsigned long.
 */
public class in_addr
{
  U32 s_addr; /* IPv4 address */
};

/*
 * The sockaddr_in class represents an IPv4 socket address,
 * including length, family, port, address, and padding to match the size of sockaddr.
 */
public class sockaddr_in
{
  U8 sin_len;         /* Length of the structure */
  U8 sin_family;      /* Address family, AF_INET */
  U16 sin_port;       /* Port number */
  in_addr sin_addr;   /* IPv4 address */
  U8 sin_zero[8];     /* Padding to make structure the same size as sockaddr */
};

/*
 * The in6_addr class is used to store an IPv6 address,
 * with three different representations for convenience.
 */
public class in6_addr
{
  union
  {
    U8   __u6_addr8[16];  /* 16 bytes of IPv6 address */
    U16  __u6_addr16[8];  /* 8 shorts of IPv6 address */
    U32  __u6_addr32[4];  /* 4 ints of IPv6 address */
  };                      /* Represents a 128-bit IPv6 address */
};

/*
 * The sockaddr_in6 class represents an IPv6 socket address,
 * including length, family, port, flow information, the IPv6 address,
 * and the scope zone index for link-local addresses.
 */
public class sockaddr_in6
{
  U8  sin6_len;        /* Length of this struct */
  U8  sin6_family;     /* Address family, AF_INET6 */
  I64 sin6_port;       /* Transport layer port number */
  U32 sin6_flowinfo;   /* IPv6 flow information */
  in6_addr sin6_addr;  /* IPv6 address */
  U32 sin6_scope_id;   /* Scope zone index */
};

/*
 * The addrinfo class is used for domain name resolution,
 * containing flags, family, socket type, protocol, address length,
 * canonical name, address, and pointer to the next in list.
 */
class addrinfo;
public class addrinfo
{
  I32 ai_flags;	     /* Input flags */
  I32 ai_family;     /* Address family */
  I32 ai_socktype;   /* Socket type */
  I32 ai_protocol;   /* Protocol for the socket */
  U32 ai_addrlen;    /* Length of the address */
  U8  *ai_canonname; /* Canonical name for the host */
  sockaddr *ai_addr; /* Binary address */
  addrinfo *ai_next; /* Next structure in the linked list */
};


#define _SS_PAD1SIZE (6)
#define _SS_PAD2SIZE (112)

/*
 * The sockaddr_storage class is designed to accommodate all supported address families,
 * ensuring sufficient storage and proper alignment. It's used where a socket address
 * structure's size and family are known, but the exact type is not.
 */
public class sockaddr_storage
{
  U8  ss_len;         /* Total length of the address */
  U8  ss_family;      /* Address family (AF_*) */
  U8  __ss_pad1[_SS_PAD1SIZE]; /* Padding to achieve alignment */
  I64 __ss_align;     /* Force alignment of the structure */
  U8  __ss_pad2[_SS_PAD2SIZE]; /* Additional padding to reach desired size */
};

/*
 * The msghdr class is used with message-based socket operations (sendmsg, recvmsg).
 * It includes an optional socket address, scatter/gather array, ancillary data,
 * and flags for received messages.
 */
public class msghdr
{
  U8    *msg_name;      /* Optional address */
  I64   msg_namelen;    /* Size of the address */
  iovec *msg_iov;       /* Scatter/gather array of buffers */
  I32   msg_iovlen;     /* Number of elements in msg_iov */
  U0    *msg_control;   /* Ancillary data */
  I64   msg_controllen; /* Length of ancillary data buffer */
  I32   msg_flags;      /* Flags on received message */
};

/*
 * The sockaddr_un class specifies a local socket address for Unix domain sockets.
 * It includes the address length, family, and path.
 */
public class sockaddr_un
{
  U8 sun_len;        /* Length of the sockaddr including null */
  U8 sun_family;     /* Address family: AF_UNIX */
  U8 sun_path[104];  /* Path name */
};

public extern "c" I32 setsockopt(I32 __sockfd, I32 __level, I32 __option_name,
    U0 *__option_value, I64 __option_len);
public extern "c" I32 getsockopt(I32 socket, I32 level, I32 option_name,
    U0 * option_value, I64 *option_len);
public extern "c" I32 shutdown(I32 socket, I32 how);
public extern "c" U32 inet_addr(U8 *cp);
public extern "c" I32 inet_aton(U8 *cp, in_addr *pin);
public extern "c" U32 inet_lnaof(in_addr in);
public extern "c" in_addr inet_makeaddr(U32 net, U32 lna);
public extern "c" U32 inet_netof(in_addr in);
public extern "c" U32 inet_network(U8 *cp);
public extern "c" U8 *inet_ntoa(in_addr in);
public extern "c" U8 *inet_ntop(I32 af, U0 *src, U8 *dst, I64 size);
public extern "c" I32 inet_pton(I32 af, U8 *src, U0 *dst);

public extern "c" U0 endhostent();
public extern "c" U0 endnetent();
public extern "c" U0 endprotoent();
public extern "c" U0 endservent();

public extern "c" U0 freeaddrinfo(addrinfo *info);
public extern "c" U8 *gai_strerror(I32 __err);
public extern "c" I32 getaddrinfo(U8 *hostname, U8 *servname, addrinfo *hints, addrinfo **res);
/* Depricated */
hostent *gethostbyaddr(U0 *ptr, I64 len, I32 type);
/* Depricated */
hostent *gethostbyname(U8 * name);

public extern "c" hostent *gethostent();
public extern "c" I32 getnameinfo(sockaddr *sa, I64 salen, U8 *host,
    I64 hostlen, U8 *serv, I64 servlen, I32 flags);
public extern "c" netent *getnetbyaddr(U32 net, I32 type);
public extern "c" netent *getnetbyname(U8 *name);
public extern "c" netent *getnetent();
public extern "c" protoent *getprotobyname(U8 *name);
public extern "c" protoent *getprotobynumber(I32 proto);
public extern "c" protoent *getprotoent();
public extern "c" servent *getservbyname(U8 *name, U8 *proto);
public extern "c" servent *getservbyport(I32 port, U8 *proto);
public extern "c" servent *getservent();
public extern "c" U0 sethostent(I32 stayopen);
public extern "c" U0 setnetent(I32 stayopen);
public extern "c" U0 setprotoent(I32 stayopen);
public extern "c" U0 setservent(I32 stayopen);
public extern "c" U8 *hstrerror(I32 err);

public extern "c" I32 socket(I32 domain, I32 type, I32 protocol);
public extern "c" I32 connect(I32 socket, sockaddr *address, I64 address_len);
public extern "c" I32 accept(I32 socket, sockaddr *address, I64 *address_len);
public extern "c" I32 bind(I32 socket, sockaddr *address, I64 address_len);
public extern "c" I32 getpeername(I32 socket, sockaddr *address, I64 *address_len);
public extern "c" I64 recvmsg(I32 socket, msghdr *message, I32 flags); 
public extern "c" I64 recvfrom(I32 socket, U0 *buffer, U64 length, I32 flags, sockaddr *address, U64 *address_len);
public extern "c" I64 recv(I32 socket, U0 *buffer, U64 length, I32 flags);
public extern "c" I32 listen(I32 socket, I32 backlog);
public extern "c" I32 getsockname(I32 socket, sockaddr *address, I64 *address_len);
public extern "c" I64 send(I32 socket, U0 *buffer, U64 length, I32 flags);
public extern "c" I64 sendmsg(I32 socket, msghdr *message, I32 flags);
public extern "c" I64 sendto(I32 socket, U0 *buffer, U64 length, I32 flags, sockaddr *dest_addr, U64 dest_len);

public Bool NetSetSocketNonBlocking(I32 sockfd);
public Bool NetSetSockReuseAddr(I32 sockfd);
public Bool NetSetSocketNoDelay(I32 sockfd);
public Bool NetGetIpv4Address(sockaddr *sAddr, I32 ai_family, U8 *outbuf, I32 size);
public I32 NetConnect(U8 *addr, I32 port, Bool non_blocking=FALSE);
public Bool NetListen(I32 sockfd, addrinfo *sa, I32 backlog);
public I32 NetCreateServer(I32 port, U8 *bindaddr, I32 backlog,
    I32 addrfam=AF_UNSPEC, Bool non_blocking=FALSE);
public Bool NetAccept(I32 socket, Bool non_blocking=FALSE);
public Bool NetGetIpv4AddressFromAcceptedSocket(I32 sockfd, U8 *ipbuf, I32 outlen);
public I32 NetCreateUnixServerSocket(U8 *name, I32 backlog);
public I32 NetCreateUnixClientSocket(U8 *name);


class RBNode;
public class RBNode
{
  U0 *key;
  U0 *value;
  I64 color;
  RBNode *left;
  RBNode *right;
  RBNode *parent;
};

public class RBTree
{
  RBNode *root;
  I64 size;
  U0 (*free_key)(U0 *);
  U0 (*free_value)(U0 *);
  I64 (*key_cmp)(U0 *, U0 *);
};

public RBTree *RBTreeNew(U0 (*free_key)(U0 *),
                         U0 (*free_value)(U0 *), 
                         U0 (*key_cmp)(U0 *, U0 *));
public U0 RBTreeInsert(RBTree *tree, U0 *key, U0 *value);
public U0 *RBTreeGet(RBTree *tree, U0 *key);
public Bool RBTreeHas(RBTree *tree, U0 *key);
public U0 RBTreeDelete(RBTree *tree, U0 *key);
public U0 RBTreeForEach(RBTree *tree,
                        U0 *user_data,
                        U0 (*callback)(U0 *user_data,
                                       U0 *key, 
                                       U0 *value));
public U0 RBTreeRelease(RBTree *tree);

public class RBIter
{
  I64 sp;
  RBNode **stack;
  RBNode *cur;
  RBNode *node;
};

public RBIter *RBIterNew(RBTree *tree);
public U0 RBIterRelease(RBIter *it);
public RBNode *RBIterNext(RBIter *it);

public class IntSet
{
  RBTree *tree;
};
public class IntSetEntry
{
  I64 key;
};
public class IntSetIter
{
  RBIter *it;
  IntSetEntry *entry;
};

public IntSet *IntSetNew(U0);
public Bool IntSetHas(IntSet *set, I64 key);
public U0 IntSetAdd(IntSet *set, I64 key);
public U0 IntSetDelete(IntSet *set, I64 key);
public IntSet *IntSetUnion(IntSet *s1, IntSet *s2);
public IntSet *IntSetDiff(IntSet *s1, IntSet *s2);
public IntSet *IntSetSymDiff(IntSet *s1, IntSet *s2);
public Bool IntSetEq(IntSet *s1, IntSet *s2);
public IntVec *IntSetToVec(IntSet *set);
public U8 *IntSetToString(IntSet *set);
public U0 IntSetRelease(IntSet *set);
public IntSetEntry *IntSetIterNext(IntSetIter *iter);
public U0 IntSetIterRelease(IntSetIter *iter);
public IntSetIter *IntSetIterNew(IntSet *set);
public I64 IntSetSize(IntSet *set);
