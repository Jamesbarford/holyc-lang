/* All prototypes and classes for all functions and classes provided 
 * by the library */
#define SF_ARG1	8
#define SF_ARG2	16
#define SF_ARG3	24
#define SF_ARG4	32
#define SF_ARG5	40
#define SF_ARG6	48
#define SF_ARG7	52
#define SF_ARG8	56

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

public class CsvParser
{
  U8 *csv;
  U64 len,off,row_off,nrows,nfields;
  I64 state;
};

public class CsvSlice
{
  U64 idx,off,len;
};
public U0 CsvParserInit(CsvParser *p, U8 *csv, U64 len);
public I64 CsvParse(CsvParser *p, CsvSlice *s)

#define CDATE_YEAR_DAYS		365.24225
#define CDATE_YEAR_DAYS_INT	36524225
#define CDATE_BASE_DAY_OF_WEEK	0

public class timeval
{ // c compatability
  I64 tv_sec;   /* seconds since Jan. 1, 1970 */
  I32 tv_usec;  /* and microseconds */
};

public class tm
{ // c compatability
	I32	tm_sec;		/* seconds after the minute [0-60] */
	I32	tm_min;		/* minutes after the hour [0-59] */
	I32	tm_hour;	/* hours since midnight [0-23] */
	I32	tm_mday;	/* day of the month [1-31] */
	I32	tm_mon;		/* months since January [0-11] */
	I32	tm_year;	/* years since 1900 */
	I32	tm_wday;	/* days since Sunday [0-6] */
	I32	tm_yday;	/* days since January 1 [0-365] */
	I32	tm_isdst;	/* Daylight Savings Time flag */
	I64 tm_gmtoff;	/* offset from UTC in seconds */
	I8  *tm_zone;	/* timezone abbreviation */
};

public extern "c" tm *localtime(I64 *clock);
public extern "c" I32 gettimeofday(timeval *tp, U0 *tzp);

public I64 class CDate
{
  U32 time;
  I32 date;
};

public class CDateStruct
{
  U64 sec10000,sec100,sec,min,hour,
	day_of_week,day_of_mon,mon;
  I32 year;
  I64 zone_diff;
};


public U64 NowMilliseconds();
public U0 NowDateTimeStruct(CDateStruct *_ds);
public I64 YearStartDate(I64 year);
public I64 DayOfWeek(I64 i);
public U0 Date2Struct(CDateStruct *_ds,CDate cdt);
public CDate Struct2Date(CDateStruct *_ds);
public I32 FirstDayOfMon(I64 i);
public I64 LastDayOfMon(I64 i);
public I32 FirstDayOfYear(I64 i);
public I64 LastDayOfYear(I64 i);
public CDate Now();

#define HT_LOAD 0.60
#define HT_DELETED 0x7fffffffffffffff
#define HT_PROBE_1 1
#define HT_PROBE_3 3

public class MapIndex
{
  I64 capacity;
  I64 len;
  I64 *entries;
};

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
  MapIndex *indexes; /* Where all of the values are in the entries array, in 
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
  MapIndex *indexes; /* Where all of the values are in the entries array, in 
                      * insertion order. Means we can iterate over the HashTable 
                      * quickly at the cost of memory */
  U64 threashold; /* rebuild threashold */
  U0 (*_free_value)(U0 *_value); /* User defined callback for freeing values */
  U0 (*_free_key)(U0 *_key); /* User defined callback for freeing keys */
  StrMapNode **entries; /* All of the entries, XXX: could this be IntMapNode *entries? */
};

public Bool StrMapSet(StrMap *map, U8 *key, U0 *value);
public U0 *StrMapGet(StrMap *map, U8 *key);
public StrMap *StrMapNew(U64 capacity=1<<8);
public U0 StrMapSetFreeValue(StrMap *map, U0 (*_free_value)(U0 *value));
public U0 StrMapSetFreeKey(StrMap *map, U0 (*_free_key)(U0 *key));
public U0 StrMapRelease(StrMap *map);
public Bool StrMapResize(StrMap *map, U64 size);

public Bool IntMapSet(IntMap *map, I64 key, U0 *value);
public U0 *IntMapGet(IntMap *map, I64 key);
public IntMap *IntMapNew(U64 capacity=1<<8);
public U0 IntMapSetFreeValue(IntMap *map, U0 (*_free_value)(U0 *value));
public U0 IntMapRelease(IntMap *map);
public Bool IntMapResize(IntMap *map, U64 size);

#define __MUTEX_LOCK_PADDING__   56
#define __MUTEX_LOCK_ATTR_SIZE__ 8
#define __THREAD_COND_SIZE__     40
#define __THREAD_CONDATTR_SIZE__ 8
#define __THREAD_SIZE__          8176

/* This I _think_ is specific to MacOS implementation of pthreads 
 * It does work on linux though. */
public class MutexLock 
{
  I64 __sig;
  I8 __opaque[__MUTEX_LOCK_PADDING__];
};

public class MutexLockattr
{
  I64 __sig;
  I8 __opaque[__MUTEX_LOCK_ATTR_SIZE__];
};

public class ThreadCond
{
  I64 __sig;
  I8 __opaque[__THREAD_COND_SIZE__];
};

public class ThreadCondattr {
  I64 __sig;
  I8 __opaque[__THREAD_CONDATTR_SIZE__];
};

public class pthread_handler_rec
{
  U0 (*__routine)(U0 *__arg); // Routine to call
  U0 *__arg;                  // Argument to pass
  pthread_handler_rec *__next;
};

public class pthread_t 
{
  I64 __sig;
  pthread_handler_rec  *__cleanup_stack;
  I8 __opaque[__THREAD_SIZE__];
};

public class ThreadWorker
{
  pthread_t *th;
  I32 id;
};

public class ThreadSemaphore
{
  ThreadCond cond;
  MutexLock lk;
  I32 val;
}; 

public class ThreadWorkerJob
{
  U0 *(*callback)(U0 *priv_data, U0 *argv);
  U0 *argv;
  ThreadWorkerJob *next;
};

public class ThreadPool
{
  U64 worker_count,alive_threads,active_threads;
  Bool run;
  ThreadCond has_work,no_work;
  MutexLock lock, qlock;
  ThreadSemaphore *sem;
  ThreadWorker *workers;
  List *jobs;
  U0 *priv_data;
};

public extern "c" I64 pthread_create(pthread_t **th, U0 *thread_attr=NULL,
    		U0 *(*function_ptr)(U0 *argv), U0 *argv=NULL);
public extern "c" I32 pthread_mutex_init(MutexLock *lk, MutexLockattr *mutex_attr=NULL);
public extern "c" I32 pthread_mutex_destroy(MutexLock *lk);
public extern "c" I32 pthread_mutex_lock(MutexLock *lk);
public extern "c" I32 pthread_mutex_unlock(MutexLock *lk);
public extern "c" I32 pthread_join(pthread_t *th, U0 *ptr=NULL);
public extern "c" I32 pthread_cond_broadcast(ThreadCond *cond);
public extern "c" I32 pthread_cond_destroy(ThreadCond *cond);
public extern "c" I32 pthread_cond_init(ThreadCond *cond,
    		ThreadCondattr *attr=NULL);
public extern "c" I32 pthread_cond_signal(ThreadCond *cond);
public extern "c" I32 pthread_cond_wait(ThreadCond *cond, MutexLock *lk);
public extern "c" I32 pthread_exit(U0 *data=NULL);
public extern "c" I32 pthread_detach(pthread_t *th);
ThreadPool *ThreadPoolNew(I64 worker_count);
public U0 ThreadPoolEnqueue(ThreadPool *pool, U0 *argv, U0 (*callback)(U0 *priv_data, U0 *argv));
public U0 ThreadPoolWait(ThreadPool *pool);
public ThreadPool *ThreadPoolNew(I64 worker_count);
public U0 ThreadPoolStop(ThreadPool *pool);
public U0 ThreadPoolRelease(ThreadPool *pool);

public _extern _EXIT U0 Exit(I64 exit_code=EXIT_FAIL);
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
public F64 Pow(F64 a, I64 b);
public U64 ModU64(U64 *k, U64 m);

public extern "c" U0 *memchr(U0 *__s, I32 __c, U64 __n);
public extern "c" I32 memcmp(U0 *__s1, U0 *__s2, U64 __n);
public extern "c" U0 *memmove(U0 *__dst, U0 *__src, U64 __len);
public _extern _MALLOC U0 *MAlloc(I64 size);
public _extern _FREE U0 Free(U0 *ptr);
public _extern _REALLOC U0 *ReAlloc(U0 *ptr, U64 new_size);
public _extern _CALLOC U0 *CAlloc(U64 new_size);
public _extern _MSIZE U64 MSize(U0 *ptr);
public _extern _MALLOC_ALIGN16 U0 *MAlloc16(I64 size);
public _extern _FREE_ALIGNED U0 Free16(U0 *ptr);
public _extern _MEMCPY U0 *MemCpy(U0 *dst, U0 *src, I64 len);
public _extern _MEMSET U0 *MemSet(U0 *dst, I32 ch, I64 len);


#define STR_LEN 256

#define PRTF_PAD_ZERO     0x001
#define PRTF_LEFT_JUSTIFY 0x002
#define PRTF_TRUNCATE     0x004
#define PRTF_COMMA        0x008
#define PRTF_DOLLAR       0x010
#define PRTF_SLASH        0x020
#define PRTF_QUESTION     0x040
#define PRTF_AUX_FMT_NUM  0x080
#define PRTF_DECIMAL      0x100
#define PRTF_NEG          0x200
#define PRTF_NEG_E           0x400
#define PRTF_NEG_AUX_FMT_NUM 0x800

#define CH_BACKSPACE   0x08
#define CH_ESC         0x1B
#define CH_SHIFT_ESC   0x1C
#define CH_SHIFT_SPACE 0x1F
#define CH_SPACE       0x20

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
public U8 *StrNCpy(U8 *buf, I64 len);
public U8 *FileExtDot(U8 *src);
public U8 *FileExtRem(U8 *src,U8 *dst=NULL);
public Bool IsDotZ(U8 *filename);
public Bool IsDotC(U8 *filename);
public U0 SPutChar(U8 **_dst,U8 ch,U8 **_buf);
public U0 OutStr(U8 *instr,U8 **_buf,U8 **_dst,I64 len,I64 flags);
public I64 StrOcc(U8 *src, I64 ch);
public U8 *StrFirstOcc(U8 *src,U8 *marker);
public U8 *StrLastOcc(U8 *src,U8 *marker);
public U8 *StrLastRem(U8 *src,U8 *marker,U8 *dst=NULL);
public U0 StrReverse(U8 *buf, I64 length);
public I64 I64ToStr(U8 *buf, I64 num, Bool *_is_neg=NULL, I64 _len=5000);
public U64 U64ToStr(U8 *dst, U64 num, I64 len=5000);
public U8 *StrMergeJoin(I64 argc, U64 *argv);
public U8 *StrMerge(...);
public U8 *StrPrint(U8 *dst,U8 *fmt,...);
public U8 *StrPrintJoin(U8 *_dst, U8 *fmt,I64 argc,I64 *argv);
public U8 *MPrintDate(CDate cdt);
public U8 *MPrintTime(CDate cdt);
public U8 *MPrintTimestampNow();
public U8 *StrNew(U8 *buf);
public U8 *StrFirstRem(U8 *src,U8 *marker,U8 *dst=NULL);
public I64 StrIntToHex(U8 *dst, I64 num);
public U8 *MPrintQ(U8 *ptr,I64 flags = 0);
public U8 *MStrPrint(U8 *fmt,...);
public U8 *CatPrint(U8 *_dst,U8 *fmt,...);
public Bool Str2CDateStruct(U8 *strdate, U8 *format, CDateStruct *t);
public Bool WildMatch(U8 *test_str, U8 *wild_str);
public CDate StrToCDate(U8 *_str, U8 *fmt);

#define F_DUPFD         0               /* duplicate file descriptor */
#define F_GETFD         1               /* get file descriptor flags */
#define F_SETFD         2               /* set file descriptor flags */
#define F_GETFL         3               /* get file status flags */
#define F_SETFL         4               /* set file status flags */
#define F_GETOWN        5               /* get SIGIO/SIGURG proc/pgrp */
#define F_SETOWN        6               /* set SIGIO/SIGURG proc/pgrp */
#define F_GETLK         7               /* get record locking information */
#define F_SETLK         8               /* set record locking information */
#define F_SETLKW        9               /* F_SETLK; wait if blocked */

#define F_FLUSH_DATA    40
#define F_CHKCLEAN      41              /* Used for regression test */
#define F_PREALLOCATE   42              /* Preallocate storage */
#define F_SETSIZE       43              /* Truncate a file. Equivalent to calling truncate(2) */
#define F_RDADVISE      44              /* Issue an advisory read async with no copy to user */
#define F_RDAHEAD       45              /* turn read ahead off/on for this fd */

#define FD_CLOEXEC      1               /* close-on-exec flag */

/* record locking flags (F_GETLK, F_SETLK, F_SETLKW) */
#define F_RDLCK         1               /* shared or read lock */
#define F_UNLCK         2               /* unlock */
#define F_WRLCK         3               /* exclusive or write lock */

#define O_RDONLY        0x0000          /* open for reading only */
#define O_WRONLY        0x0001          /* open for writing only */
#define O_RDWR          0x0002          /* open for reading and writing */
#define O_ACCMODE       0x0003          /* mask for above modes */
#define O_CREAT         0x00000200      /* create if nonexistant */
#define O_TRUNC         0x00000400      /* truncate to zero length */
#define O_EXCL          0x00000800      /* error if already exists */

#define SEEK_SET        0       /* set file offset to offset */
#define SEEK_CUR        1       /* set file offset to current plus offset */
#define SEEK_END        2       /* set file offset to EOF plus offset */

public extern "c" I32 open(U8 *__name, I32 flags, ...); 
public extern "c" I32 creat(U8 *__name, U16 __flags);
public extern "c" I32 fcntl(I32 __fd, I32 __flags, ...);
public extern "c" I32 close(I32 __fd);
public extern "c" I64 read(I32 __fd, U0 *__buf, U64 __nbyte);
public extern "c" I64 write(I32 __fd, U0 *__buf, U64 __nbyte);
public extern "c" I64 lseek(I32 __fd, I64 __offset, I32 __whence);
public extern "c" I32 chdir(U8 *path);

public U8 *FileRead(U8 *path, I64 *_size=NULL);
public Bool FileWrite(U8 *filename, U8 *buf, I64 size,
    I64 flags=O_CREAT|O_RDWR);
public I32 Cd(U8 *path);

#define DT_UNKNOWN       0
#define DT_FIFO          1
#define DT_CHR           2
#define DT_DIR           4
#define DT_BLK           6
#define DT_REG           8
#define DT_LNK          10
#define DT_SOCK         12
#define DT_WHT          14

class cDIR {};

#ifdef IS_MACOS
#define DIR_MAXPATHLEN (1024)

class Dirent
{ 
  U64  ino;
  U64  off; 
  U16  reclen;   /* length of this record */
  U16   namlen;   /* length of string in d_name */
  U8   type;     /* file type, see below */
  I8   name[DIR_MAXPATHLEN]; /* entry name (up to MAXPATHLEN bytes) */
};
public _extern _opendir$INODE64 cDIR *opendir(U8 *dirname);
public _extern _readdir$INODE64 Dirent *readdir(cDIR *dir);
#elifdef IS_LINUX
#define DIR_MAXPATHLEN (256)

class Dirent
{
  U64  ino;      /* file number of entry */ 
  U64  seekoff;  /* seek offset (optional, used by servers) */ 
  U16  reclen;   /* length of this record */
  U8   type;     /* file type, see below */
  I8   name[DIR_MAXPATHLEN]; /* entry name (up to MAXPATHLEN bytes) */
};
public cDIR *opendir(U8 *dirname);
public Dirent *readdir(cDIR *dir);
#endif

public extern "c" I32 closedir(cDIR *dir);
public extern "c" U0 rewinddir(cDIR *dir);
public extern "c" U0 seekdir(cDIR *dir, I64 pos);
public extern "c" I64 telldir(cDIR *dir);
