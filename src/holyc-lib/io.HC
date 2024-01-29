#include <defs.HH>
#include <memory.HC>

/* command values */
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

/*XXX: Should create a script that converts a select amount of c-class to 
 * HC bindings so that they can be called and variance between operating systems
 * can be accounted for, i.e: the return value of `stat()` and flags */ 
extern "c" I32 open(U8 *__nane, I32 flags, ...);
extern "c" I32 creat(U8 *__name, U16 __flags);
extern "c" I32 fcntl(I32 __fd, I32 __flags, ...);
extern "c" I32 close(I32 __fd);
extern "c" I64 read(I32 __fd, U0 *__buf, U64 __nbyte);
extern "c" I64 write(I32 __fd, U0 *__buf, U64 __nbyte);
extern "c" I64 lseek(I32 __fd, I64 __offset, I32 __whence);
extern "c" I32 chdir(U8 *path);
I32 errno;

public U8 *FileRead(U8 *path, I64 *_size=NULL)
{
  if (!path) return NULL;
  I32 fd;
  I64 len, rbytes;
  U8 *s;

  if ((fd = open(path,O_RDONLY,438)) == -1) {
    return NULL;    
  }

  /* get the file size */
  if ((len = lseek(fd,0,SEEK_END)) == -1) {
    close(fd);
    return NULL;
  }

  lseek(fd,0,SEEK_SET);

  s = MAlloc(len+100);

  if ((rbytes = read(fd,s,len)) != len) {
    close(fd);
    Free(s);
    return NULL;
  }

  if (_size) {
    *_size = rbytes;
  }

  s[rbytes-1] = '\0';
  close(fd);
  return s;
}

public Bool FileWrite(U8 *filename, U8 *buf, I64 size,
    I64 flags=O_CREAT|O_RDWR)
{
  I64 towrite = size, total=0, nwritten = 0;
  I32 fd;

  if ((fd = open(filename,flags,438)) == -1) {
    return FALSE;
  }

  while (towrite > 0) {
    nwritten = write(fd,buf,towrite);
    if (nwritten < 0) {
      close(fd);
      return FALSE;
    }
    total += nwritten;
    towrite -= nwritten;
  }
  close(fd);
  return TRUE;
}

public U0 Cd(U8 *path)
{
  return chdir(path);
}
