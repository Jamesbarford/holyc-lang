#include "./defs.HH"
#include "./memory.HC"

#define F_DUPFD 0  
#define F_GETFD 1  
#define F_SETFD 2  
#define F_GETFL 3  
#define F_SETFL 4  

#ifdef IS_LINUX
  #define F_GETOWN 0x9
  #define F_SETOWN 0x8
  #define F_GETLK  0x5
  #define F_SETLK  0x6
  #define F_SETLKW 0x7
#else
  #define F_GETOWN 0x5
  #define F_SETOWN 0x6
  #define F_GETLK  0x7
  #define F_SETLK  0x8
  #define F_SETLKW 0x9
#endif

#define F_FLUSH_DATA  40
#define F_CHKCLEAN    41
#define F_PREALLOCATE 42
#define F_SETSIZE     43
#define F_RDADVISE    44
#define F_RDAHEAD     45

#define FD_CLOEXEC 1

/* record locking flags */
#ifdef IS_LINUX
  #define F_RDLCK 0
  #define F_WRLCK 1
  #define F_UNLCK 2
#else
  #define F_RDLCK 1
  #define F_UNLCK 2
  #define F_WRLCK 3
#endif

#define O_RDONLY  0x0000
#define O_WRONLY  0x0001
#define O_RDWR    0x0002
#define O_ACCMODE 0x0003

/* Just why? */
#ifdef IS_LINUX
  #define O_CREAT    0x040
  #define O_TRUNC    0x200
  #define O_EXCL     0x080
  #define O_NONBLOCK 0x800
  #define O_APPEND   0x400
#else
  #define O_CREAT    0x200
  #define O_TRUNC    0x400
  #define O_EXCL     0x800
  #define O_NONBLOCK 0x004
  #define O_APPEND   0x008
#endif

#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

/*XXX: Should create a script that converts a select amount of c-class to 
 * HC bindings so that they can be called and variance between operating systems
 * can be accounted for, i.e: the return value of `stat()` and flags */ 
extern "c" I32 open(U8 *__name, I32 flags, ...); 
extern "c" I32 creat(U8 *__name, U16 __flags);
extern "c" I32 fcntl(I32 __fd, I32 __flags, ...);
extern "c" I32 close(I32 __fd);
extern "c" I64 read(I32 __fd, U0 *__buf, U64 __nbyte);
extern "c" I64 write(I32 __fd, U0 *__buf, U64 __nbyte);
extern "c" I64 lseek(I32 __fd, I64 __offset, I32 __whence);
extern "c" I32 chdir(U8 *path);

class iovec
{
  U0 *iov_base;
  U64 iov_len;
};


public U8 *FileRead(U8 *path, I64 *_size=NULL)
{
  if (!path) return NULL;
  I32 fd;
  I64 len, rbytes, size;
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
  size = 0;

  while ((rbytes = read(fd,s+size,len)) != 0) {
    size += rbytes;
  }

  if (size != len) {
    close(fd);
    Free(s);
    if (_size) {
      *_size = 0;
    }
    return NULL;
  }

  if (_size) {
    *_size = len;
  }

  s[len] = '\0';
  close(fd);
  return s;
}

public Bool FileWrite(U8 *filename, U8 *buf, I64 size,
    I64 flags=O_CREAT|O_RDWR)
{
  I64 towrite = size, nwritten = 0;
  I32 fd;
  I32 perms = 420;

  if ((fd = open(filename,flags,perms)) == -1) {
    return FALSE;
  }

  while (towrite > 0) {
    nwritten = write(fd,buf,towrite);
    if (nwritten < 0) {
      close(fd);
      return FALSE;
    }
    buf += nwritten;
    towrite -= nwritten;
  }
  close(fd);
  return TRUE;
}

public I32 Cd(U8 *path)
{
  return chdir(path);
}
