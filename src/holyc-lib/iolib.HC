#include <strlib.HC>

/* Dynamically generate the below based on the os */

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

/*XXX: Replace direct c-calls */ 
extern "c" i32 open(u8 *__nane, i32 flags, ...);
extern "c" i32 creat(u8 *__name, u16 __flags);
extern "c" i32 fcntl(i32 __fd, i32 __flags, ...);
extern "c" i32 close(i32 __fd);
extern "c" i64 read(i32 __fd, u0 *__buf, u64 __nbyte);
extern "c" i64 write(i32 __fd, u0 *__buf, u64 __nbyte);
extern "c" i64 lseek(i32 __fd, i64 __offset, i32 __whence);

class File {
    i32 fd;
    u64 len;
    str *data;
};

u0 print(u8 *fmt, ...) {
    str *s = strnew();
    s = strCatVaPrintf(s,fmt,argc,argv);
    write(STDOUT,s->buf,s->len);
    strFree(s);
}

u0 panic(u8 *fmt, ...) {
    str *s = strnew();
    s = strCatVaPrintf(s,fmt,argc,argv);
    write(STDERR,s->buf,s->len);
    strFree(s);
    Exit(EXIT_FAIL);
}

/* Read the whole contents of a file into a str, NULL on failure */
str *ioReadFile(u8 *file_path) {
    i32 fd;
    i64 len, rbytes;
    str *s;

    if ((fd = open(file_path,O_RDONLY,420)) == -1) {
        return NULL;    
    }

    /* get the file size */
    if ((len = lseek(fd,0,SEEK_END)) == -1) {
        close(fd);
        return NULL;
    }

    lseek(fd,0,SEEK_SET);
    s = strnew(len+100);
    if ((rbytes = read(fd,s->buf,s->size)) != len) {
        close(fd);
        strFree(s);
        return NULL;
    }

    s->buf[rbytes-1] = '\0';
    s->len = rbytes;
    close(fd);
    return s;
}

str *ioReadOrPanic(u8 *file_path) {
    str *s = ioReadFile(file_path);
    if (!s) {
        panic("Failed to read file: %s\n", file_path);
    }
    return s;
}
