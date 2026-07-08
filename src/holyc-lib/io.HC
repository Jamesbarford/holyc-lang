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
extern "c" I8 *getcwd(U8 *buf, U64 size);
extern "c" I8 *getenv(U8 *buf);
extern "c" I32 isatty(I32 fd);

class iovec
{
  U0 *iov_base;
  U64 iov_len;
};

/* Stantardise the stat function, though `tos.HH` also allows 
 * calling the libc version. This is just much easier to use.
 * With the added benifit of including when a file was created 
 * for linux, which mac includes with `st_birthtimespec` */
I32 Stat(Fstat *fst, U8 *pathname)
{
#if IS_LINUX
  Statx stx;

  #define AT_FDCWD -100
  #define AT_STATX_SYNC_AS_STAT 0x0000
  #define STATX_BTIME 0x0800U
  #define STATX_ATIME 0x0020U
  #define STATX_MTIME 0x0040U
  #define STATX_CTIME 0x0080U

  if (I32 ret = statx(AT_FDCWD, pathname, AT_STATX_SYNC_AS_STAT,
        STATX_BTIME, &stx); ret != 0) {
    return ret;
  }

  U64 st_dev = (stx.stx_dev_minor & 0xFF)           |
               ((stx.stx_dev_major & 0xFFF) << 8)   |
               ((stx.stx_dev_minor & ~0xFF)  << 12) |
               ((stx.stx_dev_major & ~0xFFF) << 32);

  U64 st_rdev = (stx.stx_rdev_minor & 0xFF)           |
                ((stx.stx_rdev_major & 0xFFF) << 8)   |
                ((stx.stx_rdev_minor & ~0xFF)  << 12) |
                ((stx.stx_rdev_major & ~0xFFF) << 32);

  fst->st_dev     = st_dev;
  fst->st_ino     = stx.stx_ino;
  fst->st_nlink   = stx.stx_nlink;
  fst->st_mode    = stx.stx_mode;
  fst->st_uid     = stx.stx_uid;
  fst->st_gid     = stx.stx_gid;
  fst->st_rdev    = st_rdev;
  fst->st_size    = stx.stx_size;
  fst->st_blksize = stx.stx_blksize;
  fst->st_blocks  = stx.stx_blocks;
  /* statx_timestamp's tv_nsec is a U32 followed by padding - copy
   * field-wise rather than struct-wise into cTimespec. */
  fst->st_atim.tv_sec  = stx.stx_atime.tv_sec;
  fst->st_atim.tv_nsec = stx.stx_atime.tv_nsec;
  fst->st_mtim.tv_sec  = stx.stx_mtime.tv_sec;
  fst->st_mtim.tv_nsec = stx.stx_mtime.tv_nsec;
  fst->st_ctim.tv_sec  = stx.stx_ctime.tv_sec;
  fst->st_ctim.tv_nsec = stx.stx_ctime.tv_nsec;
  fst->st_btim.tv_sec  = stx.stx_btime.tv_sec;
  fst->st_btim.tv_nsec = stx.stx_btime.tv_nsec;

#elif IS_MACOS
  cStat st;
  if (I32 ret = stat(pathname, &st); ret != 0) {
    return ret;
  }

  fst->st_dev = st.st_dev;
  fst->st_ino = st.st_ino;
  fst->st_nlink = st.st_nlink;
  fst->st_mode = st.st_mode;
  fst->st_uid = st.st_uid;
  fst->st_gid = st.st_gid;
  fst->st_rdev = st.st_rdev;
  fst->st_size = st.st_size;
  fst->st_blksize = st.st_blksize;
  fst->st_blocks = st.st_blocks;
  fst->st_atim = st.st_atimespec;
  fst->st_mtim = st.st_mtimespec;
  fst->st_ctim = st.st_ctimespec;
  fst->st_btim = st.st_birthtimespec;
#else
#error "Unknown platform"
#endif
  return 0;
}

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
  U8 *new_path = NULL;
  U64 len = StrLen(path);
  if ((len == 1 && path[0] == '~') ||
      (len == 2 && path[0] == '~' && path[1] == '/')) {
    U8 *home = getenv("HOME");
    home = StrNew(home);
    new_path = home;
  } else if (len >= 3 && path[0] == '~' && path[1] == '/') {
    U8 *home = getenv("HOME");
    U8 *p = path;
    if (*(p + 2) != '\0') {
      new_path = StrPrint(,"%s/%s",home,p+2);
    } else {
      new_path = StrPrint(,"%s",home);
    }
  } else {
    new_path = path;
  }
  I32 ret_val = chdir(new_path);
  if (new_path != path) {
    Free(new_path);
  }
  return ret_val;
}

public U8 *Pwd(U8 *buf=NULL, U64 buf_size=0)
{
  return getcwd(buf, buf_size);
}

/* `ls`-style entry colours. */
#define DIR_COL_DIR "\033[1;34m"  /* directory  - bold blue  */
#define DIR_COL_LNK "\033[1;36m"  /* symlink    - bold cyan  */
#define DIR_COL_EXE "\033[1;32m"  /* executable - bold green */
#define DIR_COL_END "\033[0m"

class DirHuman
{
  U8 *pathname;
  U32 mode;  /* st_mode: file type + permission bits */
  U8  type;  /* Dirent type - catches symlinks, which Stat() follows */
};

DirHuman *DirHumanNew(U8 *pathname, Fstat *fst, U8 type)
{
  DirHuman *dh = MAlloc(sizeof(DirHuman));
  dh->pathname = pathname;
  dh->mode = fst->st_mode;
  dh->type = type;
  return dh;
}

U0 DirHumanRelease(DirHuman *dh)
{
  if (dh) {
    Free(dh->pathname);
    Free(dh);
  }
}

/* The colour `ls` would paint this entry, or NULL for plain. */
U8 *DirHumanColor(DirHuman *dh)
{
  if (dh->type == DT_LNK) return DIR_COL_LNK;
  if (dh->type == DT_DIR || (dh->mode & S_IFMT) == S_IFDIR) {
    return DIR_COL_DIR;
  }
  if (dh->mode & (S_IXUSR|S_IXGRP|S_IXOTH)) return DIR_COL_EXE;
  return NULL;
}

I64 CmpDirHuman(DirHuman *dh1, DirHuman *dh2)
{
  U8 *str1 = dh1->pathname;
  U8 *str2 = dh2->pathname;
  if ('0'<=*str1<='9' && '0'<=*str2<='9') {
    I64 n1 = 0, n2 = 0;
    while ('0'<=*str1<='9') {
      n1 = (n1 * 10) + (*str1-'0');
      str1++;
    }
    while ('0'<=*str2<='9') {
      n2 = (n2 * 10) + (*str2-'0');
      str2++;
    }
    return n2 > n1;
  }
  /* Non-numeric names: plain lexicographic order. */
  return StrCmp(str1,str2) < 0;
}

U0 QSortDirHuman(U0 **arr, I64 high, I64 low=0)
{
  if (low < high) {
    U0 *pivot = arr[high];
    I64 idx = low;

    for (I64 i = low; i < high; ++i) {
      if (CmpDirHuman(arr[i],pivot)) {
        U0 *tmp = arr[i];
        arr[i] = arr[idx];
        arr[idx] = tmp;
        idx++;
      }
    }

    arr[high] = arr[idx];
    arr[idx] = pivot;
    QSortDirHuman(arr,high,idx+1);
    QSortDirHuman(arr,idx-1,low);
  }
}

public I64 Dir(U8 *pathname = "./")
{
  auto dir = opendir(pathname);
  if (!dir) return 0;
  auto vec = PtrVecNew();
  /* Strip a trailing '/' on a copy - the argument is often a string
   * literal (the default!), and literals live in read-only memory. */
  U8 *path = StrNew(pathname);
  U64 len = StrLen(path);
  if (len > 0 && path[len-1] == '/') {
    path[len-1] = '\0';
  }
  Dirent *ent;
  while ((ent = readdir(dir)) != NULL) {
    if (!StrCmp(ent->name,".") || !StrCmp(ent->name,"..")) continue;
    auto str = StrNew(ent->name);
    auto full_path = StrPrint(,"%s/%s",path,ent->name);
    Fstat fst;
    if (Stat(&fst,full_path) == 0) {
      DirHuman *dh = DirHumanNew(str, &fst, ent->type);
      PtrVecPush(vec,dh);
    } else {
      Free(str);
    }
    Free(full_path);
  }
  Free(path);
  QSortDirHuman(vec->entries(U0 **), vec->size-1);
  closedir(dir);
  Bool colors = isatty(1) != 0;
  for (DirHuman *dh : vec) {
    U8 *col = NULL;
    if (colors) col = DirHumanColor(dh);
    if (col) {
      "%s%s"DIR_COL_END"\n", col, dh->pathname;
    } else {
      "%s\n", dh->pathname;
    }
  }
  I64 nfiles = vec->size;
  PtrVecRelease(vec,&DirHumanRelease);
  return nfiles;
}
