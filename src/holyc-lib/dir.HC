#define DT_UNKNOWN       0
#define DT_FIFO          1
#define DT_CHR           2
#define DT_DIR           4
#define DT_BLK           6
#define DT_REG           8
#define DT_LNK          10
#define DT_SOCK         12
#define DT_WHT          14

class cDIR;

#ifdef IS_MACOS
#define DIR_MAXPATHLEN 1024

public class Dirent
{ 
  U64  ino;      /* file number of entry */ 
  U64  seekoff;  /* seek offset (optional, used by servers) */ 
  U16  reclen;   /* length of this record */
  U16  namlen;   /* length of string in d_name */
  U8   type;     /* file type, see below */
  I8   name[DIR_MAXPATHLEN]; /* entry name (up to MAXPATHLEN bytes) */
};

/* Mac os calls _<fn>$INODE64 for 64bit systems...*/
public _extern _opendir$INODE64 cDIR *opendir(U8 *dirname);
public _extern _readdir$INODE64 Dirent *readdir(cDIR *dir);

#elifdef IS_LINUX
#define DIR_MAXPATHLEN 256
public class Dirent
{
  U64  ino;      /* file number of entry */ 
  U64  seekoff;  /* seek offset (optional, used by servers) */ 
  U16  reclen;   /* length of this record */
  U8   type;     /* file type, see below */
  I8   name[DIR_MAXPATHLEN]; /* entry name (up to MAXPATHLEN bytes) */
};
public extern "c" cDIR *opendir(U8 *dirname);
public extern "c" Dirent *readdir(cDIR *dir);
#endif

public extern "c" I32 closedir(cDIR *dir);
public extern "c" U0 rewinddir(cDIR *dir);
public extern "c" U0 seekdir(cDIR *dir, I64 pos);
public extern "c" I64 telldir(cDIR *dir);
