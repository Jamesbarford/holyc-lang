#include "./defs.HH"
#include "./memory.HC"
#include "./system.HC"
#include "./strings.HC"

/* Yup we can mix #define's and assembly */
#define ALIGN_16 0xFFFFFFFFFFFFFFF0

public extern "c" I32 open(U8 *path, I32 flags, I32 mode);
public extern "c" I64 read(I32 fd, U0 *buf, U64 nbyte);

/* This used to be an assembly routine. Codegen is good enough that
 * this is so similar to what used to be here. There is no point in having
 * this implemented in assembly */
public U8 *FileRead(U8 *name, I64 max_size, I64 *_len)
{
  U8 *buf = MAlloc(max_size);
  I32 fd = open(name, 0, 420);
  I64 n = read(fd, buf, max_size);
  *_len = n;
  buf[n] = 0;
  return buf;
}

public U64 MemAddr(U0 *ptr)
{
  U8 buffer[__BUFSIZ__];
  I64 len;
  len = StrNPrintf(buffer, __BUFSIZ__, "%p", ptr);
  buffer[len] = '\0';
  return strtoll(buffer,NULL,16);
}

U8 *SysExec(U8 *command, U64 *_captured_len=NULL,
    U8 *file_out="/tmp/capture.txt")
{ // execute a command using System and capture the output so you can do
  // something with it. Grim but useful for scripting
  U8 *full_command, *capture;
  U64 size,len;

  size = StrLen(command) * sizeof(U8) + 100;
  full_command = MAlloc(size);

  len = StrNPrintf(full_command,size,"%s > %s",command,file_out);
  full_command[len] = '\0';

  System(full_command);
  capture = FileRead(file_out,__BUFSIZ__,&len);
  if (_captured_len) {
    *_captured_len = len;
  }

  Free(full_command);
  return capture;
}
