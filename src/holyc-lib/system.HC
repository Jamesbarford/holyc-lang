#include "./defs.HH"

#define SYS_CALL_WRITE 0x2000004 /* Non portable */

public extern "c" U0 exit(I32 status);
public extern "c" I32 system(U8 *command);
public extern "c" I64 write(I32 fd, U0 *buf, U64 nbyte);

#ifdef IS_X86_64
asm {
_EXIT::
    CALL    exit

_SYSTEM::
    PUSHQ  RBP
    MOVQ   RBP, RSP
    CALL   system
    LEAVE
    RET

_WRITE::
    MOVQ    RAX, SYS_CALL_WRITE
    SYSCALL
    LEAVE
    RET
}

public _extern _EXIT U0 Exit(I64 exit_code=EXIT_FAIL);
public _extern _WRITE I64 Write(I64 fd, U8 *buf, I64 len);
public _extern _SYSTEM I64 System(U8 *command);
#endif

#ifdef IS_ARM_64
public U0 Exit(I64 exit_code=EXIT_FAIL)
{
  exit(exit_code);
}

public I64 Write(I64 fd, U8 *buf, I64 len)
{
  return write(fd, buf, len);
}

public I64 System(U8 *command)
{
  return system(command);
}
#endif

/* Call the shell with `printf`-like formatting */
public I32 Sh(U8 *fmt, ...)
{
  if (!fmt) return -1;
  if (argc == 0) return System(fmt);
  U8 *cmd = StrPrintJoin(NULL,fmt,argc,argv);
  I32 retval = System(cmd);
  Free(cmd);
  return retval;
}

/* Capture whatever is printed to stdout in to a heap allocated buffer */
public U8 *Shlurp(U8 *fmt, ...)
{
  if (!fmt) return -1;
  if (argc == 0) return System(fmt);
  /* We redirect stdout to a temporary file, read it and then return the
   * contents */
  U8 *cmd = StrPrintJoin(NULL,fmt,argc,argv);
  cmd = StrPrint(,"%s > /tmp/__hcc_shell_out.txt",cmd);
  I32 retval = System(cmd);
  Free(cmd);
  U64 len = 0;
  U8 *output = FileRead("/tmp/__hcc_shell_out.txt", &len);
  if (output[len-1] == '\n') {
    output[len-1] = '\0';
  }
  return output;
}
