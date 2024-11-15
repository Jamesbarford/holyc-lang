#include "./testhelper.HC"

#define TEST_FILE "__file__.txt"
#define TEST_DATA "hello\n"

Bool __fail_and_cleanup(U0)
{
  Rm(TEST_FILE);
  return FALSE;
}

Bool __success_and_cleanup(U0)
{
  Rm(TEST_FILE);
  return TRUE;
}

Bool TestWriteFile(U0)
{
  auto flags = O_CREAT|O_RDWR;
  auto ok = FileWrite(TEST_FILE,TEST_DATA,6,flags);
  if (ok == FALSE) return __fail_and_cleanup;

  auto buffer = FileRead(TEST_FILE);
  auto len = StrLen(buffer);
  if (len != 6) return __fail_and_cleanup;
  if (StrNCmp(buffer,TEST_DATA,len) != 0) return __fail_and_cleanup;

  return __success_and_cleanup;
}

Bool TestTruncFile(U0)
{ /* We write and write the same thing with the truncation flag set meaning
   * we should only have one copy of TEST_DATA */
  auto flags = O_CREAT|O_RDWR|O_TRUNC;

  auto ok = FileWrite(TEST_FILE,TEST_DATA,6,flags);
  if (ok == FALSE) return __fail_and_cleanup;
  ok = FileWrite(TEST_FILE,TEST_DATA,6,flags);
  if (ok == FALSE) return __fail_and_cleanup;
  ok = FileWrite(TEST_FILE,TEST_DATA,6,flags);
  if (ok == FALSE) return __fail_and_cleanup;
  ok = FileWrite(TEST_FILE,TEST_DATA,6,flags);
  if (ok == FALSE) return __fail_and_cleanup;

  auto buffer = FileRead(TEST_FILE);
  auto len = StrLen(buffer);
  if (len != 6) return __fail_and_cleanup;
  if (StrNCmp(buffer,TEST_DATA,len) != 0) return __fail_and_cleanup;

  return __success_and_cleanup;
}

Bool TestAppendFile(U0)
{
  auto flags = O_CREAT|O_RDWR|O_APPEND;

  auto ok = FileWrite(TEST_FILE,TEST_DATA,6,flags);
  if (ok == FALSE) return __fail_and_cleanup;
  ok = FileWrite(TEST_FILE,TEST_DATA,6,flags);
  if (ok == FALSE) return __fail_and_cleanup;
  ok = FileWrite(TEST_FILE,TEST_DATA,6,flags);
  if (ok == FALSE) return __fail_and_cleanup;
  ok = FileWrite(TEST_FILE,TEST_DATA,6,flags);
  if (ok == FALSE) return __fail_and_cleanup;

  auto buffer = FileRead(TEST_FILE);

  auto len = StrLen(buffer);
  if (len != 6*4) return __fail_and_cleanup;
  if (StrNCmp(buffer,TEST_DATA TEST_DATA TEST_DATA TEST_DATA,6*4) != 0) 
    return __fail_and_cleanup;

  return __success_and_cleanup;
}

Bool TestReadOnlyFile(U0)
{
  auto flags = O_CREAT|O_RDONLY;
  auto ok = FileWrite(TEST_FILE,TEST_DATA,6,flags);
  /* We want the write to fail as the user should not have permissions to be 
   * able to write to the file as the write flag has not been set. */
  if (ok != FALSE) return __fail_and_cleanup;
  return __success_and_cleanup;
}

I32 Main()
{
  "Test - basic io: ";
  I64 correct = 0, tests = 4;
  if (TestWriteFile()) correct++;
  if (TestTruncFile()) correct++;
  if (TestAppendFile()) correct++;
  if (TestReadOnlyFile()) correct++;
  PrintResult(correct,tests);
  "====\n";
  return 0;
}
