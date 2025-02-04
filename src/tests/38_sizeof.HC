#include "./testhelper.HC"

Bool SizeofString()
{
  if (sizeof("hello") != 6) return FALSE;
  if (sizeof("h") != 2) return FALSE;
  if (sizeof("hello world") != 12) return FALSE;
  if (sizeof("hello" " world") != 12) return FALSE;
  if (sizeof("hey" "how " "are " " you?") != 17) return FALSE;
  return TRUE;
}

Bool SizeofChars()
{
  /* This is very quirky */
  if (sizeof('f') != 1) return FALSE;
  return TRUE;
}

Bool SizeofEscapedChars()
{
  if (sizeof("\f\f\f") != 4) return FALSE;
  if (sizeof("\\\\\\") != 4) return FALSE;
  if (sizeof("\0\0\0") != 4) return FALSE;
  if (sizeof("\n\n\n") != 4) return FALSE;
  if (sizeof("\r\r\r") != 4) return FALSE;
  if (sizeof("\t\t\t") != 4) return FALSE;
  if (sizeof("\v\v\v") != 4) return FALSE;
  if (sizeof("\"\"\"") != 4) return FALSE;
  return TRUE;
}

Bool SizeofBytes()
{
  if (sizeof("\xa3\xFF") != 2) return FALSE;
  return TRUE;
}

Bool SizeofBuiltinTypes()
{
    if (sizeof(U0)   != 0) return FALSE;
    if (sizeof(Bool) != 1) return FALSE;
    if (sizeof(I8)   != 1) return FALSE;
    if (sizeof(U8)   != 1) return FALSE;
    if (sizeof(I16)  != 2) return FALSE;
    if (sizeof(U16)  != 2) return FALSE;
    if (sizeof(I32)  != 4) return FALSE;
    if (sizeof(U32)  != 4) return FALSE;
    if (sizeof(I64)  != 8) return FALSE;
    if (sizeof(U64)  != 8) return FALSE;
    if (sizeof(F64)  != 8) return FALSE;
    return TRUE;
}

class Type1
{
  I64 a;     /* 8 */
  I16 b;     /* 2 */
  U8 buf[8]; /* 8 */
};

class Type2
{
  I64 a; /* 8 */
};

class Type3
{
  I64 a;   /* 8 */
  I16 b;   /* 2.. */
  U8 *buf; /* 8 */
};

Bool SizeofTypes()
{
  /* Structs are aligned */
  if (sizeof(Type1) != 24) return FALSE;
  if (sizeof(Type2) != 8) return FALSE;

  return TRUE;
}

Bool SizeofPointer()
{ /* Our pointers are all of size 8 */
  U8 *u8_ptr;
  I16 *i16_ptr;
  I32 *i32_ptr;
  I64 *i64_ptr;

  if (sizeof(u8_ptr) != 8) return FALSE;
  if (sizeof(i16_ptr) != 8) return FALSE;
  if (sizeof(i32_ptr) != 8) return FALSE;
  if (sizeof(i64_ptr) != 8) return FALSE;

  return TRUE;
}

Bool SizeofArrays()
{
  I8  array1[] = {1,2,3};
  I16 array2[] = {1,2,3};
  I32 array3[] = {1,2,3};
  I64 array4[] = {1,2,3};
  Type3 array5[] = {
    {1,1,"a"},
    {2,2,"b"},
    {3,3,"c"},
  };
  U8 *strs[] = {"hello", " world\n"};

  if (sizeof(array1) != sizeof(I8)  * 3) return FALSE;
  if (sizeof(array2) != sizeof(I16) * 3) return FALSE;
  if (sizeof(array3) != sizeof(I32) * 3) return FALSE;
  if (sizeof(array4) != sizeof(I64) * 3) return FALSE;
  if (sizeof(array5) != sizeof(Type3) * 3) return FALSE;


  if (sizeof(array1) / sizeof(array1[0]) != 3) return FALSE;
  if (sizeof(array2) / sizeof(array2[0]) != 3) return FALSE;
  if (sizeof(array3) / sizeof(array3[0]) != 3) return FALSE;
  if (sizeof(array4) / sizeof(array4[0]) != 3) return FALSE;
  if (sizeof(array5) / sizeof(array5[0]) != 3) return FALSE;
  if (sizeof(strs) / sizeof(strs[0]) != 2) return FALSE;

  if (sizeof(strs)    != sizeof(U8 *) * 2) return FALSE;
  if (sizeof(strs[0]) != 8) return FALSE;
  if (sizeof(strs[1]) != 8) return FALSE;

  return TRUE;
}

I32 Main()
{
  "Test - sizeof(...): \n";
  I64 tests = 8;
  I64 correct = 0;
  
  if (SizeofString()) {
    correct++;
  } else {
    "Failed - SizeofString()\n";
  }

  if (SizeofChars()) {
    correct++;
  } else {
    "Failed - SizeofChars()\n";
  }
  
  if (SizeofEscapedChars()) {
    correct++;
  } else {
    "Failed - SizeofEscapedChars()\n";
  }

  if (SizeofBytes()) {
    correct++;
  } else {
    "Failed - SizeofBytes()\n";
  }

  if (SizeofBuiltinTypes()) {
    correct++;
  } else {
    "Failed - SizeofBuiltinTypes()\n";
  }
  
  if (SizeofTypes()) {
    correct++;
  } else {
    "Failed - SizeofTypes()\n";
  }
  
  if (SizeofPointer()) {
    correct++;
  } else {
    "Failed - SizeofPointer()\n";
  }

  if (SizeofArrays()) {
    correct++;
  } else {
    "Failed - SizeofArrays()\n";
  }

  PrintResult(correct,tests);
  "====\n";
  return 0;
}
