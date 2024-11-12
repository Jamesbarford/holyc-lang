I64 CmpFileNames(U8 *str1, U8 *str2)
{
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
  return 0;
}

U0 QSortStrings(U8 **arr, I64 high, I64 low=0)
{
  if (low < high) {
    auto pivot = arr[high];
    auto idx = low;

    for (auto i = low; i < high; ++i) {
      if (CmpFileNames(arr[i],pivot)) {
        auto tmp = arr[i];
        arr[i] = arr[idx];
        arr[idx] = tmp;
        idx++;
      }
    }

    arr[high] = arr[idx];
    arr[idx] = pivot;
    QSortStrings(arr,high,idx+1);
    QSortStrings(arr,idx-1,low);
  }
}

Bool ShouldExclude(U8 *filename, I64 namelen)
{
  U8 *exclude[] = {
    "run.HC",
    "testhelper.HC",
    "tests.sh",
    "Makefile",
    "a.out",
#ifndef __HCC_LINK_SQLITE3__
    "32_sql.HC",
#endif
    "test-runner"};

  for (U8 *name : exclude) {
    if (!StrNCmp(name,filename,namelen)) return TRUE;
  }
  return FALSE;
}

U0 RunTest(U8 *filename)
{
  U8 buffer[256];
  auto len = snprintf(buffer,sizeof(buffer),"hcc %s",filename);
  buffer[len] = '\0';
  auto res = System(buffer);

  if (res != 0) {
    "\033[0;31mFailed to compile\033[0;0m: %s\n",filename;
  } else {
    if ((res = System("./a.out")) != 0) {
      "\033[0;31mFAILED\033[0;0m to run: %s\n",filename;
    }
    Rm("./a.out");
  }
}

I32 Main(I32 argc, U8 **argv)
{
  auto dir = opendir(".");
  auto vec = PtrVecNew();
  Dirent *ent;

  if (argc != 1) {
    RunTest(argv[1]);
    Exit(0);
  }

  if (!dir) {
    "Failed to open directory\n";
    Exit;
  }

  while ((ent = readdir(dir)) != NULL) {
    if (ent->type & DT_REG) {
      auto len = StrLen(ent->name);
      auto res = FzF(ent->name,"HC$");

      if (ShouldExclude(ent->name,len) || res == NULL) {
        continue;
      }
      FzFPositionRelease(res);
      auto str = StrNew(ent->name);
      PtrVecPush(vec,str);
    }
  }

  QSortStrings(vec->entries(U8 **),vec->size-1);

  for (U8 *entry : vec) {
    RunTest(entry);
    Free(entry);
  }

  closedir(dir);
  PtrVecRelease(vec);
  return 0;
}
