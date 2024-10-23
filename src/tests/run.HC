class StringArray
{
  I64 len;
  I64 cap;
  U8 **entries;
};

StringArray *StringArrayNew()
{
  StringArray *arr = MAlloc(sizeof(StringArray));
  arr->entries = MAlloc(sizeof(U8 *) * 32);
  arr->cap = 32;
  arr->len = 0;
  return arr;
}

U0 StringArrayRelease(StringArray *arr)
{
  for (auto i = 0; i < arr->len; ++i) {
    Free(arr->entries[i]);
  }
  Free(arr->entries);
  Free(arr);
}

U0 StringArrayPush(StringArray *arr, U8 *str)
{
  if (arr->len + 1 >= arr->cap) {
    auto new_cap = arr->cap * 2 * sizeof(U8 *);
    auto e = arr->entries;
    auto new_entries = ReAlloc(e,new_cap);
    if (new_entries == NULL) {
      "Failed to reallocate array\n";
      Exit;
    }
    arr->cap = new_cap;
    arr->entries = new_entries;
  }
  arr->entries[arr->len] = str;
  arr->len++;
}

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
  auto len = sizeof(exclude)/sizeof(exclude[0]);
  for (auto i = 0; i < len; ++i) {
    if (!StrNCmp(exclude[i],filename,namelen)) return TRUE;
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
  auto arr = StringArrayNew;
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
      StringArrayPush(arr,str);
    }
  }

  QSortStrings(arr->entries,arr->len-1);

  for (auto i = 0; i < arr->len; ++i) {
    RunTest(arr->entries[i]);
  }

  closedir(dir);
  StringArrayRelease(arr);
  return 0;
}
