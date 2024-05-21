class StringArray
{
  U8 **entries;
  I64 len;
  I64 cap;
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
    auto new_cap = arr->cap * 2;
    arr->entries = ReAlloc(arr->entries,new_cap);
    if (arr->entries == NULL) {
      "Failed to reallocate array\n";
      Exit;
    }
    arr->cap = new_cap;
  }
  arr->entries[arr->len++] = str;
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
  U8 *exclude[] = {"run.HC", "testhelper.HC", "tests.sh", "Makefile", "a.out"};
  auto len = sizeof(exclude)/sizeof(exclude[0]);
  for (auto i = 0; i < len; ++i) {
    if (!StrNCmp(exclude[i],filename,namelen)) return TRUE;
  }
  return FALSE;
}

I32 Main()
{
  U8 buffer[256];
  auto dir = opendir(".");
  auto arr = StringArrayNew;
  Dirent *ent;

  if (!dir) {
    "Failed to open directory\n";
    Exit;
  }


  while ((ent = readdir(dir)) != NULL) {
    if (ent->type & DT_REG) {
      if (ShouldExclude(ent->name,StrLen(ent->name))) {
        continue;
      }
      StringArrayPush(arr,StrNew(ent->name));
    }
  }

  QSortStrings(arr->entries,arr->len-1);

  for (auto i = 0; i < arr->len; ++i) {
    auto name = arr->entries[i];
    auto len = snprintf(buffer,sizeof(buffer),"hcc %s",name);
    buffer[len] = '\0';
    auto res = System(buffer);

    if (res != 0) {
      "\033[0;31mFailed to compile\033[0;0m: %s\n",name;
    } else {
      if ((res = System("./a.out")) != 0) {
        "\033[0;31mFAILED\033[0;0m to run: %s\n",name;
      }
      Rm("./a.out");
    }
  }

  closedir(dir);
  StringArrayRelease(arr);
  return 0;
}
