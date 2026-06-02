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
    "run_shared.HC",
    "run_jit.HC",
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

/* Cross-compilation hook.
 *
 * When the runner is invoked with `--target=<triple>` as argv[1] we
 * pass `--target=<triple> --install-dir=../../build/<triple>` to hcc
 * for each test, then invoke the resulting binary via the
 * appropriate emulator (`arch -x86_64 ./a.out` on Apple Silicon for
 * x86_64 targets). Empty target string = host arch, default flow.
 */
/* HolyC doesn't seem to honour `U8 *name = "lit";` initialisers
 * for globals - the pointer comes through NULL and any later
 * dereference segfaults. Declare uninitialised and assign in Main
 * before any reader runs. */
U8 *g_target;

PtrVec *CollectTests()
{
  auto dir = opendir(".");
  if (!dir) {
    "Failed to open directory\n";
    Exit;
  }
  auto vec = PtrVecNew();
  Dirent *ent;
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
  closedir(dir);
  return vec;
}
