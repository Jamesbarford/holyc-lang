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

I64 IsX86_64Target(U8 *target)
{
  return StrNCmp(target, "x86_64-", 7) == 0;
}

U0 RunTest(U8 *filename)
{
  U8 buffer[512];
  I64 len;

  /* Use the local build (../../hcc relative to src/tests/) so we
   * always test what's actually been compiled, not whatever older
   * hcc happens to be on PATH (e.g. an installed /usr/local/bin/hcc
   * from a prior version - this trap cost us a session of confused
   * cross-test triage). */
  if (g_target[0] == '\0') {
    len = snprintf(buffer, sizeof(buffer), "../../hcc %s", filename);
  } else {
    len = snprintf(buffer, sizeof(buffer),
                   "../../hcc --target=%s --install-dir=../../build/%s %s",
                   g_target, g_target, filename);
  }
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

  /* Argv contract:
   *   ./run                     -> host arch, all tests
   *   ./run --target=<triple>   -> cross to <triple>, all tests
   *   ./run <file.HC>           -> host arch, single file
   *   ./run --target=<triple> <file.HC>
   *                             -> cross to <triple>, single file
   *
   * Disambiguation is by the --target= prefix so the existing
   * single-file workflow keeps working unchanged. */
  g_target = "";
  I64 arg_idx = 1;
  if (argc >= 2 && StrNCmp(argv[1], "--target=", 9) == 0) {
    g_target = argv[1] + 9;
    arg_idx = 2;
  }

  if (argc > arg_idx) {
    RunTest(argv[arg_idx]);
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
