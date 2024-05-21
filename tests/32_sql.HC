#include "testhelper.HC"

#define DB_NAME ("./_test.db")

Bool TestInsert(SqlCtx *ctx)
{
  auto err = SqlExecRaw(ctx,
      "INSERT INTO example(id,name) VALUES 
        (1,'Foo'),
        (2,'Bar'),
        (3,'Baz'),
        (4,'Dog'),
        (5,'Cat');");
  if (err) {
    "%s\n",err;
    return FALSE;
  }
  return TRUE;
}

Bool TestDelete(SqlCtx *ctx)
{
  SqlRow row;
  I64 count = 0;

  /* Delete one */
  auto ok = SqlQuery(ctx,"DELETE FROM example WHERE id = 2;");
  if (!ok) goto error;

  /* Count them 5-1 = 4*/
  ok = SqlSelect(ctx,&row,"SELECT * FROM example;");
  if (!ok) goto error;

  count = 0;
  while (SqlIter(&row)) {
    count++;
  }

  if (count != 4) goto error;

  /* Ensure that despite the query being ok that the count is 0, as 
   * it has been deleted. */
  ok = SqlSelect(ctx,&row,"SELECT * FROM example WHERE id = 2;");
  if (!ok) goto error;

  count = 0;
  while (SqlIter(&row)) {
    count++;
  }

  return count == 0;

error:
  return FALSE;
}

Bool TestSelect(SqlCtx *ctx)
{
  SqlRow row;
  auto ok = SqlSelect(ctx,&row,"SELECT * FROM example;");
  I64 count = 0;
  if (ok) {
    while (SqlIter(&row)) {
      auto id = row.col[0].i64;
      auto name = row.col[1].str;
      count++;
      switch (count) {
        case 1: {
          if (id != 1 && StrNCmp(name,"Foo",3) != 0)
            goto error;
          break;
        }
        case: {
          if (id != 2 && StrNCmp(name,"Bar",3) != 0)
            goto error;
          break;
        }
        case: {
          if (id != 3 && StrNCmp(name,"Baz",3) != 0)
            goto error;
          break;
        }
        case: {
          if (id != 4 && StrNCmp(name,"Dog",3) != 0)
            goto error;
          break;
        }
        case: {
          if (id != 5 && StrNCmp(name,"Cat",3) != 0)
            goto error;
          break;
        }
      }
    }
  }
  return count == 5;
error:
  return FALSE;
}

Bool TestCreate(SqlCtx *ctx)
{
  auto err = SqlExecRaw(ctx,
      "CREATE TABLE IF NOT EXISTS example(
         id INTEGER PRIMARY KEY, 
         name TEXT
       );");
  if (err) {
    "%s\n",err;
    return FALSE;
  }
  return TRUE;
}

I32 Main()
{// These tests need to be run in a specific order.
  "Test - sqlite bindings & abstraction: \n";
  I64 tests = 4, correct = 0;
  auto ctx = SqlCtxNew(DB_NAME);

  if (TestCreate(ctx)) correct++;
  if (TestInsert(ctx)) correct++;
  if (TestSelect(ctx)) correct++;
  if (TestDelete(ctx)) correct++;

  /* Destroy connection and free class */
  SqlCtxRelease(ctx);
  /* Delete the database */
  unlink(DB_NAME);
  PrintResult(correct,tests);
  "====\n";
  return 0;
}
