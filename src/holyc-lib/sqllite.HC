/* Factoid: Terry was not a fan of SQL. */
#define SQLITE_OK           0   /* Successful result */
/* beginning-of-error-codes */
#define SQLITE_ERROR        1   /* Generic error */
#define SQLITE_INTERNAL     2   /* Internal logic error in SQLite */
#define SQLITE_PERM         3   /* Access permission denied */
#define SQLITE_ABORT        4   /* Callback routine requested an abort */
#define SQLITE_BUSY         5   /* The database file is locked */
#define SQLITE_LOCKED       6   /* A table in the database is locked */
#define SQLITE_NOMEM        7   /* A malloc() failed */
#define SQLITE_READONLY     8   /* Attempt to write a readonly database */
#define SQLITE_INTERRUPT    9   /* Operation terminated by sqlite3_interrupt()*/
#define SQLITE_IOERR       10   /* Some kind of disk I/O error occurred */
#define SQLITE_CORRUPT     11   /* The database disk image is malformed */
#define SQLITE_NOTFOUND    12   /* Unknown opcode in sqlite3_file_control() */
#define SQLITE_FULL        13   /* Insertion failed because database is full */
#define SQLITE_CANTOPEN    14   /* Unable to open the database file */
#define SQLITE_PROTOCOL    15   /* Database lock protocol error */
#define SQLITE_EMPTY       16   /* Internal use only */
#define SQLITE_SCHEMA      17   /* The database schema changed */
#define SQLITE_TOOBIG      18   /* String or BLOB exceeds size limit */
#define SQLITE_CONSTRAINT  19   /* Abort due to constraint violation */
#define SQLITE_MISMATCH    20   /* Data type mismatch */
#define SQLITE_MISUSE      21   /* Library used incorrectly */
#define SQLITE_NOLFS       22   /* Uses OS features not supported on host */
#define SQLITE_AUTH        23   /* Authorization denied */
#define SQLITE_FORMAT      24   /* Not used */
#define SQLITE_RANGE       25   /* 2nd parameter to sqlite3_bind out of range */
#define SQLITE_NOTADB      26   /* File opened that is not a database file */
#define SQLITE_NOTICE      27   /* Notifications from sqlite3_log() */
#define SQLITE_WARNING     28   /* Warnings from sqlite3_log() */
#define SQLITE_ROW         100  /* sqlite3_step() has another row ready */
#define SQLITE_DONE        101  /* sqlite3_step() has finished executing */

#define SQLITE_INTEGER  1
#define SQLITE_FLOAT    2
#define SQLITE_BLOB     4
#define SQLITE_NULL     5
#define SQLITE_TEXT     3
#define SQLITE3_TEXT    3

#define SQL_INT   SQLITE_INTEGER
#define SQL_FLOAT SQLITE_FLOAT
#define SQL_TEXT  SQLITE3_TEXT
#define SQL_BLOB  SQLITE_BLOB
#define SQL_NULL  SQLITE_NULL

class sqlite3_stmt {};
class sqlite3 {};
class sqlite3_value {};

class SqlParam
{
  I64 type;
  union 
  {
    I64 i64;
    F64 f64;
    U8 *str;
    class 
    {
      U0 *blob;
      I64 blob_len;
    };
  };
};

class SqlPreparedStmt
{
  sqlite3_stmt *stmt;
  I64  params_count;
  I64 *params_type;
};

class SqlColumn
{
  I64 len;
  I64 type;
  union {
    I64 i64;
    U8 *str;
    U0 *blob;
    F64 f64;
  };
};

class SqlRow
{
  sqlite3_stmt *stmt;
  I64 cols;
  SqlColumn *col;
};

class SqlCtx
{
  sqlite3 *conn;
  U8 *dbname;
};

public extern "c" I32 sqlite3_prepare_v2(
  sqlite3 *db,            /* Database handle */
  U8 *zSql,               /* SQL statement, UTF-8 encoded */
  I32 nByte,              /* Maximum length of zSql in bytes. */
  sqlite3_stmt **ppStmt,  /* OUT: Statement handle */
  U8 **pzTail             /* OUT: Pointer to unused portion of zSql */
);

public extern "c" I32 sqlite3_bind_parameter_count(sqlite3_stmt *stmt);
public extern "c" I32 sqlite3_close(sqlite3 *db);
public extern "c" I32 sqlite3_clear_bindings(sqlite3_stmt *stmt);
public extern "c" I32 sqlite3_reset(sqlite3_stmt *pStmt);
public extern "c" I32 sqlite3_finalize(sqlite3_stmt *pStmt);
public extern "c" I32 sqlite3_exec(
  sqlite3 *db,                               /* An open database */
  U8 *sql,                                  /* SQL to be evaluated */
  I32 (*callback)(U0 *_priv, I32 argc, U8 **ptr, U8 **ptr2),  /* Callback function */
  U8 *priv,                                    /* 1st argument to callback */
  U8 **errmsg                              /* Error msg written here */
);
public extern "c" I32 sqlite3_bind_blob(sqlite3_stmt *stmt, I32 __i, U0 *__ptr, I32 n,
    U0 *(*_bind)(U0 *param));
public extern "c" I32 sqlite3_bind_blob64(sqlite3_stmt *stmt, I32 __i, U0 *__ptr, I64 __i2,
    U0 *(*_bind)(U0 *param));
public extern "c" I32 sqlite3_bind_double(sqlite3_stmt *stmt, I32 __i, F64 __f64);
public extern "c" I32 sqlite3_bind_int(sqlite3_stmt *stmt, I32 __i, I32 __int);
public extern "c" I32 sqlite3_bind_int64(sqlite3_stmt *stmt, I32 __i, I64 __int64);
public extern "c" I32 sqlite3_bind_null(sqlite3_stmt *stmt, I32 __i);
public extern "c" I32 sqlite3_bind_text(sqlite3_stmt *stmt, I32 __i, U8 *__buf, I32 __i2, 
    U0 *(*_bind)(U0 *param));
public extern "c" I32 sqlite3_bind_text16(sqlite3_stmt *stmt, I32 __i, U0 *__buf, I32 __i2, 
    U0 *(*_bind)(U0 *param));
public extern "c" I32 sqlite3_bind_text64(sqlite3_stmt *stmt, I32 __i, U8 *__buf, I64 __int64,
    U0 *(*_bind)(U0 *param), U8 encoding);
public extern "c" I32 sqlite3_bind_value(sqlite3_stmt *stmt, I32 __i, sqlite3_value *__val);

public extern "c" I32 sqlite3_step(sqlite3_stmt *stmt);
public extern "c" I32 sqlite3_data_count(sqlite3_stmt *pStmt);

public extern "c" I32 sqlite3_column_bytes(sqlite3_stmt *stmt, I32 iCol);
public extern "c" U0 *sqlite3_column_blob(sqlite3_stmt *stmt, I32 iCol);
public extern "c" F64 sqlite3_column_double(sqlite3_stmt *stmt, I32 iCol);
public extern "c" I32 sqlite3_column_int(sqlite3_stmt *stmt, I32 iCol);
public extern "c" I64 sqlite3_column_int64(sqlite3_stmt *stmt, I32 iCol);
public extern "c" U8 *sqlite3_column_text(sqlite3_stmt *stmt, I32 iCol);
public extern "c" U0 *sqlite3_column_text16(sqlite3_stmt *stmt, I32 iCol);
public extern "c" sqlite3_value *sqlite3_column_value(sqlite3_stmt *stmt, I32 iCol);
public extern "c" I32 sqlite3_column_bytes(sqlite3_stmt *stmt, I32 iCol);
public extern "c" I32 sqlite3_column_bytes16(sqlite3_stmt *stmt, I32 iCol);
public extern "c" I32 sqlite3_column_type(sqlite3_stmt *stmt, I32 iCol);
public extern "c" Bool sqlite3_open(U8 *dbname, sqlite3 **db);
public extern "c" U8 *sqlite3_errmsg(sqlite3 *db);

SqlCtx *SqlCtxNew(U8 *dbname, Bool printerror=FALSE)
{
  sqlite3 *db;
  SqlCtx *ctx;
  Bool ok = sqlite3_open(dbname, &db);
  
  if (ok != SQLITE_OK) {
    if (printerror) {
      U8 *s = sqlite3_errmsg(db);
      "NOT OK: %s\n", s;
    }
    return NULL;
  }

  ctx = MAlloc(sizeof(SqlCtx));
  ctx->dbname = dbname;
  ctx->conn = db;
  return ctx;
}

U0 SqlCtxRelease(SqlCtx *ctx)
{
  sqlite3_close(ctx->conn);
  Free(ctx);
}

SqlPreparedStmt *SqlPrepare(SqlCtx *ctx, U8 *sql)
{
  SqlPreparedStmt *pstmt = MAlloc(sizeof(SqlPreparedStmt));
  sqlite3_prepare_v2(ctx->conn, sql, -1, &(pstmt->stmt), NULL);
  pstmt->params_count = sqlite3_bind_parameter_count(pstmt->stmt);
  pstmt->params_type = MAlloc(pstmt->params_count * sizeof(I64));
  return pstmt;
}

static U0 SqlReset(SqlPreparedStmt *pstmt)
{
  sqlite3_clear_bindings(pstmt->stmt);
  sqlite3_reset(pstmt->stmt);
}

U0 SqlFinalizePrepared(SqlPreparedStmt *pstmt)
{
  sqlite3_finalize(pstmt->stmt);
  Free(pstmt->params_type);
  Free(pstmt);
}

/* Unsafe */ 
U8 *SqlExecRaw(SqlCtx *ctx, U8 *sql)
{
  U8 *errmsg;
  I32 rc = sqlite3_exec(ctx->conn, sql, 0, 0, &errmsg);
  if (!rc) {
    return errmsg;
  }
  return NULL;
}

I32 SqlExecPrepared(SqlPreparedStmt *pstmt, SqlRow *row, SqlParam *params)
{
  I32 rc = 0;
  SqlReset(pstmt);

  if (row) {
    row->stmt = NULL;
  }

  for (auto i = 0; i < pstmt->params_count; ++i) {
    SqlParam *param = &params[i];
    switch (param->type) {
      case SQL_INT:
        sqlite3_bind_int64(pstmt->stmt, i + 1, param->i64);
        break;
      case SQL_FLOAT:
        sqlite3_bind_double(pstmt->stmt, i + 1, param->f64);
        break;
      case SQL_TEXT:
        sqlite3_bind_text(pstmt->stmt, i + 1, param->str, -1, NULL);
        break;
      case SQL_BLOB:
        sqlite3_bind_blob(pstmt->stmt, i + 1, param->blob, param->blob_len,
            NULL);
        break;
      case SQL_NULL:
        sqlite3_bind_text(pstmt->stmt, i + 1, NULL, -1, NULL);
        break;
    }
  }

  /* Execute. */
  rc = sqlite3_step(pstmt->stmt);
  if (rc == SQLITE_ROW && row) {
    row->stmt = pstmt->stmt;
    row->cols = 0;
    row->col = NULL;
  }

  return rc;
}

static I32 SqlExecQuery(SqlCtx *ctx, SqlRow *row, U8 *sql, SqlParam *params,
                        I32 param_count)
{
  I32 rc = SQLITE_ERROR;
  sqlite3_stmt *stmt = NULL;

  if (row) {
    row->stmt = NULL;
  }

  rc = sqlite3_prepare_v2(ctx->conn, sql, -1, &stmt, NULL);

  if (rc != SQLITE_OK) {
    goto err;
  }

  for (auto i = 0; i < param_count; ++i) {
    switch (params[i].type) {
      case SQL_INT:
        /* The leftmost SQL parameter has an index of 1 ref:
         * https://www.sqlite.org/c3ref/bind_blob.html */
        rc = sqlite3_bind_int64(stmt, i + 1, params[i].i64);
        break;
      case SQL_FLOAT:
        rc = sqlite3_bind_double(stmt, i + 1, params[i].f64);
        break;
      case SQL_TEXT:
        rc = sqlite3_bind_text(stmt, i + 1, params[i].str, -1, NULL);
        break;
      case SQL_BLOB:
        rc = sqlite3_bind_blob(stmt, i + 1, params[i].blob,
            params[i].blob_len, NULL);
        break;
      default:
        goto err;
    }
    if (rc != SQLITE_OK) {
      goto err;
    }
  }

  /* Execute. */
  rc = sqlite3_step(stmt);
  if (rc == SQLITE_ROW && row) {
    row->stmt = stmt;
    row->cols = 0;
    row->col = NULL;
    stmt = NULL;
  }

err:
  if (stmt) {
    sqlite3_finalize(stmt);
  }
  return rc;
}

U0 SqlRowRelease(SqlRow *row)
{
  if (row->stmt == NULL) {
    return;
  }
  Free(row->col);
  sqlite3_finalize(row->stmt);
  row->col = NULL;
  row->stmt = NULL;
}

Bool SqlSelect(SqlCtx *ctx, SqlRow *row, U8 *sql, SqlParam *params=NULL,
    I32 param_count=0)
{
  auto result = SqlExecQuery(ctx,row,sql,params,param_count);
  return result == SQLITE_DONE || result == SQLITE_ROW;
}

Bool SqlQuery(SqlCtx *ctx, U8 *sql, SqlParam *params=NULL, I32 param_count=0)
{
  auto result = SqlExecQuery(ctx,NULL,sql,params,param_count);
  return result == SQLITE_DONE || result == SQLITE_ROW;
}

static Bool SqlIterGeneric(SqlRow *row, I32 free_row)
{
  if (row->stmt == NULL) {
    return FALSE;
  }

  if (row->col != NULL) {
    if (sqlite3_step(row->stmt) != SQLITE_ROW) {
      if (free_row) {
        SqlRowRelease(row);
      }
      return FALSE;
    }
  }

  /* XXX: MAlloc's and Free's on each call which is a bit expensive seeing 
   * as this function will be called in a loop */
  Free(row->col);
  row->cols = sqlite3_data_count(row->stmt);
  row->col = MAlloc(row->cols * sizeof(SqlColumn));

  for (auto i = 0; i < row->cols; i++) {
    row->col[i].type = sqlite3_column_type(row->stmt,i);
    switch (row->col[i].type) {
      case SQLITE_INTEGER:
        row->col[i].i64 = sqlite3_column_int64(row->stmt,i);
        break;
      case SQLITE_FLOAT:
        row->col[i].f64 = sqlite3_column_double(row->stmt,i);
        break;
      case SQLITE3_TEXT:
        row->col[i].str = sqlite3_column_text(row->stmt,i)(U8 *);
        row->col[i].len = sqlite3_column_bytes(row->stmt,i);
        break;
      case SQLITE_BLOB:
        row->col[i].blob = sqlite3_column_blob(row->stmt,i)(U0 *);
        row->col[i].len = sqlite3_column_bytes(row->stmt,i);
        break;
      default:
        /* XXX: You may want to handle this case differently */
        row->col[i].str = NULL;
        row->col[i].i64 = 0;
        row->col[i].f64 = 0;
        break;
    }
  }
  return TRUE;
}

I32 SqlIter(SqlRow *row)
{
  return SqlIterGeneric(row,1);
}

I64 SqlIterPrepared(SqlRow *row)
{
  return SqlIterGeneric(row,0);
}
