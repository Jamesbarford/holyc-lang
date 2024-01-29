/**
 * The original can be found:
 * https://github.com/skeeto/scratch/blob/master/parsers/csv.c 
 * 
 * From the docs:
 * Parses a CSV file in memory, building an index over a chosen field
 * allowing for exact-match searches against that field, returning the
 * row index/offset/length. The library itself makes no allocations,
 * which is left to the caller, and the index is just a single, large
 * allocation. While the index references the CSV buffer, it contains no
 * pointers into that buffer, only offsets, so it can be serialized for
 * later use with a different copy of the CSV buffer.
 */
#define CSV_EOF   0
#define CSV_ROW   1
#define CSV_FIELD 2

class CsvParser
{
  U8 *csv;
  U64 len,off,row_off,nrows,nfields;
  I64 state;
};

class CsvSlice
{
  U64 idx,off,len;
};

U0 CsvParserInit(CsvParser *p, U8 *csv, U64 len)
{
  p->csv = csv;
  p->len = len;
  p->off = p->row_off = p->nrows = p->nfields = p->state = 0;
}

I64 CsvParse(CsvParser *p, CsvSlice *s)
{
  U8 *csv = p->csv;
  switch (p->state) {
    case 0:
      s->idx = p->nfields++;
      s->off = p->off;
      s->len = 0;
      for (I64 state = 1; p->off < p->len;) {
        I64 b = csv[p->off++];
        state ^= b == 0x22;
        switch (-state & b) {
          case 0x2C: return CSV_FIELD;
          case 0x0D:
            p->off += p->off < p->len && csv[p->off] == 0x0A;
          case 0x0A:
            p->state = 1;
            return CSV_FIELD;
        }
        s->len++;
      }
      p->state = 1;
      return CSV_FIELD;

    case 1:
      s->idx = p->nrows++;
      s->off = p->row_off;
      s->len = p->off - p->row_off;
      p->nfields = 0;
      p->row_off = p->off;
      if (p->off < p->len) p->state = 0;
      else                 p->state = 2;
      return CSV_ROW;

    case 2: return CSV_EOF;
  }
  return CSV_EOF;
}
