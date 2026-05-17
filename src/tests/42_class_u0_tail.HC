#include "testhelper.HC"

/* TempleOS "flexible array" trick: a trailing `U0 body;` member is a
 * zero-size placeholder whose address points to the byte just past the
 * real fields. Allocate `sizeof(Hdr) + N`, then read/write N payload
 * bytes via `&hdr->body`. */

class Hdr
{
  I64 size;
  /* Flexibly array member */
  U0  body;
};

Bool LayoutMatchesSizeof()
{
  Hdr *h = MAlloc(64);
  I64 base   = h(U8 *);
  I64 bodyat = &h->body;
  I64 ok = sizeof(Hdr) == 8 && bodyat - base == 8;
  Free(h);
  return ok;
}

Bool RoundTripPayload()
{
  U8 *src = "hello";
  I64 n = 5;
  /* Creating the flexible Array, all be it dynamically. You can only `MemCpy`
   * in and out of it*/
  Hdr *h = MAlloc(sizeof(Hdr) + n);
  h->size = n;
  MemCpy(&h->body, src, n);

  U8 buf[8];
  MemCpy(buf, &h->body, h->size);
  buf[h->size] = 0;

  Bool ok = h->size == 5 && !StrCmp(buf, "hello");
  Free(h);
  return ok;
}

class HdrMixed
{
  I64 a;
  U8  b;
  U0  body;
};

Bool MixedFieldsAligned()
{
  HdrMixed *h = MAlloc(64);
  I64 base   = h(U8 *);
  I64 bodyat = &h->body;
  /* Real fields occupy 0..8 (I64) + 8..9 (U8) = 9 bytes. body sits at
   * offset 9; class size pads up to the next 8-byte boundary. */
  Bool ok = bodyat - base == 9 && sizeof(HdrMixed) == 16;
  Free(h);
  return ok;
}

I32 Main()
{
  "Test - Trailing U0 placeholder fields: ";
  I64 correct = 0, tests = 3;
  if (LayoutMatchesSizeof) correct++;
  if (RoundTripPayload) correct++;
  if (MixedFieldsAligned) correct++;
  PrintResult(correct, tests);
  "====\n";
  return 0;
}
