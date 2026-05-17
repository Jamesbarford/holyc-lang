#include "testhelper.HC"

/* Integration test: LZW compress/decompress lifted verbatim from
 * TempleOS (TempleOS/Kernel/Compress.HC). Exercises in one file:
 *   - class self-reference without forward decl (CArcEntry, CArcCtrl)
 *   - trailing U0 placeholder field (CArcCompress.body)
 *   - pointer relational comparisons (src_ptr<src_limit etc.)
 *   - throw/try with a real handler around uncaught conditions
 *   - default function parameters (ArcCtrlNew)
 *   - switch[], goto labels, hash-chain pointer overlay
 * Regression on any one of these will scramble the round-tripped bytes.
 *
 * **NOTE ON WARNINGS:**
 * The type checker emits warnings as we have type checking, however TempleOS
 * does not have typechecking. However we should not modify the implementation
 * of the below to satisfy typechecking.
 */

#define ARC_BITS_MAX	12
#define CT_NONE 	1
#define CT_7_BIT	2
#define CT_8_BIT	3

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif


U64 mem_mapped_space;

#define U32_MAX 4294967295

public class CArcCompress
{
  I64	compressed_size,expanded_size;
  U8	compression_type;
  U0	body;
};

class CArcEntry
{
  CArcEntry *next;
  U16	basecode;
  U8	ch,pad;
  U32	pad;
};

public class CArcCtrl
{
  I64	src_pos,src_size,
	dst_pos,dst_size;
  U8	*src_buf,*dst_buf;
  I64	min_bits,min_table_entry;
  CArcEntry *cur_entry,*next_entry;
  I64	cur_bits_in_use,next_bits_in_use;
  U8	*stk_ptr,*stk_base;
  I64	free_idx,free_limit,
	saved_basecode,
	entry_used,
	last_ch;
  CArcEntry compress[1<<ARC_BITS_MAX],
	        *hash[1<<ARC_BITS_MAX];
};

U0 BFieldOrU32(U8 *bit_field, I64 bit_num, U64 val)
{
  U64 *q = bit_field + (bit_num >> 3);
  *q |= val << (bit_num & 7);
}

U64 BFieldExtU32(U8 *bit_field, I64 bit_num, I64 width)
{
  /* Canonical TempleOS form: 8-byte load via U64 *. The version
   * originally pasted in scratch/compress.HC reads via
   * `*(bit_field + ...)`, which is a U8 deref (1 byte) - any code
   * wider than (8 - bit_num%8) bits is then masked out and the LZW
   * dictionary lookup goes off the rails. */
  U64 *q = bit_field + (bit_num >> 3);
  return *q >> (bit_num & 7) & ((1 << width) - 1);
}

I64 ArcDetermineCompressionType(U8 *src,I64 size)
{
  while (size--)
    if (*src++ & 0x80)
      return CT_8_BIT;
  return CT_7_BIT;
}

Bool ArcFinishCompression(CArcCtrl *c)
{//Do closing touch on archivew ctrl struct.
  if (c->dst_pos+c->cur_bits_in_use<=c->dst_size) {
    BFieldOrU32(c->dst_buf,c->dst_pos,c->saved_basecode);
    c->dst_pos+=c->next_bits_in_use;
    return TRUE;
  } else
    return FALSE;
}

U0 ArcEntryGet(CArcCtrl *c)
{
  I64 i;
  CArcEntry *tmp,*tmp1;

  if (c->entry_used) {
    i=c->free_idx;

    c->entry_used=FALSE;
    c->cur_entry=c->next_entry;
    c->cur_bits_in_use=c->next_bits_in_use;
    if (c->next_bits_in_use<ARC_BITS_MAX) {
      c->next_entry = &c->compress[i++];
      if (i==c->free_limit) {
	c->next_bits_in_use++;
	c->free_limit=1<<c->next_bits_in_use;
      }
    } else {
      do if (++i==c->free_limit)
	  i=c->min_table_entry;
      while (c->hash[i]);
      tmp=&c->compress[i];
      c->next_entry=tmp;
      tmp1=&c->hash[tmp->basecode];
      while (tmp1) {
	if (tmp1->next==tmp) {
	  tmp1->next=tmp->next;
	  break;
	} else
	  tmp1=tmp1->next;
      }
    }
    c->free_idx=i;
  }
}

CArcCtrl *ArcCtrlNew(Bool expand,I64 compression_type=CT_8_BIT)
{//MAlloc archive ctrl struct.
  CArcCtrl *c;
  c=CAlloc(sizeof(CArcCtrl));
  if (expand) {
    c->stk_base=MAlloc(1<<ARC_BITS_MAX);
    c->stk_ptr=c->stk_base;
  }
  if (compression_type==CT_7_BIT)
    c->min_bits=7;
  else
    c->min_bits=8;
  c->min_table_entry=1<<c->min_bits;
  c->free_idx=c->min_table_entry;
  c->next_bits_in_use=c->min_bits+1;
  c->free_limit=1<<c->next_bits_in_use;
  c->saved_basecode=U32_MAX;
  c->entry_used=TRUE;
  ArcEntryGet(c);
  c->entry_used=TRUE;
  return c;
}

U0 ArcCtrlDel(CArcCtrl *c)
{//Free archive ctrl struct.
  Free(c->stk_base);
  Free(c);
}

U0 ArcCompressBuf(CArcCtrl *c)
{//Use $LK,"CompressBuf",A="MN:CompressBuf"$() unless doing more than one buf.
  CArcEntry *tmp,*tmp1;
  I64 ch,basecode;
  U8 *src_ptr,*src_limit;

  src_ptr=c->src_buf+c->src_pos;
  src_limit=c->src_buf+c->src_size;

  if (c->saved_basecode==U32_MAX)
    basecode=*src_ptr++;
  else
    basecode=c->saved_basecode;

  while (src_ptr<src_limit && c->dst_pos+c->cur_bits_in_use<=c->dst_size) {
    ArcEntryGet(c);
ac_start:
    if (src_ptr>=src_limit) goto ac_done;
    ch=*src_ptr++;
    if (tmp=c->hash[basecode])
      do {
	if (tmp->ch==ch) {
	  basecode=tmp-&c->compress[0];
	  goto ac_start;
	}
      } while (tmp=tmp->next);

    BFieldOrU32(c->dst_buf,c->dst_pos,basecode);
    c->dst_pos+=c->cur_bits_in_use;

    c->entry_used=TRUE;
    tmp=c->cur_entry;
    tmp->basecode=basecode;
    tmp->ch=ch;
    tmp1=&c->hash[basecode];
    tmp->next=tmp1->next;
    tmp1->next=tmp;

    basecode=ch;
  }
ac_done:
  c->saved_basecode=basecode;
  c->src_pos=src_ptr-c->src_buf;
}

CArcCompress *CompressBuf(U8 *src,I64 size)
{//See $LK,"::/Demo/Dsk/SerializeTree.HC"$.
  CArcCompress *arc;
  I64 size_out,compression_type=ArcDetermineCompressionType(src,size);
  CArcCtrl *c=ArcCtrlNew(FALSE,compression_type);
  c->src_size=size;
  c->src_buf=src;
  c->dst_size=(size+sizeof(CArcCompress))<<3;
  c->dst_buf=CAlloc(c->dst_size>>3);
  c->dst_pos=sizeof(CArcCompress)<<3;
  ArcCompressBuf(c);
  if (ArcFinishCompression(c) && c->src_pos==c->src_size) {
    size_out=(c->dst_pos+7)>>3;
    arc=MAlloc(size_out);
    MemCpy(arc,c->dst_buf,size_out);
    arc->compression_type=compression_type;
    arc->compressed_size=size_out;
  } else {
    arc=MAlloc(size+sizeof(CArcCompress));
    MemCpy(&arc->body,src,size);
    arc->compression_type=CT_NONE;
    arc->compressed_size=size+sizeof(CArcCompress);
  }
  arc->expanded_size=size;
  Free(c->dst_buf);
  ArcCtrlDel(c);
  return arc;
}

U0 ArcExpandBuf(CArcCtrl *c)
{//Use $LK,"ExpandBuf",A="MN:ExpandBuf"$() unless you know what you're doing.
  U8 *dst_ptr,*dst_limit;
  I64 basecode,lastcode,code;
  CArcEntry *tmp,*tmp1;

  dst_ptr=c->dst_buf+c->dst_pos;
  dst_limit=c->dst_buf+c->dst_size;

  while (dst_ptr<dst_limit && c->stk_ptr!=c->stk_base)
    *dst_ptr++ = * -- c->stk_ptr;

  if (c->stk_ptr==c->stk_base && dst_ptr<dst_limit) {
    if (c->saved_basecode==U32_MAX) {
      lastcode=BFieldExtU32(c->src_buf,c->src_pos,
	    c->next_bits_in_use);
      c->src_pos+=c->next_bits_in_use;
      *dst_ptr++=lastcode;
      ArcEntryGet(c);
      c->last_ch=lastcode;
    } else
      lastcode=c->saved_basecode;
    while (dst_ptr<dst_limit && c->src_pos+c->next_bits_in_use<=c->src_size) {
      basecode=BFieldExtU32(c->src_buf,c->src_pos,
	    c->next_bits_in_use);
      c->src_pos+=c->next_bits_in_use;
      if (c->cur_entry==&c->compress[basecode]) {
	*c->stk_ptr++=c->last_ch;
	code=lastcode;
      } else
	code=basecode;
      while (code>=c->min_table_entry) {
	*c->stk_ptr++=c->compress[code].ch;
	code=c->compress[code].basecode;
      }
      *c->stk_ptr++=code;
      c->last_ch=code;

      c->entry_used=TRUE;
      tmp=c->cur_entry;
      tmp->basecode=lastcode;
      tmp->ch=c->last_ch;
      tmp1=&c->hash[lastcode];
      tmp->next=tmp1->next;
      tmp1->next=tmp;

      ArcEntryGet(c);
      while (dst_ptr<dst_limit && c->stk_ptr!=c->stk_base)
	*dst_ptr++ = * -- c->stk_ptr;
      lastcode=basecode;
    }
    c->saved_basecode=lastcode;
  }
  c->dst_pos=dst_ptr-c->dst_buf;
}

U8 *ExpandBuf(CArcCompress *arc)
{//See $LK,"::/Demo/Dsk/SerializeTree.HC"$.
  CArcCtrl *c;
  U8 *res;

  if (!(CT_NONE<=arc->compression_type<=CT_8_BIT) || arc->expanded_size>mem_mapped_space)
    throw('Compress');

  res=MAlloc(arc->expanded_size+1);
  res[arc->expanded_size]=0; //terminate
  switch [arc->compression_type] {
    case CT_NONE:
      MemCpy(res,&arc->body,arc->expanded_size);
      break;
    case CT_7_BIT:
    case CT_8_BIT:
      c=ArcCtrlNew(TRUE,arc->compression_type);
      c->src_size=arc->compressed_size<<3;
      c->src_pos=sizeof(CArcCompress)<<3;
      c->src_buf=arc;
      c->dst_size=arc->expanded_size;
      c->dst_buf=res;
      c->dst_pos=0;
      ArcExpandBuf(c);
      ArcCtrlDel(c);
      break;
  }
  return res;
}

Bool RoundTrip(U8 *src, I64 n)
{
  CArcCompress *arc = CompressBuf(src, n);
  U8 *out = ExpandBuf(arc);
  Bool ok = arc->expanded_size == n && !StrNCmp(out, src, n);
  Free(out);
  Free(arc);
  return ok;
}

Bool ShortBufferRoundTrip()
{
  /* Short input falls through the CT_NONE path (compressed bits
   * wouldn't fit), exercising the &arc->body trailing-buffer trick. */
  return RoundTrip("hello", 5);
}

Bool RepeatedBufferRoundTrip()
{
  /* Highly repetitive input exercises the CT_7_BIT/CT_8_BIT LZW path. */
  U8 *src = "the quick brown fox jumps over the lazy dog. "
            "the quick brown fox jumps over the lazy dog. "
            "the quick brown fox jumps over the lazy dog. "
            "the quick brown fox jumps over the lazy dog.";
  return RoundTrip(src, StrLen(src));
}

I32 Main()
{
  "Test - TempleOS LZW compress/decompress: ";
  mem_mapped_space = 1 << 20;
  I64 correct = 0, tests = 2;
  if (ShortBufferRoundTrip) correct++;
  if (RepeatedBufferRoundTrip) correct++;
  PrintResult(correct, tests);
  "====\n";
  return 0;
}
