#include <defs.HH>
#include <memory.HC>

#define BIT_VEC_BITS 32

class BitVec
{
  I64 blocks;
  U32 *entries;
};

BitVec *BitVecNew()
{
  BitVec *bv = MAlloc(sizeof(BitVec));
  bv->entries = MAlloc(sizeof(U32));
  bv->entries[0] = 0;
  bv->blocks = 1;
  return bv;
}

U0 BitVecRelease(BitVec *bv)
{
  Free(bv->entries);
  Free(bv);
}

U0 BitVecSet(BitVec *bv, U64 idx)
{
  U32 block_idx = idx/BIT_VEC_BITS;
  if (block_idx >= bv->blocks) {
    auto old = bv->entries;
    U32 *new = MAlloc(sizeof(U32)*block_idx+1);
    for (I64 i = 0; i < block_idx; ++i) {
      new[i] = 0;
    }
    MemCpy(new,old,sizeof(U32)*bv->blocks);
    bv->blocks = block_idx+1;
    bv->entries = new;
    Free(old);
  }
  bv->entries[block_idx] |= 1 << (idx & (BIT_VEC_BITS - 1));
}

public U0 BitVecUnSet(BitVec *bv, U64 idx) 
{
  U32 block_idx = idx/BIT_VEC_BITS;
  if (block_idx < bv->blocks) {
    bv->entries[block_idx] &= ~(1 << idx & (BIT_VEC_BITS - 1));
  }
}

public Bool BitVecTest(BitVec *bv, U64 idx)
{
  U32 block_idx = idx/BIT_VEC_BITS;
  if (block_idx < bv->blocks) {
    return bv->entries[block_idx] >> ((idx & (BIT_VEC_BITS - 1)) & 1);
  }
  return FALSE;
}

public U0 BitVecClear(BitVec *bv)
{
  for (I64 i = 0; i < bv->blocks; ++i) {
    bv->entries[i] = 0;
  }
}
