/* There is a lot of duplication here but it does 'work', generics would clean
 * all of this up... */
class IntVec
{
  I64 size;
  I64 *entries;
};

class PtrVec
{
  I64 size;
  U0 **entries;
};

class FloatVec 
{
  I64 size;
  F64 *entries;
};

FloatVec *FloatVecNew(U0)
{
  FloatVec *vec = MAlloc(sizeof(FloatVec));
  vec->size = 0;
  vec->entries = MAlloc(sizeof(F64) * 32);
  return vec;
}

U64 FloatVecCapacity(IntVec *vec)
{
  return MSize(vec->entries)/sizeof(F64);
}

U0 FloatVecPush(FloatVec *vec, F64 value)
{
  auto capacity = MSize(vec->entries)/sizeof(F64);
  if (vec->size + 1 >= capacity) {
    auto new_capacity = capacity * 2;
    auto new_entries = ReAlloc(vec->entries, (new_capacity * sizeof(F64))(U64));
    vec->entries = new_entries;
  }
  vec->entries[vec->size++] = value;
}

F64 FloatVecPop(FloatVec *vec, Bool *_ok)
{
  if (vec->size > 0) {
    auto value = vec->entries[--vec->size];
    *_ok = TRUE;
    return value;
  }
  *_ok = FALSE;
  return -1.0;
}

F64 FloatVecGet(FloatVec *vec, I64 idx)
{
#ifdef DEBUG
  if (idx < 0 || idx >= vec->size) {
    "idx %ld is out of range for vector of size %ld\n",idx,vec->size;
    Exit(EXIT_FAILURE);
  }
#endif
  return vec->entries[idx];
}

U0 FloatVecClear(FloatVec *vec)
{
  vec->size = 0;
}

U0 FloatVecRelease(FloatVec *vec)
{
  if (vec) {
    Free(vec->entries);
    Free(vec);
  }
}



IntVec *IntVecNew(U0)
{
  IntVec *vec = MAlloc(sizeof(IntVec));
  vec->size = 0;
  vec->entries = MAlloc(sizeof(I64) * 32);
  return vec;
}

U64 IntVecCapacity(IntVec *vec)
{
  return MSize(vec->entries)/sizeof(I64);
}

U0 IntVecPush(IntVec *vec, I64 value)
{
  auto capacity = IntVecCapacity(vec);
  if (vec->size + 1 >= capacity) {
    auto new_capacity = capacity * 2;
    auto new_entries = ReAlloc(vec->entries, (new_capacity * sizeof(I64))(U64));
    vec->entries = new_entries;
  }
  vec->entries[vec->size++] = value;
}

I64 IntVecPop(IntVec *vec, Bool *_ok)
{
  if (vec->size > 0) {
    auto value = vec->entries[--vec->size];
    *_ok = TRUE;
    return value;
  }
  *_ok = FALSE;
  return -1;
}

I64 IntVecGet(IntVec *vec, I64 idx)
{
#ifdef DEBUG
  if (idx < 0 || idx >= vec->size) {
    "idx %ld is out of range for vector of size %ld\n",idx,vec->size;
    Exit(EXIT_FAILURE);
  }
#endif
  return vec->entries[idx];
}

I64 IntVecClear(IntVec *vec)
{
  vec->size = 0;
}

I64 IntVecRelease(IntVec *vec)
{
  if (vec) {
    Free(vec->entries);
    Free(vec);
  }
}

PtrVec *PtrVecNew(U0)
{
  PtrVec *vec = MAlloc(sizeof(PtrVec));
  vec->size = 0;
  vec->entries = MAlloc(sizeof(U0*) * 32);
  return vec;
}

U64 PtrVecCapacity(IntVec *vec)
{
  return MSize(vec->entries)/sizeof(U0*);
}


U0 PtrVecPush(PtrVec *vec, U0 *value)
{
  auto capacity = MSize(vec->entries)/sizeof(U0*);
  if (vec->size + 1 >= capacity) {
    auto new_capacity = capacity * 2;
    auto new_entries = ReAlloc(vec->entries, (new_capacity * sizeof(U0*))(U64));
    vec->entries = new_entries;
  }
  vec->entries[vec->size++] = value;
}

U0 *PtrVecPop(PtrVec *vec, Bool *_ok)
{
  if (vec->size > 0) {
    auto value = vec->entries[--vec->size];
    *_ok = TRUE;
    return value;
  }
  *_ok = FALSE;
  return NULL;
}

U0 *PtrVecGet(PtrVec *vec, I64 idx)
{
#ifdef DEBUG
  if (idx < 0 || idx >= vec->size) {
    "idx %ld is out of range for vector of size %ld\n",idx,vec->size;
    Exit(EXIT_FAILURE);
  }
#endif
  return vec->entries[idx];
}

I64 PtrVecClear(PtrVec *vec)
{
  vec->size = 0;
}

I64 PtrVecRelease(PtrVec *vec)
{
  if (vec) {
    Free(vec->entries);
    Free(vec);
  }
}

#ifdef VEC_TEST
U0 Main()
{
  auto vec = FloatVecNew();
  FloatVecPush(vec,32.32);
  FloatVecPush(vec,30.33);
  FloatVecPush(vec,42.65);
  FloatVecPush(vec,52.99);

  for (I64 i = 0; i < vec->size; ++i) {
    auto val = vec->entries[i];// FloatVecGet(vec,i);
    "[%d] => %f\n",i,val(F64);
  }
  FloatVecRelease(vec);
}
#endif
