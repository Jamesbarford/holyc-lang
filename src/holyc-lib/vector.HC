/* There is a lot of duplication here but it does 'work', generics would clean
 * all of this up... */
public U0 QSortGeneric(U0 **arr, I64 high, I64 low,
    I64 (*_compare_fn)(U0 *_arg1, U0 *_arg2));

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

FloatVec *FloatVecNew(I64 size = 32)
{
  FloatVec *vec = MAlloc(sizeof(FloatVec));
  vec->size = 0;
  vec->entries = MAlloc(sizeof(F64) * size);
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

I64 _FloatCmp(F64 f1, F64 f2)
{
  return (f1 < f2)(Bool);
}

U0 FloatVecSort(FloatVec *vec)
{
  QSortGeneric(vec->entries(U0**),vec->size-1,0,&_FloatCmp);
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


IntVec *IntVecNew(I64 size = 32)
{
  IntVec *vec = MAlloc(sizeof(IntVec));
  vec->size = 0;
  vec->entries = MAlloc(sizeof(I64) * size);
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

I64 _IntCmp(I64 i1, I64 i2)
{
  return i1 < i2;
}

U0 IntVecSort(IntVec *vec)
{
  QSortGeneric(vec->entries(U0**),vec->size-1,0,&_IntCmp);
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

PtrVec *PtrVecNew(I64 size = 32)
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

U0 QSortGeneric(U0 **arr, I64 high, I64 low, 
    I64 (*_compare_fn)(U0 *_arg1, U0 *_arg2))
{
  if (low < high) {
    U0 *pivot = arr[high];
    I64 idx = low;

    for (I64 i = low; i < high; ++i) {
      if (_compare_fn(arr[i],pivot)) {
        U0 *tmp = arr[i];
        arr[i] = arr[idx];
        arr[idx] = tmp;
        idx++;
      }
    }

    arr[high] = arr[idx];
    arr[idx] = pivot;
    QSortGeneric(arr,high,idx+1,_compare_fn);
    QSortGeneric(arr,idx-1,low,_compare_fn);
  }
}

U0 PtrVecSort(PtrVec *vec, I64 (*_compare_fn)(U0 *_arg1, U0 *_arg2))
{
  QSortGeneric(vec->entries,vec->size-1,0,_compare_fn);
}

I64 PtrVecClear(PtrVec *vec)
{
  vec->size = 0;
}

I64 PtrVecRelease(PtrVec *vec, U0 (*_free_value)(U0 *_val) = NULL)
{ // Optionally pass a callback to free each item in the vector 
  if (vec) {
    if (_free_value) {
      U0 **entries = vec->entries;
      I64 size = vec->size;
      for (I64 i = 0; i < size; ++i) {
        _free_value(entries[i]);
      }
    }
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
