#include "./defs.HH"
#include "./memory.HC"
#include "./system.HC"
#include "./strings.HC"

#define HT_LOAD 0.60
#define HT_DELETED 0x7fffffffffffffff
#define HT_PROBE_1 1
#define HT_PROBE_3 3

class IntMapNode
{
  I64 key;
  U0 *value;
};

class StrMapNode
{
  U8 *key;
  I64 key_len;
  U0 *value;
};

public class IntMap
{
  U64 size;     /* How many entries are in the hashtable */
  U64 capacity; /* How much capacity we have in the entries array */
  U64 mask;     /* Used for hashing, as the capacity is always a power of 2
                 * we can use fast modulo of `<int> & capacity-1`. */
  I64 *indexes; /* Where all of the values are in the entries array, in 
                 * insertion order. Means we can iterate over the HashTable 
                 * quickly at the cost of memory */
  U64 threashold; /* rebuild threashold */
  U0 (*_free_value)(U0 *value); /* User defined callback for freeing values */
  IntMapNode **entries; /* All of the entries, XXX: could this be IntMapNode *entries? */
};

public class StrMap
{
  U64 size;     /* How many entries are in the hashtable */
  U64 capacity; /* How much capacity we have in the entries array */
  U64 mask;     /* Used for hashing, as the capacity is always a power of 2
                 * we can use fast modulo of `<int> & capacity-1`. */
  I64 *indexes; /* Where all of the values are in the entries array, in 
                 * insertion order. Means we can iterate over the HashTable 
                 * quickly at the cost of memory */
  U64 threashold; /* rebuild threashold */
  U0 (*_free_value)(U0 *_value); /* User defined callback for freeing values */
  U0 (*_free_key)(U0 *_key); /* User defined callback for freeing keys */
  StrMapNode **entries; /* All of the entries, XXX: could this be IntMapNode *entries? */
};

U64 RoundUpToNextPowerOf2(U64 v)
{
  v--;
  v |= v >> 1;
  v |= v >> 2;
  v |= v >> 4;
  v |= v >> 8;
  v |= v >> 16;
  v |= v >> 32;
  v++;
  return v;
}

public Bool IntMapSet(IntMap *map, I64 key, U0 *value);
public IntMap *IntMapNew(U64 capacity=1<<8);
public U0 IntMapSetFreeValue(IntMap *map, U0 (*_free_value)(U0 *value));
public U0 IntMapRelease(IntMap *map);
public Bool IntMapResize(IntMap *map, U64 size);

U64 IntMapHashFunction(I64 key, U64 mask)
{
  return key & mask;
}

IntMap *IntMapNew(U64 capacity)
{
  IntMap *map = MAlloc(sizeof(IntMap));
  map->capacity = RoundUpToNextPowerOf2(capacity);
  map->mask = capacity-1;
  map->size = 0;
  map->indexes = CAlloc(sizeof(I64) * capacity);
  map->threashold = (HT_LOAD * capacity)(U64);
  map->_free_value = NULL;
  map->entries = CAlloc(capacity * sizeof(IntMapNode *));
  return map;
}

U0 IntMapSetFreeValue(IntMap *map, U0 (*_free_value)(U0 *value))
{
  map->_free_value = _free_value;
}

IntMapNode *IntMapNodeNew(I64 key, U0 *value)
{
  IntMapNode *n = MAlloc(sizeof(IntMapNode));
  n->key = key;
  n->value = value;
  return n;
}

static U64 IntMapGetNextIdx(IntMap *map, I64 key, Bool *_is_free)
{// Finds the next avalible slot and marks it in the bit vector as set
  U64 mask = map->mask;
  U64 idx = key & mask;
  U64 probe = 1;
  IntMapNode *cur;
  *_is_free = FALSE;

  while ((cur = map->entries[idx]) != NULL) {
    if (cur->key == key || cur->key == HT_DELETED) {
      *_is_free = FALSE;
      return idx;
    }
    idx = (idx + HT_PROBE_1 * probe + HT_PROBE_3 * probe * probe) & mask;
    probe++;
  }
  *_is_free = TRUE;
  return idx;
}

U0 IntMapRelease(IntMap *map)
{// free the entire hashtable
  if (map) {
    U0 (*free_value)(U0 *value) = map->_free_value;
    for (auto i = 0; i < map->capacity; ++i) {
      auto n = map->entries[i];
      if (n) {
        if (free_value) free_value(n->value);
        Free(n);
      }
    }
    Free(map->entries);
    Free(map->indexes);
    Free(map);
  }
}

public Bool IntMapResize(IntMap *map, U64 size)
{// Resize the hashtable, will return false if OMM
  U64 new_capacity,old_capacity,new_mask,old_mask;
  IntMapNode **new_entries, **old_entries;
  Bool is_free = 0;
  I64 *new_indexes, *old_indexes, new_size;

  old_entries = map->entries;
  old_mask = map->mask;

  new_capacity = map->capacity << 1;
  new_mask = new_capacity - 1;

  /* OOM */
  if ((new_indexes = CAlloc(new_capacity * sizeof(I64))) == NULL) {
    return FALSE;
  }

  /* OOM */
  if ((new_entries = CAlloc(new_capacity * sizeof(IntMapNode*))) == NULL) {
    Free(new_indexes);
    return FALSE;
  }

  map->mask = new_mask;
  map->entries = new_entries;
  map->capacity = new_capacity;
  new_size = 0;

  for (auto i = 0; i < map->size; ++i) {
    auto idx = old_indexes[i];
    auto old = old_entries[idx];
    if (old->key != HT_DELETED) {
      auto new_idx = IntMapGetNextIdx(map,old->key,&is_free);
      new_indexes[new_size] = new_idx;
      new_indexes[new_idx] = old;
      new_size++;
    } else {
      Free(old);
    }
  }

  Free(old_entries);
  Free(old_indexes);
  map->indexes = new_indexes;
  map->threashold = (map->capacity * HT_LOAD)(U64);
  map->size = new_size;
  return TRUE;
}

auto IntMapSet(IntMap *map, I64 key, U0 *value)
{
  Bool is_free;

  if (map->size >= map->threashold) {
    if (!IntMapResize(map)) {
      /* This means we have run out of memory */
      return FALSE;
    }
  }

  U64 idx = IntMapGetNextIdx(map,key,&is_free);
  if (is_free) {
    auto n = IntMapNodeNew(key,value); 
    map->indexes[map->size] = idx;
    map->entries[idx] = n;
    map->size++;
    return TRUE;
  } else {
    auto n = map->entries[idx];
    n->key = key;
    n->value = value;
    return TRUE;
  }
}

public Bool IntMapDelete(IntMap *map, I64 key)
{
  U64 idx,mask,probe;
  IntMapNode **entries = map->entries;
  IntMapNode *cur;
  mask = map->mask;
  idx = IntMapHashFunction(key,mask);
  probe = 1;
  while ((cur = entries[idx])) {
    if (cur->key == key) {
      cur->key = HT_DELETED;
      map->indexes[idx] = HT_DELETED;
      map->size--;
      return TRUE;
    }
    idx = (idx + HT_PROBE_1 * probe + HT_PROBE_3 * probe * probe) & mask;
    probe++;
  }
  return FALSE;
}

public Bool IntMapHas(IntMap *map, I64 key)
{
  U64 idx, mask, probe;
  IntMapNode **entries = map->entries;
  IntMapNode *cur;
  mask = map->mask;
  probe = 1;
  idx = IntMapHashFunction(key,mask);
  while ((cur = entries[idx])) {
    if (cur->key == key) return TRUE;
    idx = (idx + HT_PROBE_1 * probe + HT_PROBE_3 * probe * probe) & mask;
    probe++;
  }
  return FALSE;
}

public U0 *IntMapGet(IntMap *map, I64 key)
{
  U64 idx, mask, probe;
  IntMapNode **entries = map->entries;
  IntMapNode *cur;

  mask = map->mask;
  probe = 1;
  idx = IntMapHashFunction(key,mask);
  while ((cur = entries[idx])) {
    if (cur->key == key) {
      return cur->value;
    }
    idx = (idx + HT_PROBE_1 * probe + HT_PROBE_3 * probe * probe) & mask;
    probe++;
  }
  return NULL;
}

public Bool IntMapIter(IntMap *map, I64 *_idx, IntMapNode **_node)
{
  I64 idx = *_idx;
  auto indexes = map->indexes;
  while (idx < map->size) {
    I64 index = map->indexes[idx];
    if (index != HT_DELETED) {
      *_idx = idx + 1;
      *_node = map->entries[index];
      return TRUE;
    }
    idx++;
  }
  return FALSE;
}

public Bool IntMapValueIter(IntMap *map, I64 *_idx, U0 **_value)
{
  IntMapNode *node;
  if (IntMapIter(map,_idx,&node)) {
    *_value = node->value;
    return TRUE;
  }
  return FALSE;
}

public Bool IntMapKeyIter(IntMap *map, I64 *_idx, I64 *_key)
{
  IntMapNode *node;
  if (IntMapIter(map,_idx,&node)) {
    *_key = node->key;
    return TRUE;
  }
  return FALSE;
}

public Bool StrMapSet(StrMap *map, I64 key, U0 *value);
public StrMap *StrMapNew(U64 capacity=1<<8);
public U0 StrMapSetFreeValue(StrMap *map, U0 (*_free_value)(U0 *value));
public U0 StrMapSetFreeKey(StrMap *map, U0 (*_free_key)(U0 *key));
public U0 StrMapRelease(StrMap *map);
public Bool StrMapResize(StrMap *map, U64 size);

StrMap *StrMapNew(U64 capacity)
{
  StrMap *map = MAlloc(sizeof(StrMap));
  map->capacity = RoundUpToNextPowerOf2(capacity);
  map->mask = capacity-1;
  map->size = 0;
  map->indexes = CAlloc(capacity * sizeof(I64));
  map->threashold = (HT_LOAD * capacity)(U64);
  map->_free_value = NULL;
  map->_free_key = NULL;
  map->entries = CAlloc(capacity * sizeof(StrMapNode *));
  return map;
}

U0 StrMapSetFreeValue(StrMap *map, U0 (*_free_value)(U0 *value))
{
  map->_free_value = _free_value;
}

U0 StrMapSetFreeKey(StrMap *map, U0 (*_free_key)(U0 *key))
{
  map->_free_key = _free_key;
}

StrMapNode *StrMapNodeNew(U8 *key, I64 key_len, U0 *value)
{
  StrMapNode *n = MAlloc(sizeof(StrMapNode));
  n->key = key;
  n->key_len = key_len;
  n->value = value;
  return n;
}

static U64 StrMapGetNextIdx(StrMap *map, U8 *key, I64 key_len, Bool *_is_free)
{// Finds the next avalible slot and marks it in the bit vector as set
  U64 mask = map->mask;
  U64 idx = StrHash(key) & mask;
  U64 probe = 1;
  StrMapNode *cur;
  *_is_free = FALSE;
  while ((cur = map->entries[idx]) != NULL) {
    if (cur->key == NULL) {
      *_is_free = FALSE;
      return idx;
    } else if (!StrNCmp(cur->key,key,cur->key_len)) {
      *_is_free = FALSE;
      return idx;
    }
    idx = (idx + HT_PROBE_1 * probe + HT_PROBE_3 * probe * probe) & mask;
    probe++;
  }
  *_is_free = TRUE;
  return idx;
}

U0 StrMapRelease(StrMap *map)
{// free the entire hashtable
  if (map) {
    U0 (*free_value)(U0 *_val) = map->_free_value;
    U0 (*free_key)(U0 *_key) = map->_free_key;
    for (auto i = 0; i < map->capacity; ++i) {
      auto n = map->entries[i];
      if (n) {
        if (free_value) free_value(n->value);
        if (free_key)   free_key(n->key);
        Free(n);
      }
    }
    Free(map->entries);
    Free(map->indexes);
    Free(map);
  }
}

public Bool StrMapResize(StrMap *map, U64 size)
{// Resize the hashtable, will return false if OMM
  U64 new_capacity,old_capacity,new_mask,old_mask;
  StrMapNode **new_entries, **old_entries;
  I64 *new_indexes, *old_indexes;
  Bool is_free;

  old_entries = map->entries;
  old_mask = map->mask;
  old_capacity = map->capacity;
  new_capacity = map->capacity << 1;
  new_mask = new_capacity - 1;

  /* OOM */
  if ((new_indexes = CAlloc(sizeof(I64) * new_capacity)) == NULL) {
    return FALSE;
  }

  /* OOM */
  if ((new_entries = CAlloc(new_capacity * sizeof(StrMapNode*))) == NULL) {
    Free(new_indexes);
    return FALSE;
  }

  map->mask = new_mask;
  map->entries = new_entries;
  map->capacity = new_capacity;
  I64 new_size = 0;
  I64 old_size = map->size;

  for (I64 i = 0; i < old_size; ++i) {
    I64 idx = old_indexes[i];
    auto old = old_entries[idx];
    if (old->key != NULL) {
      I64 new_idx = StrMapGetNextIdx(map,old->key,old->key_len,*is_free);
      new_indexes[new_size] = new_idx;
      new_entries[new_idx] = old;
      new_size++;
    } else {
      Free(old);
    }
  }

  Free(old_entries);
  Free(map->indexes);
  map->indexes = new_indexes;
  map->threashold = (map->capacity * HT_LOAD)(U64);
  return TRUE;
}

Bool StrMapSet(StrMap *map, U8 *key, U0 *value)
{
  Bool is_free;
  U64 mask = map->mask;

  if (map->size >= map->threashold) {
    if (!StrMapResize(map)) {
      /* This means we have run out of memory */
      return FALSE;
    }
  }

  I64 key_len = StrLen(key);

  U64 idx = StrMapGetNextIdx(map,key,key_len,&is_free);
  if (is_free) {
    auto n = StrMapNodeNew(key,key_len,value); 
    map->indexes[map->size] = idx;
    map->entries[idx] = n;
    map->size++;
    return TRUE;
  } else {
    auto n = map->entries[idx];
    n->key = key;
    n->key_len = key_len;
    n->value = value;
    return TRUE;
  }
}

public Bool StrMapDelete(StrMap *map, U8 *key)
{
  U64 idx,mask,probe;
  I64 len = StrLen(key);
  StrMapNode **entries = map->entries;
  StrMapNode *cur;
  mask = map->mask;
  idx = StrHash(key) & mask;
  probe = 1;
  U0 (*free_value)(U0 *_val) = map->_free_value;
  U0 (*free_key)(U0 *_key) = map->_free_key;
  while ((cur = entries[idx])) {
    if (cur->key_len == len && !StrNCmp(cur->key,key,len)) {
      if (free_key)   free_key(cur->key);
      if (free_value) free_value(cur->value);
      cur->value = cur->key = NULL;
      map->indexes[idx] = HT_DELETED;
      map->size--;
      return TRUE;
    }
    idx = (idx + HT_PROBE_1 * probe + HT_PROBE_3 * probe * probe) & mask;
    probe++;
  }
  return TRUE;
}

public U0 *StrMapGet(StrMap *map, U8 *key)
{
  U64 idx, mask, probe;
  I64 len = StrLen(key);
  StrMapNode **entries = map->entries;
  StrMapNode *cur;

  mask = map->mask;
  probe = 1;
  idx = StrHash(key) & mask;
  while ((cur = entries[idx])) {
    if (cur->key == NULL) {
      return NULL;
    }
    if (cur->key_len == len && !StrNCmp(cur->key,key,len)) {
      return cur->value;
    }
    idx = (idx + HT_PROBE_1 * probe + HT_PROBE_3 * probe * probe) & mask;
    probe++;
  }
  return NULL;
}

public Bool StrMapIter(StrMap *map, I64 *_idx, StrMapNode **_node)
{
  I64 idx = *_idx;
  auto indexes = map->indexes;
  while (idx < indexes->len) {
    I64 index = map->indexes[idx];
    if (index != HT_DELETED) {
      *_idx = idx+1;
      *_node = map->entries[index];
      return TRUE;
    }
    idx++;
  }
  return FALSE;
}

public Bool StrMapValueIter(StrMap *map, I64 *_idx, U0 **_value)
{
  StrMapNode *node;
  if (StrMapIter(map,_idx,&node)) {
    *_value = node->value;
    return TRUE;
  }
  return FALSE;
}

public Bool StrMapKeyIter(StrMap *map, I64 *_idx, U8 **_key)
{
  StrMapNode *node;
  if (StrMapIter(map,_idx,&node)) {
    *_key = node->key;
    return TRUE;
  }
  return FALSE;
}

#ifdef HOLY_C_HT_TEST
U0 IntMapTests(U0)
{
  IntMap *map = IntMapNew(8);
  IntMapNode *n;
  U8 *strs[] = {
    "hello",
    "world",
    "foo",
    "bar",
    "baz",
    "cat",
    "dog",
    "owl"
  };
  I64 len = sizeof(strs)/sizeof(strs[0]);
  for (I64 i = 0; i < len; i++) {
    IntMapSet(map,i,strs[i]);
  }

  for (I64 i = 0; i < len; i++) {
    if (IntMapGet(map,i) != strs[i]) {
      "Failed IntMapTests -> IntMapGet\n";
      Exit;
    }
  }

  for (I64 i = 0; i < len; i++) {
    if (!IntMapHas(map,i)) {
      "Failed IntMapTests -> IntMapHas\n";
      Exit;
    }
  }

  /* Inorder iteration */
  for (I64 i = 0; IntMapIter(map,&i,&n);) {
    if (i != n->key || str[i] != n->value) {
      "Failed IntMapTests -> IntMapIter\n";
      Exit;
    }
  }

  for (I64 i = 0; i < len; i++) {
    if (!IntMapDelete(map,i)) {
      "Failed IntMapTests -> IntMapDelete\n";
      Exit;
    }
  }

  IntMapRelease(map);
}

U0 StrMapTests(U0)
{
  StrMap *map = StrMapNew(8);
  StrMapNode *n;
  U8 *keys[] = {
    "card",
    "can",
    "will",
    "shill",
    "cup",
    "mug",
    "glass",
    "tin"
  };
  U8 *strs[] = {
    "hello",
    "world",
    "foo",
    "bar",
    "baz",
    "cat",
    "dog",
    "owl"
  };
  I64 len = sizeof(strs)/sizeof(strs[0]);
  for (I64 i = 0; i < len; i++) {
    StrMapSet(map,keys[i],strs[i]);
  }

  for (I64 i = 0; i < len; i++) {
    if (StrMapGet(map,keys[i]) != strs[i]) {
      "Failed StrMapTests -> StrMapGet\n";
      Exit;
    }
  }

  for (I64 i = 0; i < len; i++) {
    if (!StrMapHas(map,keys[i])) {
      "Failed StrMapTests -> StrMapHas\n";
      Exit;
    }
  }

  /* Inorder iteration */
  for (I64 i = 0; StrMapIter(map,&i,&n);) {
    if (keys[i] != n->key || str[i] != n->value) {
      "Failed StrMapTests -> StrMapIter\n";
      Exit;
    }
  }

  for (I64 i = 0; i < len; i++) {
    if (!StrMapDelete(map,keys[i])) {
      "Failed StrMapTests -> StrMapDelete\n";
      Exit;
    }
  }

  StrMapRelease(map);
}

U0 Main()
{
  IntMapTests;
  StrMapTests;
}
#endif
