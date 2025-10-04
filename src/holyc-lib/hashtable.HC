#include "./defs.HH"
#include "./memory.HC"
#include "./system.HC"
#include "./strings.HC"
#include "./vector.HC"

#define HT_LOAD 60
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
  IntVec *indexes; /* Where all of the values are in the entries array, in 
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
  IntVec *indexes; /* Where all of the values are in the entries array, in 
                 * insertion order. Means we can iterate over the HashTable 
                 * quickly at the cost of memory */
  U64 threashold; /* rebuild threashold */
  U0 (*_free_value)(U0 *_value); /* User defined callback for freeing values */
  U0 (*_free_key)(U0 *_key); /* User defined callback for freeing keys */
  StrMapNode **entries; /* All of the entries, XXX: could this be StrMapNode *entries? */
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
public Bool IntMapHas(IntMap *map, I64 key);
public IntMap *IntMapNew(U64 capacity=1<<8);
public U0 IntMapSetFreeValue(IntMap *map, U0 (*_free_value)(U0 *value));
public U0 IntMapRelease(IntMap *map);
public Bool IntMapResize(IntMap *map);

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
  map->indexes = IntVecNew();
  map->threashold = (capacity * HT_LOAD)/100;
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

static U64 IntMapGetIdx(IntMap *map, I64 key)
{
  U64 idx = IntMapHashFunction(key, map->mask); 
  U64 mask = map->mask;
  IntMapNode **entries = map->entries;
  IntMapNode *cur;

  while ((cur = entries[idx])) {
    if (cur->key == key) {
      return idx;
    }
    idx = (idx + 1) & mask;
  }
  return HT_DELETED;
}

static U64 IntMapGetNextIdx(IntMap *map, I64 key, Bool *_is_free)
{// Finds the next avalible slot and marks it in the bit vector as set
  U64 mask = map->mask;
  U64 idx = key & mask;
  IntMapNode *cur;

  while ((cur = map->entries[idx]) != NULL) {
    if (cur->key == key || cur->key == HT_DELETED) {
      *_is_free = FALSE;
      return idx;
    }
    idx = (idx + 1) & mask;
  }
  *_is_free = TRUE;
  return idx;
}

U0 IntMapRelease(IntMap *map)
{// free the entire hashtable
  if (map) {
    auto indexes = map->indexes;
    U0 (*free_value)(U0 *value) = map->_free_value;
    for (auto i = 0; i < map->capacity; ++i) {
      auto n = map->entries[i];
      if (n) {
        if (free_value) {
          free_value(n->value);
        }
        Free(n);
      }
    }
    IntVecRelease(indexes);
    Free(map->entries);
    Free(map);
  }
}

public Bool IntMapResize(IntMap *map)
{// Resize the hashtable, will return false if OMM
  U64 new_capacity,new_mask,old_mask;
  IntMapNode **new_entries, **old_entries;
  Bool is_free = 0;
  I64 *new_indexes, *old_indexes, new_size;
  auto indexes_capacity = IntVecCapacity(map->indexes);
  auto indexes_size = map->indexes->size;

  old_entries = map->entries;
  old_mask = map->mask;
  old_indexes = map->indexes->entries;

  new_capacity = map->capacity << 1;
  new_mask = new_capacity - 1;

  /* OOM */
  if ((new_indexes = CAlloc(indexes_capacity * sizeof(I64))) == NULL) {
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

  for (U64 i = 0; i < indexes_size; ++i) {
    I64 idx = old_indexes[i];
    auto old = old_entries[idx];
    if (old->key != HT_DELETED) {
      auto new_idx = IntMapGetNextIdx(map,old->key,&is_free);
      new_indexes[new_size] = new_idx;
      new_entries[new_idx] = old;
      new_size++;
    } else {
      Free(old);
    }
  }

  Free(old_entries);
  Free(old_indexes);
  map->indexes->size = new_size;
  map->indexes->entries = new_indexes;

  map->threashold = (new_capacity * HT_LOAD)/100;
  map->entries = new_entries;
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
    IntVecPush(map->indexes,idx);
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
  auto idx = IntMapGetIdx(map,key);
  if (idx != HT_DELETED) {
    auto n = map->entries[idx];
    if (map->_free_value) {
      map->_free_value(n->value);
    }
    n->key = HT_DELETED;
    map->size--;
    return TRUE;
  }
  return FALSE;
}

public Bool IntMapHas(IntMap *map, I64 key)
{
  auto idx = IntMapGetIdx(map,key);
  return idx != HT_DELETED;
}

public U0 *IntMapGet(IntMap *map, I64 key)
{
  auto idx = IntMapGetIdx(map,key);
  if (idx != HT_DELETED) {
    return map->entries[idx]->value;
  }
  return NULL;
}

public Bool IntMapIter(IntMap *map, I64 *_idx, IntMapNode **_node)
{
  I64 idx = *_idx;
  auto indexes = map->indexes->entries;

  while (idx < map->indexes->size) {
    I64 index = indexes[idx];
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

public U8 *IntMapToString(IntMap *map, U8 *(*stringify_value)(U0 *))
{
  auto map_size = map->size;
  if (map_size == 0) {
    return StrPrint(NULL,"{}");
  }

  IntMapNode *entry = NULL;
  auto buffer = StrPrint(NULL,"{");
  for (I64 i = 0; IntMapIter(map,&i,&entry);) {
    auto str_val = stringify_value(entry->value);
    if (i == map_size) {
      buffer = CatPrint(buffer, "[%ld] => %s}",entry->key,str_val);
    } else {
      buffer = CatPrint(buffer, "[%ld] => %s, ",entry->key,str_val);
    }
    Free(str_val);
  }
  return buffer;
}

public U8 *IntMapKeysToString(IntMap *map)
{
  auto map_size = map->size;
  if (map_size == 0) {
    return StrPrint(NULL,"{}");
  }

  IntMapNode *entry = NULL;
  auto buffer = StrPrint(NULL,"{");
  for (I64 i = 0; IntMapIter(map,&i,&entry);) {
    if (i == map_size) {
      buffer = CatPrint(buffer, "%ld}",entry->key);
    } else {
      buffer = CatPrint(buffer, "%ld, ",entry->key);
    }
  }
  return buffer;
}


public Bool StrMapSet(StrMap *map, I64 key, U0 *value);
public StrMap *StrMapNew(U64 capacity=1<<8);
public U0 StrMapSetFreeValue(StrMap *map, U0 (*_free_value)(U0 *value));
public U0 StrMapSetFreeKey(StrMap *map, U0 (*_free_key)(U0 *key));
public U0 StrMapRelease(StrMap *map);
public Bool StrMapResize(StrMap *map);

StrMap *StrMapNew(U64 capacity)
{
  StrMap *map = MAlloc(sizeof(StrMap));
  map->capacity = RoundUpToNextPowerOf2(capacity);
  map->mask = capacity-1;
  map->size = 0;
  map->indexes = IntVecNew();
  map->threashold = (capacity * HT_LOAD)/100;
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
  StrMapNode *cur;
  *_is_free = FALSE;

  while ((cur = map->entries[idx]) != NULL) {
    if (cur->key == NULL) {
      *_is_free = FALSE;
      return idx;
    } else if (key_len == cur->key_len && !StrNCmp(cur->key,key,cur->key_len)) {
      *_is_free = FALSE;
      return idx;
    }
    idx = (idx + 1) & mask;
  }
  *_is_free = TRUE;
  return idx;
}

static I64 StrMapGetIdx(StrMap *map, U8 *key, I64 key_len)
{
  U64 mask = map->mask;
  U64 idx = StrHash(key) & mask;
  StrMapNode **entries = map->entries;
  StrMapNode *cur;

  while ((cur = entries[idx]) != NULL) {
    if (cur->key_len == key_len && !StrNCmp(cur->key, key, key_len)) {
      return idx;
    }
    idx = (idx + 1) & mask;
  }
  return HT_DELETED;
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

public Bool StrMapResize(StrMap *map)
{// Resize the hashtable, will return false if OMM
  U64 new_capacity,new_mask,old_mask;
  StrMapNode **new_entries, **old_entries;
  I64 *new_indexes, *old_indexes;
  Bool is_free = FALSE;
  auto indexes_capacity = IntVecCapacity(map->indexes);
  auto indexes_size = map->indexes->size;

  old_entries = map->entries;
  old_mask = map->mask;
  old_indexes = map->indexes->entries;

  new_capacity = map->capacity << 1;
  new_mask = new_capacity - 1;

  /* OOM */
  if ((new_indexes = CAlloc(indexes_capacity * sizeof(I64))) == NULL) {
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

  for (I64 i = 0; i < indexes_size; ++i) {
    I64 idx = old_indexes[i];
    auto old = old_entries[idx];
    if (old->key != NULL) {
      I64 new_idx = StrMapGetNextIdx(map,old->key,old->key_len,&is_free);
      new_indexes[new_size] = new_idx;
      new_entries[new_idx] = old;
      new_size++;
    } else {
      Free(old);
    }
  }

  Free(old_entries);
  Free(old_indexes);
  map->indexes->size = new_size;
  map->indexes->entries = new_indexes;

  map->size = new_size;
  map->entries = new_entries;
  map->threashold = (new_capacity * HT_LOAD)/100;
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
    IntVecPush(map->indexes, idx);
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
  I64 len = StrLen(key);
  StrMapNode **entries = map->entries;
  StrMapNode *cur;
  U64 mask = map->mask;
  U64 idx = StrHash(key) & mask;
  U0 (*free_value)(U0 *_val) = map->_free_value;
  U0 (*free_key)(U0 *_key) = map->_free_key;

  while ((cur = entries[idx])) {
    if (cur->key_len == len && !StrNCmp(cur->key,key,len)) {
      if (free_key)   free_key(cur->key);
      if (free_value) free_value(cur->value);
      cur->value = cur->key = NULL;
      cur->key_len = 0;
      map->indexes->entries[idx] = HT_DELETED;
      map->size--;
      return TRUE;
    }
    idx = (idx + 1) & mask;
  }
  return TRUE;
}

public U0 *StrMapGetLen(StrMap *map, U8 *key, I64 key_len)
{
  auto idx = StrMapGetIdx(map,key,key_len);
  if (idx != HT_DELETED) {
    return map->entries[idx]->value;
  }
  return NULL;
}

public U0 *StrMapGet(StrMap *map, U8 *key)
{
  I64 key_len = StrLen(key);
  return StrMapGetLen(map,key,key_len);
}

public Bool StrMapHasLen(StrMap *map, U8 *key, I64 key_len)
{
  I64 idx = StrMapGetIdx(map,key,key_len);
  if (idx != HT_DELETED) {
    return TRUE;
  }
  return FALSE;
}

public Bool StrMapHas(StrMap *map, U8 *key)
{
  I64 key_len = StrLen(key);
  return StrMapHasLen(map,key,key_len);
}

public Bool StrMapIter(StrMap *map, I64 *_idx, StrMapNode **_node)
{
  I64 idx = *_idx;
  auto indexes = map->indexes->entries;
  auto size = map->indexes->size;

  while (idx < size) {
    I64 index = indexes[idx];
    if (index != HT_DELETED) {
      *_idx = idx + 1;
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

public U8 *StrMapToString(StrMap *map, U8 *(*stringify_value)(U0 *))
{
  auto map_size = map->size;

  if (map_size == 0) {
    return StrPrint(NULL,"{}");
  }

  StrMapNode *entry = NULL;
  auto buffer = StrPrint(NULL,"{");
  for (I64 i = 0; StrMapIter(map,&i,&entry);) {
    auto str_val = stringify_value(entry->value);
    if (i == map_size) {
      buffer = CatPrint(buffer, "[%s] => %s}",entry->key,str_val);
    } else {
      buffer = CatPrint(buffer, "[%s] => %s, ",entry->key,str_val);
    }
    Free(str_val);
  }
  return buffer;

}

public U8 *StrMapKeysToString(StrMap *map)
{
  auto map_size = map->size;
  if (map_size == 0) {
    return StrPrint(NULL,"{}");
  }

  StrMapNode *entry = NULL;
  auto buffer = StrPrint(NULL,"{");
  for (I64 i = 0; StrMapIter(map,&i,&entry);) {
    if (i == map_size) {
      buffer = CatPrint(buffer, "%s}",entry->key);
    } else {
      buffer = CatPrint(buffer, "%s, ",entry->key);
    }
  }
  return buffer;
}

#ifdef HOLYC_HT_TEST
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
    U8 *value = IntMapGet(map,i);
    if (value != strs[i]) {
      "Failed IntMapTests -> IntMapGet: %d=>%s\n",i,value;
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
    if (strs[i-1] != n->value) {
      "Failed IntMapTests -> IntMapIter i=%d key=%d => value=%s str = %s\n",
        i,n->key,n->value,strs[i];
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
  "PASS IntMap\n";
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
    if (keys[i-1] != n->key || strs[i-1] != n->value) {
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
  "PASS StrMap\n";
}

U0 Main(U0)
{
  IntMapTests;
  StrMapTests;
}
#endif
