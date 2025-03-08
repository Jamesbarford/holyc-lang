#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "aostr.h"
#include "map.h"
#include "util.h"

#define VECTOR_INITIAL_CAPACITY 4

void setAllLongs(long *array, unsigned long len, long value) {
    for (unsigned long i = 0; i < len; ++i) {
        array[i] = value;
    }
}

#define vectorNew(ret_type, entry_type)                               \
    ret_type *vec;                                                    \
    do {                                                              \
        vec = (ret_type *)malloc(sizeof(ret_type));                   \
        vec->size = 0;                                                \
        vec->capacity = VECTOR_INITIAL_CAPACITY;                      \
        vec->entries = (entry_type *)malloc(sizeof(entry_type) * VECTOR_INITIAL_CAPACITY); \
    } while (0)

#define vectorResize(vec, type)                           \
    do {                                                  \
        int new_capacity = vec->capacity * 2;             \
        type *new_entries = (type *)realloc(vec->entries, \
                    sizeof(type)*new_capacity);           \
        vec->entries = new_entries;                       \
        vec->capacity = new_capacity;                     \
    } while (0)

#define vectorPush(vec, type, value)                      \
    do {                                                  \
        if (vec->size + 1 >= vec->capacity) {             \
            vectorResize(vec,type);                       \
        }                                                 \
        vec->entries[vec->size++] = value;                \
    } while (0)

#define vectorBoundsCheck(vec_size, idx)                                   \
    do {                                                                   \
        if (idx < 0 || idx >= vec_size) {                                  \
            loggerPanic("idx '%d' is out of range for vector of size %d\n",\
                    idx,vec->size);                                        \
        }                                                                  \
    } while (0)

#define vectorRelease(vec)                                                 \
    do {                                                                   \
        if (vec) {                                                         \
            free(vec->entries);                                            \
            free(vec);                                                     \
        }                                                                  \
    } while (0)
    

/* I feel combining all of these data structures into one file will lead 
 * to a more happy prosperous codebase */
IntVec *intVecNew(void) {
    vectorNew(IntVec,long);
#ifdef DEBUG
    assert(vec != NULL);
    assert(vec->entries != NULL);
#endif
    return vec;
}

void intVecPush(IntVec *vec, long value) {
    vectorPush(vec,long,value);
}

long intVecPop(IntVec *vec, int *ok) {
    if (vec->size > 0) {
        long item = vec->entries[--vec->size];
        *ok = 1;
        return item;
    }
    *ok = 0;
    return -1;
}

int intVecGet(IntVec *vec, int idx) {
#ifdef DEBUG
    vectorBoundsCheck(vec->size,idx);
#endif
    /* No checking */
    return vec->entries[idx];
}

void intVecClear(IntVec *vec) {
    vec->size = 0;
}

void intVecRelease(IntVec *vec) {
    if (vec) {
        free(vec->entries);
        free(vec);
    }
}

/* Make the first instance of key the first element in the vector */
void intVecMoveFirst(IntVec *vec, int key) {
    int idx = -1;
    for (int i = 0; i < vec->size; ++i) {
        if (vec->entries[i] == key) {
            idx = i;
            break;
        }
    }
    if (idx == -1) return;
    int tmp = vec->entries[0];
    vec->entries[0] = vec->entries[idx];
    vec->entries[idx] = tmp;
}

PtrVec *ptrVecNew(void) {
    vectorNew(PtrVec,void*);
#ifdef DEBUG
    assert(vec != NULL);
    assert(vec->entries != NULL);
#endif
    return vec;
}

void ptrVecPush(PtrVec *vec, void *value) {
    vectorPush(vec,void*,value);
}

void *ptrVecGet(PtrVec *vec, int idx) {
#ifdef DEBUG
    vectorBoundsCheck(vec->size,idx);
#endif
    /* No checking */
    return vec->entries[idx];
}

void ptrVecRelease(PtrVec *vec) {
    vectorRelease(vec);
}

void ptrVecClear(PtrVec *vec, void (*free_entry_fnptr)(void *value)) {
    if (free_entry_fnptr) {
        while (vec->size) {
            void *entry = vec->entries[vec->size - 1];
            free_entry_fnptr(entry);
            vec->size--;
        }
    } else {
        vec->size = 0;
    }
}

/* Return the element for the user to handle. We do our removes */
void *ptrVecRemoveIdx(PtrVec *vec, int idx) {
    if (idx < 0 || idx >= vec->size) {
#ifdef DEBUG
        loggerPanic("Vector idx out of bounds: >= 0 %d <= %d does not hold\n",
                     idx, vec->size);
#endif
        return NULL;
    }
    /**
     *     v
     * [1, 2, 3, 4]
     *     v    
     * [1, 2, 3, 4]
     */
    void *entry = vec->entries[idx];

    /* The previous entry is the current entry */
    for (int cur = idx + 1; cur < vec->size -1; ++cur) {
        vec->entries[cur - 1] = vec->entries[cur];
    }
    return entry;
}

void *ptrVecRemove(PtrVec *vec,
                   void *match_param,
                   int (*entry_match_fnptr)(void *map_entry, void *match_param))
{
    /* Find the matching index then call `ptrVecRemoveIdx(...)` to remove
     * the element or return NULL if there is no match */
    int idx = -1;
    for (int i = 0; i < vec->size; ++i) {
        void *needle = vec->entries[i];
        if (entry_match_fnptr(needle, match_param)) {
            idx = i;
            break;
        }
    }
    if (idx != -1) {
        return ptrVecRemoveIdx(vec, idx);
    }
    return NULL;
}

static unsigned long intMapHashFunction(long key, unsigned long mask) {
    return key & mask;
}

unsigned long roundUpToNextPowerOf2(unsigned long v) {
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


IntMap *intMapNew(unsigned long capacity) {
    IntMap *map = (IntMap *)malloc(sizeof(IntMap));
    capacity = roundUpToNextPowerOf2(capacity);
    map->capacity = capacity;
    map->mask = capacity - 1;
    map->size = 0;
    map->threashold = (unsigned long)(HT_LOAD * capacity);
    map->_free_value = NULL;
    map->entries = (IntMapNode **)calloc(capacity, sizeof(IntMapNode *));
    map->indexes = intVecNew();
    return map;
}

void intMapSetFreeValue(IntMap *map, void (*_free_value)(void *value)) {
    map->_free_value = _free_value;
}

IntMapNode *intMapNodeNew(long key, void *value) {
    IntMapNode *n = (IntMapNode *)malloc(sizeof(IntMapNode));
    n->key = key;
    n->value = value;
    return n;
}

static unsigned long intMapGetIdx(IntMap *map, long key) {
    unsigned long idx = intMapHashFunction(key, map->mask); 
    unsigned long mask = map->mask;
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

IntMapIterator *intMapIteratorNew(IntMap *map) {
    IntMapIterator *it = (IntMapIterator *)malloc(sizeof(IntMapIterator));
    it->idx = 0;
    it->map = map;
    return it;
}

void intMapIteratorRelease(IntMapIterator *it) {
    if (it) {
        free(it);
    }
}

IntMapNode *intMapNext(IntMapIterator *it) {
    while (it->idx < (unsigned long)it->map->indexes->size) {
        long idx = intVecGet(it->map->indexes,it->idx);
        IntMapNode *n = it->map->entries[idx];
        if (n->key != HT_DELETED) {
            it->idx++;
            return n;
        }
        it->idx++;
    }
    return NULL;
}

/* Finds the next avalible slot */ 
static unsigned long intMapGetNextIdx(IntMap *map, long key, int *_is_free) { 
    unsigned long mask = map->mask;
    unsigned long idx = key & mask;
    IntMapNode *cur;

    while ((cur = map->entries[idx]) != NULL) {
        if (cur->key == key || cur->key == HT_DELETED) {
            *_is_free = 0;
            return idx;
        }
        idx = (idx + 1) & mask;
    }
    *_is_free = 1;
    return idx;
}

void intMapClear(IntMap *map) {
    IntVec *indexes = map->indexes;
    void (*free_value)(void *value) = map->_free_value;
    for (int i = 0; i < indexes->size; ++i) {
        long idx = vecGet(long,indexes,i);
        IntMapNode *n = map->entries[idx];
        if (n) {
            if (free_value)
                free_value(n->value);
            free(n);
        }
    }
    map->size = 0;
    memset(map->entries,0,map->capacity*sizeof(IntMapNode *));
    intVecClear(map->indexes);
}

void intMapRelease(IntMap *map) { // free the entire hashtable
    if (map) {
        void (*free_value)(void *value) = map->_free_value;
        for (unsigned long i = 0; i < map->capacity; ++i) {
            IntMapNode *n = map->entries[i];
            if (n) {
                if (free_value)
                    free_value(n->value);
                free(n);
            }
        }
        intVecRelease(map->indexes);
        free(map->entries);
        free(map);
    }
}

int intMapResize(IntMap *map) {
    // Resize the hashtable, will return false if OMM
    unsigned long new_capacity, new_mask;
    IntMapNode **new_entries, **old_entries;
    long *new_indexes;
    long *old_index_entries = map->indexes->entries;
    int indexes_capacity = (int)map->indexes->size;
    int is_free;

    old_entries = map->entries;

    new_capacity = map->capacity << 1;
    new_mask = new_capacity - 1;

    new_indexes = (long *)calloc(indexes_capacity, sizeof(long));
    /* OOM */
    if (new_indexes == NULL) {
        return 0;
    }

    new_entries = (IntMapNode **)calloc(new_capacity, sizeof(IntMapNode *));
    /* OOM */
    if (new_entries == NULL) {
        free(new_indexes);
        return 0;
    }

    map->mask = new_mask;
    map->entries = new_entries;
    map->capacity = new_capacity;
    long new_size = 0;

    /* Keeps insertion order, and does not have to go over the capacity of 
     * the hashtable which 'dict.c' has to do on a resize thus this should in
     * theory be faster, but there are more array lookups however they should 
     * have good spatial locality */
    for (long i = 0; i < indexes_capacity; ++i) {
        long idx = old_index_entries[i];
        IntMapNode *old = old_entries[idx];
        if (old->key != HT_DELETED) {
            long new_idx = intMapGetNextIdx(map,old->key,&is_free);
            new_indexes[new_size] = new_idx;
            new_entries[new_idx] = old;
            /* keep track of the new size of this hashtable */
            new_size++;
        } else {
            free(old);
        }
    }

    free(old_entries);
    free(map->indexes->entries);
    map->indexes->size = new_size;
    map->indexes->capacity = indexes_capacity;
    map->indexes->entries = new_indexes;

    map->entries = new_entries;
    map->size = new_size;
    map->threashold = (unsigned long)(new_capacity * HT_LOAD);
    return 1;
}

int intMapAdd(IntMap *map, long key, void *value) {
    if (map->size >= map->threashold) {
        if (!intMapResize(map)) {
            /* This means we have run out of memory */
            return 0;
        }
    }

    int is_free;
    unsigned long idx = intMapGetNextIdx(map, key, &is_free);
    if (is_free) {
        IntMapNode *n = intMapNodeNew(key, value);
        intVecPush(map->indexes,idx);
        map->entries[idx] = n;
        map->size++;
        return 1;
    } else {
        IntMapNode *n = map->entries[idx];
        n->key = key;
        n->value = value;
        return 1;
    }
}

int intMapDelete(IntMap *map, long key) {
    unsigned long idx = intMapGetIdx(map,key);
    if (idx != HT_DELETED) {
        IntMapNode *n = map->entries[idx];
        if (map->_free_value)
            map->_free_value(n->value);
        n->key = HT_DELETED;
        map->size--;
        return 1;
    }
    return 0;
}

void *intMapGet(IntMap *map, long key) {
    unsigned long idx = intMapGetIdx(map,key);
    if (idx != HT_DELETED) {
        return map->entries[idx]->value;
    }
    return NULL;
}

/* @UnSafe 
 * What if the nth entry is deleted? */
void *intMapGetAt(IntMap *map, long index) {
    long idx = intVecGet(map->indexes,index);
    return map->entries[idx]->value;
}

/* While seemingly an overkill this means that we can use the map as a set 
 * as we can now have the value as NULL */
int intMapHas(IntMap *map, long key) {
    unsigned long idx = intMapGetIdx(map,key);
    return idx != HT_DELETED;
}

aoStr *intMapToString(IntMap *map, char *delimiter, aoStr *(*stringify_value)(void *)) {
    aoStr *buf = aoStrNew();
    unsigned long map_size = map->size;

    if (map_size == 0) {
        aoStrCatLen(buf,"{}",2);
        return buf;
    }

    unsigned long i = 0;
    IntMapIterator *it = intMapIteratorNew(map);
    IntMapNode *entry;
    while ((entry = intMapNext(it)) != NULL) {
        aoStr *value_string = stringify_value(entry->value);
        if ((i + 1) == map_size) {
            aoStrCatFmt(buf,"[%I] => %S",entry->key,value_string);
        } else {
            aoStrCatFmt(buf,"[%I] => %S%s",entry->key,value_string,delimiter);
        }
        aoStrRelease(value_string);
        i++;
    }
    intMapIteratorRelease(it);
    return buf;
}

aoStr *intMapKeysToString(IntMap *map) {
    aoStr *buf = aoStrNew();
    unsigned long map_size = map->size;

    if (map_size == 0) {
        aoStrCatLen(buf,"{}",2);
        return buf;
    }

    unsigned long i = 0;
    IntMapIterator *it = intMapIteratorNew(map);
    IntMapNode *entry;
    aoStrPutChar(buf,'{');
    while ((entry = intMapNext(it)) != NULL) {
        if ((i + 1) == map_size) {
            aoStrCatPrintf(buf,"%ld",entry->key);
        } else {
            aoStrCatPrintf(buf,"%ld, ",entry->key);
        }
        i++;
    }
    aoStrPutChar(buf,'}');
    intMapIteratorRelease(it);
    return buf;
}

unsigned long strMapHashFunction(char *key, long key_len, unsigned long mask) {
    unsigned long hash = 0;
    for (long i = 0; i < key_len; ++i) {
        hash = ((hash << 5) - hash) + key[i];
    }
    return hash & mask;
}

StrMap *strMapNew(unsigned long capacity) {
    StrMap *map = (StrMap *)malloc(sizeof(StrMap));
    capacity = roundUpToNextPowerOf2(capacity);
    map->capacity = capacity;
    map->mask = capacity - 1;
    map->size = 0;
    map->threashold = (unsigned long)(HT_LOAD * map->capacity);
    map->_free_value = NULL;
    map->_free_key = NULL;
    map->entries = (StrMapNode **)calloc(map->capacity, sizeof(StrMapNode *));
    map->indexes = intVecNew();
    map->parent = NULL;
    return map;
}

StrMap *strMapNewWithParent(unsigned long capacity, StrMap *parent) {
    StrMap *map = strMapNew(capacity);
    map->parent = parent;
    return map;
}

void strMapSetFreeValue(StrMap *map, void (*_free_value)(void *value)) {
    map->_free_value = _free_value;
}

void strMapSetFreeKey(StrMap *map, void (*_free_key)(void *key)) {
    map->_free_key = _free_key;
}

StrMapNode *strMapNodeNew(char *key, long key_len, void *value) {
    StrMapNode *n = (StrMapNode *)malloc(sizeof(StrMapNode));
    n->key = key;
    n->key_len = key_len;
    n->value = value;
    return n;
}

static int strMapKeyMatch(StrMapNode *n, char *key, ssize_t key_len) {
    return n->key_len == key_len && !memcmp(n->key,key,key_len);
}

static long strMapGetIdx(StrMap *map, char *key, long key_len) {
    unsigned long mask = map->mask;
    unsigned long idx = strMapHashFunction(key, key_len, mask);
    StrMapNode **entries = map->entries;
    StrMapNode *cur;

    while ((cur = entries[idx]) != NULL) {
        if (strMapKeyMatch(cur,key,key_len)) {
            return idx;
        }
        idx = (idx + 1) & mask;
    }
    return HT_DELETED;
}

static unsigned long strMapGetNextIdx(StrMap *map, char *key, long key_len,
        int *_is_free)
{ // Finds the next avalible slot
    unsigned long mask = map->mask;
    unsigned long idx = strMapHashFunction(key, key_len, mask);
    StrMapNode *cur;
    *_is_free = 0;
    while ((cur = map->entries[idx]) != NULL) {
        if (cur->key == NULL) {
            *_is_free = 0;
            return idx;
        } else if (strMapKeyMatch(cur,key,key_len)) {
            *_is_free = 0;
            return idx;
        }
        idx = (idx + 1) & mask;
    }
    *_is_free = 1;
    return idx;
}

void strMapClear(StrMap *map) {
    IntVec *indexes = map->indexes;
    void (*free_value)(void *value) = map->_free_value;
    void (*free_key)(void *_key) = map->_free_key;
    for (int i = 0; i < indexes->size; ++i) {
        long idx = vecGet(long,indexes,i);
        StrMapNode *n = map->entries[idx];
        if (n) {
            if (free_value)
                free_value(n->value);
            if (free_key)
                free_key(n->key);
            free(n);
        }
    }
    map->size = 0;
    memset(map->entries,0,map->capacity*sizeof(StrMapNode *));
    intVecClear(map->indexes);
}

void strMapRelease(StrMap *map) { // free the entire hashtable
    if (map) {
        void (*free_value)(void *_val) = map->_free_value;
        void (*free_key)(void *_key) = map->_free_key;
        for (unsigned long i = 0; i < map->capacity; ++i) {
            StrMapNode *n = map->entries[i];
            if (n) {
                if (free_value)
                    free_value(n->value);
                if (free_key)
                    free_key(n->key);
                free(n);
            }
        }
        intVecRelease(map->indexes);
        free(map->entries);
        free(map);
    }
}

void strMapMerge(StrMap *map1, StrMap *map2) {
    StrMapIterator *it = strMapIteratorNew(map2);
    StrMapNode *n = NULL;
    while ((n = strMapNext(it)) != NULL) {
        strMapAddOrErr(map1, n->key, n->value);
    }
    strMapIteratorRelease(it);
}

void strMapRemoveKeys(StrMap *map1, StrMap *map2) {
    StrMapIterator *it = strMapIteratorNew(map2);
    StrMapNode *n = NULL;
    while ((n = strMapNext(it)) != NULL) {
        strMapRemove(map1, n->key);
    }
    strMapIteratorRelease(it);
}

// Resize the hashtable, will return false if OMM
int strMapResize(StrMap *map) {
    unsigned long new_capacity, new_mask;
    long *new_indexes;
    long *old_index_entries = map->indexes->entries;
    int indexes_capacity = (int)map->indexes->size;
    StrMapNode **old_entries, **new_entries;
    int is_free;

    new_capacity = map->capacity << 1;
    new_mask = new_capacity - 1;
    old_entries = map->entries;

    new_indexes = (long *)calloc(new_capacity, sizeof(long));
    /* OOM */
    if (new_indexes == NULL) {
        return 0;
    }

    new_entries = (StrMapNode **)calloc(new_capacity, sizeof(StrMapNode *));
    /* OOM */
    if (new_entries == NULL) {
        free(new_indexes);
        return 0;
    }

    map->mask = new_mask;
    map->entries = new_entries;
    map->capacity = new_capacity;
    long new_size = 0;

    /* Keeps insertion order, and does not have to go over the capacity of 
     * the hashtable which 'dict.c' has to do on a resize thus this should in
     * theory be faster, but there are more array lookups however they should 
     * have good spatial locality */
    for (unsigned long i = 0; i < map->size; ++i) {
        long idx = old_index_entries[i];
        StrMapNode *old = old_entries[idx];
        if (old->key != NULL) {
            long new_idx = strMapGetNextIdx(map,old->key,old->key_len,&is_free);
            new_indexes[new_size] = new_idx;
            new_entries[new_idx] = old;
            /* keep track of the new size of this hashtable */
            new_size++;
        } else {
            free(old);
        }
    }

    free(old_entries);
    free(map->indexes->entries);
    map->indexes->size = new_size;
    map->indexes->capacity = indexes_capacity;
    map->indexes->entries = new_indexes;

    map->size = new_size;
    map->threashold = (unsigned long)(new_capacity * HT_LOAD);
    return 1;
}

int strMapAddLen(StrMap *map, char *key, long key_len, void *value) {
    int is_free;

    if (map->size >= map->threashold) {
        if (!strMapResize(map)) {
            /* This means we have run out of memory */
            return 0;
        }
    }

    unsigned long idx = strMapGetNextIdx(map, key, key_len, &is_free);

    if (is_free) {
        StrMapNode *n = strMapNodeNew(key, key_len, value);
        intVecPush(map->indexes,idx);
        map->entries[idx] = n;
        map->size++;
        return 1;
    } else {
        StrMapNode *n = map->entries[idx];
        n->key = key;
        n->key_len = key_len;
        n->value = value;
        return 1;
    }
}

/* Perform a linear scan of the hashtable to try and find an element */
StrMapNode *strMapFindByValue(StrMap *map,
                              void *value,
                              int (*is_match)(void *v1, StrMapNode *_entry))
{
    if (map->size == 0) {
        return NULL;
    }
    StrMapIterator *it = strMapIteratorNew(map);
    StrMapNode *entry;
    StrMapNode *found = NULL;
    while ((entry = strMapNext(it)) != NULL) {
        if (is_match(value, entry)) {
            found = entry;
            break;
        }
    }
    strMapIteratorRelease(it);
    return found;
}

int strMapAddAoStr(StrMap *map, aoStr *key, void *value) {
    return strMapAddLen(map,key->data,key->len,value);
}

int strMapAdd(StrMap *map, char *key, void *value) {
    long key_len = strlen(key);
    return strMapAddLen(map,key,key_len,value);
}

int strMapAddLenOrErr(StrMap *map, char *key, long key_len, void *value) {
    int is_free;

    if (map->size >= map->threashold) {
        if (!strMapResize(map)) {
            /* This means we have run out of memory */
            return 0;
        }
    }

    unsigned long idx = strMapGetNextIdx(map, key, key_len, &is_free);

    if (is_free) {
        StrMapNode *n = strMapNodeNew(key, key_len, value);
        intVecPush(map->indexes,idx);
        map->entries[idx] = n;
        map->size++;
        return 1;
    } else {
        return 0;
    }
}

int strMapAddOrErr(StrMap *map, char *key, void *value) {
    long key_len = strlen(key);
    return strMapAddLenOrErr(map,key,key_len,value);
}

int strMapAddAoStrOrErr(StrMap *map, aoStr *key, void *value) {
    return strMapAddLenOrErr(map,key->data,key->len,value);
}

int strMapRemove(StrMap *map, char *key) {
    long len = strlen(key);
    unsigned long mask = map->mask;
    unsigned long idx = strMapHashFunction(key, len, mask);
    StrMapNode **entries = map->entries;
    StrMapNode *cur;

    while ((cur = entries[idx])) {
        if (cur->key_len == len && !strncmp(cur->key, key, len)) {
            if (map->_free_key)   map->_free_key(cur->key);
            if (map->_free_value) map->_free_value(cur->value);
            cur->value = cur->key = NULL;
            map->size--;
            return 1;
        }
        idx = (idx + 1) & mask;
    }
    return 0;
}

void *strMapGetLen(StrMap *map, char *key, long key_len) {
    if (!key) return NULL;
    for (; map != NULL; map = map->parent) {
        unsigned long idx = strMapGetIdx(map,key,key_len);
        if (idx != HT_DELETED) {
            StrMapNode *n = map->entries[idx];
            assert(n);
            return n->value;
        }
    }
    return NULL;
}

void *strMapGetAoStr(StrMap *map, aoStr *key) {
    return strMapGetLen(map,key->data,key->len);
}

void *strMapGet(StrMap *map, char *key) {
    long key_len = strlen(key);
    return strMapGetLen(map,key,key_len);
}

int strMapHas(StrMap *map, char *key) {
    long key_len = strlen(key);
    for (; map; map = map->parent) {
        unsigned long idx = strMapGetIdx(map,key,key_len);
        if (idx != HT_DELETED)
            return 1;
    }
    return 0;
}

StrMapIterator *strMapIteratorNew(StrMap *map) {
    StrMapIterator *it = (StrMapIterator *)malloc(sizeof(StrMapIterator));
    it->idx = 0;
    it->map = map;
    return it;
}

void strMapIteratorRelease(StrMapIterator *it) {
    if (it) {
        free(it);
    }
}

StrMapNode *strMapNext(StrMapIterator *it) {
    while (it->idx < it->map->indexes->size) {
        long idx = intVecGet(it->map->indexes, it->idx);
        StrMapNode *n = it->map->entries[idx];
        if (n->key != NULL) {
            it->idx++;
            return n;
        }
        it->idx++;
    }
    return NULL;
}

char *strMapToString(StrMap *map, char *(*stringify_value)(void *)) {
    aoStr *str = aoStrNew();
    unsigned long map_size = map->size;

    if (map_size == 0) {
        aoStrCatLen(str,"{}",2);
        return aoStrMove(str);
    }

    unsigned long i = 0;
    StrMapIterator *it = strMapIteratorNew(map);
    StrMapNode *entry;
    aoStrPutChar(str,'{');
    while ((entry = strMapNext(it)) != NULL) {
        char *value_string = stringify_value(entry->value);
        if ((i + 1) == map_size) {
            aoStrCatPrintf(str,"[%s] => %s}",entry->key,value_string);
        } else {
            aoStrCatPrintf(str,"[%s] => %s, ",entry->key,value_string);
        }
        free(value_string);
        i++;
    }
    strMapIteratorRelease(it);
    return aoStrMove(str);
}

char *strMapKeysToString(StrMap *map) {
    aoStr *str = aoStrNew();
    unsigned long map_size = map->size;

    if (map_size == 0) {
        aoStrCatLen(str,"{}",2);
        return aoStrMove(str);
    }

    unsigned long i = 0;
    StrMapIterator *it = strMapIteratorNew(map);
    StrMapNode *entry;
    aoStrPutChar(str,'{');
    while ((entry = strMapNext(it)) != NULL) {
        if ((i + 1) == map_size) {
            aoStrCatPrintf(str,"%s}",entry->key);
        } else {
            aoStrCatPrintf(str,"%s, ",entry->key);
        }
        i++;
    }
    strMapIteratorRelease(it);
    return aoStrMove(str);
}

IntSet *intSetNew(unsigned long capacity) {
    IntSet *iset = (IntSet *)malloc(sizeof(IntSet));
    iset->size = 0;
    iset->capacity = capacity;
    iset->mask = capacity-1;
    iset->threashold = (unsigned long)(HT_LOAD * capacity);
    iset->entries = (long *)malloc(sizeof(long)*capacity);
    iset->indexes = intVecNew();
    setAllLongs(iset->entries,capacity,HT_VACANT);
    return iset;
}

static unsigned long intSetGetNextIdx(long *entries, unsigned long mask,
        long key, int *_is_free)
{ 
    unsigned long idx = key & mask;
    long cur;

    while ((cur = entries[idx]) != HT_VACANT) {
        if (cur == key || cur == HT_DELETED) {
            *_is_free = 0;
            return idx;
        }
        idx = (idx + 1) & mask;
    }
    *_is_free = 1;
    return idx;
}

static long intSetGetIdx(long *entries, unsigned long mask, long key) {
    unsigned long idx = intMapHashFunction(key, mask);
    long cur_key;

    while ((cur_key = entries[idx]) != HT_VACANT) {
        if (cur_key == key) {
            return idx;
        }
        idx = (idx + 1) & mask;
    }
    return HT_DELETED;
}

/* @CutNPaste
 * This is very similar to intMapResize
 * */
int intSetResize(IntSet *iset) {
    // Resize the hashtable, will return false if OMM
    unsigned long new_capacity = iset->capacity << 1;
    unsigned long new_mask = new_capacity - 1;
    long *old_entries = iset->entries;
    long *old_index_entries = iset->indexes->entries;
    int indexes_capacity = (int)iset->indexes->size;

    long *new_entries = (long *)malloc(new_capacity * sizeof(long));
    /* OOM */
    if (new_entries == NULL) {
        return 0;
    }

    long *new_indexes = (long *)calloc(indexes_capacity, sizeof(long));
    /* OOM */
    if (new_indexes == NULL) {
        free(new_entries);
        return 0;
    }

    setAllLongs(new_entries,new_capacity,HT_VACANT);

    iset->mask = new_mask;
    iset->entries = new_entries;
    iset->capacity = new_capacity;

    long new_size = 0;
    int is_free;
    for (int i = 0; i < indexes_capacity; ++i) {
        long idx = old_index_entries[i];
        long old_key = old_entries[idx];
        if (old_key != HT_DELETED) {
            long new_idx = intSetGetNextIdx(new_entries,new_mask,old_key,&is_free);
            new_entries[new_idx] = old_key;
            new_indexes[new_size] = new_idx;
            /* keep track of the new size of this Set */
            new_size++;
        }
    } 

    free(old_entries);
    free(iset->indexes->entries);
    iset->indexes->capacity = indexes_capacity;
    iset->indexes->entries = new_indexes;
    iset->indexes->size = new_size;

    iset->size = new_size;
    iset->threashold = (unsigned long)(new_capacity * HT_LOAD);
    return 1;
}

int intSetAdd(IntSet *iset, long key) {
    if (iset->size >= iset->threashold) {
        if (!intSetResize(iset)) {
            /* This means we have run out of memory */
            return 0;
        }
    }

    int is_free = 0;
    unsigned long idx = intSetGetNextIdx(iset->entries,iset->mask,key,&is_free);
    if (is_free) {
        iset->entries[idx] = key;
        intVecPush(iset->indexes,idx);
        iset->size++;
    }
    return 1;
}

void intSetRemove(IntSet *iset, long key) {
    long idx = intSetGetIdx(iset->entries,iset->mask,key);
    if (idx != HT_DELETED) {
        /* We do nothing with the indexes, this does mean if there are multiple 
         * deletes the indexes vector would grow indefinitely */
        iset->entries[idx] = HT_DELETED;
        iset->size--;
    }
}

int intSetHas(IntSet *iset, long key) {
    return intSetGetIdx(iset->entries,iset->mask,key) != HT_DELETED;
}

long intSetGetAt(IntSet *iset, long index) {
    long idx = intVecGet(iset->indexes,index);
    return iset->entries[idx];
}

void intSetClear(IntSet *iset) {
    setAllLongs(iset->entries,iset->capacity,HT_VACANT);
    intVecClear(iset->indexes);
}

void intSetRelease(IntSet *iset) {
    if (iset) {
        free(iset->entries);
        free(iset);
    }
}

IntSetIterator *intSetIteratorNew(IntSet *iset) {
    IntSetIterator *it = (IntSetIterator *)malloc(sizeof(IntSetIterator));
    it->idx = 0;
    it->iset = iset;
    return it;
}

long intSetNext(IntSetIterator *it) {
    while ((int)it->idx < it->iset->indexes->size) {
        long idx = intVecGet(it->iset->indexes,it->idx);
        long key = it->iset->entries[idx];
        if (key != HT_DELETED && key != HT_VACANT) {
            it->idx++;
            return key;
        }
        it->idx++;
    }
    return HT_DELETED;
}

void intSetIteratorRelease(IntSetIterator *it) {
    if (it) free(it);
}

aoStr *intSetToString(IntSet *iset) {
    aoStr *buf = aoStrNew();
    unsigned long set_size = iset->size;

    if (set_size == 0) {
        aoStrCatLen(buf,str_lit("{}"));
        return buf;
    }

    unsigned long i = 0;
    long key;
    IntSetIterator *it = intSetIteratorNew(iset);
    aoStrPutChar(buf,'{');
    while ((key = intSetNext(it)) != HT_DELETED) {
        if ((i + 1) == set_size) {
            aoStrCatFmt(buf,"%i",key);
        } else {
            aoStrCatFmt(buf,"%i, ",key);
        }
        i++;
    }
    aoStrPutChar(buf,'}');
    intSetIteratorRelease(it);
    return buf;
}
