#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "map.h"
#include "util.h"

#define HT_LOAD    0.60
#define HT_DELETED LONG_MAX
#define HT_PROBE_1 1
#define HT_PROBE_3 3

static unsigned long intMapHashFunction(long key, unsigned long mask) {
    return key & mask;
}

IntMap *intMapNew(unsigned long capacity) {
    IntMap *map = malloc(sizeof(IntMap));
    map->capacity = capacity;
    map->mask = capacity - 1;
    map->size = 0;
    map->threashold = (unsigned long)(HT_LOAD * capacity);
    map->_free_value = NULL;
    map->entries = (IntMapNode **)calloc(capacity, sizeof(IntMapNode *));
    map->indexes = (long *)calloc(capacity, sizeof(long));
    return map;
}

void intMapSetfreeValue(IntMap *map, void (*_free_value)(void *value)) {
    map->_free_value = _free_value;
}

IntMapNode *intMapNodeNew(long key, void *value) {
    IntMapNode *n = malloc(sizeof(IntMapNode));
    n->key = key;
    n->value = value;
    return n;
}

/* Finds the next avalible slot */ 
static unsigned long intMapGetNextIdx(IntMap *map, long key, int *_is_free) { 
    unsigned long mask = map->mask;
    unsigned long idx = key & mask;
    unsigned long probe = 1;
    IntMapNode *cur;

    while ((cur = map->entries[idx]) != NULL) {
        if (cur->key == key || cur->key == HT_DELETED) {
            *_is_free = 0;
            return idx;
        }
       idx = (idx + HT_PROBE_1 * probe + HT_PROBE_3 * probe * probe) & mask;
       probe++;
    }
    *_is_free = 1;
    return idx;
}

void intMapClear(IntMap *map) {
    long *indexes = map->indexes;
    void (*free_value)(void *value) = map->_free_value;
    for (int i = 0; i < map->size; ++i) {
        long idx = indexes[i];
        IntMapNode *n = map->entries[idx];
        if (n) {
            if (free_value)
                free_value(n->value);
            free(n);
        }
    }
    map->size = 0;
    memset(map->entries,0,map->capacity*sizeof(IntMapNode *));
    memset(map->indexes,0,map->size*sizeof(long));
}

void intMapRelease(IntMap *map) { // free the entire hashtable
    if (map) {
        void (*free_value)(void *value) = map->_free_value;
        for (long i = 0; i < map->capacity; ++i) {
            IntMapNode *n = map->entries[i];
            if (n) {
                if (free_value)
                    free_value(n->value);
                free(n);
            }
        }
        free(map->entries);
        free(map->indexes);
        free(map);
    }
}

int intMapResize(IntMap *map) {
    // Resize the hashtable, will return false if OMM
    unsigned long new_capacity, new_mask;
    IntMapNode **new_entries, **old_entries;
    long *new_indexes, *old_indexes;
    int is_free;

    old_entries = map->entries;
    old_indexes = map->indexes;

    new_capacity = map->capacity << 1;
    new_mask = new_capacity - 1;

    new_indexes = calloc(new_capacity, sizeof(long));
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
    for (long i = 0; i < map->size; ++i) {
        long idx = old_indexes[i];
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
    free(old_indexes);
    map->size = new_size;
    map->indexes = new_indexes;
    map->threashold = (unsigned long)(new_capacity * HT_LOAD);
    return 1;
}

int intMapSet(IntMap *map, long key, void *value) {
    int is_free;

    if (map->size >= map->threashold) {
        if (!intMapResize(map)) {
            /* This means we have run out of memory */
            return 0;
        }
    }

    unsigned long idx = intMapGetNextIdx(map, key, &is_free);
    if (is_free) {
        IntMapNode *n = intMapNodeNew(key, value);
        map->indexes[map->size] = idx;
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
    unsigned long idx, mask, probe;
    IntMapNode **entries = map->entries;
    IntMapNode *cur;
    mask = map->mask;
    idx = intMapHashFunction(key, mask);
    probe = 1;
    while ((cur = entries[idx])) {
        if (cur->key == key) {
            cur->key = HT_DELETED;
            map->indexes[idx] = HT_DELETED;
            map->size--;
            return 1;
        }
        idx = (idx + HT_PROBE_1 * probe + HT_PROBE_3 * probe * probe) & mask;
        probe++;
    }
    return 0;
}

void *intMapGet(IntMap *map, long key) {
    unsigned long idx, mask, probe;
    IntMapNode **entries = map->entries;
    IntMapNode *cur;

    mask = map->mask;
    probe = 1;
    idx = intMapHashFunction(key, mask);
    while ((cur = entries[idx])) {
        if (cur->key == key) {
            return cur->value;
        }
        idx = (idx + HT_PROBE_1 * probe + HT_PROBE_3 * probe * probe) & mask;
        probe++;
    }
    return NULL;
}

/* While seemingly an overkill this means that we can use the map as a set 
 * as we can now have the value as NULL */
int intMapHas(IntMap *map, long key) {
    unsigned long idx, mask, probe;
    IntMapNode **entries = map->entries;
    IntMapNode *cur;
    mask = map->mask;
    probe = 1;
    idx = intMapHashFunction(key, mask);
    while ((cur = entries[idx])) {
        if (cur->key == key) {
            return 1;
        }
        idx = (idx + HT_PROBE_1 * probe + HT_PROBE_3 * probe * probe) & mask;
        probe++;
    }
    return 0;
}

int intMapIter(IntMap *map, long *_idx, IntMapNode **_node) {
    long idx = *_idx;
    while (idx < map->size) {
        unsigned long index = map->indexes[idx];
        if (index != HT_DELETED) {
            *_idx = idx + 1;
            *_node = map->entries[index];
            return 1;
        }
        idx++;
    }
    return 0;
}

int intMapValueIter(IntMap *map, long *_idx, void **_value) {
    IntMapNode *node;
    if (intMapIter(map, _idx, &node)) {
        *_value = node->value;
        return 1;
    }
    return 0;
}

int intMapKeyIter(IntMap *map, long *_idx, long *_key) {
    IntMapNode *node;
    if (intMapIter(map, _idx, &node)) {
        *_key = node->key;
        return 1;
    }
    return 0;
}

unsigned long strMapHashFunction(char *key, long key_len, unsigned long mask) {
    unsigned long hash = 0;
    for (long i = 0; i < key_len; ++i) {
        hash = ((hash << 5) - hash) + key[i];
    }
    return hash & mask;
}

StrMap *strMapNew(unsigned long capacity) {
    StrMap *map = malloc(sizeof(StrMap));
    map->capacity = capacity;
    map->mask = capacity - 1;
    map->size = 0;
    map->threashold = (unsigned long)(HT_LOAD * map->capacity);
    map->_free_value = NULL;
    map->_free_key = NULL;
    map->entries = (StrMapNode **)calloc(map->capacity, sizeof(StrMapNode *));
    map->indexes = (long *)calloc(capacity, sizeof(long));
    return map;
}

void strMapSetfreeValue(StrMap *map, void (*_free_value)(void *value)) {
    map->_free_value = _free_value;
}

void strMapSetfreeKey(StrMap *map, void (*_free_key)(void *key)) {
    map->_free_key = _free_key;
}

StrMapNode *strMapNodeNew(char *key, long key_len, void *value) {
    StrMapNode *n = malloc(sizeof(StrMapNode));
    n->key = key;
    n->key_len = key_len;
    n->value = value;
    return n;
}

static unsigned long strMapGetNextIdx(StrMap *map, char *key, long key_len,
                 int *_is_free)
{ // Finds the next avalible slot
    unsigned long mask = map->mask;
    unsigned long idx = strMapHashFunction(key, key_len, mask);
    unsigned long probe = 1;
    StrMapNode *cur;
    *_is_free = 0;
    while ((cur = map->entries[idx]) != NULL) {
        if (cur->key == NULL) {
            *_is_free = 0;
            return idx;
        } else if (!strncmp(cur->key, key, cur->key_len)) {
            *_is_free = 0;
            return idx;
        }
        idx = (idx + HT_PROBE_1 * probe + HT_PROBE_3 * probe * probe) & mask;
        probe++;
    }
    *_is_free = 1;
    return idx;
}

void strMapRelease(StrMap *map) { // free the entire hashtable
    if (map) {
        void (*free_value)(void *_val) = map->_free_value;
        void (*free_key)(void *_key) = map->_free_key;
        for (long i = 0; i < map->capacity; ++i) {
            StrMapNode *n = map->entries[i];
            if (n) {
                if (free_value)
                    free_value(n->value);
                if (free_key)
                    free_key(n->key);
                free(n);
            }
        }
        free(map->entries);
        free(map->indexes);
        free(map);
    }
}

// Resize the hashtable, will return false if OMM
int strMapResize(StrMap *map) {
    unsigned long new_capacity, new_mask;
    long *new_indexes, *old_indexes;
    StrMapNode **old_entries, **new_entries;
    int is_free;

    old_entries = map->entries;
    old_indexes = map->indexes;

    new_capacity = map->capacity << 1;
    new_mask = new_capacity - 1;

    new_indexes = calloc(new_capacity, sizeof(long));
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
    for (long i = 0; i < map->size; ++i) {
        long idx = old_indexes[i];
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
    free(old_indexes);
    map->size = new_size;
    map->indexes = new_indexes;
    map->threashold = (unsigned long)(new_capacity * HT_LOAD);
    return 1;
}

int strMapSet(StrMap *map, char *key, void *value) {
    int is_free;

    if (map->size >= map->threashold) {
        if (!strMapResize(map)) {
            /* This means we have run out of memory */
            return 0;
        }
    }

    long key_len = strlen(key);
    unsigned long idx = strMapGetNextIdx(map, key, key_len, &is_free);

    if (is_free) {
        StrMapNode *n = strMapNodeNew(key, key_len, value);
        map->entries[idx] = n;
        map->indexes[map->size] = idx;
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

int strMapDelete(StrMap *map, char *key) {
    unsigned long idx, mask, probe;
    long len = strlen(key);
    StrMapNode **entries = map->entries;
    StrMapNode *cur;
    mask = map->mask;
    idx = strMapHashFunction(key, len, mask);
    probe = 1;

    while ((cur = entries[idx])) {
        if (cur->key_len == len && !strncmp(cur->key, key, len)) {
            if (map->_free_key)   map->_free_key(cur->key);
            if (map->_free_value) map->_free_value(cur->value);
            cur->value = cur->key = NULL;
            map->indexes[idx] = HT_DELETED;
            map->size--;
            return 1;
        }
        idx = (idx + HT_PROBE_1 * probe + HT_PROBE_3 * probe * probe) & mask;
        probe++;
    }
    return 0;
}

void *strMapGet(StrMap *map, char *key) {
    unsigned long idx, mask, probe;
    long len = strlen(key);
    StrMapNode **entries = map->entries;
    StrMapNode *cur;

    mask = map->mask;
    probe = 1;
    idx = strMapHashFunction(key, len, mask);
    while ((cur = entries[idx])) {
        if (cur->key == NULL) {
            return NULL;
        }
        if (cur->key_len == len && !strncmp(cur->key, key, len)) {
            return cur->value;
        }
        idx = (idx + HT_PROBE_1 * probe + HT_PROBE_3 * probe * probe) & mask;
        probe++;
    }
    return NULL;
}

int strMapIter(StrMap *map, long *_idx, StrMapNode **_node) {
    long idx = *_idx;
    long *indexes = map->indexes;
    while (idx < map->size) {
        long index = indexes[idx];
        if (index != HT_DELETED) {
            *_idx = idx + 1;
            *_node = map->entries[index];
            return 1;
        }
        idx++;
    }
    return 0;
}

int strMapValueIter(StrMap *map, long *_idx, void **_value) {
    StrMapNode *node;
    if (strMapIter(map, _idx, &node)) {
        *_value = node->value;
        return 1;
    }
    return 0;
}

int strMapKeyIter(StrMap *map, long *_idx, char **_key) {
    StrMapNode *node;
    if (strMapIter(map, _idx, &node)) {
        *_key = node->key;
        return 1;
    }
    return 0;
}

#define vectorNew(ret_type, entry_type)                               \
    ret_type *vec;                                                    \
    do {                                                              \
        vec = (ret_type *)malloc(sizeof(ret_type));                   \
        vec->size = 0;                                                \
        vec->capacity = 32;                                           \
        vec->entries = (entry_type *)malloc(sizeof(entry_type) * 32); \
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
