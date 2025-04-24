#include <limits.h>
#include <stdarg.h>
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

#ifndef DEBUG

#define vectorNew(ret_type, entry_type)                               \
    ret_type *vec;                                                    \
    do {                                                              \
        vec = (ret_type *)malloc(sizeof(ret_type));                   \
        vec->size = 0;                                                \
        vec->capacity = VECTOR_INITIAL_CAPACITY;                      \
        vec->entries = (entry_type *)malloc(sizeof(entry_type) *      \
                VECTOR_INITIAL_CAPACITY);                             \
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
    
#else 
#define vectorNew(ret_type, entry_type)                               \
    ret_type *vec;                                                    \
    do {                                                              \
        vec = (ret_type *)malloc(sizeof(ret_type));                   \
        vec->size = 0;                                                \
        vec->capacity = VEC_DEBUG_SIZE;                               \
        memset(vec->entries,0,sizeof(vec->entries));                  \
    } while (0)

#define vectorPush(vec, type, value)                               \
    do {                                                           \
        if (vec->size + 1 >= VEC_DEBUG_SIZE) {                     \
            loggerPanic("Vec size would be: %d\n", vec->size + 1); \
        }                                                          \
        vec->entries[vec->size++] = value;                         \
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
            free(vec);                                                     \
        }                                                                  \
    } while (0)

#endif

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
#ifndef DEBUG
        free(vec->entries);
#endif
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

/*====== GENERIC VECTOR ======================================================*/

Vec *vecNew(VecType *type) {
    Vec *vec = (Vec *)malloc(sizeof(Vec));
    vec->size = 0;

#ifndef DEBUG
    vec->capacity = 16;
    vec->entries = (void **)malloc(sizeof(void *) * vec->capacity);
#else
    vec->capacity = VEC_DEBUG_SIZE;
#endif
    memset(vec->entries, 0, sizeof(void *) * vec->capacity);
    assert(type);
    vec->type = type;
    return vec;
}

void vecReserve(Vec *vec, unsigned long capacity) {
#ifndef DEBUG
    if (capacity > vec->capacity) {
        void **new_entries = malloc(sizeof(void *) * capacity);
        assert(new_entries != NULL);
        memcpy(new_entries, vec->entries, sizeof(void *) * vec->size);
        free(vec->entries);
        vec->entries = new_entries;
        vec->capacity = capacity;
    }
#else
    (void)capacity;
    AoStr *type = vecTypeToString(vec->type->type_str);
    fprintf(stderr, "%s with capacity %lu not big enough for Debug Mode!\n",
            type->data,
            vec->capacity);
    aoStrRelease(type);
    exit(1);
#endif
}

Vec *vecNewFrom(VecType *type, ...) {
    va_list ap;
    va_start(ap, type);

    Vec *vec = vecNew(type);
    void *value = NULL;
    while (1) {
        if ((value = va_arg(ap, void *)) == NULL) {
            break;
        }
        vecPush(vec, value);
    }
    va_end(ap);
    return vec;
}

void vecPush(Vec *vec, void *value) {
    if (vec->size + 1 >= vec->capacity) {
        vecReserve(vec, vec->capacity * 2);
    }
    vec->entries[vec->size++] = value;
}

void *vecPop(Vec *vec, int *_ok) {
    if (vec->size == 0) {
        *_ok = 0;
        return NULL;
    }
    void *value = vec->entries[--vec->size];
    return value;
}

int vecRemoveAt(Vec *vec, unsigned long idx) {
    if (idx >= vec->size) return 0;

    if (vec->type->release) {
        vec->type->release(vec->entries[idx]);
    }
    unsigned long elements_to_move = vec->size - idx - 1;
    if (elements_to_move > 0) {
        memmove(&vec->entries[idx],
                &vec->entries[idx + 1],
                elements_to_move * sizeof(void *));
    }
    vec->size--;
    return 1;
}

int vecRemove(Vec *vec, void *value) {
    unsigned long idx = 0;
    int found = 0;
    for (unsigned long i = 0; i < vec->size; ++i) {
        if (vec->type->match(vec->entries[i], value)) {
            idx = i;
            found = 1;
            break;
        }
    }

    if (found) {
        if (vec->type->release) {
            vec->type->release(vec->entries[idx]);
        }
        unsigned long elements_to_move = vec->size - idx - 1;
        if (elements_to_move > 0) {
            memmove(&vec->entries[idx],
                    &vec->entries[idx + 1],
                    elements_to_move * sizeof(void *));
        }
        vec->size--;
    }
    
    return found;
}

void vecInsertAt(Vec *vec, void *value, unsigned long idx) {
    if (vec->size + 1 >= vec->capacity) {
        vecReserve(vec, vec->capacity * 2);
    }
    if (idx >= vec->size) {
        fprintf(stderr, "Vec - idx %lu is out of range", idx);
    }
    unsigned long elements_to_move = vec->size - idx;
    memmove(&vec->entries[idx + 1],
            &vec->entries[idx],
            elements_to_move * sizeof(void *));
    vec->entries[idx] = value;
    vec->size++;
}

void *vecGetAt(Vec *vec, unsigned long idx) {
#ifdef DEBUG
    if (idx >= vec->size) {
        fprintf(stderr, "Vec - idx %lu is out of range for Vec of size %lu",
                idx, vec->size);
    }
#endif
    return vec->entries[idx];
}

int vecHas(Vec *vec, void *needle) {
    for (unsigned long i = 0; i < vec->size; ++i) {
        if (vec->type->match(vec->entries[i], needle)) {
            return 1;
        }
    }
    return 0;
}

void vecClear(Vec *vec) {
    if (vec->size != 0) {
        int (*free_value)(void *) = vec->type->release;
        if (free_value) {
            for (unsigned long i = 0; i < vec->size; ++i) {
                free_value(vec->entries[i]);
            }
        }
        vec->size = 0;
    }
}

void vecRelease(Vec *vec) {
    vecClear(vec);
#ifndef DEBUG
    free(vec->entries);
#endif
    free(vec);
}

AoStr *vecTypeToString(char *type) {
    if (is_terminal) {
        return aoStrPrintf(ESC_GREEN"Vec"ESC_RESET"<"ESC_CYAN"%s"ESC_RESET">",type);
    } else {
        return aoStrPrintf("Vec<%s>", type);
    }
}

/* Simply get the contents of the array without the type information */
AoStr *vecEntriesToString(Vec *vec) {
    AoStr *buf = aoStrNew();
    if (vec->size == 0) {
        aoStrCatLen(buf, str_lit("[]"));
        return buf;
    }
    aoStrCatLen(buf, str_lit("["));
    for (unsigned long i = 0; i < vec->size; ++i) {
        vec->type->stringify(buf, vec->entries[i]);
        if (i + 1 != vec->size) {
            aoStrCatLen(buf, str_lit(", "));
        }
    }
    aoStrPutChar(buf, ']');
    return buf;
}

AoStr *vecToString(Vec *vec) {
    AoStr *buf = vecTypeToString(vec->type->type_str);
    AoStr *entries_str = vecEntriesToString(vec);
    aoStrCatFmt(buf, " %S", entries_str);
    return buf;
}

void vecPrint(Vec *vec) {
    AoStr *vec_str = vecToString(vec);
    printf("%s\n",vec_str->data);
    aoStrRelease(vec_str);
}

/*====== HASH TABLES =========================================================*/
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
    IntMapNode *cur = NULL;

    while ((cur = map->entries[idx])) {
        if (cur->key == key) {
            return idx;
        }
        idx = (idx + 1) & map->mask;
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
    *_is_free = 1;

    while ((cur = map->entries[idx]) != NULL) {
        if (cur->key == key || cur->key == HT_DELETED) {
            *_is_free = 0;
            break;
        }
        idx = (idx + 1) & mask;
    }
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
    int is_free;
    unsigned long new_capacity = map->capacity << 1;
    unsigned long new_mask = new_capacity - 1;
    long *old_index_entries = map->indexes->entries;
    int indexes_size = map->indexes->size;
    IntMapNode **old_entries = old_entries = map->entries;

    long *new_indexes = (long *)calloc(indexes_size, sizeof(long));
    if (new_indexes == NULL) {
        return 0;
    }

    IntMapNode **new_entries = (IntMapNode **)calloc(new_capacity, sizeof(IntMapNode *));
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
    for (int i = 0; i < indexes_size; ++i) {
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
    map->indexes->size = new_size;

#ifdef DEBUG
    memcpy(map->indexes->entries, new_indexes, new_size * sizeof(long));
    free(new_indexes);
#else
    free(map->indexes->entries);
    map->indexes->capacity = indexes_size;
    map->indexes->entries = new_indexes;
#endif

    map->indexes->size = new_size;
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
        if (n->key == HT_DELETED) {
            map->size++;
        }
        n->key = key;
        n->value = value;
        return 1;
    }
}

int intMapDelete(IntMap *map, long key) {
    unsigned long idx = intMapGetIdx(map,key);
    if (idx != HT_DELETED) {
        IntMapNode *n = map->entries[idx];
        if (map->_free_value) {
            map->_free_value(n->value);
        }
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

/* Get the first non-deleted item from the map */
void *intMapGetFirst(IntMap *map) {
    for (int i = 0; i < map->indexes->size; ++i) {
        long idx = map->indexes->entries[i];
        IntMapNode *node = map->entries[idx];
        if (node->key != HT_DELETED) {
            return node->value;
        }
    }
    return NULL;
}

/* While seemingly an overkill this means that we can use the map as a set 
 * as we can now have the value as NULL */
int intMapHas(IntMap *map, long key) {
    unsigned long idx = intMapGetIdx(map,key);
    return idx != HT_DELETED;
}

AoStr *intMapToString(IntMap *map, char *delimiter, AoStr *(*stringify_value)(void *)) {
    AoStr *buf = aoStrNew();
    unsigned long map_size = map->size;

    if (map_size == 0) {
        aoStrCatLen(buf,"{}",2);
        return buf;
    }

    unsigned long i = 0;
    IntMapIterator *it = intMapIteratorNew(map);
    IntMapNode *entry;
    while ((entry = intMapNext(it)) != NULL) {
        AoStr *value_string = stringify_value(entry->value);
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

AoStr *intMapKeysToString(IntMap *map) {
    AoStr *buf = aoStrNew();
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
        aoStrCatFmt(buf,"%I",entry->key);
        if ((i + 1) != map->size) {
            aoStrCatLen(buf, str_lit(", "));
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
    int is_free;
    unsigned long new_capacity = map->capacity << 1;
    unsigned long new_mask = new_capacity - 1;
    long *old_index_entries = map->indexes->entries;
    StrMapNode **old_entries = map->entries;
    int indexes_size = map->indexes->size;

    long *new_indexes = (long *)calloc(indexes_size, sizeof(long));
    /* OOM */
    if (new_indexes == NULL) {
        return 0;
    }

    StrMapNode **new_entries = (StrMapNode **)calloc(new_capacity, sizeof(StrMapNode *));
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
    for (long i = 0; i < indexes_size; ++i) {
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
    map->indexes->size = new_size;

#ifdef DEBUG
    memcpy(map->indexes->entries, new_indexes, new_size * sizeof(long));
    free(new_indexes);
#else
    free(map->indexes->entries);
    map->indexes->entries = new_indexes;
    map->indexes->capacity = indexes_size;
#endif

    map->indexes->size = new_size;
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
        /* Deleted nodes need to increment the size. This counts as adding 
         * something to the hashtable despite a node existing in entries */
        if (n->key == NULL) {
            map->size++;
        }
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

int strMapAddAoStr(StrMap *map, AoStr *key, void *value) {
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

int strMapAddAoStrOrErr(StrMap *map, AoStr *key, void *value) {
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

void *strMapGetAoStr(StrMap *map, AoStr *key) {
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
    AoStr *str = aoStrNew();
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
    AoStr *str = aoStrNew();
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

static unsigned long intSetGetNextIdx(long *entries,
                                      unsigned long mask,
                                      long key,
                                      int *_is_free)
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
    int indexes_size = iset->indexes->size;

    long *new_entries = (long *)malloc(new_capacity * sizeof(long));
    /* OOM */
    if (new_entries == NULL) {
        return 0;
    }

    long *new_indexes = (long *)calloc(indexes_size, sizeof(long));
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
    for (int i = 0; i < indexes_size; ++i) {
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
    iset->indexes->size = new_size;

#ifdef DEBUG
    memcpy(iset->indexes->entries, new_indexes, new_size * sizeof(long));
    free(new_indexes);
#else
    free(iset->indexes->entries);
    iset->indexes->capacity = indexes_size;
    iset->indexes->entries = new_indexes;
#endif

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

IntSetIter *intSetIterNew(IntSet *iset) {
    IntSetIter *it = (IntSetIter *)malloc(sizeof(IntSetIter));
    it->idx = 0;
    it->iset = iset;
    return it;
}

long intSetNext(IntSetIter *it) {
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

void intSetIterRelease(IntSetIter *it) {
    if (it) free(it);
}

AoStr *intSetToString(IntSet *iset) {
    AoStr *buf = aoStrNew();
    unsigned long set_size = iset->size;

    if (set_size == 0) {
        aoStrCatLen(buf,str_lit(" {}"));
        return buf;
    }

    unsigned long i = 0;
    long key;
    IntSetIter *it = intSetIterNew(iset);
    aoStrPutChar(buf,'{');
    while ((key = intSetNext(it)) != HT_DELETED) {
        aoStrCatFmt(buf,"%I",key);
        if ((i + 1) != set_size) {
            aoStrCatLen(buf,str_lit(", "));
        }
        i++;
    }
    aoStrPutChar(buf,'}');
    intSetIterRelease(it);
    return buf;
}

/*================ Generic Set Implementation ===============================*/

static AoStr *setTypeToString(Set *set);

static AoStr *setStats(Set *set) {
    AoStr *buf = aoStrNew();
    AoStr *type = setTypeToString(set);
    aoStrCatFmt(buf, "%S Stats {\n", type);
    aoStrCatFmt(buf, "  capacity = %U\n"
                     "  size = %U\n"
                     "  filled = %f%%\n"
                     "  threashold = %U\n"
                     "}",
                     set->capacity,
                     set->size,
                     (double)((double)((double)set->size/set->capacity)*100.0),
                     set->threashold);
    aoStrRelease(type);
    return buf;
}

void setPrintStats(Set *set) {
    AoStr *stats = setStats(set);
    printf("%s\n",stats->data);
    aoStrRelease(stats);
}

Set *setNew(unsigned long capacity, SetType *type) {
    Set *set = (Set *)malloc(sizeof(Set));
    set->size = 0;
#ifdef DEBUG
    (void)capacity;
    set->capacity = VEC_DEBUG_SIZE;
#else
    set->capacity = capacity;
#endif

    set->mask = set->capacity - 1;
    set->threashold = (unsigned long)(HT_LOAD * set->capacity);

#ifdef DEBUG
    for (int i = 0; i < VEC_DEBUG_SIZE; ++i) {
        SetNode *node = &set->entries[i];
        node->free = 1;
        node->key = NULL;
    }
#else
    set->entries = (void **)calloc(set->capacity, sizeof(void *));
#endif
    set->indexes = intVecNew();
    set->type = type;
    return set;
}

static unsigned long setGetNextIdx(Set *set, void *key, int *_is_free) { 
    unsigned long hash = set->type->hash(key);
    unsigned long idx = hash & set->mask;
    SetNode *cur = &set->entries[idx];

    while (!cur->free) { 
        if (cur && set->type->match(cur->key, key)) {
            *_is_free = 0;
            return idx;
        }
        idx = (idx + 1) & set->mask;
        cur = &set->entries[idx];
    }
    *_is_free = 1;
    return idx;
}

static unsigned long setGetIdx(Set *set, void *key, int *_ok) {
    unsigned long hash = set->type->hash(key);
    unsigned long idx = hash & set->mask;
    SetNode *cur = &set->entries[idx];

    while (!cur->free) { 
        if (set->type->match(cur->key, key)) {
            *_ok = 1;
            return idx;
        }
        idx = (idx + 1) & set->mask;
        cur = &set->entries[idx];
    }
    *_ok = 0;
    return 0;
}

static int setResize(Set *set) {
    unsigned long new_capacity = set->capacity << 1;
    unsigned long new_mask = new_capacity - 1;
    SetNode *old_entries = set->entries;
    long *old_index_entries = set->indexes->entries;
    int indexes_size = set->indexes->size;

    SetNode *new_entries = (SetNode *)malloc(new_capacity * sizeof(SetNode *));
    /* OOM */
    if (new_entries == NULL) {
        return 0;
    }


    long *new_indexes = (long *)calloc(indexes_size, sizeof(long));
    /* OOM */
    if (new_indexes == NULL) {
        free(new_entries);
        return 0;
    }

    memset(new_entries, 1, new_capacity * sizeof(SetNode  *));

    set->mask = new_mask;
#ifndef DEBUG
    set->entries = new_entries;
#endif
    set->capacity = new_capacity;

    long new_size = 0;
    int is_free;
    for (int i = 0; i < indexes_size; ++i) {
        long idx = old_index_entries[i];
        SetNode *old = &old_entries[idx];
        if (!old->free) {
            unsigned long new_idx = setGetNextIdx(set, old, &is_free);
            SetNode *new_node = &new_entries[idx];
            new_node->free = 0;
            new_node->key = old->key;
            new_indexes[new_size] = new_idx;
            /* keep track of the new size of this Set */
            new_size++;
        }
    } 

    set->indexes->size = new_size;

#ifdef DEBUG
    memcpy(set->indexes->entries, new_indexes, new_size * sizeof(long));
    free(new_indexes);
#else
    free(old_entries);
    free(set->indexes->entries);
    set->indexes->capacity = indexes_size;
    set->indexes->entries = new_indexes;
#endif

    set->indexes->size = new_size;
    set->size = new_size;
    set->threashold = (unsigned long)(new_capacity * HT_LOAD);
    return 1;
}

int setAdd(Set *set, void *key) {
    if (set->size >= set->threashold) {
#ifdef DEBUG
        AoStr *stats = setStats(set);
        printf("%s\n",stats->data);
        aoStrRelease(stats);
        loggerPanic("Set with size: %lu and Capacity %lu not big enough for debug mode\n",
                set->size,
                set->capacity);
#endif
        if (!setResize(set)) {
            /* This means we have run out of memory */
            return 0;
        }
    }

    int is_free = 0;
    unsigned long idx = setGetNextIdx(set, key, &is_free);
    
    /* Add only if not in the set */
    if (is_free) {
        SetNode *node = &set->entries[idx];
        node->free = 0;
        node->key = key;
        intVecPush(set->indexes, idx);
        set->size++;
        return 1;
    }
    
    return 0;
}

void *setRemove(Set *set, void *key) {
    int ok = 0;
    unsigned long idx = setGetIdx(set, key, &ok);
    if (ok) {
        /* We do nothing with the indexes, this does mean if there are multiple 
         * deletes the indexes vector would grow indefinitely */
        SetNode *node = &set->entries[idx];
        void *key = node->key;
        node->free = 1;
        node->key = NULL;
        set->size--;
        return key;
    }
    return NULL;
}

int setHas(Set *set, void *key) {
    int ok = 0;
    setGetIdx(set, key, &ok);
    return ok == 1;
}

void *setGetAt(Set *set, long index) {
    if (index < 0 || index >= set->indexes->size)
        return NULL;
        
    long idx = intVecGet(set->indexes, index);
    SetNode *node = &set->entries[idx];
    return node->key;
}

void setClear(Set *set) {
    memset(set->entries, 1, sizeof(void *) * set->capacity);
    intVecClear(set->indexes);
    set->size = 0;
}

void setRelease(Set *set) {
    if (set) {
        if (set->type->value_release) {
            SetIter *it = setIterNew(set);
            SetFor(it, value) {
                set->type->value_release(value);
            }
            setIterRelease(it);
        }
#ifndef DEBUG
        free(set->entries);
#endif
        intVecRelease(set->indexes);
        free(set);
    }
}

void setIterInit(SetIter *it, Set *set) {
    it->idx = 0;
    it->vecidx = 0;
    it->set = set;
}

SetIter *setIterNew(Set *set) {
    SetIter *it = (SetIter *)malloc(sizeof(SetIter));
    setIterInit(it, set);
    return it;
}

void *setNext(SetIter *it) {
    while ((int)it->vecidx < it->set->indexes->size) {
        long idx = intVecGet(it->set->indexes, it->vecidx);
        SetNode *node = &it->set->entries[idx];
        it->vecidx++;
        if (!node->free) {
            it->idx++;
            it->value = node->key;
            return it->value;
        }
    }
    return NULL;
}

int setIterNext(SetIter *it) {
    while ((int)it->vecidx < it->set->indexes->size) {
        long idx = intVecGet(it->set->indexes, it->vecidx);
        SetNode *node = &it->set->entries[idx];
        it->vecidx++;
        if (!node->free) {
            it->idx++;
            it->value = node->key;
            return 1;
        }
    }
    return 0;
}

void setIterRelease(SetIter *it) {
    if (it) free(it);
}

static AoStr *setTypeToString(Set *set) {
    if (is_terminal) {
        return aoStrPrintf(ESC_GREEN"Set"ESC_RESET"<"ESC_CYAN"%s"
                           ESC_RESET">",
                           set->type->type);
    } else {
        return aoStrPrintf("Set<%s>", set->type->type);
    }
}

AoStr *setEntriesToString(Set *set) {
    AoStr *buf = aoStrNew();

    if (!set->type->stringify) {
        aoStrCatLen(buf, str_lit("{...}"));
        return buf;
    }

    aoStrPutChar(buf, '{');
    SetIter it;
    setIterInit(&it, set);
    while (setIterNext(&it)) {
        AoStr *str_val = set->type->stringify(it.value);
        aoStrCatAoStr(buf, str_val);
        aoStrRelease(str_val);
        if ((int)it.idx != set->indexes->size) {
            aoStrCatLen(buf, str_lit(", "));
        }
    }
    aoStrPutChar(buf, '}');
    return buf;
}

AoStr *setToString(Set *set) {
    AoStr *buf = aoStrNew();
    AoStr *type = setTypeToString(set);
    AoStr *entries_str = setEntriesToString(set);
    aoStrCatFmt(buf, "%S %S", type, entries_str);
    aoStrRelease(type);
    aoStrRelease(entries_str);
    return buf;
}

void setPrint(Set *set) {
    AoStr *str = setToString(set);
    printf("%s\n",str->data);
    aoStrRelease(str);
}

/* With `void *` and an ambigious match this will segfault */
int setEq(Set *s1, Set *s2) {
    if (s1->size != s2->size) return 0;
    int retval = 1;
    SetIter *it = setIterNew(s1);

    SetFor(it, value) {
        if (!setHas(s2, value)) {
            retval = 0;
            goto out;
        }
    }

out:
    setIterRelease(it);
    return retval;
}

/* Add all of `s2` to `s1` */
Set *setUnion(Set *s1, Set *s2) {
    Set *set = setNew(s1->capacity, s1->type);
    SetIter *it = setIterNew(s1);

    SetFor(it, value) {
        setAdd(set, value);
    }
    setIterRelease(it);

    it = setIterNew(s2);
    SetFor(it, value) {
        setAdd(set, value);
    }
    setIterRelease(it);

    return set;
}

/* Add from s1 that are not in s2 */
Set *setDifference(Set *s1, Set *s2) {
    Set *set = setNew(s1->capacity, s1->type);
    SetIter *it = setIterNew(s1);
    SetFor(it, value) {
        if (!setHas(s2, value)) {
            setAdd(set, value);
        }
    }
    return set;
}

Set *setCopy(Set *set) {
    Set *copy = setNew(set->capacity, set->type);
    /* Union will add of `set` to `copy` */
    setUnion(copy, set);
    return copy;
}

AoStr *setAoStrPassThrough(AoStr *value) {
    return value;
}

SetType aostr_set_type = {
    .match         = (setValueMatch *)aoStrEq,
    .hash          = (setValueHash *)aoStrHashFunction,
    .stringify     = (setValueToString *)setAoStrPassThrough,
    .value_release = NULL,
    .type          = "AoStr *",
};

SetType int_set_type = {
    .match         = (mapKeyMatch *)intMapKeyMatch,
    .hash          = (mapKeyHash *)intMapKeyHash,
    .stringify     = (mapKeyToString *)intMapKeyToString,
    .value_release = NULL,
    .type          = "long",
};

/*================ Generic Map Implementation ===============================*/
/* @TODO - use this everywhere */

static AoStr *mapTypeToString(Map *map) {
    /* I'm staring at maps a lot and it is nicer to colour them */
    if (is_terminal) {
        return aoStrPrintf(ESC_GREEN"Map"ESC_RESET"<"ESC_CYAN"%s"
                           ESC_RESET", "ESC_CYAN"%s"ESC_RESET">",
                           map->type->key_type,
                           map->type->value_type);
    } else {
        return aoStrPrintf("Map<%s, %s>", map->type->key_type,
                           map->type->value_type);
    }
}

Map *mapNew(unsigned long capacity, MapType *type) {
    Map *map = (Map *)malloc(sizeof(Map));
#ifdef DEBUG
    (void)capacity;
    map->capacity = VEC_DEBUG_SIZE;
#else
    capacity = roundUpToNextPowerOf2(capacity);
    map->capacity = capacity;
    map->entries = (MapNode *)malloc(sizeof(MapNode) * map->capacity);
#endif

    map->mask = map->capacity - 1;
    map->size = 0;
    map->threashold = (unsigned long)(HT_LOAD * map->capacity);
    memset(map->entries, 1, sizeof(MapNode) * map->capacity);
    map->indexes = intVecNew();
    map->parent = NULL;
    map->type = type;
    return map;
}

static unsigned long mapGetNextIdx(Map *map, void *key, int *_is_free) {
    unsigned long hash = map->type->hash ? map->type->hash(key) : (unsigned long)key;
    unsigned long idx = hash & map->mask;
    MapNode *cur = &map->entries[idx];
    *_is_free = 1;

    /* A free node is one that is either free or previously deleted */
    while (1) {
        if (cur->flags & (MAP_FLAG_FREE | MAP_FLAG_DELETED)) {
            break;
        }
        /* This means we have found a node with the same key we are trying to
         * add*/
        else if (cur->flags & MAP_FLAG_TAKEN && map->type->match(cur->key, key)) {
            *_is_free = 0;
            break;
        }
        idx = (idx + 1) & map->mask;
        cur = &map->entries[idx];
    }

    return idx;
}

static unsigned long mapGetIdx(Map *map, void *key, int *_ok) {
    unsigned long hash = map->type->hash(key);
    unsigned long idx = hash & map->mask;
    MapNode *cur = &map->entries[idx];

    /* We are only interested in TAKEN nodes */
    while (cur->flags & MAP_FLAG_TAKEN) {
        /* We know something is in this position as it is flagged as TAKEN */
        if (map->type->match(cur->key, key)) {
            *_ok = 1;
            return idx;
        }
        idx = (idx + 1) & map->mask;
        cur = &map->entries[idx];
    }
    *_ok = 0;
    return 0;
}

static int mapResize(Map *map) {
    int is_free;
    unsigned long new_capacity = map->capacity << 1;
    unsigned long new_mask = new_capacity - 1;
    long *old_index_entries = map->indexes->entries;
    MapNode *old_entries = map->entries;
    int indexes_size = map->indexes->size;

    long *new_indexes = (long *)calloc(indexes_size, sizeof(long));
    /* OOM */
    if (new_indexes == NULL) {
        return 0;
    }

    MapNode *new_entries = (MapNode *)calloc(new_capacity, sizeof(MapNode));
    /* OOM */
    if (new_entries == NULL) {
        free(new_indexes);
        return 0;
    }

    map->mask = new_mask;
#ifndef DEBUG
    map->entries = new_entries;
#endif
    map->capacity = new_capacity;
    long new_size = 0;

    /* Keeps insertion order, and does not have to go over the capacity of 
     * the hashtable which should in theory be faster, but there are more 
     * array lookups however they should have good spatial locality */
    for (long i = 0; i < indexes_size; ++i) {
        long idx = old_index_entries[i];
        MapNode *old = &old_entries[idx];
        if (old->flags & MAP_FLAG_TAKEN) {
            long new_idx = mapGetNextIdx(map,old->key,&is_free);
            MapNode *new_node = &new_entries[new_idx];
            new_node->key = old->key;
            new_node->value = old->value;
            new_node->key_len = old->key_len;
            new_node->flags = MAP_FLAG_TAKEN;
            new_indexes[new_size] = new_idx;
            /* keep track of the new size of this hashtable */
            new_size++;
        }
    }

    map->indexes->size = new_size;

#ifdef DEBUG
    memcpy(map->indexes->entries, new_indexes, new_size * sizeof(long));
    free(new_indexes);
#else
    free(old_entries);
    free(map->indexes->entries);
    map->indexes->entries = new_indexes;
    map->indexes->capacity = indexes_size;
#endif

    map->indexes->size = new_size;
    map->size = new_size;
    map->threashold = (unsigned long)(new_capacity * HT_LOAD);
    return 1;
}

int mapAdd(Map *map, void *key, void *value) {
    if (map->size >= map->threashold) {
        if (!mapResize(map)) {
            /* This means we have run out of memory */
            return 0;
        }
    }

    int is_free;
    unsigned long idx = mapGetNextIdx(map, key, &is_free);
    MapNode *n = &map->entries[idx];

    /* Only if it is not free do we add to the vector... Though this does mean
     * the ordering that we are trying to keep is now messed up... */
    if (is_free) {
        intVecPush(map->indexes, idx);
        map->size++;
    }

    n->key = key;
    n->value = value;
    n->key_len = map->type->get_key_len(key);
    n->flags = MAP_FLAG_TAKEN;
    return 1;
}

int mapAddOrErr(Map *map, void *key, void *value) {
    if (map->size >= map->threashold) {
#ifdef DEBUG
        mapPrintStats(map);
        loggerPanic("Map capacity of %lu is not big enough!\n", map->capacity);
#endif
        if (!mapResize(map)) {
            /* This means we have run out of memory */
            return 0;
        }
    }

    int is_free = 0;
    unsigned long idx = mapGetNextIdx(map, key, &is_free);

    /* We only add if it is free otherwise this operation is an error - 
     * in practice this ensures we can't overwrite values. */
    if (is_free) {
        MapNode *n = &map->entries[idx];
        intVecPush(map->indexes, idx);
        n->key = key;
        n->value = value;
        n->key_len = map->type->get_key_len(key);
        n->flags = MAP_FLAG_TAKEN;
        map->size++;
        return 1;
    }
    return 0;
}

void mapRemove(Map *map, void *key) {
    int ok = 0;
    unsigned long idx = mapGetIdx(map, key, &ok);
    if (ok == 0) return;
    MapNode *n = &map->entries[idx];
    if (map->type->value_release) map->type->value_release(n->value);
    if (map->type->key_release) map->type->key_release(n->key);
    n->flags = MAP_FLAG_DELETED;
    n->value = NULL;
    n->key = NULL;
    n->key_len = 0;
    /* @Bug
     * Why does reducing the map size cause the hashtable to do weird
     * things?*/
    map->size--;
}

int mapHas(Map *map, void *key) {
    int ok = 0;
    unsigned long idx = mapGetIdx(map, key, &ok);
    if (ok == 0) return 0;
    map->cached_key = key;
    map->cached_idx = idx;
    return 1;
}

/* Check map has all of the values */
int mapHasAll(Map *map, ...) {
    va_list ap;
    va_start(ap, map);
    void *value = va_arg(ap, void *);
    int retval = 1;

    while (value != NULL) {
        int ok = 0;
        mapGetIdx(map, value, &ok);
        if (ok == 0) {
            retval = 0;
            goto out;
        }
        value = va_arg(ap, void *);
    }
out:
    va_end(ap);
    return retval;
}

void *mapGetAt(Map *map, unsigned long index) {
    for (unsigned long i = index; i < (unsigned long)map->indexes->size; ++i) {
        long vec_idx = map->indexes->entries[i];
        MapNode *n = &map->entries[vec_idx];
        if (n->flags & MAP_FLAG_TAKEN) {
            return n->value;
        }
    }
    return NULL;
}

void *mapGet(Map *map, void *key) {
    /* Retrive from cache if we can */
    if (map->cached_key == key) {
        MapNode *n = &map->entries[map->cached_idx];
        if (n->flags & MAP_FLAG_TAKEN) {
            return n->value;
        }
        return NULL;
    }

    for (; map != NULL; map = map->parent) {
        int ok = 0;
        unsigned long idx = mapGetIdx(map, key, &ok);
        if (ok) {
            /* May as well... at least it means the value is not stale */
            MapNode *n = &map->entries[idx];
            map->cached_key = key;
            map->cached_idx = idx;
            return n->value;
        }
    }
    return NULL;
}

void mapClear(Map *map) {
    MapIter *it = mapIterNew(map);
    while (mapIterNext(it)) {
        MapNode *n = it->node;
        n->flags = MAP_FLAG_FREE;
        if (map->type->value_release) map->type->value_release(n->value);
        if (map->type->key_release) map->type->key_release(n->key);
        n->key = NULL;
        n->value = NULL;
        n->key_len = 0;
    }
    map->size = 0;
    intVecClear(map->indexes);
    mapIterRelease(it);
}

void mapRelease(Map *map) {
    mapClear(map);
#ifndef DEBUG
    free(map->entries);
#endif
    intVecRelease(map->indexes);
    free(map);
}

void mapIterInit(Map *map, MapIter *iter) {
    iter->map = map;
    iter->node = NULL;
    iter->idx = 0;
    iter->vecidx = 0;
}

MapIter *mapIterNew(Map *map) {
    MapIter *iter = (MapIter *)malloc(sizeof(MapIter));
    mapIterInit(map, iter);
    return iter;
}

int mapIterNext(MapIter *it) {
    while ((int)it->vecidx < it->map->indexes->size) {
        unsigned long idx = intVecGet(it->map->indexes, it->vecidx);
        MapNode *n = &it->map->entries[idx];
        it->vecidx++;
        if (n->flags & MAP_FLAG_TAKEN) {
            it->idx++;
            it->node = n;
            return 1;
        }
    }
    return 0;
}


void mapIterRelease(MapIter *it) {
    if (it) free(it);
}

static AoStr *mapStats(Map *map) {
    AoStr *buf = aoStrNew();
    AoStr *type = mapTypeToString(map);
    aoStrCatFmt(buf, "%S Stats {\n", type);
    aoStrCatFmt(buf, "  capacity = %U\n"
                     "  size = %U\n"
                     "  filled = %f\n"
                     "  threashold = %U\n"
                     "}",
                     map->capacity,
                     map->size,
                     (double)(((double)map->size/map->capacity)/100),
                     map->threashold);
    aoStrRelease(type);
    return buf;
}

void mapPrintStats(Map *map) {
    AoStr *stats = mapStats(map);
    printf("%s\n",stats->data);
    aoStrRelease(stats);
}

/* Convert the map to a string */
AoStr *mapToString(Map *map, char *delimiter) {
    AoStr *buf = aoStrNew();
    AoStr *type = mapTypeToString(map);
    if (map->size == 0) {
        aoStrCatFmt(buf, "%S {}", type);
        aoStrRelease(type);
        return buf;
    }

    aoStrCatFmt(buf, "%S {\n", type);
    aoStrRelease(type);
    MapIter *it = mapIterNew(map);
    while (mapIterNext(it)) {
        MapNode *n = it->node;
        AoStr *value_string = map->type->value_to_string(n->value);
        AoStr *key_string = map->type->key_to_string(n->key);
        if (it->idx == map->size) {
            aoStrCatFmt(buf,"  [%S] => %S", key_string, value_string);
        } else {
            aoStrCatFmt(buf,"  [%S] => %S%s", key_string, value_string, delimiter);
        }
        aoStrRelease(value_string);
        aoStrRelease(key_string);
    }
    aoStrCatLen(buf, str_lit("\n}"));
    mapIterRelease(it);
    aoStrRelease(type);
    return buf;
}

AoStr *mapKeysToString(Map *map) {
    AoStr *buf = aoStrNew();
    if (map->size == 0) {
        aoStrCatLen(buf, str_lit(" {}"));
        return buf;
    }

    aoStrCatFmt(buf, "{");
    MapIter *it = mapIterNew(map);
    while (mapIterNext(it)) {
        MapNode *n = it->node;
        AoStr *key_string = map->type->key_to_string(n->key);
        if (it->idx == map->size) {
            aoStrCatFmt(buf,"%S",key_string);
        } else {
            aoStrCatFmt(buf,"%S, ", key_string);
        }
        aoStrRelease(key_string);
    }
    aoStrCatLen(buf, str_lit("}"));
    mapIterRelease(it);
    return buf;
}

void mapPrint(Map *map) {
    AoStr *map_str = mapToString(map, ",\n");
    printf("%s\n\n",map_str->data);
    aoStrRelease(map_str);
}

/*================= Int Map speciality ========================================*/
int intMapKeyMatch(void *a, void *b) {
    return (long)a == (long)b;
}

long intMapKeyLen(void *key) {
    (void)key;
    return 0;
}

/* This does not hash the integer... AND-ing it with the mask seems reasonable 
 * enough. _mostly_ in this application the integers are incrementing 
 * which means they're not going to clash, though will bunch together meaning
 * wasted space */
unsigned long intMapKeyHash(void *key) {
    return (unsigned long)(long)key;
}

AoStr *intMapKeyToString(void *key) {
    return aoStrPrintf("%ld", (long)key);
}

MapType int_map_type = {
    .match = (mapKeyMatch *)intMapKeyMatch,
    .hash = (mapKeyHash *)intMapKeyHash,
    .get_key_len = (mapKeyLen *)intMapKeyLen,
    .key_to_string = (mapKeyToString *)intMapKeyToString,
    .value_to_string = NULL,
    .value_release = NULL,
    .value_type = NULL,
    .key_type = "long",
};
