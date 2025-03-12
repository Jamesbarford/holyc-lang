#ifndef MAP_H
#define MAP_H 

#include <limits.h>

#include "aostr.h"

#define HT_LOAD    0.60
#define HT_DELETED LONG_MAX
#define HT_VACANT  LONG_MAX-1
#define HT_PROBE_1 1
#define HT_PROBE_3 3

/* This is very similar to the MapIndex struct, maybe we can repurpose it 
 * or repurpose this to be the index */
typedef struct IntVec {
    int size;
    int capacity;
    long *entries;
} IntVec;

IntVec *intVecNew(void);
void intVecPush(IntVec *vec, long value);
void intVecClear(IntVec *vec);
void intVecRelease(IntVec *vec);
void intVecMoveFirst(IntVec *vec, int key);
long intVecPop(IntVec *vec, int *ok);

/* This could be used to replace the `List` */
typedef struct PtrVec {
    int size;
    int capacity;
    void **entries;
} PtrVec;

#define vecInBounds(vec, idx) ((idx >= 0) && idx < (vec)->size)
#define vecGetInBounds(vec, idx) (vecInBounds(vec,idx) ? (vec)->entries[idx] : NULL)
#define vecEmpty(vec) ((vec)->size == 0)
#define vecGet(type,vec,idx) ((type)((vec)->entries[idx]))
#define vecTail(type,vec) ((type)((vec)->entries[(vec)->size-1]))
#define vecFor(vec) \
    for (int i = 0; i < vec->size; ++i)
#define vecForGet(type, vec) vecGet(type,vec,i)

PtrVec *ptrVecNew(void);
void ptrVecPush(PtrVec *vec, void *value);
void *ptrVecGet(PtrVec *vec, int idx);
void *ptrVecRemoveIdx(PtrVec *vec, int idx);
void *ptrVecRemove(PtrVec *vec, void *entry, 
                   int (*entry_match_fnptr)(void *map_entry, void *match_param));
void ptrVecClear(PtrVec *vec, void (*free_entry_fnptr)(void *value));
void ptrVecRelease(PtrVec *vec);

typedef struct IntMapNode {
    long key;
    void *value;
} IntMapNode;

typedef struct IntMap {
    unsigned long size;     /* How many entries are in the hashtable */
    unsigned long capacity; /* How much capacity we have in the entries array */
    unsigned long
            mask; /* Used for hashing, as the capacity is always a power of 2
                   * we can use fast modulo of `<int> & capacity-1`. */
    unsigned long threashold; /* rebuild threashold */
    void (*_free_value)(
            void *value); /* User defined callback for freeing values */
    IntVec *indexes; /* Where all of the values are in the entries array, in
                    * insertion order. Means we can iterate over the
                    * HashTable quickly at the cost of memory */
    IntMapNode **entries; /* All of the entries, XXX: could this be IntMapNode
                           *entries? */
} IntMap;

typedef struct IntMapIterator {
    IntMap *map;
    unsigned long idx;
} IntMapIterator;

int intMapAdd(IntMap *map, long key, void *value);
void *intMapGet(IntMap *map, long key);
void *intMapGetAt(IntMap *map, long index);
void *intMapGetFirst(IntMap *map);
int intMapDelete(IntMap *map, long key);
int intMapHas(IntMap *map, long key);
IntMap *intMapNew(unsigned long capacity);
void intMapSetFreeValue(IntMap *map, void (*_free_value)(void *value));
void intMapClear(IntMap *map);
void intMapRelease(IntMap *map);
int intMapResize(IntMap *map);
aoStr *intMapToString(IntMap *map, char *delimiter, aoStr *(*stringify_value)(void *));
aoStr *intMapKeysToString(IntMap *map);

IntMapIterator *intMapIteratorNew(IntMap *map);
void intMapIteratorRelease(IntMapIterator *it);
IntMapNode *intMapNext(IntMapIterator *it);

typedef struct StrMapNode {
    char *key;
    long key_len;
    void *value;
} StrMapNode;

typedef struct StrMap {
    unsigned long size;     /* How many entries are in the hashtable */
    unsigned long capacity; /* How much capacity we have in the entries array */
    unsigned long
            mask; /* Used for hashing, as the capacity is always a power of 2
                   * we can use fast modulo of `<int> & capacity-1`. */
    unsigned long threashold; /* rebuild threashold */
    struct StrMap *parent;
    void (*_free_value)(
            void *_value); /* User defined callback for freeing values */
    void (*_free_key)(void *_key); /* User defined callback for freeing keys */
    IntVec *indexes; /* Where all of the values are in the entries array, in
                    * insertion order. Means we can iterate over the
                    * HashTable quickly at the cost of memory */
    StrMapNode **entries; /* All of the entries, XXX: could this be IntMapNode
                           *entries? */
} StrMap;

typedef struct StrMapIterator {
    StrMap *map;
    long idx;
} StrMapIterator;

unsigned long roundUpToNextPowerOf2(unsigned long v);

StrMap *strMapNew(unsigned long capacity);
StrMap *strMapNewWithParent(unsigned long capacity, StrMap *parent);
void *strMapGetLen(StrMap *map, char *key, long key_len);
void *strMapGetAoStr(StrMap *map, aoStr *key);
void *strMapGet(StrMap *map, char *key);

int strMapAdd(StrMap *map, char *key, void *value);
int strMapAddLen(StrMap *map, char *key, long key_len, void *value);
int strMapAddAoStr(StrMap *map, aoStr *key, void *value);

/* Return an error rather than updating a value in the map with the same key */
int strMapAddOrErr(StrMap *map, char *key, void *value);
int strMapAddLenOrErr(StrMap *map, char *key, long key_len, void *value);
int strMapAddAoStrOrErr(StrMap *map, aoStr *key, void *value);

int strMapHas(StrMap *map, char *key);
int strMapRemove(StrMap *map, char *key);
void strMapRelease(StrMap *map);
void strMapClear(StrMap *map);
int strMapResize(StrMap *map);
void strMapSetFreeValue(StrMap *map, void (*_free_value)(void *value));
void strMapSetFreeKey(StrMap *map, void (*_free_key)(void *key));
void strMapMerge(StrMap *map1, StrMap *map2);
void strMapRemoveKeys(StrMap *map1, StrMap *map2);

StrMapNode *strMapFindByValue(StrMap *map,
                              void *value,
                              int (*is_match)(void *v1, StrMapNode *_entry));

StrMapIterator *strMapIteratorNew(StrMap *map);
void strMapIteratorRelease(StrMapIterator *it);
StrMapNode *strMapNext(StrMapIterator *it);

char *strMapToString(StrMap *map, char *(*stringify_value)(void *));
char *strMapKeysToString(StrMap *map);


typedef struct IntSet {
    unsigned long size;
    unsigned long capacity;
    unsigned long mask;
    unsigned long threashold;
    long *entries;
    IntVec *indexes;
} IntSet;

typedef struct IntSetIterator {
    IntSet *iset;
    unsigned long idx;
} IntSetIterator;

IntSet *intSetNew(unsigned long capacity);
int intSetAdd(IntSet *iset, long key);
void intSetRemove(IntSet *iset, long key);
int intSetHas(IntSet *iset, long key);
long intSetGetAt(IntSet *iset, long index);
void intSetClear(IntSet *iset);
void intSetRelease(IntSet *iset);
IntSetIterator *intSetIteratorNew(IntSet *iset);
long intSetNext(IntSetIterator *it);
void intSetIteratorRelease(IntSetIterator *it);
aoStr *intSetToString(IntSet *iset);

#endif
