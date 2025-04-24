#ifndef MAP_H
#define MAP_H 

#include <limits.h>

#include "aostr.h"

#define HT_LOAD    0.60
#define HT_DELETED LONG_MAX
#define HT_VACANT  LONG_MAX-1
#define HT_PROBE_1 1
#define HT_PROBE_3 3

#ifdef DEBUG
    #define VEC_DEBUG_SIZE 512
#endif

/* This is very similar to the MapIndex struct, maybe we can repurpose it 
 * or repurpose this to be the index */
typedef struct IntVec {
    int size;
    int capacity;
#ifdef DEBUG
    long entries[VEC_DEBUG_SIZE];
#else
    long *entries;
#endif
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
#ifdef DEBUG
    void *entries[VEC_DEBUG_SIZE];
#else
    void **entries;
#endif
} PtrVec;

#define vecInBounds(vec, idx) ((idx >= 0) && idx < (vec)->size)
#define vecGetInBounds(vec, idx) (vecInBounds(vec,idx) ? (vec)->entries[idx] : NULL)
#define vecEmpty(vec) ((vec)->size == 0)
#define vecGet(type,vec,idx) ((type)((vec)->entries[idx]))
#define vecTail(type,vec) ((type)((vec)->entries[(vec)->size-1]))

PtrVec *ptrVecNew(void);
void ptrVecPush(PtrVec *vec, void *value);
void *ptrVecGet(PtrVec *vec, int idx);
void *ptrVecRemoveIdx(PtrVec *vec, int idx);
void *ptrVecRemove(PtrVec *vec, void *entry, 
                   int (*entry_match_fnptr)(void *map_entry, void *match_param));
void ptrVecClear(PtrVec *vec, void (*free_entry_fnptr)(void *value));
void ptrVecRelease(PtrVec *vec);

/*====== GENERIC VECTOR ======================================================*/
typedef struct Vec Vec;
typedef struct VecType VecType;

typedef void (vecValueStringify)(AoStr *buf, void *);
typedef int (vecValueMatch)(void *, void *);
typedef int (vecValueRelease)(void *);

struct VecType {
    /* For printing the vector */
    vecValueStringify *stringify;
    /* For matching values in the vector */
    vecValueMatch *match;
    /* Optional call back for freeing the value when removing the value,
     * freeing the vector or clearing the vector*/
    vecValueRelease *release;
    /* For pretty printing the `Vec<...>`*/
    char *type_str;
};

/* For pointers */
struct Vec {
    unsigned long size;
    unsigned long capacity;
#ifdef DEBUG
    void *entries[VEC_DEBUG_SIZE];
#else
    void **entries;
#endif
    VecType *type;
};

Vec *vecNew(VecType *type);
Vec *vecNewFrom(VecType *type, ...);
void vecReserve(Vec *vec, unsigned long capacity);
void vecPush(Vec *vec, void *value);
void *vecPop(Vec *vec, int *_ok);
int vecRemove(Vec *vec, void *value);
int vecRemoveAt(Vec *vec, unsigned long idx);
void vecInsertAt(Vec *vec, void *value, unsigned long idx);
void *vecGetAt(Vec *vec, unsigned long idx);
int vecHas(Vec *vec, void *needle);
void vecRelease(Vec *vec);
void vecClear(Vec *vec);
AoStr *vecTypeToString(char *type_str);
AoStr *vecEntriesToString(Vec *vec);
AoStr *vecToString(Vec *vec);
void vecPrint(Vec *vec);


/*====== HASH TABLES =========================================================*/

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
AoStr *intMapToString(IntMap *map, char *delimiter, AoStr *(*stringify_value)(void *));
AoStr *intMapKeysToString(IntMap *map);

IntMapIterator *intMapIteratorNew(IntMap *map);
void intMapIteratorRelease(IntMapIterator *it);
IntMapNode *intMapNext(IntMapIterator *it);

#define IntMapFor(it, value) \
    for (void *value = intMapNext(it); value != NULL; value = intMapNext(it))


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
void *strMapGetAoStr(StrMap *map, AoStr *key);
void *strMapGet(StrMap *map, char *key);

int strMapAdd(StrMap *map, char *key, void *value);
int strMapAddLen(StrMap *map, char *key, long key_len, void *value);
int strMapAddAoStr(StrMap *map, AoStr *key, void *value);

/* Return an error rather than updating a value in the map with the same key */
int strMapAddOrErr(StrMap *map, char *key, void *value);
int strMapAddLenOrErr(StrMap *map, char *key, long key_len, void *value);
int strMapAddAoStrOrErr(StrMap *map, AoStr *key, void *value);

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

typedef struct IntSetIter {
    IntSet *iset;
    unsigned long idx;
} IntSetIter;


IntSet *intSetNew(unsigned long capacity);
int intSetAdd(IntSet *iset, long key);
void intSetRemove(IntSet *iset, long key);
int intSetHas(IntSet *iset, long key);
long intSetGetAt(IntSet *iset, long index);
void intSetClear(IntSet *iset);
void intSetRelease(IntSet *iset);
IntSetIter *intSetIterNew(IntSet *iset);
long intSetNext(IntSetIter *it);
void intSetIterRelease(IntSetIter *it);
AoStr *intSetToString(IntSet *iset);

/* @DataStructures
 * We need a massive refactor, and ONLY use the generic implmentations, it's 
 * extremely annoying having mulitple hashtables */

/*================== Generic MAP ==============================================*/
typedef struct MapNode MapNode;
typedef struct Map Map;
typedef struct MapIter MapIter;
typedef struct MapType MapType;

#define MAP_FLAG_FREE    (1<<0)
#define MAP_FLAG_TAKEN   (1<<1)
#define MAP_FLAG_DELETED (1<<2)

typedef int (mapKeyMatch)(void *v1, void *v2);
typedef unsigned long (mapKeyHash)(void *value);
typedef long (mapKeyLen)(void *value);
typedef AoStr *(mapKeyToString)(void *value);
typedef AoStr *(mapValueToString)(void *value);
typedef void (mapValueRelease)(void *value);
typedef void (mapKeyRelease)(void *value);

extern MapType int_map_type;

struct MapType {
    mapKeyMatch *match;
    mapKeyHash *hash;
    mapKeyLen *get_key_len;
    mapKeyToString *key_to_string;
    mapValueToString *value_to_string;
    mapValueRelease *value_release;
    mapKeyRelease *key_release;
    const char *value_type;
    const char *key_type;
};

struct MapNode {
    unsigned int flags;
    void *key;
    void *value;
    long key_len;
};

struct Map {
    unsigned long size;
    unsigned long capacity;
    unsigned long mask;
    unsigned long threashold;
#ifdef DEBUG
    MapNode entries[VEC_DEBUG_SIZE];
#else
    MapNode *entries;
#endif
    IntVec *indexes;
    MapType *type;
    Map *parent;
    /* A _very_ common idiom is to see if the `mapHas(...)` some value 
     * immediately followed by a `mapGet(...)` so we cache the key on
     * `mapHas(...)` */
    void *cached_key;
    unsigned long cached_idx;
};

#define mapSetValueToString(map, func) ((map)->type->value_to_string = (func))
#define mapSetValueRelease(map, func) ((map)->type->value_release = (func))
#define mapSetValueType(map, type) ((map)->type->value_type = (type))
#define mapSetKeyType(map, type) ((map)->type->key_type = (type))

struct MapIter {
    Map *map;
    MapNode *node;
    unsigned long idx;
    unsigned long vecidx;
};

Map *mapNew(unsigned long capacity, MapType *type);
int mapAdd(Map *map, void *key, void *value);
int mapAddOrErr(Map *map, void *key, void *value);
void mapRemove(Map *map, void *key);
int mapHas(Map *map, void *key);
int mapHasAll(Map *map, ...);
void *mapGet(Map *map, void *key);
void *mapGetAt(Map *map, unsigned long index);
void mapClear(Map *map);
void mapRelease(Map *map);
MapIter *mapIterNew(Map *map);
void mapIterInit(Map *map, MapIter *iter);
int mapIterNext(MapIter *it);
void mapIterRelease(MapIter *it);
AoStr *mapToString(Map *map, char *delimiter);
AoStr *mapKeysToString(Map *map);
void mapPrint(Map *map);
void mapPrintStats(Map *map);

int intMapKeyMatch(void *a, void *b);
long intMapKeyLen(void *key);
unsigned long intMapKeyHash(void *key);
AoStr *intMapKeyToString(void *key);

/*================== Generic SET ==============================================*/
typedef struct Set Set;
typedef struct SetIter SetIter;
typedef struct SetType SetType;
typedef struct SetNode SetNode;

typedef int (setValueMatch)(void *v1, void *v2);
typedef unsigned long (setValueHash)(void *value);
typedef AoStr *(setValueToString)(void *value);
typedef AoStr *(setValueRelease)(void *value);

struct SetType {
    setValueMatch *match;
    setValueToString *stringify;
    setValueHash *hash;
    setValueRelease *value_release;
    char *type;
};

struct SetNode {
    int free;
    void *key;
};

extern SetType aostr_set_type;
extern SetType int_set_type;

struct Set {
    unsigned long size;
    unsigned long capacity;
    unsigned long mask;
    unsigned long threashold;
#ifdef DEBUG
    SetNode entries[VEC_DEBUG_SIZE];
#else
    SetNode *entries;
#endif
    IntVec *indexes;
    SetType *type;
};

#define SetFor(it, value) \
    for (void *value = setNext(it); value != NULL; value = setNext(it))

#define setAssignValueMatch(set, func) ((set)->type->match = (func))
#define setAssignValueToString(set, func) ((set)->type->stringify = (func))
#define setAssignValueHash(set, func) ((set)->type->hash = (func))

struct SetIter {
    /* Set being iterated over */
    Set *set;
    /* The user facing index which the size relative to the size of the Set */
    unsigned long idx;
    /* The internal index into the vector of indexes */
    unsigned long vecidx;
    /* Current value being pointed to */
    void *value;
};

Set *setNew(unsigned long capacity, SetType *type);
int setAdd(Set *set, void *key);
void *setRemove(Set *set, void *key);
int setHas(Set *set, void *key);
void *setGetAt(Set *set, long index);
void setClear(Set *set);
void setRelease(Set *set);
SetIter *setIterNew(Set *set);
void setIterInit(SetIter *it, Set *set);
void *setNext(SetIter *it);
int setIterNext(SetIter *it);
void setIterRelease(SetIter *it);
AoStr *setToString(Set *set);
AoStr *setEntriesToString(Set *set);
int setEq(Set *s1, Set *s2);
/* Add all of `s2` to `s1` */
Set *setUnion(Set *s1, Set *s2);
/* Add from s1 that are not in s2 */
Set *setDifference(Set *s1, Set *s2);
/* this is not a deep copy in that the values are not duplicated */
Set *setCopy(Set *set);
void setPrint(Set *set);
void setPrintStats(Set *set);

#endif
