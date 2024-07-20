#ifndef MAP_H
#define MAP_H 

#define HT_LOAD    0.60
#define HT_DELETED LONG_MAX
#define HT_PROBE_1 1
#define HT_PROBE_3 3

typedef struct IntMapNode {
    long key;
    void *value;
} IntMapNode;

typedef struct StrMapNode {
    char *key;
    long key_len;
    void *value;
} StrMapNode;

typedef struct IntMap {
    unsigned long size;     /* How many entries are in the hashtable */
    unsigned long capacity; /* How much capacity we have in the entries array */
    unsigned long
            mask; /* Used for hashing, as the capacity is always a power of 2
                   * we can use fast modulo of `<int> & capacity-1`. */
    unsigned long threashold; /* rebuild threashold */
    void (*_free_value)(
            void *value); /* User defined callback for freeing values */
    long *indexes; /* Where all of the values are in the entries array, in
                    * insertion order. Means we can iterate over the
                    * HashTable quickly at the cost of memory */
    IntMapNode **entries; /* All of the entries, XXX: could this be IntMapNode
                           *entries? */
} IntMap;

typedef struct StrMap {
    unsigned long size;     /* How many entries are in the hashtable */
    unsigned long capacity; /* How much capacity we have in the entries array */
    unsigned long
            mask; /* Used for hashing, as the capacity is always a power of 2
                   * we can use fast modulo of `<int> & capacity-1`. */
    unsigned long threashold; /* rebuild threashold */
    void (*_free_value)(
            void *_value); /* User defined callback for freeing values */
    void (*_free_key)(void *_key); /* User defined callback for freeing keys */
    long *indexes; /* Where all of the values are in the entries array, in
                    * insertion order. Means we can iterate over the
                    * HashTable quickly at the cost of memory */
    StrMapNode **entries; /* All of the entries, XXX: could this be IntMapNode
                           *entries? */
} StrMap;

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

/* This could be used to replace the `List` */
typedef struct PtrVec {
    int size;
    int capacity;
    void **entries;
} PtrVec;
#define vecEmpty(vec) (vec)->size == 0
#define vecGet(type,vec,idx) ((type)((vec)->entries[idx]))

PtrVec *ptrVecNew(void);
void ptrVecPush(PtrVec *vec, void *value);
void *ptrVecGet(PtrVec *vec, int idx);
void ptrVecRelease(PtrVec *vec);

int intMapSet(IntMap *map, long key, void *value);
void *intMapGet(IntMap *map, long key);
int intMapDelete(IntMap *map, long key);
int intMapHas(IntMap *map, long key);
IntMap *intMapNew(unsigned long capacity);
void intMapSetfreeValue(IntMap *map, void (*_free_value)(void *value));
void intMapClear(IntMap *map);
void intMapRelease(IntMap *map);
int intMapResize(IntMap *map);
int intMapIter(IntMap *map, long *_idx, IntMapNode **_node);
int intMapValueIter(IntMap *map, long *_idx, void **_value);
int intMapKeyIter(IntMap *map, long *_idx, long *_key);

int strMapSet(StrMap *map, char *key, void *value);
void *strMapGet(StrMap *map, char *key);
StrMap *strMapNew(unsigned long capacity);
void strMapSetfreeValue(StrMap *map, void (*_free_value)(void *value));
void strMapSetfreeKey(StrMap *map, void (*_free_key)(void *key));
void strMapRelease(StrMap *map);
int strMapResize(StrMap *map);
int strMapIter(StrMap *map, long *_idx, StrMapNode **_node);
int strMapValueIter(StrMap *map, long *_idx, void **_value);
int strMapKeyIter(StrMap *map, long *_idx, char **_key);

#endif
