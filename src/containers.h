#ifndef CONTAINERS_H__
#define CONTAINERS_H__

#include "aostr.h"

#define MAP_LOAD 0.60

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
    const char *type_str;
};

/* For pointers */
struct Vec {
    unsigned long size;
    unsigned long capacity;
    void **entries;
    VecType *type;
};

#define vecInBounds(vec, idx) ((idx >= 0) && idx < (vec)->size)
#define vecGetInBounds(vec, idx) (vecInBounds(vec,idx) ? (vec)->entries[idx] : NULL)
#define vecEmpty(vec) ((vec)->size == 0)
#define vecGet(type,vec,idx) ((type)((vec)->entries[idx]))
#define vecTail(type,vec) ((type)((vec)->entries[(vec)->size-1]))

Vec *vecNew(VecType *type);
Vec *vecNewFrom(VecType *type, ...);
void vecReserve(Vec *vec, unsigned long capacity);
void vecPush(Vec *vec, void *value);
void vecPushInt(Vec *vec, unsigned long value);
void *vecPop(Vec *vec, int *_ok);
int vecRemove(Vec *vec, void *value);
int vecRemoveAt(Vec *vec, unsigned long idx);
void vecInsertAt(Vec *vec, void *value, unsigned long idx);
void *vecGetAt(Vec *vec, unsigned long idx);
int vecHas(Vec *vec, void *needle);
void vecRelease(Vec *vec);
void vecClear(Vec *vec);
AoStr *vecTypeToString(const char *type_str);
AoStr *vecEntriesToString(Vec *vec);
AoStr *vecToString(Vec *vec);
void vecPrint(Vec *vec);

extern VecType vec_long_type;
extern VecType vec_unsigned_long_type;

/*================== Generic MAP =============================================*/

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

struct MapType {
    mapKeyMatch *match;
    mapKeyHash *hash;
    mapKeyLen *get_key_len;
    mapKeyToString *key_to_string;
    mapKeyRelease *key_release;
    mapValueToString *value_to_string;
    mapValueRelease *value_release;
    const char *key_type;
    const char *value_type;
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
    MapNode *entries;
    Vec *indexes;
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
Map *mapNewWithParent(Map *parent, unsigned long capacity, MapType *type);
int mapAdd(Map *map, void *key, void *value);
int mapAddOrErr(Map *map, void *key, void *value);
int mapAddLen(Map *map, char *key, long key_len, void *value);
void mapRemove(Map *map, void *key);
int mapHas(Map *map, void *key);
int mapHasAll(Map *map, ...);
void *mapGet(Map *map, void *key);
void *mapGetLen(Map *map, char *key, long len);
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
void mapMerge(Map *map1, Map *map2);
void mapPrintStats(Map *map);

int mapIntKeyMatch(void *a, void *b);
long mapIntKeyLen(void *key);
unsigned long mapIntKeyHash(void *key);
AoStr *mapIntToString(void *key);
int mapAoStrKeyMatch(void *s1, void *s2);
long mapAoStrLen(void *s);
AoStr *mapAoStrToString(void *s);
void mapAoStrRelease(void *s);
unsigned long mapCStringHashLen(void *str, long len);
unsigned long mapCStringHash(void *str);
int mapCStringEq(void *s1, void *s2);
long mapCStringLen(void *s);
AoStr *mapCStringToString(void *s);

extern MapType map_cstring_cstring_type;
/* `Map<char *, void *>` for when we don't care about the value, this should
 * be used sparingly */
extern MapType map_cstring_opaque_type;

/*================== Generic SET =============================================*/
typedef struct Set Set;
typedef struct SetIter SetIter;
typedef struct SetType SetType;
typedef struct SetNode SetNode;

typedef int (setValueMatch)(void *v1, void *v2);
typedef unsigned long (setValueHash)(void *value);
typedef AoStr *(setValueToString)(void *value);
typedef void (setValueRelease)(void *value);
typedef long (setKeyLen)(void *key);

struct SetType {
    setValueMatch *match;
    setValueHash *hash;
    setKeyLen* get_key_len;
    setValueToString *stringify;
    setValueRelease *value_release;
    const char *type;
};

struct SetNode {
    int occupied;
    void *key;
    long key_len;
};

extern SetType set_aostr_type;
extern SetType set_int_type;

struct Set {
    unsigned long size;
    unsigned long capacity;
    unsigned long mask;
    unsigned long threashold;
    SetNode *entries;
    Vec *indexes;
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
int setHasLen(Set *set, char *key, long len);
void *setGetAt(Set *set, long index);
void setClear(Set *set);
void setRelease(Set *set);
SetIter *setIterNew(Set *set);
void setIterInit(Set *set, SetIter *it);
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

/* `Set<char *>` set does not own the `char *`*/
extern SetType set_cstring_type;
/* `Set<char *>` the set owns the `char *`*/
extern SetType set_cstring_owned_type;

unsigned long roundUpToNextPowerOf2(unsigned long v);
#endif
