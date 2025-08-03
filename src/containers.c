#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#include "aostr.h"
#include "containers.h"
#include "util.h"

static void vecIntValueStringify(AoStr *buf, void *ptr) {
    s64 value = (long)ptr;
    aoStrCatFmt(buf, "%I", value);
}

static int vecIntValueMatch(void *v1, void *v2) {
    return (long)v1 == (long)v2;
}

int vecIntValueRelease(void *ptr) {
    (void)ptr;
    return 1;
}

VecType vec_long_type = {
    .stringify = vecIntValueStringify,
    .match     = vecIntValueMatch,
    .release   = vecIntValueRelease,
    .type_str  = "long",
};

static void vecUnsignedIntValueStringify(AoStr *buf, void *ptr) {
    u64 value = (unsigned long)ptr;
    aoStrCatFmt(buf, "%U", value);
}

static int vecUnsignedIntValueMatch(void *v1, void *v2) {
    return (unsigned long)v1 == (unsigned long)v2;
}

VecType vec_unsigned_long_type = {
    .stringify = vecUnsignedIntValueStringify,
    .match     = vecUnsignedIntValueMatch,
    .release   = vecIntValueRelease,
    .type_str  = "unsigned long",
};

Vec *vecNew(VecType *type) {
    Vec *vec = (Vec *)malloc(sizeof(Vec));
    vec->size = 0;

    vec->capacity = 16;
    vec->entries = (void **)malloc(sizeof(void *) * vec->capacity);
    memset(vec->entries, 0, sizeof(void *) * vec->capacity);
    assert(type);
    vec->type = type;
    return vec;
}

void vecReserve(Vec *vec, u64 capacity) {
    if (capacity > vec->capacity) {
        void **new_entries = malloc(sizeof(void *) * capacity);
        assert(new_entries != NULL);
        memcpy(new_entries, vec->entries, sizeof(void *) * vec->size);
        free(vec->entries);
        vec->entries = new_entries;
        vec->capacity = capacity;
    }
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

void vecPushInt(Vec *vec, u64 value) {
    if (vec->size + 1 >= vec->capacity) {
        vecReserve(vec, vec->capacity * 2);
    }
    vec->entries[vec->size++] = (void *)value;
}

void *vecPop(Vec *vec, int *_ok) {
    if (vec->size == 0) {
        *_ok = 0;
        return NULL;
    }
    void *value = vec->entries[--vec->size];
    return value;
}

int vecRemoveAt(Vec *vec, u64 idx) {
    if (idx >= vec->size) return 0;

    if (vec->type->release) {
        vec->type->release(vec->entries[idx]);
    }
    u64 elements_to_move = vec->size - idx - 1;
    if (elements_to_move > 0) {
        memmove(&vec->entries[idx],
                &vec->entries[idx + 1],
                elements_to_move * sizeof(void *));
    }
    vec->size--;
    return 1;
}

int vecRemove(Vec *vec, void *value) {
    u64 idx = 0;
    int found = 0;
    for (u64 i = 0; i < vec->size; ++i) {
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
        u64 elements_to_move = vec->size - idx - 1;
        if (elements_to_move > 0) {
            memmove(&vec->entries[idx],
                    &vec->entries[idx + 1],
                    elements_to_move * sizeof(void *));
        }
        vec->size--;
    }
    
    return found;
}

void vecInsertAt(Vec *vec, void *value, u64 idx) {
    if (vec->size + 1 >= vec->capacity) {
        vecReserve(vec, vec->capacity * 2);
    }
#ifdef DEBUG
    if (idx >= vec->size) {
        loggerWarning("Vec - idx %lu is out of range\n", idx);
    }
#endif
    u64 elements_to_move = vec->size - idx;
    memmove(&vec->entries[idx + 1],
            &vec->entries[idx],
            elements_to_move * sizeof(void *));
    vec->entries[idx] = value;
    vec->size++;
}

void *vecGetAt(Vec *vec, u64 idx) {
    /* Bounds check */
#ifdef DEBUG
    if (idx >= vec->size) {
        loggerWarning("Vec - idx %lu is out of range for Vec of size %lu\n",
                idx, vec->size);
    }
#endif
    return vec->entries[idx];
}

int vecHas(Vec *vec, void *needle) {
    for (u64 i = 0; i < vec->size; ++i) {
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
            for (u64 i = 0; i < vec->size; ++i) {
                free_value(vec->entries[i]);
            }
        }
        vec->size = 0;
    }
}

void vecRelease(Vec *vec) {
    vecClear(vec);
    free(vec->entries);
    free(vec);
}

AoStr *vecTypeToString(const char *type) {
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
    for (u64 i = 0; i < vec->size; ++i) {
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

Set *setNew(u64 capacity, SetType *type) {
    Set *set = (Set *)malloc(sizeof(Set));
    set->size = 0;
    set->capacity = capacity;
    set->mask = set->capacity - 1;
    set->threashold = (unsigned long)(MAP_LOAD * set->capacity);

    set->entries = (SetNode *)malloc(set->capacity * sizeof(SetNode));
    memset(set->entries, 0, set->capacity * sizeof(SetNode));
    set->indexes = vecNew(&vec_unsigned_long_type);
    set->type = type;
    return set;
}

static u64 setGetNextIdx(Set *set, void *key, int *_is_free) { 
    u64 hash = set->type->hash(key);
    u64 idx = hash & set->mask;
    SetNode *cur = &set->entries[idx];

    while (cur->occupied) {
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

/* We need this function as often we are storing a key which may be a pointer 
 * to a larger string and we want to be able to use the length of the part of
 * the string we are interested in. */
static u64 setGetCStringNextIdx(Set *set,
                                          char *key,
                                          s64 key_len,
                                          int *_is_free)
{
    u64 hash = mapCStringHashLen(key, key_len);
    u64 idx = hash & set->mask;
    SetNode *cur = &set->entries[idx];

    while (cur->occupied) {
        if (cur->key_len == key_len && !memcmp(cur->key, key, key_len)) {
            *_is_free = 0;
            break;
        }
        idx = (idx + 1) & set->mask;
        cur = &set->entries[idx];
    }

    *_is_free = 1;
    return idx;
}

static u64 setGetIdx(Set *set, void *key, int *_ok) {
    u64 hash = set->type->hash(key);
    u64 idx = hash & set->mask;
    SetNode *cur = &set->entries[idx];

    while (cur->occupied) { 
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

static u64 setGetCStringIdx(Set *set, char *key, s64 len, int *_ok) {
    u64 hash = cstringMurmur(key, len);
    u64 idx = hash & set->mask;
    SetNode *cur = &set->entries[idx];

    while (cur->occupied) {
        if (cur->key_len == len && !memcmp(cur->key, key, len)) {
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
    SetNode *old_entries = set->entries;
    Vec *old_index_entries = set->indexes;

    u64 new_capacity = set->capacity << 1;
    u64 new_mask = new_capacity - 1;
    u64 indexes_size = set->indexes->size;

    int is_cstring_key = !strncmp(set->type->type, str_lit("char *"));

    SetNode *new_entries = (SetNode *)calloc(new_capacity, sizeof(SetNode));
    /* OOM */
    if (new_entries == NULL) {
        return 0;
    }

    void **new_indexes = (void **)calloc(indexes_size, sizeof(void *));
    /* OOM */
    if (new_indexes == NULL) {
        free(new_entries);
        return 0;
    }

    set->mask = new_mask;
    set->entries = new_entries;
    set->capacity = new_capacity;

    u64 new_size = 0;
    int is_free;
    for (u64 i = 0; i < indexes_size; ++i) {
        u64 idx = (unsigned long)vecGetAt(old_index_entries, i);
        SetNode *old = &old_entries[idx];
        if (old->occupied) {
            u64 new_idx = 0;
            if (is_cstring_key) {
                new_idx = setGetCStringNextIdx(set,
                                           old->key,
                                           old->key_len,
                                           &is_free);
            } else {
                new_idx = setGetNextIdx(set, old, &is_free);
            }
            assert(is_free);
            SetNode *new_node = &new_entries[new_idx];
            new_node->occupied = 1;
            new_node->key = old->key;
            new_node->key_len = old->key_len;
            /* keep track of the new size of this Set */
            new_indexes[new_size++] = (void *)new_idx;
        }
    }

    free(old_entries);
    free(set->indexes->entries);
    set->indexes->size = new_size;
    set->indexes->capacity = indexes_size;
    set->indexes->entries = new_indexes;

    set->size = new_size;
    set->threashold = (unsigned long)(new_capacity * MAP_LOAD);
    return 1;
}

int setAdd(Set *set, void *key) {
    if (set->size >= set->threashold) {
        if (!setResize(set)) {
            /* This means we have run out of memory */
            return 0;
        }
    }

    int is_free = 0;
    u64 idx = setGetNextIdx(set, key, &is_free);

    /* Add only if not in the set */
    if (is_free) {
        SetNode *node = &set->entries[idx];
        node->occupied = 1;
        node->key = key;
        node->key_len = set->type->get_key_len(key);
        vecPushInt(set->indexes, idx);
        set->size++;
        return 1;
    }

    return 0;
}

void *setRemove(Set *set, void *key) {
    int ok = 0;
    u64 idx = setGetIdx(set, key, &ok);
    if (ok) {
        /* We do nothing with the indexes, this does mean if there are multiple 
         * deletes the indexes vector would grow indefinitely */
        SetNode *node = &set->entries[idx];
        void *key = node->key;
        node->occupied = 0;
        node->key = NULL;
        node->key_len = 0;
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

int setHasLen(Set *set, char *key, s64 len) {
    int ok = 0;
    setGetCStringIdx(set, key, len, &ok);
    return ok == 1;
}

void *setGetAt(Set *set, s64 index) {
    if (index < 0 || (unsigned long)index >= set->indexes->size)
        return NULL;
        
    s64 idx = (long)vecGetAt(set->indexes, index);
    SetNode *node = &set->entries[idx];
    return node->key;
}

void setClear(Set *set) {
    memset(set->entries, 1, sizeof(void *) * set->capacity);
    vecClear(set->indexes);
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
        free(set->entries);
        vecRelease(set->indexes);
        free(set);
    }
}

void setIterInit(Set *set, SetIter *it) {
    it->idx = 0;
    it->vecidx = 0;
    it->set = set;
}

SetIter *setIterNew(Set *set) {
    SetIter *it = (SetIter *)malloc(sizeof(SetIter));
    setIterInit(set, it);
    return it;
}

void *setNext(SetIter *it) {
    while (it->vecidx < it->set->indexes->size) {
        s64 idx = (long)vecGetAt(it->set->indexes, it->vecidx);
        SetNode *node = &it->set->entries[idx];
        it->vecidx++;
        if (node->occupied) {
            it->idx++;
            it->value = node->key;
            return it->value;
        }
    }
    return NULL;
}

int setIterNext(SetIter *it) {
    while (it->vecidx < it->set->indexes->size) {
        s64 idx = (long)vecGetAt(it->set->indexes, it->vecidx);
        SetNode *node = &it->set->entries[idx];
        it->vecidx++;
        if (node->occupied) {
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
    setIterInit(set, &it);
    while (setIterNext(&it)) {
        AoStr *str_val = set->type->stringify(it.value);
        aoStrCatAoStr(buf, str_val);
        aoStrRelease(str_val);
        if (it.idx != set->indexes->size) {
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
    SetIter it;

    for (setIterInit(s1, &it); setIterNext(&it); ) {
        if (!setHas(s2, it.value)) {
            retval = 0;
            break;
        }
    }

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

SetType set_aostr_type = {
    .match         = (setValueMatch *)aoStrEq,
    .hash          = (setValueHash *)aoStrHashFunction,
    .get_key_len   = (setKeyLen *)aoStrGetLen,
    .stringify     = (setValueToString *)setAoStrPassThrough,
    .value_release = NULL,
    .type          = "AoStr *",
};

SetType set_int_type = {
    .match         = (mapKeyMatch *)mapIntKeyMatch,
    .hash          = (mapKeyHash *)mapIntKeyHash,
    .get_key_len   = mapIntKeyLen,
    .stringify     = (mapKeyToString *)mapIntToString,
    .value_release = NULL,
    .type          = "long",
};

SetType set_cstring_type = {
    .match         = mapCStringEq,
    .hash          = mapCStringHash,
    .get_key_len   = mapCStringLen,
    .stringify     = mapCStringToString,
    .value_release = NULL,
    .type          = "char *",
};

SetType set_cstring_owned_type = {
    .match         = mapCStringEq,
    .hash          = mapCStringHash,
    .get_key_len   = mapCStringLen,
    .stringify     = mapCStringToString,
    .value_release = free,
    .type          = "char *",
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

u64 roundUpToNextPowerOf2(u64 v) {
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

Map *mapNew(u64 capacity, MapType *type) {
    Map *map = (Map *)malloc(sizeof(Map));
    capacity = roundUpToNextPowerOf2(capacity);
    map->capacity = capacity;
    map->entries = (MapNode *)malloc(sizeof(MapNode) * map->capacity);

    map->mask = map->capacity - 1;
    map->size = 0;
    map->threashold = (unsigned long)(MAP_LOAD * map->capacity);
    memset(map->entries, 1, sizeof(MapNode) * map->capacity);
    map->indexes = vecNew(&vec_unsigned_long_type);
    map->parent = NULL;
    map->type = type;
    map->cached_key = (void *)ULONG_MAX;
    map->cached_idx = ULONG_MAX;
    return map;
}

Map *mapNewWithParent(Map *parent, u64 capacity, MapType *type) {
    Map *map = mapNew(capacity, type);
    map->parent = parent;
    return map;
}

static u64 mapGetNextIdx(Map *map, void *key, int *_is_free) {
    u64 hash = map->type->hash(key);
    u64 idx = hash & map->mask;
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

static u64 mapGetIdx(Map *map, void *key, int *_ok) {
    u64 hash = map->type->hash(key);
    u64 idx = hash & map->mask;
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

static u64 mapGetCStringNextIdx(Map *map,
                                          char *key,
                                          s64 key_len,
                                          int *_is_free)
{
    u64 hash = mapCStringHashLen(key, key_len);
    u64 idx = hash & map->mask;
    MapNode *cur = &map->entries[idx];
    *_is_free = 1;

    /* A free node is one that is either free or previously deleted */
    while (1) {
        if (cur->flags & (MAP_FLAG_FREE | MAP_FLAG_DELETED)) {
            break;
        }
        /* This means we have found a node with the same key we are trying to
         * add*/
        else if (cur->flags & MAP_FLAG_TAKEN &&
                 cur->key_len == key_len &&
                 !memcmp(cur->key, key, key_len))
        {
            *_is_free = 0;
            break;
        }
        idx = (idx + 1) & map->mask;
        cur = &map->entries[idx];
    }

    return idx;
}


static u64 mapGetCStringIdx(Map *map,
                                      char *key,
                                      s64 key_len,
                                      int *_ok)
{
    u64 hash = mapCStringHashLen(key, key_len);
    u64 idx = hash & map->mask;
    MapNode *cur = &map->entries[idx];

    /* We are only interested in TAKEN nodes */
    while (cur->flags & MAP_FLAG_TAKEN) {
        /* We know something is in this position as it is flagged as TAKEN */
        if (cur->key_len == key_len && !memcmp(cur->key, key, key_len)) {
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
    u64 new_capacity = map->capacity << 1;
    u64 new_mask = new_capacity - 1;
    Vec *old_index_entries = map->indexes;
    u64 indexes_size = map->indexes->size;
    MapNode *old_entries = map->entries;

    void **new_indexes = (void **)calloc(indexes_size, sizeof(void *));
    /* OOM */
    if (new_indexes == NULL) {
        return 0;
    }

    MapNode *new_entries = (MapNode *)malloc(new_capacity * sizeof(MapNode));
    /* OOM */
    if (new_entries == NULL) {
        free(new_indexes);
        return 0;
    }

    map->mask = new_mask;
    map->entries = new_entries;
    map->capacity = new_capacity;
    s64 new_size = 0;
    int is_cstring_key = !strncmp(map->type->key_type, str_lit("char *"));

    memset(map->entries, 1, sizeof(MapNode) * map->capacity);

    /* Keeps insertion order, and does not have to go over the capacity of 
     * the hashtable which should in theory be faster, but there are more 
     * array lookups however they should have good spatial locality */
    for (u64 i = 0; i < indexes_size; ++i) {
        u64 idx = (unsigned long)vecGetAt(old_index_entries, i);
        MapNode *old = &old_entries[idx];
        if (old->flags & MAP_FLAG_TAKEN) {
            
            u64 new_idx = 0;
            if (is_cstring_key) {
                new_idx = mapGetCStringNextIdx(map,old->key,old->key_len,&is_free);
            } else {
                new_idx = mapGetNextIdx(map,old->key,&is_free);
            }
            MapNode *new_node = &new_entries[new_idx];
            new_node->key = old->key;
            new_node->value = old->value;
            new_node->key_len = old->key_len;
            new_node->flags = MAP_FLAG_TAKEN;
            new_indexes[new_size] = (void *)new_idx;
            /* keep track of the new size of this hashtable */
            new_size++;
        }
    }

    map->indexes->size = new_size;

    free(old_entries);
    free(map->indexes->entries);
    map->indexes->entries = new_indexes;
    map->indexes->capacity = indexes_size;
    map->indexes->size = new_size;

    map->size = new_size;
    map->threashold = (unsigned long)(new_capacity * MAP_LOAD);
    return 1;
}

int mapAddLen(Map *map, char *key, s64 key_len, void *value) {
    if (map->size >= map->threashold) {
        if (!mapResize(map)) {
            /* This means we have run out of memory */
            return 0;
        }
    }

    int is_free;
    u64 idx = mapGetCStringNextIdx(map, key, key_len, &is_free);
    MapNode *n = &map->entries[idx];

    /* Only if it is not free do we add to the vector... Though this does mean
     * the ordering that we are trying to keep is now messed up... */
    if (is_free) {
        vecPushInt(map->indexes, idx);
        map->size++;
    }

    n->key = key;
    n->value = value;
    n->key_len = key_len;
    n->flags = MAP_FLAG_TAKEN;
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
    u64 idx = mapGetNextIdx(map, key, &is_free);
    MapNode *n = &map->entries[idx];

    /* Only if it is not free do we add to the vector... Though this does mean
     * the ordering that we are trying to keep is now messed up... */
    if (is_free) {
        vecPushInt(map->indexes, idx);
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
        if (!mapResize(map)) {
            /* This means we have run out of memory */
            return 0;
        }
    }

    int is_free = 0;
    u64 idx = mapGetNextIdx(map, key, &is_free);

    /* We only add if it is free otherwise this operation is an error - 
     * in practice this ensures we can't overwrite values. */
    if (is_free) {
        MapNode *n = &map->entries[idx];
        vecPushInt(map->indexes, idx);
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
    u64 idx = mapGetIdx(map, key, &ok);
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
    u64 idx = mapGetIdx(map, key, &ok);
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

void *mapGetAt(Map *map, u64 index) {
    for (u64 i = index; i < (unsigned long)map->indexes->size; ++i) {
        s64 vec_idx = (long)map->indexes->entries[i];
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
        u64 idx = mapGetIdx(map, key, &ok);
        if (ok) {
            /* May as well... at least it means the value is not stale */
            MapNode *n = &map->entries[idx];
            map->cached_key = n->key;
            map->cached_idx = idx;
            return n->value;
        }
    }
    return NULL;
}

void *mapGetLen(Map *map, char *key, s64 len) {
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
        u64 idx = mapGetCStringIdx(map, key, len, &ok);
        if (ok) {
            /* May as well... at least it means the value is not stale */
            MapNode *n = &map->entries[idx];
            map->cached_key = n->key;
            map->cached_idx = idx;
            return n->value;
        }
    }
    return NULL;
}

void mapClear(Map *map) {
    MapIter it;
    mapIterInit(map, &it);
    while (mapIterNext(&it)) {
        MapNode *n = it.node;
        n->flags = MAP_FLAG_FREE;
        if (map->type->value_release) map->type->value_release(n->value);
        if (map->type->key_release) map->type->key_release(n->key);
        n->key = NULL;
        n->value = NULL;
        n->key_len = 0;
    }
    map->size = 0;
    vecClear(map->indexes);
}

void mapMerge(Map *map1, Map *map2) {
    MapIter it;
    mapIterInit(map2, &it);
    while (mapIterNext(&it)) {
        MapNode *n = it.node;
        mapAddOrErr(map1, n->key, n->value);
    }
}

void mapRelease(Map *map) {
    mapClear(map);
    free(map->entries);
    vecRelease(map->indexes);
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
    while (it->vecidx < it->map->indexes->size) {
        u64 idx = (unsigned long)vecGetAt(it->map->indexes, it->vecidx);
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

/*================= Map speciality ===========================================*/
int mapIntKeyMatch(void *a, void *b) {
    return (long)a == (long)b;
}

s64 mapIntKeyLen(void *key) {
    (void)key;
    return 0;
}

/* This does not hash the integer... AND-ing it with the mask seems reasonable 
 * enough. _mostly_ in this application the integers are incrementing 
 * which means they're not going to clash, though will bunch together meaning
 * wasted space */
u64 mapIntKeyHash(void *key) {
    return (unsigned long)(long)key;
}

AoStr *mapIntToString(void *key) {
    return aoStrPrintf("%ld", (long)key);
}

int mapAoStrKeyMatch(void *s1, void *s2) {
    return aoStrEq((AoStr *)s1, (AoStr *)s2);
}

s64 mapAoStrLen(void *s) {
    return (s64 )aoStrGetLen((AoStr *)s);
}

AoStr *mapAoStrToString(void *s) {
    return (AoStr *)s;
}

void mapAoStrRelease(void *s) {
    aoStrRelease((AoStr *)s);
}

u64 mapCStringHashLen(void *str, s64 len) {
    return cstringMurmur(str, len);
}

u64 mapCStringHash(void *str) {
    s64 len = strlen(str);
    return cstringMurmur(str, len);
}

int mapCStringEq(void *s1, void *s2) {
    if (s1 == NULL || s2 == NULL) return 0;
    s64 s1_len = strlen(s1);
    s64 s2_len = strlen(s2);
    return s1_len == s2_len && !memcmp(s1, s2, s1_len);
}

s64 mapCStringLen(void *s) {
    return strlen((char *)s);
}

AoStr *mapCStringToString(void *s) {
    u64 len = strlen(s);
    return aoStrDupRaw(s, len);
}

MapType map_cstring_cstring_type = {
    .match           = mapCStringEq,
    .hash            = mapCStringHash,
    .get_key_len     = mapCStringLen,
    .key_to_string   = mapCStringToString,
    .key_release     = NULL,
    .value_to_string = mapCStringToString,
    .value_release   = NULL,
    .key_type        = "char *",
    .value_type      = "char *",
};

MapType map_cstring_opaque_type = {
    .match           = mapCStringEq,
    .hash            = mapCStringHash,
    .get_key_len     = mapCStringLen,
    .key_to_string   = mapCStringToString,
    .key_release     = NULL,
    .value_to_string = NULL,
    .value_release   = NULL,
    .key_type        = "char *",
    .value_type      = "void *",
};
