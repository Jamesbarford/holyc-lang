#ifndef DICT_H
#define DICT_H

#include <stddef.h>

#define DICT_INTITAL_CAPACITY (1 << 5)
#define DICT_LOAD             (0.75)

/* This is what will get stored in the hash table */
typedef struct DictNode {
    unsigned char *key;
    void *val;
    struct DictNode *next;
} DictNode;

typedef struct DictType {
    void (*freeKey)(void *);
    void (*freeValue)(void *);
    int (*keyCmp)(void *, void *);
    size_t (*hashFunction)(void *);
} DictType;

typedef struct Dict {
    struct Dict *parent;
    long mask;
    long locked_flags;
    size_t size;
    size_t hash_mask;
    size_t threashold;
    size_t capacity;
    DictType *type;
    DictNode **body;
} Dict;

#define DictSetHashFunction(d, fn) ((d)->type->hashFunction = (fn))
#define DictSetFreeKey(d, fn)      ((d)->type->freeKey = (fn))
#define DictSetFreeValue(d, fn)    ((d)->type->freeValue = (fn))
#define DictSetKeyCmp(d, fn)       ((d)->type->keyCmp = (fn))

#define DictHashFunction(d, k) ((d)->type->hashFunction(k))
#define DictFreeKey(d, k)      ((d)->type->freeKey ? d->type->freeKey(k) : (void)k)
#define DictFreeValue(d, k) \
    ((d)->type->freeValue ? d->type->freeValue(k) : (void)k)
#define DictKeyCmp(k1, k2) \
    ((d)->type->keyCmp ? d->type->keyCmp(k1, k2) : (k1 == k2))

void DictDefaultInit(Dict *ht);
Dict *DictNewWithParent(Dict *parent);
Dict *DictNew(DictType *type);
void DictRelease(Dict *d);
void DictClear(Dict *d);
DictNode *DictFind(Dict *d, void *key);
void *DictGet(Dict *d, void *key);
size_t DictGenericHashFunction(void *key);
int DictDelete(Dict *d, void *key);
int DictStrCmp(void *s1, void *s2);
void *DictGetLen(Dict *d, void *key, int len);

/* For adding things */
int DictSet(Dict *d, void *key, void *value);
void DictSetOrReplace(Dict *d, void *key, void *value);
void DictPrint(Dict *d, void (*printfn)(void *));

extern DictType default_table_type;

#endif
