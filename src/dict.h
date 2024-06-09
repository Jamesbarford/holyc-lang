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

#define dictSetHashFunction(d, fn) ((d)->type->hashFunction = (fn))
#define dictSetFreeKey(d, fn)      ((d)->type->freeKey = (fn))
#define dictSetFreeValue(d, fn)    ((d)->type->freeValue = (fn))
#define dictSetKeyCmp(d, fn)       ((d)->type->keyCmp = (fn))

#define dictHashFunction(d, k) ((d)->type->hashFunction(k))
#define dictFreeKey(d, k)      ((d)->type->freeKey ? d->type->freeKey(k) : (void)k)
#define DictFreeValue(d, k) \
    ((d)->type->freeValue ? d->type->freeValue(k) : (void)k)
#define dictKeyCmp(k1, k2) \
    ((d)->type->keyCmp ? d->type->keyCmp(k1, k2) : (k1 == k2))

void dictDefaultInit(Dict *ht);
Dict *dictNewWithParent(Dict *parent);
Dict *dictNew(DictType *type);
void dictRelease(Dict *d);
DictNode *dictFind(Dict *d, void *key);
void *dictGet(Dict *d, void *key);
size_t dictGenericHashFunction(void *key);
int dictDelete(Dict *d, void *key);
int dictStrCmp(void *s1, void *s2);
void *dictGetLen(Dict *d, void *key, int len);

/* For adding things */
int dictSet(Dict *d, void *key, void *value);
void dictSetOrReplace(Dict *d, void *key, void *value);
void dictPrint(Dict *d, void (*printfn)(void *));

extern DictType default_table_type;

#endif
