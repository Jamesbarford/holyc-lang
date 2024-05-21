#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "dict.h"
#include "util.h"

#define DictShouldResize(d) ((d)->size >= (d)->threashold)

DictType default_table_type = {
    .freeKey = free,
    .freeValue = NULL,
    .keyCmp = DictStrCmp,
    .hashFunction = DictGenericHashFunction,
};

size_t DictGenericHashFunctionLen(void *key, int len) {
    char *s = (char *)key;
    size_t h = (size_t)*s;
    for (int i = 1; i < len; ++i) {
        h = (h << 5) - h + (unsigned int)s[i];
    }
    return h;
}

size_t DictGenericHashFunction(void *key) {
    char *s = (char *)key;
    size_t h = (size_t)*s;

    if (h) {
        for (++s; *s; ++s) {
            h = (h << 5) - h + (unsigned int)*s;
        }
    }
    return h;
}

int DictStrCmp(void *s1, void *s2) {
    return !strcmp(s1, s2);
}

void DictDefaultInit(Dict *ht) {
    ht->type = &default_table_type;
    ht->capacity = DICT_INTITAL_CAPACITY;
    ht->hash_mask = ht->capacity - 1;
    ht->size = 0;
    ht->body = (DictNode **)calloc(ht->capacity, sizeof(DictNode *));
    ht->threashold = ~~((size_t)(DICT_LOAD * ht->capacity));
}

Dict *DictNewWithParent(Dict *parent) {
    Dict *d = DictNew(&default_table_type);
    d->parent = parent;
    return d;
}

Dict *DictNew(DictType *type) {
    DictNode **body = (DictNode **)calloc(DICT_INTITAL_CAPACITY,
            sizeof(DictNode *));
    Dict *d = malloc(sizeof(Dict));
    if (d == NULL) {
        loggerPanic("Dict creation failed\n");
    }
    d->parent = NULL;
    d->capacity = DICT_INTITAL_CAPACITY;
    d->body = body;
    d->type = type;
    d->hash_mask = d->capacity - 1;
    d->size = 0;
    d->threashold = ~~((size_t)(DICT_LOAD * d->capacity));
    return d;
}

static DictNode *DictNodeNew(void *key, void *value) {
    DictNode *n = (DictNode *)malloc(sizeof(DictNode));
    n->val = value;
    n->key = key;
    n->next = NULL;
    return n;
}

void DictClear(Dict *d) {
    /* Clear the dictionary but don't release it, if size is 0 there is
     * no need to do anything. We leave the capacity at whatever it was 
     * previously. As clearing implies reuse it feels reasonable */
    if (d && d->size) {
        DictNode *next = NULL;
        for (size_t i = 0; i < d->capacity && d->size > 0; ++i) {
            DictNode *n = d->body[i];
            while (n) {
                next = n->next;
                DictFreeKey(d, n->key);
                DictFreeValue(d, n->val);
                free(n);
                d->size--;
                n = next;
            }
        }
    }
}

void DictRelease(Dict *d) {
    if (d) {
        DictClear(d);
        free(d->body);
        free(d);
    }
}

DictNode *DictFindLen(Dict *d, void *key, int len) {
    size_t hash = DictGenericHashFunctionLen(key,len);
    size_t idx = hash & d->hash_mask;
    int key_len = 0;
    DictNode *n;

    for (; d; d = d->parent) {
        n = d->body[idx];
        for (; n != NULL; n = n->next) {
            key_len = strlen((char *)n->key);
            if (key_len == len && !memcmp(key,n->key,len)) {
                return n;
            }
        }
    }

    return NULL;
}

DictNode *DictFind(Dict *d, void *key) {
    size_t hash = DictHashFunction(d, key);
    size_t idx = hash & d->hash_mask;
    DictNode *n = d->body[idx];

    for (; d; d = d->parent) {
        for (; n != NULL; n = n->next) {
            if (DictKeyCmp(key, n->key)) {
                return n;
            }
        }
    }

    return NULL;
}

void *DictGet(Dict *d, void *key) {
    DictNode *needle = DictFind(d, key);
    return needle ? needle->val : NULL;
}

void *DictGetLen(Dict *d, void *key, int len) {
    DictNode *needle = DictFindLen(d, key, len);
    return needle ? needle->val : NULL;
}

static size_t DictGetIndex(Dict *d, void *key, size_t idx, int *ok) {
    DictNode *n = d->body[idx];

    for (; n != NULL; n = n->next) {
        if (DictKeyCmp(key, n->key)) {
            *ok = 0;
            return idx;
        }
    }
    *ok = 1;
    return idx;
}

static void DictResize(Dict *d) {
    size_t new_capacity = d->capacity << 1;
    size_t new_mask = new_capacity - 1;
    size_t new_threashold = ~~((size_t)(DICT_LOAD * new_capacity));
    size_t size = d->size;
    DictNode **new_entries = (DictNode **)calloc(new_capacity,
                                                 sizeof(DictNode *));
    DictNode **old_entries = d->body;

    for (size_t i = 0; i < d->capacity && size > 0; ++i) {
        DictNode *n = old_entries[i];
        DictNode *next = NULL;
        size_t idx = 0;

        while (n) {
            idx = DictHashFunction(d, n->key) & new_mask;
            next = n->next;
            n->next = new_entries[idx];
            new_entries[idx] = n;
            size--;
            n = next;
        }
    }
    d->body = new_entries;
    d->hash_mask = new_mask;
    d->capacity = new_capacity;
    d->threashold = new_threashold;
    free(old_entries);
}

void DictSetOrReplace(Dict *d, void *key, void *value) {
    if (DictShouldResize(d)) {
        DictResize(d);
    }

    size_t hash = DictHashFunction(d, key);
    size_t idx = hash & d->hash_mask;

    DictNode *n = d->body[idx];
    if (n == NULL) {
        DictNode *newnode = DictNodeNew(key, value);
        d->body[idx] = newnode;
        newnode->next = d->body[idx];
    } else {
        /* This could be a collision or we have the exact key and thus are
         * replacing */
        while (n) {
            if (DictKeyCmp(key, n->key)) {
                /* Update the value */
                n->val = value;
                return;
            }
            n = n->next;
        }
        DictNode *newnode = DictNodeNew(key, value);
        newnode->next = d->body[idx];
        d->body[idx] = newnode;
    }
    d->size++;
}

int DictSet(Dict *d, void *key, void *value) {
    if (DictShouldResize(d)) {
        DictResize(d);
    }

    size_t hash = DictHashFunction(d, key);
    size_t idx = hash & d->hash_mask;
    int ok = 0;

    idx = DictGetIndex(d, key, idx, &ok);

    /* Key exists and we do not want to update the value */
    if (ok == 0) {
        return 0;
    }

    DictNode *newnode = DictNodeNew(key, value);
    newnode->next = d->body[idx];
    d->body[idx] = newnode;
    d->size++;
    return 1;
}

int DictDelete(Dict *d, void *key) {
    size_t hash = DictHashFunction(d, key);
    size_t idx = hash & d->hash_mask;
    DictNode *n = d->body[idx];
    DictNode *prev = NULL;

    for (; n != NULL; n = n->next) {
        if (DictKeyCmp(n->key, key)) {
            if (prev) {
                prev->next = n->next;
            } else {
                d->body[idx] = n->next;
            }
            d->size--;
            DictFreeKey(d, n->key);
            DictFreeValue(d, n->val);
            free(n);
            return 1;
        }
        prev = n;
    }
    return 0;
}

void DictPrint(Dict *d, void (*printfn)(void *)) {
    for (size_t i = 0; i < d->capacity; ++i) {
        for (DictNode *dn = d->body[i]; dn; dn = dn->next) {
            if (printfn) {
                printfn(dn->val);
            }
        }
    }
}
