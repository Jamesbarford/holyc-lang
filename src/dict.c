#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "dict.h"
#include "util.h"

#define dictShouldResize(d) ((d)->size >= (d)->threashold)

DictType default_table_type = {
    .freeKey = free,
    .freeValue = NULL,
    .keyCmp = dictStrCmp,
    .hashFunction = dictGenericHashFunction,
};

size_t dictGenericHashFunctionLen(void *key, int len) {
    char *s = (char *)key;
    size_t h = (size_t)*s;
    for (int i = 1; i < len; ++i) {
        h = (h << 5) - h + (unsigned int)s[i];
    }
    return h;
}

size_t dictGenericHashFunction(void *key) {
    char *s = (char *)key;
    size_t h = (size_t)*s;

    if (h) {
        for (++s; *s; ++s) {
            h = (h << 5) - h + (unsigned int)*s;
        }
    }
    return h;
}

int dictStrCmp(void *s1, void *s2) {
    return !strcmp(s1, s2);
}

void dictDefaultInit(Dict *ht) {
    ht->type = &default_table_type;
    ht->capacity = DICT_INTITAL_CAPACITY;
    ht->hash_mask = ht->capacity - 1;
    ht->size = 0;
    ht->body = (DictNode **)calloc(ht->capacity, sizeof(DictNode *));
    ht->threashold = ~~((size_t)(DICT_LOAD * ht->capacity));
}

Dict *dictNewWithParent(Dict *parent) {
    Dict *d = dictNew(&default_table_type);
    d->parent = parent;
    return d;
}

Dict *dictNew(DictType *type) {
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

static DictNode *dictNodeNew(void *key, void *value) {
    DictNode *n = (DictNode *)malloc(sizeof(DictNode));
    n->val = value;
    n->key = key;
    n->next = NULL;
    return n;
}

void dictRelease(Dict *d) {
    if (d) {
        DictNode *next = NULL;
        for (size_t i = 0; i < d->size; ++i) {
            DictNode *n = d->body[i];
            while (n) {
                next = n->next;
                dictFreeKey(d, n->key);
                DictFreeValue(d, n->val);
                free(n);
                d->size--;
                n = next;
            }
        }
        free(d);
    }
}

DictNode *dictFindLen(Dict *d, void *key, int len) {
    size_t hash = dictGenericHashFunctionLen(key,len);
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

DictNode *dictFind(Dict *d, void *key) {
    size_t hash = dictHashFunction(d, key);
    size_t idx = hash & d->hash_mask;
    DictNode *n = d->body[idx];

    for (; d; d = d->parent) {
        for (; n != NULL; n = n->next) {
            if (dictKeyCmp(key, n->key)) {
                return n;
            }
        }
    }

    return NULL;
}

void *dictGet(Dict *d, void *key) {
    DictNode *needle = dictFind(d, key);
    return needle ? needle->val : NULL;
}

void *dictGetLen(Dict *d, void *key, int len) {
    DictNode *needle = dictFindLen(d, key, len);
    return needle ? needle->val : NULL;
}

static size_t dictGetIndex(Dict *d, void *key, size_t idx, int *ok) {
    DictNode *n = d->body[idx];

    for (; n != NULL; n = n->next) {
        if (dictKeyCmp(key, n->key)) {
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
            idx = dictHashFunction(d, n->key) & new_mask;
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

void dictSetOrReplace(Dict *d, void *key, void *value) {
    if (dictShouldResize(d)) {
        DictResize(d);
    }

    size_t hash = dictHashFunction(d, key);
    size_t idx = hash & d->hash_mask;

    DictNode *n = d->body[idx];
    if (n == NULL) {
        DictNode *newnode = dictNodeNew(key, value);
        d->body[idx] = newnode;
        newnode->next = d->body[idx];
    } else {
        /* This could be a collision or we have the exact key and thus are
         * replacing */
        while (n) {
            if (dictKeyCmp(key, n->key)) {
                /* Update the value */
                n->val = value;
                return;
            }
            n = n->next;
        }
        DictNode *newnode = dictNodeNew(key, value);
        newnode->next = d->body[idx];
        d->body[idx] = newnode;
    }
    d->size++;
}

int dictSet(Dict *d, void *key, void *value) {
    if (dictShouldResize(d)) {
        DictResize(d);
    }

    size_t hash = dictHashFunction(d, key);
    size_t idx = hash & d->hash_mask;
    int ok = 0;

    idx = dictGetIndex(d, key, idx, &ok);

    /* Key exists and we do not want to update the value */
    if (ok == 0) {
        return 0;
    }

    DictNode *newnode = dictNodeNew(key, value);
    newnode->next = d->body[idx];
    d->body[idx] = newnode;
    d->size++;
    return 1;
}

int dictDelete(Dict *d, void *key) {
    size_t hash = dictHashFunction(d, key);
    size_t idx = hash & d->hash_mask;
    DictNode *n = d->body[idx];
    DictNode *prev = NULL;

    for (; n != NULL; n = n->next) {
        if (dictKeyCmp(n->key, key)) {
            if (prev) {
                prev->next = n->next;
            } else {
                d->body[idx] = n->next;
            }
            d->size--;
            dictFreeKey(d, n->key);
            DictFreeValue(d, n->val);
            free(n);
            return 1;
        }
        prev = n;
    }
    return 0;
}

void dictPrint(Dict *d, void (*printfn)(void *)) {
    for (size_t i = 0; i < d->capacity; ++i) {
        for (DictNode *dn = d->body[i]; dn; dn = dn->next) {
            if (printfn) {
                printfn(dn->val);
            }
        }
    }
}
