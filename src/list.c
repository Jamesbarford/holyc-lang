#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "list.h"

void listInit(List *ll) {
    ll->next = ll->prev = ll;
    ll->value = NULL;
}

List *listNew(void) {
    List *ll = (List *)malloc(sizeof(List));
    ll->next = ll->prev = ll;
    ll->value = NULL;
    return ll;
}

int listEmpty(List *l) {
    if (l == NULL) return 1;
    return l->next == l;
}

void listAppend(List *head, void *value) {
    List *node = listNew();
    List *tail = head->prev;
    node->value = value;
    node->prev = tail;
    node->next = head;
    tail->next = node;
    head->prev = node;
}

void listPrepend(List *head, void *value) {
    List *node = listNew();
    node->value = value;
    node->prev = head;
    node->next = head->next;
    head->next->prev = node;
    head->next = node;
}

void *listDeque(void *l) {
    List *ll = (List *)l;
    if (ll->next == ll) {
        return NULL;
    }
    List *node = ll->next;
    void *val = node->value;
    node->next->prev = ll;
    ll->next = ll->next->next;
    free(node);
    return val;
}

void listInsertBefore(List *ll, List *new_node) {
    new_node->next = ll;
    new_node->prev = ll->prev;
    ll->prev->next = new_node;
    ll->prev = new_node;
}

void listInsertValueBefore(List *ll, void *value) {
    List *prev = listNew();
    prev->value = value;
    listInsertBefore(ll,prev);
}

void *listHead(List *ll) {
    return ll->prev->value;
}

void *listPop(void *l) {
    List *ll = (List *)l;
    if (ll->next == ll) {
        return NULL;
    }
    List *node = ll->prev;
    void *val = node->value;
    node->prev->next = ll;
    ll->prev = ll->prev->prev;
    free(node);
    return val;
}

void listClear(List *ll, void (*freeValue)(void *)) {
    if (!ll) return;
    List *node = ll->next;
    List *next;
    while (node != ll) {
        next = node->next;
        if (freeValue) {
            freeValue(node->value);
        }
        free(node);
        node = next;
    }
    ll->next = ll->prev = ll;
}

void listRelease(List *ll, void (*freeValue)(void *)) {
    if (!ll) return;
    listClear(ll,freeValue);
    free(ll);
}

int listCount(List *ll) {
    if (!ll) return 0;
    int count = 0;
    listForEach(ll) {
        count++;
    }
    return count;
}

/* Merge l2 to the start of l1 and free l2
 * Example
 * l1 = [1,2,3]
 * l2 = [4,5,6]
 * listMerge(l1,l2);
 * l1 = [4,5,6,1,2,3] (and linked the otherway too)
 * l2 = nullptr
 */
void listMergePrepend(List *l1, List *l2) {
    List *h1 = l1->next;

    List *h2 = l2->next;
    List *t2 = l2->prev;

    l1->next = h2;
    h2->prev = l1;
    t2->next = h1;
    h1->prev = t2;
    assert(l1->value == NULL);
    free(l2);
}

/* Merge l2 to the end of l1 and free l2.
 * Example
 * l1 = [1,2,3]
 * l2 = [4,5,6]
 * listMerge(l1,l2);
 * l1 = [1,2,3,4,5,6] (and linked the otherway too)
 * l2 = nullptr
 * */
void listMergeAppend(List *l1, List *l2) {
    List *h1 = l1->next;
    List *t1 = l1->prev;

    List *h2 = l2->next;
    List *t2 = l2->prev;

    h2->prev = t1;
    h2->prev->next = h1;
    t1->next = h2;
    t2->next = l1;
    l1->prev = t2;
    assert(l1->value == NULL);
    free(l2);
}

List *listCopy(List *l) {
    List *cpy = listNew();
    listForEach(l) {
        listAppend(cpy,it->value);
    }
    return cpy;
}

#ifdef LIST_TEST

typedef struct Int {
    int i;
} Int;

Int *intNew(int i) {
    Int *_in = (Int *)malloc(sizeof(Int));
    _in->i = i;
    return _in;
}

void listTestPrint(List *ll) {
    for (List *it = ll->next; it != ll; it = it->next) {
        printf("%d ", ((Int *)it->value)->i);
    }
    printf("\n");
}

void listTestPrintReverse(List *ll) {
    for (List *it = ll->prev; it != ll; it = it->prev) {
        printf("%d ", ((Int *)it->value)->i);
    }
    printf("\n");
}

int main(void) {
    List *l1 = listNew();
    List *l2 = listNew();

    listAppend(l1,intNew(1));
    listAppend(l1,intNew(2));
    listAppend(l1,intNew(3));
    listTestPrint(l1);

    listAppend(l2,intNew(4));
    listAppend(l2,intNew(5));
    listAppend(l2,intNew(6));
    listTestPrint(l2);

    listMergePrepend(l1,l2);
    listTestPrint(l1);
    listTestPrintReverse(l1);
}
#endif
