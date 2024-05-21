#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "list.h"

void ListInit(List *ll) {
    ll->next = ll->prev = ll;
    ll->value = NULL;
}

List *ListNew(void) {
    List *ll = malloc(sizeof(List));
    ll->next = ll->prev = ll;
    ll->value = NULL;
    return ll;
}

int ListEmpty(List *l) {
    if (l == NULL) return 1;
    return l->next == l;
}

void ListAppend(List *head, void *value) {
    List *node = ListNew();
    List *tail = head->prev;
    node->value = value;
    node->prev = tail;
    node->next = head;
    tail->next = node;
    head->prev = node;
}

void ListPrepend(List *head, void *value) {
    List *node = ListNew();
    node->value = value;
    node->prev = head;
    node->next = head->next;
    head->next->prev = node;
    head->next = node;
}

void *ListDeque(void *l) {
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

void ListInsertBefore(List *ll, List *new_node) {
    new_node->next = ll;
    new_node->prev = ll->prev;
    ll->prev->next = new_node;
    ll->prev = new_node;
}

void ListInsertValueBefore(List *ll, void *value) {
    List *prev = ListNew();
    prev->value = value;
    ListInsertBefore(ll,prev);
}

void *ListPop(void *l) {
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

void ListClear(List *ll, void (*freeValue)(void *)) {
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

void ListRelease(List *ll, void (*freeValue)(void *)) {
    if (!ll) return;
    ListClear(ll,freeValue);
    free(ll);
}

int ListCount(List *ll) {
    if (!ll) return 0;
    int count = 0;
    ListForEach(ll) {
        count++;
    }
    return count;
}

/* Merge l2 to the start of l1 and free l2
 * Example
 * l1 = [1,2,3]
 * l2 = [4,5,6]
 * ListMerge(l1,l2);
 * l1 = [4,5,6,1,2,3] (and linked the otherway too)
 * l2 = nullptr
 */
void ListMergePrepend(List *l1, List *l2) {
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
 * ListMerge(l1,l2);
 * l1 = [1,2,3,4,5,6] (and linked the otherway too)
 * l2 = nullptr
 * */
void ListMergeAppend(List *l1, List *l2) {
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

#ifdef LIST_TEST

typedef struct Int {
    int i;
} Int;

Int *IntNew(int i) {
    Int *_in = malloc(sizeof(Int));
    _in->i = i;
    return _in;
}

void ListTestPrint(List *ll) {
    for (List *it = ll->next; it != ll; it = it->next) {
        printf("%d ", ((Int *)it->value)->i);
    }
    printf("\n");
}

void ListTestPrintReverse(List *ll) {
    for (List *it = ll->prev; it != ll; it = it->prev) {
        printf("%d ", ((Int *)it->value)->i);
    }
    printf("\n");
}

int main(void) {
    List *l1 = ListNew();
    List *l2 = ListNew();

    ListAppend(l1,IntNew(1));
    ListAppend(l1,IntNew(2));
    ListAppend(l1,IntNew(3));
    ListTestPrint(l1);

    ListAppend(l2,IntNew(4));
    ListAppend(l2,IntNew(5));
    ListAppend(l2,IntNew(6));
    ListTestPrint(l2);

    ListMergePrepend(l1,l2);
    ListTestPrint(l1);
    ListTestPrintReverse(l1);
}
#endif
