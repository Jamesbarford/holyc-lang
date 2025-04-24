#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "aostr.h"
#include "list.h"
#include "util.h"

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

/* List is one element in size */
int listIsOne(List *l) {
    if (listEmpty(l)) return 0;
    return l->next->next == l;
}

void listAppendNode(List *head, List *node) {
    List *tail = head->prev;
    node->prev = tail;
    node->next = head;
    tail->next = node;
    head->prev = node;
}

void listAppend(List *head, void *value) {
    List *node = listNew();    
    node->value = value;
    listAppendNode(head, node);
}

void listPrepend(List *head, void *value) {
    List *node = listNew();
    node->value = value;
    node->prev = head;
    node->next = head->next;
    head->next->prev = node;
    head->next = node;
}

void *listDeque(List *ll) {
    if (listEmpty(ll)) {
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

/* Merge l2 to the start of l1 and free l2. l2 is unusable after this function
 * call DO NOT USE IT
 * Example
 * l1 = [1,2,3]
 * l2 = [4,5,6]
 * listMerge(l1,l2);
 * l1 = [4,5,6,1,2,3] (and linked the otherway too)
 * l2 = nullptr
 */
void listMergePrepend(List *l1, List *l2) {
    if (listEmpty(l2)) return;
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
    if (listEmpty(l2)) return;
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

/* Unlinks the node from the list that it is in*/
void listUnlink(List *list, List *node) {
    /* If the list is empty we can't remove it from itself */
    if (listEmpty(list)) {
        return;
    }
    /* We have at least one other entry in the list so can safely remove 
     * the node */
    List *next = node->next;
    List *prev = node->prev;

    /* Before;
     * Prev2 <-> Prev <-> Node <-> Next <-> Next2
     *
     * After;
     * Prev2 <-> Prev <-> Next <-> Next2
     * */
    next->prev = node->prev;
    prev->next = next;

    //prev->next = next;
    //node->next->prev = node->prev;
    //node->prev->next = node->next;
}

/* Node becomes the new head and everything from the old head to the node 
 * gets deleted; Returns the new spliced list (which depending on what we're 
 * doing we may want to discard)
 *
 * list = [1,2,3,4,5]
 * node = [3]
 * // user frees list
 * spliced = listSpliceOut(list, node)
 * list = [3, 4, 5]
 * spliced = [1, 2]
 * */
List *listSpliceOut(List *list, List *node) {
    List *spliced = listNew();

    /* The "correct" algorithm which is very slow: */
    List *ptr = node;
    while (ptr != list) {
        listAppend(spliced, ptr->value);
        ptr = ptr->next;
    }

    List *node_prev = node->prev;
    node_prev->next = list;
    list->prev = node_prev;
    return spliced;
}

List *listCopy(List *l) {
    List *cpy = listNew();
    listForEach(l) {
        listAppend(cpy,it->value);
    }
    return cpy;
}

List *listTail(List *ll) {
    if (listEmpty(ll)) return NULL;
    return ll->prev;
}

void *listNext(List *ll) {
    if (listEmpty(ll)) return NULL;
    if (ll->next && ll->next->value) {
        return ll->next->value;
    }
    return NULL;
}

void *listPrev(List *ll) {
    if (listEmpty(ll)) return NULL;
    if (ll->prev && ll->prev->value) {
        return ll->prev->value;
    }
    return NULL;
}

AoStr *listToString(List *ll, const char *type, void (*to_string)(AoStr *buf, void *value)) {
    AoStr *buf = aoStrNew();
    if (is_terminal) {
        aoStrCatFmt(buf, ESC_GREEN"List"ESC_RESET"<"ESC_CYAN"%s"ESC_RESET">", type);
    } else {
        aoStrCatFmt(buf, "List<%s>", type);
    }
    aoStrCatLen(buf, str_lit(" ["));
    listForEach(ll) {
        to_string(buf, it->value);
        if (ll != it->next) {
            aoStrCatLen(buf, str_lit(", "));
        }
    }
    aoStrPutChar(buf, ']');
    return buf;
}

void listPrint(List *ll, const char *type, void (*to_string)(AoStr *buf, void *value)) {
    AoStr *list_str = listToString(ll, type, to_string);
    printf("%s\n",list_str->data);
    aoStrRelease(list_str);
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

void intFree(void *_int) {
    free((Int *)_int);
}

void listTestPrint(List *ll) {
    printf("[");
    listForEach(ll) {
        printf("%d", ((Int *)it->value)->i);
        if (it->next != ll) {
            printf(", ");
        }
    }
    printf("]\n");
}

void listTestPrintReverse(List *ll) {
    printf("[");
    listForEachReverse(ll) {
        printf("%d", ((Int *)it->value)->i);
        if (it->prev != ll) {
            printf(", ");
        }
    }
    printf("]\n");
}

int main(void) {
    List *l1 = listNew();
    List *l2 = listNew();

    printf("TEST: forward `listTestPrint(...)`\n");
    listAppend(l1,intNew(1));
    listAppend(l1,intNew(2));
    listAppend(l1,intNew(3));
    listTestPrint(l1);
    printf("=====================\n");

    printf("TEST: reverse `listTestPrint(...)`\n");
    listAppend(l2,intNew(4));
    listAppend(l2,intNew(5));
    listAppend(l2,intNew(6));
    listTestPrint(l2);
    printf("=====================\n");

    printf("TEST: `listMergePrepend(...)`\n");
    listMergePrepend(l1,l2);
    listTestPrint(l1);
    listTestPrintReverse(l1);
    printf("=====================\n");

    printf("TEST: `listRelease(...)`\n");
    listRelease(l1, &intFree);
    printf("=====================\n");

    printf("TEST: `listSpliceOut(...)`\n");
    l1 = listNew();
    listAppend(l1,intNew(1));
    listAppend(l1,intNew(2));
    listAppend(l1,intNew(3));
    listAppend(l1,intNew(4));
    listAppend(l1,intNew(5));
    /*
     * l1 = [1, 2, 3, 4, 5]
     *
     * We want to 'splice' the list so it becomes; 
     *
     * [1, 2]
     *
     * [3, 4, 5]
     * */
    List *list_node = l1->next->next->next;
    l2 = listSpliceOut(l1, list_node);
    listTestPrint(l1);
    listTestPrintReverse(l1);

    listTestPrint(l2);
    listTestPrintReverse(l2);

    printf("=====================\n");
}
#endif
