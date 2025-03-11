#ifndef LIST_H
#define LIST_H

/* @Performance
 * We need a linked list with a bit more umphh, currently splicing the list is 
 * a nightmare */
typedef struct List {
    struct List *next;
    struct List *prev;
    void *value;
} List;

#define listForEach(l) \
    for (List *it = l->next; it != l; it = it->next)

#define listForEachReverse(l) \
    for (List *it = l->prev; it != l; it = it->prev)

#define listValue(type, node) \
    ((type)node->value)

void listInit(List *l);
List *listNew(void);
int listEmpty(List *l);
int listIsOne(List *l);
void listAppend(List *head, void *value);
void listPrepend(List *head, void *value);
void listInsertBefore(List *ll, List *new_node);
void listInsertValueBefore(List *ll, void *value);
void *listDeque(List *ll);
void *listHead(List *ll);
void *listPop(void *l);
List *listSpliceOut(List *list, List *node);
void listUnlink(List *list, List *node);
void listRelease(List *ll, void (*freeValue)(void *));
void listClear(List *ll, void (*freeValue)(void *));
int listCount(List *ll);
void listMergeAppend(List *l1, List *l2);
void listMergePrepend(List *l1, List *l2);
List *listCopy(List *l);
List *listTail(List *ll);

#endif // !LIST_H
