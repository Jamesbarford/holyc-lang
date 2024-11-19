#ifndef LIST_H
#define LIST_H

typedef struct List {
    struct List *next;
    struct List *prev;
    void *value;
} List;

#define listForEach(l) \
    for (List *it = l->next; it != l; it = it->next)

void listInit(List *l);
List *listNew(void);
int listEmpty(List *l);
void listAppend(List *head, void *value);
void listPrepend(List *head, void *value);
void listInsertBefore(List *ll, List *new_node);
void listInsertValueBefore(List *ll, void *value);
void *listDeque(void *l);
void *listHead(List *ll);
void *listPop(void *l);
void listRelease(List *ll, void (*freeValue)(void *));
void listClear(List *ll, void (*freeValue)(void *));
int listCount(List *ll);
void listMergeAppend(List *l1, List *l2);
void listMergePrepend(List *l1, List *l2);
List *listCopy(List *l);

#endif // !LIST_H
