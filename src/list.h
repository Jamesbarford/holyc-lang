#ifndef LIST_H
#define LIST_H

typedef struct List {
    struct List *next;
    struct List *prev;
    void *value;
} List;

#define ListForEach(l) \
    for (List *it = l->next; it != l; it = it->next)

void ListInit(List *l);
List *ListNew(void);
int ListEmpty(List *l);
void ListGenericAppend(void *head, void *next_ptr);
void ListAppend(List *head, void *value);
void ListPrepend(List *head, void *value);
void ListInsertBefore(List *ll, List *new_node);
void ListInsertValueBefore(List *ll, void *value);
void *ListDeque(void *l);
void *ListPop(void *l);
void ListRelease(List *ll, void (*freeValue)(void *));
int ListCount(List *ll);
void ListMergeAppend(List *l1, List *l2);
void ListMergePrepend(List *l1, List *l2);

#endif // !LIST_H
