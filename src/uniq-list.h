#ifndef UNIQ_LIST_H__
#define UNIQ_LIST_H__

#include "list.h"
#include "map.h"

typedef int uniqListGetKey(void *value);
typedef void uniqListFreeValue(void *value);
/* Keeping track of two structs in a function can be _really_ bug prone. 
 * so while this is a bit of an overkill in abstraction it is less to mentally
 * keep track of when there is already a lot going on. i.e the Basic block 
 * processing queue */
typedef struct UniqList {
    List *work_queue;
    Set *queued; /* `Set<long>` */
    /* And accessor on `void *` that must return a long so we can keep track
     * of what is queued in the set */
    uniqListGetKey *get_key;
    uniqListFreeValue *free_value; 
} UniqList;

UniqList *uniqListNew(uniqListGetKey *get_key);
int uniqListEmpty(UniqList *uniq_list);
void uniqListAppend(UniqList *uniq_list, void *value);
void *uniqListDequeue(UniqList *uniq_list);
void uniqListRelease(UniqList *uniq_list);

#endif
