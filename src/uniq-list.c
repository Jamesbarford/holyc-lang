#include <stdlib.h>

#include "list.h"
#include "map.h"
#include "uniq-list.h"

UniqList *uniqListNew(uniqListGetKey *get_key) {
    UniqList *uniq_list = (UniqList *)malloc(sizeof(UniqList));
    uniq_list->work_queue = listNew();
    uniq_list->queued = intSetNew(16);
    uniq_list->get_key = get_key;
    uniq_list->free_value = NULL;
    return uniq_list;
}

int uniqListEmpty(UniqList *uniq_list) {
    return listEmpty(uniq_list->work_queue);
}

void uniqListAppend(UniqList *uniq_list, void *value) {
    long id = uniq_list->get_key(value);
    if (!intSetHas(uniq_list->queued, id)) {
        listAppend(uniq_list->work_queue, value);
        intSetAdd(uniq_list->queued, id);
    }
}

/* Remove and item from the start of the queue */
void *uniqListDequeue(UniqList *uniq_list) {
    if (!uniqListEmpty(uniq_list)) {
        void *value = listDeque(uniq_list->work_queue);
        long id = uniq_list->get_key(value);
        intSetRemove(uniq_list->queued, id);
        return value;
    }
    return NULL;
}

void uniqListRelease(UniqList *uniq_list) {
    listRelease(uniq_list->work_queue, uniq_list->free_value);
    intSetRelease(uniq_list->queued);
    free(uniq_list);
}
