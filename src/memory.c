/**
 * The implementation of the pool below is very simplistic. It can only allocate
 * one type of object as you give it a fixed size when creating it.
 *
 * Free objects are handled by a unique integer id (that increments) being 
 * pushed to a vector.
 */
#include <stdlib.h>

#include "aostr.h"
#include "memory.h"
#include "util.h"

void *xmalloc(size_t size) {
    void *ptr = (void *)malloc(size);
    if (!ptr) {
        loggerPanic("OOM: %s\n", aoStrError()->data);
    }
    return ptr;
}

void xfree(void *ptr) {
    if (ptr) free(ptr);
}
