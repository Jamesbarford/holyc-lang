#include "tos.HH"

I64 main() {
    printf("Simple test with source inclusion\n");
    I64 *ptr = MAlloc(sizeof(I64));
    *ptr = 42;
    printf("Allocated and set value: %ld\n", *ptr);
    Free(ptr);
    printf("Memory freed successfully\n");
    return 0;
}
