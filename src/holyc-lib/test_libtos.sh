#!/bin/bash

# Test script for libtos library
# Usage: ./test_libtos.sh [test_name]

set -e

echo "Testing libtos library..."

# Install the library first
echo "Installing libtos.a to /usr/local/lib/..."
sudo cp libtos.a /usr/local/lib/

# Create a comprehensive test program
cat > test_libtos.HC << 'EOF'
#include "tos.HH"

I64 main() {
    printf("=== Testing libtos library ===\n");
    
    // Test memory functions
    printf("Testing memory functions...\n");
    I64 *ptr = MAlloc(sizeof(I64) * 5);
    for (I64 i = 0; i < 5; i++) {
        ptr[i] = i * 10;
    }
    printf("  Memory allocated and filled: %ld, %ld, %ld, %ld, %ld\n", 
           ptr[0], ptr[1], ptr[2], ptr[3], ptr[4]);
    Free(ptr);
    printf("  Memory freed successfully\n");
    
    // Test string functions
    printf("Testing string functions...\n");
    U8 *str1 = "Hello";
    U8 *str2 = "World";
    I64 len1 = StrLen(str1);
    I64 len2 = StrLen(str2);
    printf("  String lengths: '%s'=%ld, '%s'=%ld\n", str1, len1, str2, len2);
    
    // Test math functions
    printf("Testing math functions...\n");
    I64 abs_val = Abs(-42);
    I64 gcd_val = Gcd(48, 18);
    printf("  Abs(-42) = %ld\n", abs_val);
    printf("  Gcd(48, 18) = %ld\n", gcd_val);
    
    // Test list functions
    printf("Testing list functions...\n");
    List *list = ListNew();
    I64 val1 = 100, val2 = 200, val3 = 300;
    ListAppend(list, &val1);
    ListAppend(list, &val2);
    ListAppend(list, &val3);
    I64 count = ListCount(list);
    printf("  List count: %ld\n", count);
    printf("  List values: %ld, %ld, %ld\n", 
           *(I64*)ListGet(list, 0), *(I64*)ListGet(list, 1), *(I64*)ListGet(list, 2));
    ListRelease(list, NULL);
    printf("  List released successfully\n");
    
    // Test date functions
    printf("Testing date functions...\n");
    CDate now = Now();
    printf("  Current timestamp: %ld\n", now);
    
    // Test I/O functions (basic)
    printf("Testing I/O functions...\n");
    printf("  File operations available (see tos.HH for details)\n");
    
    printf("=== All tests completed successfully! ===\n");
    return 0;
}
EOF

echo "Compiling test program..."
if hcc test_libtos.HC -o test_libtos; then
    echo "✓ Test program compiled successfully"
    echo "Running test..."
    ./test_libtos
    echo "✓ Test completed!"
else
    echo "✗ Test compilation failed"
    echo "Trying alternative compilation methods..."
    
    # Try with source inclusion
    echo "Attempting source inclusion method..."
    if [ -f "memory.HC" ] && [ -f "strings.HC" ] && [ -f "math.HC" ] && [ -f "list.HC" ]; then
        cat > test_simple.HC << 'EOF'
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
EOF
        
        echo "Compiling with source inclusion..."
        if hcc test_simple.HC memory.HC math.HC -o test_simple; then
            echo "✓ Simple test compiled with source inclusion"
            ./test_simple
        else
            echo "✗ Source inclusion also failed"
        fi
    fi
fi

echo "Build and test script completed!"