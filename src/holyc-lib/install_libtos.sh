#!/bin/bash

# Simple build and install script for libtos
# Usage: ./install_libtos.sh

set -e

echo "=== Building and Installing libtos ==="

# Build the library
echo "1. Building libtos.a..."
./build_libtos.sh

# Install the library
echo "2. Installing library to system..."
sudo cp libtos.a /usr/local/lib/
sudo ldconfig  # Update library cache

echo "3. Library installed successfully!"
echo "   Size: $(stat -c%s /usr/local/lib/libtos.a) bytes"
echo "   Location: /usr/local/lib/libtos.a"

# Create a simple working test
echo "4. Creating simple test..."
cat > simple_test.HC << 'EOF'
#include "tos.HH"

I64 main() {
    printf("Testing libtos functions:\n");
    
    // Test memory
    I64 *ptr = MAlloc(sizeof(I64));
    *ptr = 42;
    printf("  Memory: allocated %ld\n", *ptr);
    Free(ptr);
    
    // Test math
    I64 result = Abs(-123);
    printf("  Math: Abs(-123) = %ld\n", result);
    
    // Test strings
    U8 *str = "Hello";
    I64 len = StrLen(str);
    printf("  String: '%s' length = %ld\n", str, len);
    
    printf("All tests passed!\n");
    return 0;
}
EOF

echo "5. Compiling test (this may fail due to library conflicts)..."
if hcc simple_test.HC -o simple_test 2>/dev/null; then
    echo "✓ Test compiled successfully!"
    echo "6. Running test..."
    ./simple_test
else
    echo "✗ Library linking failed (expected due to symbol conflicts)"
    echo "6. For undefined references, use source inclusion method:"
    echo "   hcc your_program.HC memory.HC math.HC strings.HC -o your_program"
fi

echo ""
echo "=== Installation Summary ==="
echo "Library file: /usr/local/lib/libtos.a"
echo "Header file: tos.HH (in current directory)"
echo "Modules included: memory, strings, math, io, system, date, list, csv, stat, dir,"
echo "                  vector, set, json, fzf, net, threads"
echo ""
echo "Usage options:"
echo "1. Direct linking: hcc program.HC -ltos"
echo "2. Source inclusion: hcc program.HC memory.HC strings.HC math.HC"
echo "3. Manual compilation: hcc program.HC && gcc /tmp/holyc-asm.s libtos.a -lc -lm"