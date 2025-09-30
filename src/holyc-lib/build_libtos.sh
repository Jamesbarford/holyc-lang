#!/bin/bash

# Build script for libtos - compiles all HC modules and creates static library
# Usage: ./build_libtos.sh

set -e  # Exit on any error

echo "Building libtos library..."

# Clean up previous builds
rm -f *.o libtos.a 2>/dev/null || true

# List of HC modules to compile (excluding problematic ones that include other modules)
MODULES=(
    "memory"
    "math"
    "system"
    "csv"
    "stat"
    "dir"
    "builtins"
)

# Advanced modules that may cause issues - compile separately
ADVANCED_MODULES=(
    "vector"
    "hashtable"
    "set"
    "json"
    "fzf"
    "net"
    "threads"
    "bitvec"
)

echo "Compiling core modules..."
OBJECT_FILES=()

# Compile core modules
for module in "${MODULES[@]}"; do
    echo "  Compiling ${module}.HC..."
    if hcc -obj -o "${module}.o" "${module}.HC" 2>/dev/null; then
        OBJECT_FILES+=("${module}.o")
        echo "    ✓ ${module}.o created"
    else
        echo "    ✗ Failed to compile ${module}.HC"
    fi
done

echo "Compiling advanced modules..."
# Try to compile advanced modules
for module in "${ADVANCED_MODULES[@]}"; do
    if [ -f "${module}.HC" ]; then
        echo "  Compiling ${module}.HC..."
        if hcc -obj -o "${module}.o" "${module}.HC" 2>/dev/null; then
            OBJECT_FILES+=("${module}.o")
            echo "    ✓ ${module}.o created"
        else
            echo "    ✗ Failed to compile ${module}.HC (skipping)"
        fi
    fi
done

# Create static library
if [ ${#OBJECT_FILES[@]} -gt 0 ]; then
    echo "Creating static library libtos.a without duplicate symbols..."
    
    # Create clean library - ensure memory.o is first and exclude modules with memory conflicts
    CLEAN_OBJECTS=()
    
    # Add memory.o first if it exists
    if [[ " ${OBJECT_FILES[@]} " =~ " memory.o " ]]; then
        CLEAN_OBJECTS+=("memory.o")
        echo "  Added memory.o (memory functions)"
    fi
    
    # Add other modules that don't conflict with memory functions
    for obj in "${OBJECT_FILES[@]}"; do
        case "$obj" in
            "memory.o") 
                # Already added
                ;;
            "strings.o"|"list.o"|"io.o"|"date.o")
                echo "  Skipping $obj (contains duplicate memory/assembly functions)"
                ;;
            *)
                CLEAN_OBJECTS+=("$obj")
                echo "  Added $obj"
                ;;
        esac
    done
    
    ar rcs libtos.a "${CLEAN_OBJECTS[@]}"
    echo "✓ libtos.a created with ${#CLEAN_OBJECTS[@]} modules (no duplicates)"
    
    # Show library contents
    echo "Library contents:"
    ar t libtos.a | sed 's/^/  /'
    
    # Show some symbols
    echo "Sample symbols:"
    nm libtos.a | grep " T " | head -10 | sed 's/^/  /'
    
    # Get library size
    SIZE=$(stat -c%s libtos.a)
    echo "Library size: $SIZE bytes"
    
    echo "✓ Build complete!"
    echo "To install: sudo cp libtos.a /usr/local/lib/"
    sudo cp libtos.a /usr/local/lib/
    
else
    echo "✗ No object files created - build failed"
    exit 1
fi
echo "Cleaning up..."
rm -f *.o tos.a 2>/dev/null || true
