# libtos.a - HolyC Standard Library

A comprehensive standard library for the HolyC programming language, compiled from HolyC source files using the HolyC compiler (hcc).

## Overview

`libtos.a` is a static library that provides essential functions for HolyC programs, including memory management, string operations, data structures, mathematical functions, I/O operations, and system utilities. The library contains 168 public functions compiled from the original HolyC source files.

## Library Composition

The library is built from the following HolyC modules:

- **tos.HH** - Complete interface definitions and class declarations
- **memory.HC** - Memory management functions
- **system.HC** - System interaction functions  
- **math.HC** - Mathematical operations
- **date.HC** - Date and time utilities
- **io.HC** - Input/output operations
- **strings.HC** - String manipulation functions
- **list.HC** - Linked list data structure

## Building the Library

### Prerequisites
- HolyC compiler (`hcc`) built and available
- GCC for linking
- Standard C libraries

### Compilation
```bash
cd src/holyc-lib

# Method 1: Standard compilation (requires write permissions to /tmp)
hcc -lib tos libtos_with_tos.HC
cp tos.a libtos.a

# Method 2: If permission denied for temporary files
sudo hcc -lib tos libtos_with_tos.HC
cp tos.a libtos.a

# Method 3: Alternative with explicit temp directory
export TMPDIR=$HOME/tmp
mkdir -p $HOME/tmp
hcc -lib tos libtos_with_tos.HC
cp tos.a libtos.a
```

The compilation process:
1. Compiles HolyC source to assembly
2. Assembles to object file with GCC
3. Creates static library with `ar`

## Library Statistics

- **File size**: ~37KB
- **Public functions**: 168
- **Modules included**: 8 core modules
- **Format**: Static library (.a)

## Available Functions

### Memory Management
- `MAlloc(size)` - Allocate memory
- `Free(ptr)` - Free allocated memory
- `CAlloc(size)` - Allocate zero-initialized memory
- `ReAlloc(ptr, size)` - Reallocate memory
- `MemCpy(dst, src, len)` - Copy memory
- `MemSet(dst, ch, len)` - Set memory

### String Operations
- `StrLen(str)` - Get string length
- `StrCmp(s1, s2)` - Compare strings
- `StrNCmp(s1, s2, len)` - Compare strings with limit
- `StrCpy(dst, src)` - Copy string
- `StrHash(str)` - Hash string
- `ToLower(ch)` - Convert character to lowercase
- `ToUpper(ch)` - Convert character to uppercase
- `IsSpace(ch)` - Check if character is whitespace
- `Atoi(str)` - Convert string to integer
- `I64ToStr(buffer, num)` - Convert integer to string

### List Data Structure
- `ListNew()` - Create new list
- `ListInit(list)` - Initialize list
- `ListAppend(head, value)` - Append to list
- `ListPrepend(head, value)` - Prepend to list
- `ListPop(list)` - Remove and return last element
- `ListDeque(list)` - Remove and return first element
- `ListCount(list)` - Count list elements
- `ListEmpty(list)` - Check if list is empty
- `ListRelease(list, free_fn)` - Free entire list

### Mathematical Functions
- `Abs(num)` - Absolute value
- `Gcd(a, b)` - Greatest common divisor
- Standard math functions (sin, cos, sqrt, etc.)

### Date and Time
- `Now()` - Get current date/time
- `NowMilliseconds()` - Get current time in milliseconds
- `NowDateTimeStruct(ds)` - Fill date/time structure
- `Date2Struct(ds, date)` - Convert date to structure
- Date calculation utilities

### I/O Operations
- `FileRead(path, size_ptr)` - Read entire file
- `FileWrite(filename, buffer, size, flags)` - Write file
- `Read(fd, buffer, len)` - Read from file descriptor
- `Write(fd, buffer, len)` - Write to file descriptor
- `Open(path, flags)` - Open file
- `Close(fd)` - Close file descriptor

### System Functions
- `System(command)` - Execute system command
- `Exit(code)` - Exit program
- Environment and process utilities

## Usage Example

### Simple Program (test_example.HC)
```holyc
#include "./tos.HH"

I64 main() {
    // Memory allocation
    I64 *ptr = MAlloc(sizeof(I64));
    *ptr = 42;
    
    // String operations
    U8 *str = "Hello World";
    I64 len = StrLen(str);
    
    // List operations
    List *list = ListNew();
    ListAppend(list, ptr);
    I64 count = ListCount(list);
    
    // Math operations
    I64 abs_val = Abs(-123);
    I64 gcd_val = Gcd(48, 18);
    
    // Date/time
    CDate now = Now();
    
    // Print results
    printf("String length: %ld\n", len);
    printf("List count: %ld\n", count);
    printf("Absolute value: %ld\n", abs_val);
    printf("GCD: %ld\n", gcd_val);
    
    // Cleanup
    Free(ptr);
    ListRelease(list, NULL);
    
    return 0;
}
```

### Compilation
```bash
# Recommended: Include source files directly
hcc test_example.HC libtos_with_tos.HC -o test_example

# Alternative: Manual linking with library
hcc test_example.HC
gcc /tmp/holyc-asm.s libtos.a -lc -lm -o test_example

# Run the program
./test_example
```

## Compilation and Linking

### Compiling HolyC Programs

#### Method 1: Using Pre-built Library
```bash
# Compile HolyC source
hcc program.HC

# Manual linking with libtos.a
gcc /tmp/holyc-asm.s libtos.a -lc -lm -o program
```

#### Method 2: Including Source Files (Recommended for undefined references)
```bash
# Include libtos source directly in compilation
hcc program.HC libtos_with_tos.HC -o program

# Or include individual modules as needed
hcc program.HC memory.HC strings.HC list.HC -o program

# Manual linking if automatic linking fails
hcc program.HC libtos_with_tos.HC
gcc /tmp/holyc-asm.s -lc -lm -o program
```

#### Method 3: Combined Approach
```bash
# Create a combined source file
cat program.HC libtos_with_tos.HC > combined.HC
hcc combined.HC -o program
```

### With System Installation
If libtos.a is installed in `/usr/local/lib/`:
```bash
hcc program.HC -o program
```

## Testing

A comprehensive test suite is included:

```bash
# Compile test
hcc comprehensive_test.HC
gcc /tmp/holyc-asm.s libtos.a -lc -lm -o test

# Run test
./test
```

Test output demonstrates:
- Memory allocation/deallocation
- String operations
- List manipulations
- Mathematical calculations
- Date/time functions
- String conversions

## Troubleshooting

### Permission Denied Errors

If you encounter "Permission denied" when creating temporary files:

```bash
# Option 1: Run with sudo
sudo hcc -lib tos libtos_with_tos.HC

# Option 2: Set custom temp directory
export TMPDIR=$HOME/tmp
mkdir -p $HOME/tmp
hcc -lib tos libtos_with_tos.HC

# Option 3: Fix /tmp permissions
sudo chmod 1777 /tmp
```

### Compilation Issues

1. **Segmentation fault during compilation**: Try compiling with fewer modules or individually test each module
2. **Undefined references**: Ensure libtos.a is correctly linked with manual gcc command
3. **Missing symbols**: Verify all required HC modules are included in the source file

### Linking Issues

If automatic linking fails or you get undefined references:

#### Solution 1: Include source files directly
```bash
# Include the libtos source in compilation
hcc program.HC libtos_with_tos.HC -o program

# Or include specific modules only
hcc program.HC memory.HC strings.HC list.HC -o program
```

#### Solution 2: Manual linking approach
```bash
# Manual linking with library
hcc program.HC  # This creates /tmp/holyc-asm.s
gcc /tmp/holyc-asm.s libtos.a -lc -lm -o program
```

#### Solution 3: Create combined source file
```bash
# Combine your program with libtos source
echo '#include "./tos.HH"' > combined.HC
cat program.HC >> combined.HC
cat libtos_with_tos.HC >> combined.HC
hcc combined.HC -o program
```

### Common Undefined Reference Issues

If you see errors like:
```
undefined reference to `_MALLOC'
undefined reference to `ListNew'
undefined reference to `_STRLEN_FAST'
```

**Best solution**: Include the source files directly:
```bash
hcc your_program.HC libtos_with_tos.HC -o your_program
```

This ensures all function implementations are available during compilation.

## Library Analysis

### Symbol Inspection
```bash
# View all public functions
nm libtos.a | grep " T "

# Count functions
nm libtos.a | grep " T " | wc -l

# Check library info
file libtos.a
```

### GDB Analysis
```bash
# Load library in GDB
gdb -q
(gdb) file libtos.a
(gdb) info functions
```

## Architecture

The library follows the original TempleOS/HolyC design patterns:

1. **Memory Management**: Direct malloc/free wrappers with HolyC naming
2. **String Operations**: Fast string utilities with length tracking
3. **Data Structures**: Doubly-linked lists with sentinel nodes
4. **Type System**: Uses HolyC types (I64, U64, U8, Bool, etc.)
5. **Function Naming**: Follows HolyC conventions (PascalCase)

## File Structure

```
src/holyc-lib/
├── libtos.a              # Compiled static library
├── libtos_with_tos.HC    # Source file with tos.HH
├── tos.HH                # Complete interface definitions
├── *.HC                  # Individual module implementations
├── comprehensive_test.HC # Test suite
└── test_libtos.HC        # Basic test
```

## Dependencies

### Build Dependencies
- HolyC compiler (hcc)
- GCC compiler
- GNU binutils (ar, nm)

### Runtime Dependencies
- Standard C library (libc)
- Math library (libm)
- POSIX threads (libpthread) for threading functions

## Notes

- The library provides C-compatible function implementations
- Assembly optimizations are preserved from original HolyC implementations
- Thread-safe where applicable
- Memory management follows malloc/free semantics
- File I/O uses standard POSIX interfaces

## Future Enhancements

Potential additions:
- Vector/hashtable modules (vector.HC, hashtable.HC)
- JSON parsing (json.HC) 
- Network utilities (net.HC)
- CSV processing (csv.HC)
- Fuzzy search (fzf.HC)
- Statistical functions (stat.HC)
- Directory operations (dir.HC)

## Contributing

When adding new modules:
1. Update `libtos_with_tos.HC` to include new module
2. Ensure tos.HH contains function declarations
3. Test compilation and functionality
4. Update this documentation

## License

This library is part of the HolyC language implementation. See repository license for details.