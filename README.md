# HolyC Programming Language

> [!PATCH]
> In order to make missing libtos library,When Compiler is built the first time, it gives minimal amount of functionality. After the first build you can run ./build_libtos.sh and copy libtos.a to the /usr/local/bin or custom library directory for all functionality

> [!WARNING]
> The Compiler is in a working state and most features are implemented. Errors are a bit hit and miss! This is more for fun than for serious software development.

<p align="center">
  <img 
    src="/assets/holyc-logo.png?raw=true"
    alt="alt text"
    title="holyc logo"
    width="300"/>
</p>

A compiler for [Terry A. Davis](https://en.wikipedia.org/wiki/Terry_A._Davis)'s HolyC programming language, implemented in C.

```hc
U0 Main()
{
  "Hello world\n";
}
Main;
```

Full documentation for the language and this compiler can be found here: 
https://holyc-lang.com/

## Introduction
A holyc compiler built from scratch in C. Currently it is non optimising,
walking the AST and compiling it directly to x86_64 assembly code as text which 
is fed into gcc to assemble. Floating point arithmetic is supported as are most
of the major language features. There is experimental support for transpiling 
HolyC to C.

## Example
Below is a snippet of code showing some of the features supported by this holyc
compiler. Namely inheritance, loops, `printf` by using a string and loops. All
c-like control flows are supported by the compiler.

```hc
class SomethingWithAnAge
{
  I64 age;
};

class Person : SomethingWithAnAge
{
  U8 name[1<<5];
};

U0 ExampleFunction(U0)
{
  Person *p = MAlloc(sizeof(Person));

  MemCpy(p->name,"Bob",3);
  p->age = 0;

  while (p->age < 42) {
    p->age++;
  }
  "name: %s, age: %d\n",p->name,p->age;
  Free(p);
}

ExampleFunction;
```

## Compatibility
Currently this holyc compiler will compile holyc source code to an x86_64 
compatible binary which has been tested on amd linux and an intel mac.
Thus most x86_64 architectures should be supported.

## Building
### Operating Systems:
*MacOS & Linux:*
You should be able to follow the steps below to build and install the compiler.

*Windows:*
Please install [WSL2](https://learn.microsoft.com/en-us/windows/wsl/install) and 
then follow the steps below:

### Requirements
- A C compiler, gcc or clang.
- Linux or MacOS x86_64.
- make
- cmake

### Build Steps
There is a Makefile at the root of the repository that wraps CMake, it provides:
You can set the install prefix with `make INSTALL_PREFIX=<prefix>`

### Makefile - Simple:
A Makefile exists at the top of the repo which should simplify building.
**MacOS:**
```
make && make install && make unit-test
```

**Linux:**
```
make && sudo make install && make unit-test
```

### CMake - Not so simple
**Create the Makefiles in ./build**
```
cmake -S ./src \
  -B ./build \
  -G 'Unix Makefiles' \
  -DCMAKE_C_COMPILER=gcc \
  -DCMAKE_BUILD_TYPE=Release
```
**Compile**
```
make -C ./build
```
**Install**
```
make -C ./build install
```

This will install the compiler and holyc libraries for strings, hashtables, 
I/O, maths, networking, JSON parsing etc... see ./src/holyc-lib/.

If you would like to include `sqlite3` then please add `-DHCC_LINK_SQLITE3=1`
to either the Makefile or when configuring cmake.

## Using the compiler
Once the compiler has been compiled, aside from compiling `.HC` files, more 
options can be displayed by running `hcc --help`

## Key Differences between this and TempleOS HolyC
- `auto` key word for type inference, an addition which makes it easier
  to write code.
- Range based for loops can be used with static arrays and structs with 
  an `entries` field with an accompanying `size` field: `for (auto it : <var>)`
- `cast<type>` can be used for casting as well as post-fix type casting.
- `break` and `continue` allowed in loops.
- You can call any libc code by declaring the prototype with 
  `extern "c" <type> <function_name>`. Then call the function as you usually
  would. See [here](https://holyc-lang.com/docs/language-spec/learn-functions) for examples.


## Control Flow Graph Example
Example code:
```hc
I32 Main()
{
  auto i = 1;

  for (I64 j = 0; j < 10; ++j) {
    "%d",j;
  }
  while (i) {
    printf("hello");
  }

  return 1;
}
```
Compiled with: `hcc -cfg ./<file>.HC && dot -Tpng ./<file.dot> -o <file>.png`
Produces the following control flow graph. Note that in order to use 
`-cfg-png` or `-cfg-svg` it requires the use of [graphviz](https://graphviz.org/)
<p align="center">
  <img 
    src="/assets/cfg-example.png?raw=true"
    alt="alt text"
    title="holyc logo"
    width="400"/>
</p>

## Experimental Transpiler
A transpiler can be invoked using `hcc -transpile <file>.HC`, it is best effort 
however can handle most cases, including assembly. Comments are not preserved
and some if conditions will require brackets to work correctly

```hc
asm {
_TOINT::
    PUSH    RBP
    MOV     RBP, RSP
    MOV     RAX, 0
    XOR     R8,  R8
    CMPB    [RDI], '-'
    JNE     @@01
    ADD     RDI, 1
    MOV     R8,  1 // mark as being negative
@@01:
    CMPB    [RDI], '0'
    JL      @@02
    CMPB    [RDI], '9'
    JG      @@02
    MOVB    BL, [RDI]
    SUBB    BL, '0'
    MOVZBQ  RBX, BL
    IMUL    RAX, 10
    ADD     RAX, RBX
    ADD     RDI, 1
    JMP     @@01
@@02:
    TEST    R8, R8
    JZ      @@03
    NEG     RAX
@@03:
    LEAVE
    RET
}

public _extern _TOINT I64 ToInt(U8 *str);

U0 Main()
{ /* entry to function */
  U8 *number = "12345";
  auto num = ToInt(number);
  "%ld\n",num;
}
```

Becomes the below:
```c
long
ToInt(unsigned char *str)
{
    long retval;
    __asm__ volatile (
        "mov $0, %%rax\n\t"
        "xor %%r8, %%r8\n\t"
        "cmpb $0x2d, (%%rdi)\n\t"
        "jne ._toint_1\n\t"
        "add $1, %%rdi\n\t"
        "mov $1, %%r8\n\t"
        "._toint_1:\n\t"
        "cmpb $0x30, (%%rdi)\n\t"
        "jl ._toint_2\n\t"
        "cmpb $0x39, (%%rdi)\n\t"
        "jg ._toint_2\n\t"
        "movb (%%rdi), %%bl\n\t"
        "subb $0x30, %%bl\n\t"
        "movzbq %%bl, %%rbx\n\t"
        "imul $10, %%rax\n\t"
        "add %%rbx, %%rax\n\t"
        "add $1, %%rdi\n\t"
        "jmp ._toint_1\n\t"
        "._toint_2:\n\t"
        "test %%r8, %%r8\n\t"
        "jz ._toint_3\n\t"
        "neg %%rax\n\t"
        "._toint_3:\n\t"
        "leave\n\t"
        "ret\n\t"
        : "=a"(retval)
        : "D"(str)
    );
    return retval;
}

int
main(void)
{
    unsigned char *number = "12345";
    long num = ToInt(number);
    printf("%ld\n", num);
}
```

## Bugs
Please open an issue on [github](https://github.com/Jamesbarford/holyc-lang/issues)

## Inspirations & Resources:
A lot of the assembly has been cobbled together by running `gcc -S -O0 <file>`
or `clang -s O0 <file>`. Which has been effective in learning assembly, as 
has playing with TempleOS. The following are a non-exhaustive list of compilers
and resources that I have found particularly useful for learning.
- [TempleOS](https://templeos.org/)
- [8cc](https://github.com/rui314/8cc)
- [tcc](http://bellard.org/tcc/)
- [cc65](https://cc65.github.io/)
- [shecc](https://github.com/sysprog21/shecc/tree/master)
- [JS c compiler](https://github.com/Captainarash/CaptCC)

### Want to ask questions?
Find me on twitch: https://www.twitch.tv/Jamesbarford
