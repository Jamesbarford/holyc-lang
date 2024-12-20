# The HolyC Programming Language - BETA

<p align="center">
  <img 
    src="/assets/holyc-logo.png?raw=true"
    alt="alt text"
    title="holyc logo"
    width="200"
    height="230"/>
</p>

_An implementation of Terry A. Davis's HolyC_

```hc
U0 Main()
{
  "Hello world\n";
}
Main;
```

Full documentation for the language can be found here: https://holyc-lang.com/

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
Thus most `x86_64` architectures should be supported. Creating an `IR` with 
some optimisations and compiling to `ARM` is high on the TODO list.

## Building
### Operating Systems:
*MacOS & Linux:*
You should be able to follow the steps below to build and install the compiler.

*Windows:*
Please install [WSL2](https://learn.microsoft.com/en-us/windows/wsl/install) and 
then follow the steps below:

### Build Steps
There is a Makefile at the root of the repository that wraps CMake, it provides:
- `make`, will build the compiler
- `make install` install the compile
- `make unit-test` run the unit tests

However if you wish to use cmake directly, here's an example:

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
You can set the install prefix with `make INSTALL_PREFIX=<prefix>`

This will install the compiler and holyc libraries for strings, hashtables, 
I/O, maths, networking, JSON parsing etc... see ./src/holyc-lib/.

If you would like to include `sqlite3` then please add `-DHCC_LINK_SQLITE3=1`
to either the Makefile or when configuring cmake.

## Using the compiler
Once the compiler has been compiled the following options are available, they 
can be displayed by running `hcc --help`
```
HolyC Compiler 2024. UNSTABLE
hcc [..OPTIONS] <..file>

OPTIONS:
  -ast       Print the ast and exit
  -cfg       Create graphviz control flow graph as a .dot file
  -cfg-png   Create graphviz control flow graph as a png
  -cfg-svg   Create graphviz control flow graph as a svg
  -tokens    Print the tokens and exit
  -S         Emit assembly only
  -obj       Emit an objectfile
  -lib       Emit a dynamic and static library
  -clibs     Link c libraries like: -clibs=`-lSDL2 -lxml2 -lcurl...`
  -o         Output filename: hcc -o <name> ./<file>.HC
  -o-        Output assembly to stdout, only for use with -S
  -run       Immediately run the file (not JIT)
  -transpile Transpile the code to C, this is best effort
  -g         Not implemented
  -D<var>    Set a compiler #define (does not accept a value)
  --help     Print this message
```

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

## Differences
- `auto` key word for type inference, an addition which makes it easier
  to write code.
- Range based for loops can be used with static arrays and structs with 
  an `entries` field with an accompanying `size` field: `for (auto it : <var>)`
- `cast<type>` can be used for casting as well as post-fix type casting.
- `break` and `continue` allowed in loops.
- You can call any libc code by declaring the prototype with 
  `extern "c" <type> <function_name>`. Then call the function as you usually
  would. See [here](https://holyc-lang.com/learn-functions.html) for examples.

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
This is a non exhaustive list of things that are buggy, if you find something's
please open an issue or open a pull request. I do, however, intend to fix them 
when I get time.
- Using `%f` for string formatting floats not work
- Memory management for the compiler is virtually non-existent, presently all
  the tokens are made before compiling which is very slow.
- Line number in error messages is sometimes off and does not report the file
- Function pointers in a parameter list have to come at the end
- Variable arguments are all passed on the stack
- Casting between `I32` and `I64` is very buggy, the most obvious of which 
  is calling a function which expects `I64` and calling it with an `I32` and
  vice versa, this will often cause a segmentation fault. As such prefer using
  `I64` for integer types.
- The preprocessor for `#define` can presently _only_ accept numerical 
  expressions and strings. It is not like a c compilers preprocessor.

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
