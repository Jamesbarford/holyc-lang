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
of the major language features.

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
Run `make`, then run `make install` (`sudo make install` on linux) this will 
install the compiler and holyc libraries for strings, hashtables, I/O, maths,
networking, JSON parsing etc... see ./src/holyc-lib/
 
## Using the compiler
Once the compiler has been compiled the following options are available, they 
can be displayed by running `hcc --help`
```
HolyC Compiler 2024. UNSTABLE
hcc [..OPTIONS] <..file>

OPTIONS:
  -ast     Print the ast and exit
  -tokens  Print the tokens and exit
  -S       Emit assembly only
  -obj     Emit an objectfile
  -lib     Emit a dynamic and static library
  -clibs   Link c libraries like: -clibs=`-lSDL2 -lxml2 -lcurl...`
  -o       Output filename: hcc -o <name> ./<file>.HC
  -run     Immediately run the file (not JIT)
  -g       Not implemented
  -D<var>  Set a compiler #define (does not accept a value)
  --help   Print this message
```

## Differences
- `auto` key word for type inference, an addition which makes it easier
  to write code.
- `cast<type>` can be used for casting as well as post-fix type casting.
- `break` and `continue` allowed in loops.
- You can call any libc code by declaring the prototype with 
  `extern "c" <type> <function_name>`. Then call the function as you usually
  would. See [here](https://holyc-lang.com/learn-functions.html) for examples.

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
