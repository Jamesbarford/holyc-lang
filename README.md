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

```c
"Hello, world!\n";
```

Full documentation for the language can be found here: https://holyc-lang.com/

## Introduction
This compiler is built from scratch in C. Currently it is non optimising,
converting the AST to x86_64 assembly code which is fed into gcc to assemble.

## Compatability
Currently the this will compile x86_64 assembly and works on linux and intel
macs.
This has been tested on an intel mac and linux ubuntu on amd. Most `x86_64`
architectures should be supported. Creating an `IR` and compiling to `ARM`
is high on the TODO list.

## Building
Run `make`, then run `make install` (`sudo make install` on linux) this will 
install the compiler and holyc libraries for strings, hashtables, io, maths 
etc... see ./src/holyc-lib/
 
## Differences
- `auto` key word for type inference, an addition which makes it easier
  to write code.
- `cast<type>` can be used for casting as well as post-fix type casting.
- `break` and `continue` allowed in loops.
- You can call any libc code with `extern "c" <type> <function_name>`

## Bugs
This is a non exhuastive list of things that are buggy, if you find somethings
please open an issue or open a pr.
- using `%f` for string formatting floats not work
- memory management for the compiler is virtually non-existant, presently all
  the tokens are made before compiling which is very slow.
- line number in error messages is sometimes off and does not report the file
- function pointers in a parameter list have to come at the end
- Variable arguments are all passed on the stack

## Inspirations & Resources:
A lot of the assembly has been cobbled together by running `gcc -S -O0 <file>`
or `clang -s O0 <file>`. Which has been effective in learning assembly, as 
has playing with TempleOS. The following are a non-exhaustive list of compilers
and resources that I have found particularly useful.
- [TempleOS](https://templeos.org/)
- [8cc](https://github.com/rui314/8cc)
- [tcc](http://bellard.org/tcc/)
- [cc65](https://cc65.github.io/)
- [shecc](https://github.com/sysprog21/shecc/tree/master)
- [JS c compiler](https://github.com/Captainarash/CaptCC)

### Want to ask questions?
Find me on twitch: https://www.twitch.tv/Jamesbarford
