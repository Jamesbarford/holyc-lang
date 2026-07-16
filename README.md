# HolyC Programming Language
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

# What's here?
- AOT compiler (uses clang internally to create and link object files)
- JIT compiler - `hcc -jit <files...>`
- [REPL][#repl]
- [LSP][#lsp] and [see here for setup with neovim][#neovim-lsp-example]
- Assembler, used in `asm {}` blocks
- Disassembler
- A myriad of pernicious bugs

## Introduction
A holyc compiler built from scratch in C. Basic optimisations like constant,
folding and some dead code elimination occurs. But broadly it should be easy
to see how the assembly maps to the source code. It works by creating an AST,
lowering to an SSA based IR and emitting x86_64/AArch64 assembly code as text
which is fed into gcc to assemble. Alternatively a JIT can be invoked which
maps machine code to executable memory and then runs it, this is done with a
builtin assembler. Floating point arithmetic is supported as are most of the
major language features. There is experimental support for transpiling HolyC to
C, which is in varying states of broken and not properly supported.

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
Currently this holyc compiler will compile holyc source code to an
x86_64/AArch64 compatible binary which has been tested on amd linux and an
intel mac. An M1 mac and fedora linux on an M4 using QEMU.

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

For an interactive session run `hcc -repl` (requires a JIT-enabled build,
which is the Makefile default). Functions, classes, globals and `#define`s
accumulate as you type, statements execute immediately and the value of a
trailing expression is echoed. Line editing, persistent history
(`~/.hcc_repl_history`) and tab completion over everything you've defined
are built in. `Uf("Name");` disassembles a JIT-compiled function and
`ReplDel("name");` removes a global, function, class, union or `#define`
from the session so the name can be redefined from scratch. An `rc` file
can be placed in `~/.hcc_rc.HC` with any HolyC functions you may want for the
repl. To silence the welcome message use `#define HCC_NO_REPL_HELLO` in the rc
file. Memory in this state is semi-managed in a best effort way to prevent
crashes. The shell can be invoked by calling `Sh(...)` or `@ <shell commands>`.
Where the `@ ` prefix denotes the syntax for invoking your environments shell.

### Linking against shared libraries
The `#link` directive records a shared library dependency in the source
itself, so neither the compile command nor the REPL needs extra flags:

```hc
#link "./mylib.so"   /* literal path, relative to the working directory */
#link <sdl2>         /* library name, like a linker's -lsdl2 */

extern "c" I64 MyLibAdd(I64 a, I64 b);
```

For AOT builds the path form is passed to the linker verbatim and the name
form becomes `-l<name>` (with `-L` for Homebrew/`/usr/local` added when
present). In the JIT and the REPL the library is `dlopen`'d - the name form
probes `lib<name>.{dylib,so}` in the dynamic linker's default paths, the
configured install prefix, `/opt/homebrew/lib`, `/usr/local/lib` and the
system lib dirs. Duplicate `#link`s are ignored, so headers can `#link`
the library they wrap. Note the JIT can only load shared libraries -
`.o` files still have to go on an AOT command line.

### Conditional compilation on the execution mode
`#ifjit` / `#ifaot` select code depending on whether the program is being
JIT-compiled (`hcc -jit`, the REPL) or built ahead-of-time - shorthand for
`#ifdef __HCC_JIT__` / `#ifdef __HCC_AOT__`, exactly one of which is always
defined. They compose with `#else` / `#endif` like any other conditional:

```hc
U8 *Mode()
{
#ifjit
  return "jit";
#else
  return "aot";
#endif
}
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

## LSP
There is an lsp bundled in with the compiler that can be used with `hcc -lsp`.
On it's own it's not particularly useful as it's meant to be used with an
editor.

### neovim lsp example
Copy and paste this and you should have a mostly functioning LSP. Jump to
definition and several other useful things are working but it is not yet as
featureful as say `clangd`.

```lua
-- .HC files get a filetype; nothing else knows about HolyC
vim.filetype.add({ extension = { HH = "holyc", HC = "holyc", hc = "holyc" } })

vim.api.nvim_create_autocmd("FileType", {
  pattern = "holyc",
  callback = function(ev)
    vim.lsp.start({
      name = "hcc",
      cmd = { "hcc", "-lsp" },
      root_dir = vim.fs.root(ev.buf, { ".git" })
                 or vim.fs.dirname(vim.api.nvim_buf_get_name(ev.buf)),
      -- cmd_env = { HCC_LSP_LOG = "1" },  -- uncomment to trace, view with :LspLog
    })
  end,
})

```

## REPL
A repl can be used with `hcc -repl`, much like with bash you can have an rc file
in `~/.hcc_rc.HC` which will be automatically `#include`'d in your session.
To silence the welcome message put `#define HCC_REPL_NO_HELLO` in it. This is
TempleOS-like in so much as it immediately executes HolyC from the commandline.
If you need to use your shell directly you can do `@ <...statements>`, bare in
mind `cd` will not change directory as under the hood it calls `system(...)`
with a raw string. See `./src/repl.c` for some built in functions.

## Key Differences between this and TempleOS HolyC
- `F32` data type, TempleOS only supports `F64` however a lot of C libraries,
  like sdl or raylib require `float`. It's very handy to be able to use these
  libraries from HolyC code. Hence `F32` exists.
- `auto` key word for type inference, an addition which makes it easier
  to write code.
- `typeof(<type or expr>)` folds, at compile time, to a string literal
  naming the type: `typeof(1+1)` is `"I64"`, `typeof(&x)` is `"I64 *"`.
  The operand is only type-checked, never evaluated.
- Range based for loops can be used with static arrays and structs with 
  an `entries` field with an accompanying `size` field: `for (auto it : <var>)`
- You can call any libc code by declaring the prototype with 
  `extern "c" <type> <function_name>`. Then call the function as you usually
  would. See [here](https://holyc-lang.com/docs/language-spec/learn-functions) for examples.
- `#link` for shared objects and libraries, useful to be able to use things like
   curl, psql etc... in the JIT, repl and AOT.

## Experimental Transpiler
A transpiler can be invoked using `hcc -transpile <file>.HC`, it is best effort 
however can handle most cases, including assembly. Comments are not preserved
and some if conditions will require brackets to work correctly

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
- [linenoise](https://github.com/antirez/linenoise)

### Want to ask questions?
Find me on twitch: https://www.twitch.tv/Jamesbarford
