# IR Operations to convert
- A list I think of all of the outstanding language constructs need to 
  be lowered to IR.

/* Loosely these go from most complex to least complex */
# Statements
- [x] While loops
- [x] Loop Break
- [x] Loop Continue 
- [x] For loops
- [x] Do While loops
- [x] Return
- [x] Switch 
- [x] Goto's, we need to add place holders, store them in a
      vector and then index the instructions vector to remove them.

# Expressions
- [x] Pre Increment*
- [x] Pre Decrement
- [x] Post Increment
- [x] Post Decrement
- [x] +=
- [x] -=
- [x] %=
- [x] \*=
- [x] /=
- [x] &=
- [x] |=
- [x] <<=
- [x] >>=
- [x] Left Shift
- [x] Right Shift
- [x] Bitwise AND
- [x] Bitwise OR
- [x] Bitwise NOT
- [x] Bitwise XOR
- [x] Logical Not
- [x] Logical And
- [x] Logical Or
- [x] Taking address
- [a] Array referencing?
- [ ] Struct referencing?
- [ ] Pointer referencing?

# Casting
- [ ] Truncate (larger to smaller)
- [ ] Zero extend (smaller to larger)
- [ ] Sign extend (smaller to larger)
- [-] Floating point truncate, may not need
- [-] Floating point extend, may not need
- [ ] Floating point -> unsigned int
- [ ] Floating point -> signed int
- [ ] Floating point -> int
- [ ] Pointer -> int
- [ ] int -> Pointer
- [ ] Bitcast -> Reinterpret as a different type.

# Functions
- [ ] Assembly
- [ ] Variable arguments
- [ ] Default arguments

# Ir
- [x] Removed un-needed jumps
- [x] Link to the return node if possible, this also requires tracking the
      last instruction of a block to see if it is a jump or a branch and
      if the target or fallback points to a block that got removed. That could
      be the heuristic to determine that it should point at the return block.
