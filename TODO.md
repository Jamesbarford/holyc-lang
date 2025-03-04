# IR Operations to convert
- A list I think of all of the outstanding language constructs need to 
  be lowered to IR.

/* Loosely these go from most complex to least complex */
# Statements
- [x] While loops
- [x] Loop Break
- [x] Loop Continue 
- [ ] For loops
- [ ] Do While loops
- [x] Return
- [ ] Switch 
- [ ] Goto's

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
- [ ] Logical Not
- [ ] Logical And
- [ ] Logical Or
- [ ] Taking address
- [ ] Array referencing?
- [ ] Struct referencing?
- [ ] Pointer referencing?

# Casting
- [ ] Truncate (larger to smaller)
- [ ] Zero extend (smaller to larger)
- [ ] Sign extend (smaller to larger)
- [-] Floating point truncate
- [-] Floating point extend
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
