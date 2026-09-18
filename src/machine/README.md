# sagaris

The machine layer: everything between the compiler and the metal, with
no includes back into it.

- `asm.c` / `asm.h` — inline-asm subparser and the operand model
  (`AsmOperand`, `AsmReg`) shared by everything below
- `enc_arm64.c` / `enc_x86_64.c` — instruction encoders
- `dis_arm64.c` / `dis_x86_64.c` — disassemblers
- `mir.c` / `mir_arm64.c` — machine IR: the target-level instruction
  stream the backends select into; printed for `-S`, encoded for the
  JIT and object emission
- `asm_enc.c` / `asm_jit.c` — encoding state, fixups, executable memory
- `elf.c` / `macho.c` / `obj.c` — object file writers
- `target.c` — target triple handling
