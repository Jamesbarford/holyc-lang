CC     := gcc-15
CFLAGS := -Wall -Wpedantic -O0 -g -DDEBUG
OUT    := .

.PHONY: clean all

default: all
all: test_aarch64 test_x86_64 test_jit

LIB_OBJS = $(OUT)/asm.o $(OUT)/asm_enc.o $(OUT)/asm_jit.o $(OUT)/enc_arm64.o \
		   $(OUT)/enc_x86_64.o $(OUT)/macho.o $(OUT)/target.o \
		   $(OUT)/tasm_util.o $(OUT)/tasm_debug.o $(OUT)/test-helper.o

# Every object depends on every header: cheap at this size, and a
# changed struct layout (e.g. AsmJitCode) must rebuild the test .o's
# too or they smash the stack with the old sizeof.
HDRS := $(wildcard *.h)

$(OUT)/%.o: %.c $(HDRS)
	$(CC) $(CFLAGS) -c $< -o $@

test_aarch64: $(LIB_OBJS) $(OUT)/test_aarch64.o
	$(CC) $(FLAGS) -o $@ $^

test_x86_64: $(LIB_OBJS) $(OUT)/test_x86_64.o
	$(CC) $(FLAGS) -o $@ $^

test_jit: $(LIB_OBJS) $(OUT)/test_jit.o
	$(CC) $(FLAGS) -o $@ $^

# Cross-build the JIT test as an x86_64 binary so we can exercise the
# x86 JIT path under Rosetta on an arm64 host. Doesn't reuse the regular
# .o cache because those are arm64-native; compiles every source in one
# shot for simplicity.
SRC_ALL = asm.c asm_enc.c asm_jit.c enc_arm64.c enc_x86_64.c macho.c \
          target.c tasm_util.c tasm_debug.c test_jit.c test-helper.c
test_jit_x86: $(SRC_ALL)
	$(CC) -arch x86_64 $(CFLAGS) -o $@ $(SRC_ALL)

clean:
	rm -rf *.o
	rm -rf test_aarch64 test_x86_64 test_jit test_jit_x86
