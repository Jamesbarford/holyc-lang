C_COMPILER     ?= gcc
BUILD_TYPE     ?= Release
INSTALL_PREFIX ?= /usr/local
CFLAGS         ?= '-Wextra -Wall -Wpedantic'

default: all

.PHONY: all unit-test jit-unit-test machine-test lsp-test

# To add sqlite3 support add -DHCC_LINK_SQLITE3=1 to the below like so:
#```
#all:
#	cmake -S ./src -B ./build -G 'Unix Makefiles' \
#		-DCMAKE_C_COMPILER=$(_C_COMPILER) \
#		-DCMAKE_BUILD_TYPE=$(_BUILD_TYPE) \
#		-DHCC_LINK_SQLITE3=1 \
#		&& $(MAKE) -C ./build -j2
#```

all:
	cmake -S ./src \
		-B ./build \
		-G 'Unix Makefiles' \
		-DCMAKE_C_COMPILER=$(C_COMPILER) \
		-DCMAKE_BUILD_TYPE=$(BUILD_TYPE) \
		-DCMAKE_INSTALL_PREFIX=$(INSTALL_PREFIX) \
		-DCMAKE_C_FLAGS=$(CFLAGS) \
		-DCMAKE_EXPORT_COMPILE_COMMANDS=on \
		-DHCC_ENABLE_JIT=on \
		&& $(MAKE) -C ./build -j2

install:
	$(MAKE) -C ./build install

release-unit-test:
	$(MAKE) -C ./build unit-test

# Hermetic, and exits non-zero on a failure. Builds libtos from the tree
# into ./build/test-prefix, so nothing needs installing first.
unit-test:
	./src/tests/run.sh

jit-unit-test:
	./src/tests/run.sh --jit

# The assembler, encoders and machine IR, tested on their own.
machine-test:
	$(MAKE) -C ./src/machine CC=cc
	cd ./src/machine && ./test_aarch64 && ./test_x86_64 && ./test_jit \
		&& ./test_mir_arm64 && ./test_mir_x86_64

# Hermetic: builds libtos from the tree into a local prefix and
# compiles the (HolyC) harness against it, so the suite tests in-tree
# sources - never whatever happens to be installed in /usr/local.
lsp-test:
	mkdir -p ./build/test-prefix/include ./build/test-prefix/lib
	cp ./src/holyc-lib/tos.HH ./build/test-prefix/include/tos.HH
	cd ./src/holyc-lib && ../../hcc -fPIC -lib tos --install-dir=$(CURDIR)/build/test-prefix ./all.HC
	cd ./src/tests/lsp && ../../../hcc --install-dir=$(CURDIR)/build/test-prefix ./run_lsp_tests.HC -o lsp-test-runner && ./lsp-test-runner

lib-tos:
	cd ./src/holyc-lib \
		&& ../../hcc -lib tos ./all.HC \
		&& cd ../../

clean:
	rm -rf ./build ./hcc
