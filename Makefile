C_COMPILER     ?= gcc
BUILD_TYPE     ?= Release
INSTALL_PREFIX ?= /usr/local
CFLAGS         ?= '-Wextra -Wall -Wpedantic'

default: all

.PHONY: all

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

	LIBTOS_CHECK := $(shell $(C_COMPILER) -ltos 2>&1 >/dev/null || echo "missing")

	ifeq ($(LIBTOS_CHECK),missing)
	    $(error Warning: libtos not found, compilation cannot be done. Use the zip file at https://github.com/Jamesbarford/holyc-lang/releases to circumvent the error)
	    HAVE_LIBTOS = 0
	else
		HAVE_LIBTOS = 1
	endif

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

unit-test:
	cd ./src/tests && ../../hcc ./run.HC -o test-runner && ./test-runner && cd ../../

jit-unit-test:
	cd ./src/tests && ../../hcc ./run_jit.HC -o test-runner-jit && ./test-runner-jit && cd ../../

lib-tos:
	cd ./src/holyc-lib \
		&& ../../hcc -lib tos ./all.HC \
		&& cd ../../

clean:
	rm -rf ./build ./hcc
