TARGET         := hcc
LIB_SRC        ?= ./src/holyc-lib
C_COMPILER     ?= gcc
BUILD_TYPE     ?= debug
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
	cmake -S ./src \
		-B ./build \
		-G 'Unix Makefiles' \
		-DCMAKE_C_COMPILER=$(C_COMPILER) \
		-DCMAKE_BUILD_TYPE=$(BUILD_TYPE) \
		-DCMAKE_INSTALL_PREFIX=$(INSTALL_PREFIX) \
		-DCMAKE_C_FLAGS=$(CFLAGS) \
		&& $(MAKE) -C ./build -j2

install:
	install -c -m 555 $(TARGET) $(INSTALL_PREFIX)/bin
	cp $(LIB_SRC)/tos.HH $(INSTALL_PREFIX)/include/tos.HH
	cd $(LIB_SRC) && hcc -lib tos ./all.HC

unit-test:
	$(MAKE) -C ./build unit-test

clean:
	rm -rf ./build ./hcc
