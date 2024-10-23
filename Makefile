_C_COMPILER := gcc
_BUILD_TYPE := Release

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
	cmake -S ./src -B ./build -G 'Unix Makefiles' \
		-DCMAKE_C_COMPILER=$(_C_COMPILER) \
		-DCMAKE_BUILD_TYPE=$(_BUILD_TYPE) \
		&& $(MAKE) -C ./build -j2

install:
	$(MAKE) -C ./build install

unit-test:
	$(MAKE) -C ./build unit-test

clean:
	rm -rf ./build ./hcc
