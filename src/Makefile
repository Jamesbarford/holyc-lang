TARGET := hcc 
CC     := gcc
CFLAGS := -O0 -ggdb -Wall
OUT    := .

LIB_SRC?=./holyc-lib
PREFIX?=/usr/local
BUILT_IN_LOCATION?=~/.holyc-lib

.PHONY: unit-test

$(OUT)/%.o: %.c
	$(CC) -c $(CFLAGS) -o $@ $<

all: $(TARGET) 

clean:
	rm -rf $(OUT)/*.o $(TARGET) $(TEST_TARGET)

format:
	clang-format -i *.c
	clang-format -i *.h

OBJS = $(OUT)/main.o \
	   $(OUT)/dict.o \
	   $(OUT)/parser.o \
	   $(OUT)/list.o \
	   $(OUT)/ast.o \
	   $(OUT)/prsasm.o \
	   $(OUT)/prsutil.o \
	   $(OUT)/prslib.o \
	   $(OUT)/x86.o \
	   $(OUT)/aostr.o \
	   $(OUT)/cctrl.o \
	   $(OUT)/compile.o \
	   $(OUT)/lexer.o

$(TARGET): $(OBJS)
	$(CC) -o $(TARGET) $(OBJS) -lm

install:
	mkdir -p $(PREFIX)/bin
	install -c -m 555 $(TARGET) $(PREFIX)/bin
	mkdir -p $(BUILT_IN_LOCATION)
	cp $(LIB_SRC)/builtins.HC $(BUILT_IN_LOCATION)/builtins.HC
	cp $(LIB_SRC)/strings.HC $(BUILT_IN_LOCATION)/strings.HC
	cp $(LIB_SRC)/threads.HC $(BUILT_IN_LOCATION)/threads.HC
	cp $(LIB_SRC)/memory.HC $(BUILT_IN_LOCATION)/memory.HC
	cp $(LIB_SRC)/defs.HH $(BUILT_IN_LOCATION)/defs.HH
	cp $(LIB_SRC)/system.HC $(BUILT_IN_LOCATION)/system.HC
	cp $(LIB_SRC)/strlib.HC $(BUILT_IN_LOCATION)/strlib.HC
	cp $(LIB_SRC)/iolib.HC $(BUILT_IN_LOCATION)/iolib.HC
	cp $(LIB_SRC)/csv.HC $(BUILT_IN_LOCATION)/csv.HC
	cp $(LIB_SRC)/date.HC $(BUILT_IN_LOCATION)/date.HC
	cp $(LIB_SRC)/list.HC $(BUILT_IN_LOCATION)/list.HC
	cp $(LIB_SRC)/hashtable.HC $(BUILT_IN_LOCATION)/hashtable.HC
	cp $(LIB_SRC)/io.HC $(BUILT_IN_LOCATION)/io.HC
	cp $(LIB_SRC)/math.HC $(BUILT_IN_LOCATION)/math.HC
	cp $(LIB_SRC)/hc.vim ~/.config/nvim/after/syntax/hc.vim

prsasm-test:
	gcc -O0 -g -DPRSASM_TEST ./prsasm.c ./cctrl.c ./dict.c ./ast.c ./aostr.c ./list.c ./lexer.c ./prsutil.c

unit-test: install
	cd ../tests && ./tests.sh

$(OUT)/dict.o: dict.c dict.h aostr.h 
$(OUT)/list.o: list.c list.h aostr.h 
$(OUT)/main.o: main.c dict.h lexer.h list.h aostr.h 
$(OUT)/lexer.o: lexer.c lexer.h dict.h list.h aostr.h 
$(OUT)/parser.o: parser.c parser.h prsutil.h prslib.h dict.h aostr.h 
$(OUT)/ast.o: ast.c ast.h aostr.h 
$(OUT)/aostr.o: aostr.c aostr.h 
$(OUT)/prsasm.o: prsasm.c prsasm.h aostr.h lexer.h list.h dict.h cctrl.h ast.h
$(OUT)/prsutil.o: prsutil.c prsutil.h aostr.h lexer.h list.h dict.h cctrl.h ast.h
$(OUT)/prslib.o: prslib.c prslib.h aostr.h lexer.h list.h dict.h cctrl.h ast.h
$(OUT)/x86.o: x86.c x86.h aostr.h lexer.h list.h dict.h cctrl.h ast.h
$(OUT)/cctrl.o: cctrl.c cctrl.h dict.h aostr.h 
$(OUT)/compile.o: compile.c dict.h lexer.h list.h aostr.h cctrl.h ast.h parser.h
