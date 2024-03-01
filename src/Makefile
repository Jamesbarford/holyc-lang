TARGET := hcc 
CC     := gcc
CFLAGS := -O0 -ggdb -Wall -DDEBUG
OUT    := .

LIB_SRC?=./holyc-lib
SYNTAX?=./syntax-highlighting
PREFIX?=/usr/local
BUILT_IN_LOCATION?=$(HOME)/.holyc-lib

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

install-syntax-nvim:
	cp $(SYNTAX)/hc.vim ~/.config/nvim/after/syntax/hc.vim

install:
	install -c -m 555 $(TARGET) $(PREFIX)/bin
	mkdir -p $(BUILT_IN_LOCATION)
	cp $(LIB_SRC)/tos.HH $(BUILT_IN_LOCATION)/tos.HH
	cd ./holyc-lib && hcc -lib tos ./all.HC

prsasm-test:
	gcc -O0 -g -DPRSASM_TEST ./prsasm.c ./cctrl.c ./dict.c ./ast.c ./aostr.c ./list.c ./lexer.c ./prsutil.c

unit-test:
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
