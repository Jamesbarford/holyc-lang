TARGET := hcc 
CC     := gcc
CFLAGS := -O0 -Wall -DDEBUG -g #-fsanitize=address
OUT    := .

LIB_SRC?=./holyc-lib
SYNTAX?=./syntax-highlighting
PREFIX?=/usr/local

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
	   $(OUT)/map.o \
	   $(OUT)/parser.o \
	   $(OUT)/list.o \
	   $(OUT)/ast.o \
	   $(OUT)/prsasm.o \
	   $(OUT)/prsutil.o \
	   $(OUT)/prslib.o \
	   $(OUT)/x86.o \
	   $(OUT)/aostr.o \
	   $(OUT)/cctrl.o \
	   $(OUT)/cfg.o \
	   $(OUT)/cfg-print.o \
	   $(OUT)/compile.o \
	   $(OUT)/memory.o \
	   $(OUT)/lexer.o

$(TARGET): $(OBJS)
	$(CC) -o $(TARGET) $(OBJS) -lm #-fsanitize=address

debug-$(TARGET):
	$(CC) -o $(TARGET) $(OBJS) -lm -fsanitize=address

install-syntax-nvim:
	cp $(SYNTAX)/hc.vim ~/.config/nvim/after/syntax/hc.vim

install:
	install -c -m 555 $(TARGET) $(PREFIX)/bin
	cp $(LIB_SRC)/tos.HH $(PREFIX)/include/tos.HH
	cd ./holyc-lib && hcc -lib tos ./all.HC

prsasm-test:
	gcc -O0 -g -DPRSASM_TEST ./prsasm.c ./cctrl.c ./dict.c ./ast.c ./aostr.c ./list.c ./lexer.c ./prsutil.c

unit-test:
	cd ../tests && hcc -o test-runner ./run.HC && ./test-runner

$(OUT)/dict.o: dict.c dict.h aostr.h 
$(OUT)/map.o: map.c map.h util.h
$(OUT)/memory.o: memory.c memory.h map.h util.h
$(OUT)/list.o: list.c list.h aostr.h 
$(OUT)/main.o: main.c dict.h lexer.h list.h aostr.h 
$(OUT)/lexer.o: lexer.c lexer.h dict.h list.h aostr.h 
$(OUT)/parser.o: parser.c parser.h prsutil.h prslib.h dict.h aostr.h 
$(OUT)/ast.o: ast.c ast.h aostr.h 
$(OUT)/cfg.o: cfg.c ast.h cctrl.h list.h dict.h aostr.h
$(OUT)/cfg-print.o: cfg-print.c cfg-print.h cfg.h ast.h cctrl.h list.h dict.h aostr.h
$(OUT)/aostr.o: aostr.c aostr.h 
$(OUT)/prsasm.o: prsasm.c prsasm.h aostr.h lexer.h list.h dict.h cctrl.h ast.h
$(OUT)/prsutil.o: prsutil.c prsutil.h aostr.h lexer.h list.h dict.h cctrl.h ast.h
$(OUT)/prslib.o: prslib.c prslib.h aostr.h lexer.h list.h dict.h cctrl.h ast.h
$(OUT)/x86.o: x86.c x86.h aostr.h lexer.h list.h dict.h cctrl.h ast.h
$(OUT)/cctrl.o: cctrl.c cctrl.h dict.h aostr.h 
$(OUT)/compile.o: compile.c dict.h lexer.h list.h aostr.h cctrl.h ast.h parser.h
