SRCS = src/main.c src/utils.c src/arm.c
OBJECTS = $(SRCS:src/%.c=build/%.o)

CHK_SOURCES ?= $(SRCS)
CFLAGS ?= -Wall -Wextra -Wpedantic --std=c23 -g -fPIC -Ideps/ -Isrc/
LDFLAGS ?= -shared

.PHONY: all clean test check-syntax

all: shim.so udc_gba_emu.so

udc_gba_emu.so: $(OBJECTS)
	gcc -o $@ $^ $(LDFLAGS)

shim.so: build/shim.o
	gcc -o $@ $^ $(LDFLAGS)

build:
	mkdir build/

build/%.o: src/%.c | build
	gcc -o $@ -c $< $(CFLAGS)

clean:
	rm udc_gba_emu.so
	rm -r build/

test: udc_gba_emu.so
	emacs --batch --load=$< '--eval=(print (u/gba/c-emulate (vector 1 2 3 4)))'

TAGS: $(SRCS)
	etags $^

check-syntax: TAGS
	gcc $(CFLAGS) -fsyntax-only $(CHK_SOURCES)
