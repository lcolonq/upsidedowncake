SRCS = src/main.c src/utils.c src/arm.c
OBJECTS = $(SRCS:src/%.c=build/%.o)

CHK_SOURCES ?= $(SRCS)
CPPFLAGS ?= -MMD -MP
CFLAGS ?= -Ideps/ -Isrc/ -Wall -Wextra -Wpedantic -ftrapv --std=c23 -g -fPIC
LDFLAGS ?= -lcapstone -shared -g

.PHONY: all clean test check-syntax

all: shim.so udc_gba_emu.so

udc_gba_emu.so: $(OBJECTS)
	gcc -o $@ $^ $(LDFLAGS)

shim.so: build/shim.o
	gcc -o $@ $^ $(LDFLAGS)

build:
	mkdir build/

build/%.o: src/%.c | build
	gcc $(CPPFLAGS) $(CFLAGS) -o $@ -c $<

clean:
	rm udc_gba_emu.so
	rm -r build/

test: udc_gba_emu.so
	emacs --script test.el

TAGS: $(SRCS)
	etags $^

check-syntax: TAGS
	gcc $(CFLAGS) -fsyntax-only $(CHK_SOURCES)

-include $(OBJECTS:.o=.d)
