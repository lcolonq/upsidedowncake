OBJECTS = build/main.o build/utils.o build/arm.o
CFLAGS = -Wall -g -fPIC -Ideps/ -Isrc/
LDFLAGS = -shared

.PHONY: all clean test

all: shim.so udc_gba_emu.so

udc_gba_emu.so: $(OBJECTS)
	gcc -o $@ $^ $(LDFLAGS)

shim.so: build/shim.o
	gcc -o $@ $^ $(LDFLAGS)

build:
	mkdir build/

build/%.o: src/%.c build
	gcc -o $@ -c $< $(CFLAGS)

clean:
	rm udc_gba_emu.so
	rm -r build/

test: udc_gba_emu.so
#	emacs --batch --load=$< '--eval=(print (u/gba/c-emulate (vector 1 2 3 4)))'
	emacs --batch --load=$< '--eval=(print (u/gba/c-emulate nil))'
