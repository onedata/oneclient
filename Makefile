.PHONY: deps

all: deps compile

deps:
	git submodule init
	git submodule update

## Fuse must be installed before compilation.
compile:
	mkdir -p bin
	gcc -Wall src/fuse/veilFuse.c `pkg-config fuse --cflags --libs` -o bin/veilFuse

clean:
	rm -rf bin
