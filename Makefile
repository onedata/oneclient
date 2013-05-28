all: compile

## fuse must be instale before compilation
compile:
	mkdir -p bin
	gcc -Wall src/fuse/veilFuse.c -o bin/veilFuse

clean:
	rm -rf bin
