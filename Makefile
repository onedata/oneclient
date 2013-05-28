all: compile

compile:
	gcc -Wall src/fuse/veilFuse.c `pkg-config fuse --cflags --libs` -o bin/veilFuse

clean:
	rm -rf bin/*
