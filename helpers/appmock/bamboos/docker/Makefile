.PHONY: all base builder worker bigcouch

all: base builder worker bigcouch

base: base/Dockerfile
	docker build -t onedata/base:v2 base/

builder: base builder/Dockerfile
	docker build -t onedata/builder:v3 builder/

worker: base worker/Dockerfile
	docker build -t onedata/worker:v2 worker/

bigcouch: bigcouch/Dockerfile
	docker build -t onedata/bigcouch:v1 bigcouch/
