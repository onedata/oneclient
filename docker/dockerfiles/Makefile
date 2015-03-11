.PHONY: all base builder worker bigcouch

all: base builder worker bigcouch

base:
	docker build -t onedata/base:v4 -f base .

builder: base builder
	docker build -t onedata/builder:v5 -f builder .

worker: base worker
	docker build -t onedata/worker:v4 -f worker .

bigcouch: bigcouch
	docker build -t onedata/bigcouch:v1 -f bigcouch .

riak: riak
	docker build -t onedata/riak:v1 -f riak .

