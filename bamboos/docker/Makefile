.PHONY: all builder worker bigcouch

all: builder worker bigcouch

builder: builder/Dockerfile
	docker build -t onedata/builder builder/

worker: worker/Dockerfile
	docker build -t onedata/worker worker/

bigcouch: bigcouch/Dockerfile
	docker build -t onedata/bigcouch bigcouch/