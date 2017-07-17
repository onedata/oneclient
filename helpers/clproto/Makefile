PROTOCERL := ./_build/default/lib/gpb/bin/protoc-erl
ifeq ("$(wildcard $(PROTOCERL))","")
PROTOCERL := ../gpb/bin/protoc-erl
endif

.PHONY: all
all: priv/messages.nif.so src/messages.erl include/messages.hrl

priv/messages.nif.so: | build/messages.nif.so
	mkdir -p priv
	cp build/messages.nif.so priv/

build/messages.nif.so: | build/messages.nif.cc
	mkdir -p build
	cd build && LDFLAGS= CFLAGS= CXXFLAGS= cmake .. -GNinja -Wno-dev \
	    -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_FLAGS="$$ERL_CFLAGS" \
	    -DCMAKE_POSITION_INDEPENDENT_CODE=On -DBUILD_NIF_LIBS=On
	cmake --build build --target messages.nif

src/messages.erl include/messages.hrl build/messages.nif.cc:
	mkdir -p build include
	$(PROTOCERL) -il -strbin -nif -I$(CURDIR)/proto -o-erl src -o-hrl include \
			     -o-nif-cc build $(CURDIR)/proto/messages.proto

.PHONY: clean
clean:
	rm -rf build/ priv/ include/ src/messages.erl

