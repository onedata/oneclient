.PHONY: cmake release debug clean test cunit install all
all: release test

cmake: BUILD_DIR = $$(echo $(BUILD_TYPE) | tr '[:upper:]' '[:lower:]')
cmake:
	mkdir -p ${BUILD_DIR}
	cd ${BUILD_DIR} && cmake -GNinja -DCMAKE_BUILD_TYPE=${BUILD_TYPE} ..

release: BUILD_TYPE = Release
release: cmake
	ninja -C release

debug: BUILD_TYPE = Debug
debug: cmake
	ninja -C debug

test: release
	ninja -C release test

cunit: release
	ninja -C release cunit

install: release
	ninja -C release install

clean:
	rm -rf debug release
