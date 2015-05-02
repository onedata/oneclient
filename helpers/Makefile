.PHONY: cmake release debug clean test cunit install all
all: release test

cmake: BUILD_DIR = $$(echo $(BUILD_TYPE) | tr '[:upper:]' '[:lower:]')
cmake:
	mkdir -p ${BUILD_DIR}
	cd ${BUILD_DIR} && cmake -GNinja -DCMAKE_BUILD_TYPE=${BUILD_TYPE} ..

release: BUILD_TYPE = Release
release: cmake
	cmake --build release

debug: BUILD_TYPE = Debug
debug: cmake
	cmake --build debug

test: release
	cmake --build release --target test

cunit: release
	cmake --build release --target cunit

install: release
	cmake --build release --target install

clean:
	rm -rf debug release

