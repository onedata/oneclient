.PHONY: rpm cmake release debug deb-info test cunit install docs clean all
all: deb-info test

cmake: BUILD_DIR = $$(echo $(BUILD_TYPE) | tr '[:upper:]' '[:lower:]')
cmake:
	mkdir -p ${BUILD_DIR}
	cd ${BUILD_DIR} && cmake -GNinja -DCMAKE_BUILD_TYPE=${BUILD_TYPE} ..

deb-info: BUILD_TYPE = RelWithDebInfo
deb-info: cmake
	cmake --build relwithdebinfo --target oneclient

release: BUILD_TYPE = Release
release: cmake
	cmake --build release --target oneclient

debug: BUILD_TYPE = Debug
debug: cmake
	cmake --build debug --target oneclient

test: deb-info
	cmake --build relwithdebinfo
	cmake --build relwithdebinfo --target test

cunit: deb-info
	cmake --build relwithdebinfo
	cmake --build relwithdebinfo --target cunit

install: release
	ninja -C release install

docs:
	@doxygen Doxyfile

clean:
	rm -rf debug release relwithdebinfo doc

