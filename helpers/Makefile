.PHONY: cmake release debug clean test cunit install coverage all
all: test

INSTALL_PREFIX ?= ${HOME}/.local/helpers

cmake: BUILD_DIR = $$(echo $(BUILD_TYPE) | tr '[:upper:]' '[:lower:]')
cmake:
	mkdir -p ${BUILD_DIR}
	cd ${BUILD_DIR} && cmake -GNinja -DCMAKE_BUILD_TYPE=${BUILD_TYPE} \
	                                 -DCODE_COVERAGE=${WITH_COVERAGE} \
	                                 -DCMAKE_INSTALL_PREFIX=${INSTALL_PREFIX} \
	                                 -DBUILD_NIF_LIBS=${BUILD_NIF_LIBS} ..

release: BUILD_TYPE = Release
release: cmake
	cmake --build release

debug: BUILD_TYPE = Debug
debug: cmake
	cmake --build debug

test: debug
	cmake --build debug --target test

cunit: debug
	cmake --build debug --target cunit

install: release
	cmake --build release --target install

coverage:
	lcov --directory debug --capture --output-file helpers.info
	lcov --remove helpers.info 'test/*' '/usr/*' 'asio/*' '**/messages/*' 'relwithdebinfo/*' 'debug/*' 'release/*' --output-file helpers.info.cleaned
	genhtml -o coverage helpers.info.cleaned
	echo "Coverage written to `pwd`/coverage/index.html"

clean:
	rm -rf debug release

