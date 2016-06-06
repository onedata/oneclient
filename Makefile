.PHONY: all
all: test

INSTALL_PREFIX ?= ${HOME}/.local/helpers
BUILD_PROXY_IO ?= On
WITH_COVERAGE  ?= Off

%/CMakeCache.txt: **/CMakeLists.txt test/integration/* test/integration/**/*
	mkdir -p $*
	cd $* && cmake -GNinja -DCMAKE_BUILD_TYPE=$* \
	                       -DCODE_COVERAGE=${WITH_COVERAGE} \
	                       -DCMAKE_INSTALL_PREFIX=${INSTALL_PREFIX} .. \
	                       -DBUILD_PROXY_IO=${BUILD_PROXY_IO} ..
	touch $@

.PHONY: release
release: release/CMakeCache.txt
	cmake --build release --target helpersStatic
	cmake --build release --target helpersShared

.PHONY: debug
debug: debug/CMakeCache.txt
	cmake --build debug --target helpersStatic
	cmake --build debug --target helpersShared

.PHONY: test
test: debug
	cmake --build debug
	cmake --build debug --target test

.PHONY: cunit
cunit: debug
	cmake --build debug
	cmake --build debug --target cunit

.PHONY: install
install: release
	cmake --build release --target install

.PHONY: coverage
coverage:
	lcov --directory debug --capture --output-file helpers.info
	lcov --remove helpers.info 'test/*' '/usr/*' 'asio/*' '**/messages/*' \
	                           'relwithdebinfo/*' 'debug/*' 'release/*' \
	                           'erlang-tls/*' --output-file helpers.info.cleaned
	genhtml -o coverage helpers.info.cleaned
	echo "Coverage written to `pwd`/coverage/index.html"

.PHONY: clean
clean:
	rm -rf debug release
