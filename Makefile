.PHONY: rpm cmake release debug deb-info test cunit install docs clean all
all: rpm

rpm: release
	@cd release && cpack -C CPackConfig.cmake -G RPM
	@cd release && cpack -C CPackConfig.cmake -G DEB

	@echo
	@echo
	@echo "Following packages are now available in PROJECT_DIR directory: "
	@echo
	@for pkg in `ls release/*.rpm 2>/dev/null`; do echo $$pkg; done
	@for pkg in `ls release/*.deb 2>/dev/null`; do echo $$pkg; done

cmake: BUILD_DIR = $$(echo $(BUILD_TYPE) | tr '[:upper:]' '[:lower:]')
cmake:
	mkdir -p ${BUILD_DIR}
	cd ${BUILD_DIR} && cmake -GNinja -DCMAKE_BUILD_TYPE=${BUILD_TYPE} ..

deb-info: BUILD_TYPE = RelWithDebInfo
deb-info: cmake
	ninja -C relwithdebinfo oneclient

release: BUILD_TYPE = Release
release: cmake
	ninja -C release oneclient

debug: BUILD_TYPE = Debug
debug: cmake
	ninja -C debug oneclient

test: deb-info
	ninja -C relwithdebinfo
	ninja -C relwithdebinfo test

cunit: deb-info
	ninja -C relwithdebinfo
	ninja -C relwithdebinfo cunit

install: release
	ninja -C release install

docs:
	@doxygen Doxyfile

clean:
	rm -rf debug release relwithdebinfo build doc
