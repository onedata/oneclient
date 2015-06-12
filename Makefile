PKG_REVISION    ?= $(shell git describe --tags --always)
PKG_VERSION	    ?= $(shell git describe --tags --always| tr - .)
PKG_BUILD       ?= 1
PKG_ID           = oneclient-$(PKG_VERSION)

.PHONY: rpm cmake release debug deb-info test cunit install docs clean all deb
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

package/$(PKG_ID).tar.gz:
	mkdir -p package
	rm -rf package/$(PKG_ID)
	git archive --format=tar --prefix=$(PKG_ID)/ $(PKG_REVISION)| (cd package && tar -xf -)
	find package/$(PKG_ID) -depth -name ".git" -exec rm -rf {} \;
	tar -C package -czf package/$(PKG_ID).tar.gz $(PKG_ID)

deb: package/$(PKG_ID).tar.gz
	mv package/$(PKG_ID).tar.gz package/oneclient_$(PKG_VERSION).orig.tar.gz
	cp -R pkg_config/debian package/$(PKG_ID)/
	sed -i "s/oneclient (.*) .*;/oneclient ($(PKG_VERSION)-$(PKG_BUILD)) sid;/g" package/$(PKG_ID)/debian/changelog
	sed -i "s/Build from .*/Build from $(PKG_VERSION)/g" package/$(PKG_ID)/debian/changelog
	./make.py -i onedata/deb_builder --group sbuild --privileged -s package/$(PKG_ID) -d package/$(PKG_ID) -r package \
	-c 'sg sbuild -c "sbuild -sd sid -j4"'

clean:
	rm -rf debug release relwithdebinfo doc package

