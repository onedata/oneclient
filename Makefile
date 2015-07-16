# distro for package building (oneof: sid, fedora-21-x86_64)
DISTRIBUTION    ?= none

PKG_REVISION    ?= $(shell git describe --tags --always)
PKG_VERSION	    ?= $(shell git describe --tags --always| tr - .)
PKG_BUILD       ?= 1
PKG_ID           = oneclient-$(PKG_VERSION)

.PHONY: rpm cmake release debug deb-info test cunit install docs clean all deb coverage
all: deb-info test

cmake: BUILD_DIR = $$(echo $(BUILD_TYPE) | tr '[:upper:]' '[:lower:]')
cmake:
	mkdir -p ${BUILD_DIR}
	cd ${BUILD_DIR} && cmake -GNinja -DCMAKE_BUILD_TYPE=${BUILD_TYPE} .. -DCODE_COVERAGE=${WITH_COVERAGE}

deb-info: BUILD_TYPE = RelWithDebInfo
deb-info: cmake
	cmake --build relwithdebinfo --target oneclient

release: BUILD_TYPE = Release
release: cmake
	cmake --build release --target oneclient

debug: BUILD_TYPE = Debug
debug: cmake
	cmake --build debug --target oneclient

test: debug
	cmake --build debug
	cmake --build debug --target test

cunit: debug
	cmake --build debug
	cmake --build debug --target cunit

install: release
	ninja -C release install

docs:
	@doxygen Doxyfile

coverage:
	lcov --directory debug --capture --output-file oneclient.info
	lcov --remove oneclient.info 'test/*' '/usr/*' 'asio/*' '**/messages/*' 'relwithdebinfo/*' 'debug/*' 'release/*' '**/helpers/*' --output-file oneclient.info.cleaned
	genhtml  -o coverage oneclient.info.cleaned
	@echo "Coverage written to `pwd`/coverage/index.html"

package/$(PKG_ID).tar.gz:
	mkdir -p package
	rm -rf package/$(PKG_ID)
	git archive --format=tar --prefix=$(PKG_ID)/ $(PKG_REVISION)| (cd package && tar -xf -)
	find package/$(PKG_ID) -depth -name ".git" -exec rm -rf {} \;
	tar -C package -czf package/$(PKG_ID).tar.gz $(PKG_ID)

deb: package/$(PKG_ID).tar.gz
	rm -rf package/packages && mkdir -p package/packages
	mv -f package/$(PKG_ID).tar.gz package/oneclient_$(PKG_VERSION).orig.tar.gz
	cp -R pkg_config/debian package/$(PKG_ID)/
	sed -i "s/oneclient (.*) .*;/oneclient ($(PKG_VERSION)-$(PKG_BUILD)) sid;/g" package/$(PKG_ID)/debian/changelog
	sed -i "s/Build from .*/Build from $(PKG_VERSION)/g" package/$(PKG_ID)/debian/changelog

	cd package/$(PKG_ID) && sg sbuild -c "sbuild -sd $(DISTRIBUTION) -j4"
	mv package/*$(PKG_VERSION).orig.tar.gz package/packages/
	mv package/*$(PKG_VERSION)-$(PKG_BUILD).dsc package/packages/
	mv package/*$(PKG_VERSION)-$(PKG_BUILD)_amd64.changes package/packages/
	mv package/*$(PKG_VERSION)-$(PKG_BUILD).debian.tar.xz package/packages/
	mv package/*$(PKG_VERSION)-$(PKG_BUILD)*.deb package/packages/

rpm: package/$(PKG_ID).tar.gz
	rm -rf package/packages && mkdir -p package/packages
	mv -f package/$(PKG_ID).tar.gz package/$(PKG_ID).orig.tar.gz
	cp pkg_config/oneclient.spec package/oneclient.spec
	sed -i "s/{{version}}/$(PKG_VERSION)/g" package/oneclient.spec
	sed -i "s/{{build}}/$(PKG_BUILD)/g" package/oneclient.spec

	mock --root $(DISTRIBUTION) --buildsrpm --spec package/oneclient.spec --resultdir=package/packages \
		--sources package/$(PKG_ID).orig.tar.gz
	mock --root $(DISTRIBUTION)  --resultdir=package/packages --rebuild package/packages/$(PKG_ID)*.src.rpm


clean:
	rm -rf debug release relwithdebinfo doc package

