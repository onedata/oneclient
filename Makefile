# distro for package building (oneof: wily, fedora-23-x86_64)
DISTRIBUTION        ?= none
DOCKER_RELEASE      ?= development
DOCKER_REG_NAME     ?= "docker.onedata.org"
DOCKER_REG_USER     ?= ""
DOCKER_REG_PASSWORD ?= ""

PKG_REVISION    ?= $(shell git describe --tags --always)
PKG_VERSION     ?= $(shell git describe --tags --always | tr - .)
PKG_BUILD       ?= 1
PKG_ID           = oneclient-$(PKG_VERSION)

.PHONY: all
all: debug test

.PRECIOUS: %/CMakeCache.txt
%/CMakeCache.txt: **/CMakeLists.txt test/integration/* test/integration/**/* \
                  helpers/test/integration/* helpers/test/integration/**
	mkdir -p $*
	cd $* && cmake -GNinja -DCMAKE_BUILD_TYPE=$* -DCODE_COVERAGE=${WITH_COVERAGE} ..
	touch $@

.PHONY: phony
phony:

%/oneclient: %/CMakeCache.txt phony
	cmake --build $* --target oneclient

.PHONY: deb-info
deb-info: relwithdebinfo/oneclient

.PHONY: release
release: release/oneclient

.PHONY: debug
debug: debug/oneclient

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
	ninja -C release install

.PHONY: docs
docs:
	@doxygen Doxyfile

.PHONY: coverage
coverage:
	lcov --directory `pwd`/debug --capture --output-file `pwd`/oneclient.info
	lcov --remove `pwd`/oneclient.info 'test/*' '/usr/*' 'asio/*' '**/messages/*' 'relwithdebinfo/*' 'debug/*' 'release/*' '**/helpers/*' 'deps/*' --output-file `pwd`/oneclient.info.cleaned
	genhtml -o `pwd`/coverage `pwd`/oneclient.info.cleaned
	@echo "Coverage written to `pwd`/coverage/index.html"

.PHONY: check_distribution
check_distribution:
ifeq ($(DISTRIBUTION), none)
	@echo "Please provide package distribution. Oneof: 'trusty', 'wily', 'xenial', 'centos-7-x86_64', 'fedora-23-x86_64'"
	@exit 1
else
	@echo "Building package for distribution $(DISTRIBUTION)"
endif

package/$(PKG_ID).tar.gz:
	mkdir -p package
	rm -rf package/$(PKG_ID)
	git archive --format=tar --prefix=$(PKG_ID)/ $(PKG_REVISION)| (cd package && tar -xf -)
	find package/$(PKG_ID) -depth -name ".git" -exec rm -rf {} \;
	python cmake_version.py > package/$(PKG_ID)/version.txt
	tar -C package -czf package/$(PKG_ID).tar.gz $(PKG_ID)

.PHONY: deb
deb: check_distribution package/$(PKG_ID).tar.gz
	rm -rf package/packages && mkdir -p package/packages
	mv -f package/$(PKG_ID).tar.gz package/oneclient_$(PKG_VERSION).orig.tar.gz

	cp -R pkg_config/debian package/$(PKG_ID)/
	patch -d package/$(PKG_ID)/ -p1 -i pkg_config/$(DISTRIBUTION).patch
	sed -i "s/oneclient (.*) .*;/oneclient ($(PKG_VERSION)-$(PKG_BUILD)) sid;/g" package/$(PKG_ID)/debian/changelog
	sed -i "s/Build from .*/Build from $(PKG_VERSION)/g" package/$(PKG_ID)/debian/changelog

	cd package/$(PKG_ID) && sg sbuild -c "sbuild -sd $(DISTRIBUTION) -j4"
	mv package/*$(PKG_VERSION).orig.tar.gz package/packages/
	mv package/*$(PKG_VERSION)-$(PKG_BUILD)*.deb package/packages/
	mv package/*$(PKG_VERSION)-$(PKG_BUILD).dsc package/packages/
	mv package/*$(PKG_VERSION)-$(PKG_BUILD)_amd64.changes package/packages/
	-mv package/*$(PKG_VERSION)-$(PKG_BUILD).debian.tar.xz package/packages/
	-mv package/*$(PKG_VERSION)-$(PKG_BUILD)*.ddeb package/packages/
	-mv package/*$(PKG_VERSION)-$(PKG_BUILD)*.diff.gz package/packages/

.PHONY: rpm
rpm: check_distribution package/$(PKG_ID).tar.gz
	rm -rf package/packages && mkdir -p package/packages
	mv -f package/$(PKG_ID).tar.gz package/$(PKG_ID).orig.tar.gz

	cp pkg_config/oneclient.spec package/oneclient.spec
	patch -d package/ -p1 -i $(PKG_ID)/pkg_config/$(DISTRIBUTION).patch
	sed -i "s/{{version}}/$(PKG_VERSION)/g" package/oneclient.spec
	sed -i "s/{{build}}/$(PKG_BUILD)/g" package/oneclient.spec

	mock --root $(DISTRIBUTION) --buildsrpm --spec package/oneclient.spec --resultdir=package/packages \
		--sources package/$(PKG_ID).orig.tar.gz
	mock --root $(DISTRIBUTION) --resultdir=package/packages --rebuild package/packages/$(PKG_ID)*.src.rpm

.PHONY: docker
docker:
	./docker_build.py --repository $(DOCKER_REG_NAME) --user $(DOCKER_REG_USER) \
                          --password $(DOCKER_REG_PASSWORD) --build-arg RELEASE=$(DOCKER_RELEASE) \
                          --build-arg VERSION=$(PKG_VERSION) --name oneclient \
                          --publish --remove docker

.PHONY: clean
clean:
	rm -rf debug release relwithdebinfo doc package
