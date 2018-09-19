# distro for package building (oneof: wily, fedora-23-x86_64)
DISTRIBUTION        ?= none
DOCKER_RELEASE      ?= development
DOCKER_REG_NAME     ?= "docker.onedata.org"
DOCKER_REG_USER     ?= ""
DOCKER_REG_PASSWORD ?= ""

PKG_REVISION    ?= $(shell git describe --tags --always)
PKG_VERSION     ?= $(shell git describe --tags --always | tr - .)
PKG_COMMIT      ?= $(shell git rev-parse HEAD)
HELPERS_COMMIT  ?= $(shell git -C helpers rev-parse HEAD)
PKG_BUILD       ?= 1
PKG_ID           = oneclient-base-$(PKG_VERSION)

# Build with Ceph storge helper by default
WITH_CEPH         ?= ON
# Build with Swift storage helper by default
WITH_SWIFT        ?= ON
# Build with S3 storage helper by default
WITH_S3           ?= ON
# Build with GlusterFS storage helper by default
WITH_GLUSTERFS    ?= ON

# Oneclient FPM packaging variables
PATCHELF_DOCKER_IMAGE   ?= docker.onedata.org/patchelf:0.9
FPM_DOCKER_IMAGE        ?= docker.onedata.org/fpm:1.9.3
GLUSTERFS_VERSION       ?= 3.12.1
ONECLIENT_FPMPACKAGE_TMP := package_fpm

ifeq ($(strip $(ONECLIENT_BASE_IMAGE)),)
# Oneclient base image is an ID of the Docker container 'oneclient-base' with
# containing Oneclient installed on a reference OS (currently Ubuntu Xenial).
# This image is used to create self-contained binary packages for other distributions.
ONECLIENT_BASE_IMAGE    := ID-$(shell git rev-parse HEAD | cut -c1-10)
endif

.PHONY: all
all: debug test

.PRECIOUS: %/CMakeCache.txt
%/CMakeCache.txt: **/CMakeLists.txt test/integration/* test/integration/**/* \
                  helpers/test/integration/* helpers/test/integration/**
	mkdir -p $*
	cd $* && cmake -GNinja -DCMAKE_BUILD_TYPE=$* \
	                       -DGIT_VERSION=${PKG_REVISION} \
	                       -DGIT_COMMIT=${PKG_COMMIT} \
	                       -DGIT_HELPERS_COMMIT=${HELPERS_COMMIT} \
	                       -DCODE_COVERAGE=${WITH_COVERAGE} \
	                       -DWITH_CEPH=${WITH_CEPH} \
	                       -DWITH_SWIFT=${WITH_SWIFT} \
	                       -DWITH_S3=${WITH_S3} \
	                       -DWITH_OPENSSL=${WITH_OPENSSL} \
	                       -DOPENSSL_ROOT_DIR=${OPENSSL_ROOT_DIR} \
	                       -DOPENSSL_LIBRARIES=${OPENSSL_LIBRARIES} ..
	touch $@

##
## Submodules
##

submodules:
	git submodule sync --recursive ${submodule}
	git submodule update --init --recursive ${submodule}


.PHONY: phony
phony:

%/oneclient: %/CMakeCache.txt phony
	cmake --build $* --target oneclient

%/onebench: %/CMakeCache.txt phony
	cmake --build $* --target onebench

.PHONY: deb-info
deb-info: relwithdebinfo/oneclient

.PHONY: release
release: release/oneclient release/onebench

.PHONY: debug
debug: debug/oneclient release/onebench

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
	lcov --remove `pwd`/oneclient.info 'test/*' '/usr/*' 'asio/*' '**/messages/*' \
	                                   'relwithdebinfo/*' 'debug/*' 'release/*' \
	                                   '**/helpers/*' 'deps/*' \
	     --output-file `pwd`/oneclient.info.cleaned
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

.PHONY: runbenchmark
runbenchmark: release
	cmake --build release --target benchmark

package/$(PKG_ID).tar.gz:
	mkdir -p package
	rm -rf package/$(PKG_ID)
	git archive --format=tar --prefix=$(PKG_ID)/ $(PKG_REVISION) | (cd package && tar -xf -)
	$(eval helpers_sha=$(shell git submodule status --recursive | grep "helpers "| awk '{print $$1;}'))
	cd helpers; git archive --format=tar --prefix=$(PKG_ID)/helpers/ $(helpers_sha) | (cd ../package && tar -xf -); cd ..
	$(eval clproto_sha=$(shell git submodule status --recursive | grep "helpers/clproto" | awk '{print $$1;}'))
	cd helpers/clproto; git archive --format=tar --prefix=$(PKG_ID)/helpers/clproto/ $(clproto_sha) | (cd ../../package && tar -xf -); cd ../..
	find package/$(PKG_ID) -depth -name ".git" -exec rm -rf {} \;
	echo "set(GIT_VERSION ${PKG_REVISION})" > package/$(PKG_ID)/version.txt
	tar -C package -czf package/$(PKG_ID).tar.gz $(PKG_ID)

.PHONY: deb
deb: check_distribution package/$(PKG_ID).tar.gz
	rm -rf package/packages && mkdir -p package/packages
	mv -f package/$(PKG_ID).tar.gz package/oneclient-base_$(PKG_VERSION).orig.tar.gz

	cp -R pkg_config/debian package/$(PKG_ID)/
	patch -d package/$(PKG_ID)/ -p1 -i pkg_config/$(DISTRIBUTION).patch
	sed -i "s/{{version}}/$(PKG_VERSION)/g" package/$(PKG_ID)/debian/changelog
	sed -i "s/{{build}}/$(PKG_BUILD)/g" package/$(PKG_ID)/debian/changelog
	sed -i "s/{{distribution}}/$(DISTRIBUTION)/g" package/$(PKG_ID)/debian/changelog
	sed -i "s/{{date}}/`date -R`/g" package/$(PKG_ID)/debian/changelog

	cd package/$(PKG_ID) && sg sbuild -c "sbuild -sd $(DISTRIBUTION) -j4"
	mv package/*$(PKG_VERSION).orig.tar.gz package/packages/
	mv package/*$(PKG_VERSION)-$(PKG_BUILD)*.deb package/packages/
	mv package/*$(PKG_VERSION)-$(PKG_BUILD).dsc package/packages/
	mv package/*$(PKG_VERSION)-$(PKG_BUILD)_amd64.changes package/packages/
	-mv package/*$(PKG_VERSION)-$(PKG_BUILD).debian.tar.xz package/packages/
	-mv package/*$(PKG_VERSION)-$(PKG_BUILD)*.ddeb package/packages/
	-mv package/*$(PKG_VERSION)-$(PKG_BUILD)*.diff.gz package/packages/ || true

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

##
## Oneclient self contained packages
##
## These targets create self-contained Oneclient packages for various distributions,
## by collecting all necessary dependencies from a reference Oneclient Docker image
## (including glibc and ld-linux) and packages them into /opt/oneclient/... tree.
##

## Determine the RPM dist tag based on distribution name
ifeq ($(strip $(DISTRIBUTION)),centos-6-x86_64)
RPM_DIST     := el6.centos
endif
ifeq ($(strip $(DISTRIBUTION)),centos-7-x86_64)
RPM_DIST     := el7.centos
endif
ifeq ($(strip $(DISTRIBUTION)),fedora-23-x86_64)
RPM_DIST     := f23
endif
ifeq ($(strip $(DISTRIBUTION)),fedora-25-x86_64)
RPM_DIST     := f25
endif
ifeq ($(strip $(DISTRIBUTION)),fedora-26-x86_64)
RPM_DIST     := f26
endif
ifeq ($(strip $(DISTRIBUTION)),scientific-6-x86_64)
RPM_DIST     := el6
endif
ifeq ($(strip $(DISTRIBUTION)),scientific-7-x86_64)
RPM_DIST     := el7
endif

#
# Build Oneclient self-contained tarball
#
oneclient_tar $(ONECLIENT_FPMPACKAGE_TMP)/oneclient-bin.tar.gz:
	# Create directory structure
	mkdir -p $(ONECLIENT_FPMPACKAGE_TMP)/root/bin
	mkdir -p $(ONECLIENT_FPMPACKAGE_TMP)/root/etc
	mkdir -p $(ONECLIENT_FPMPACKAGE_TMP)/root/etc/bash_completion.d
	mkdir -p $(ONECLIENT_FPMPACKAGE_TMP)/root/lib
	mkdir -p $(ONECLIENT_FPMPACKAGE_TMP)/root/lib/x86_64-linux-gnu/glusterfs/$(GLUSTERFS_VERSION)/xlator
	mkdir -p $(ONECLIENT_FPMPACKAGE_TMP)/root/lib/x86_64-linux-gnu/glusterfs/$(GLUSTERFS_VERSION)/rpc-transport
	mkdir -p $(ONECLIENT_FPMPACKAGE_TMP)/root/share/man/man1
	mkdir -p $(ONECLIENT_FPMPACKAGE_TMP)/root/share/man/man5
	mkdir -p $(ONECLIENT_FPMPACKAGE_TMP)/root/share/doc
	mkdir -p $(ONECLIENT_FPMPACKAGE_TMP)/root/share/zsh/site-functions

	# Collect all necessary Oneclient files in one folder
	docker run -v $(CURDIR)/$(ONECLIENT_FPMPACKAGE_TMP)/root:/output \
		-v $(CURDIR)/cpld.sh:/bin/cpld.sh --entrypoint /bin/cpld.sh \
		-t docker.onedata.org/oneclient-base:$(ONECLIENT_BASE_IMAGE) \
		/usr/bin/oneclient /output/lib
	docker run -v $(CURDIR)/$(ONECLIENT_FPMPACKAGE_TMP)/root:/output \
		--entrypoint /bin/bash \
		-t docker.onedata.org/oneclient-base:$(ONECLIENT_BASE_IMAGE) \
		-c 'cp /usr/bin/oneclient /output/bin && \
		    cp /usr/bin/onebench /output/bin && \
		    cp /etc/oneclient.conf /output/etc && \
		    cp /usr/share/man/man1/oneclient.1.gz /output/share/man/man1/ && \
		    cp /usr/share/man/man5/oneclient.conf.5.gz /output/share/man/man5/ && \
		    cp /usr/share/doc/oneclient/* /output/share/doc/ && \
		    cp /var/lib/oneclient/_oneclient /output/share/zsh/site-functions/ && \
		    cp /var/lib/oneclient/oneclient.bash-completion /output/etc/bash_completion.d/ && \
		    cp -r /lib/x86_64-linux-gnu/* /output/lib/ && \
		    cp -r /usr/lib/x86_64-linux-gnu/nss/* /output/lib/ && \
	        cp -r /opt/oneclient/lib/x86_64-linux-gnu/glusterfs/$(GLUSTERFS_VERSION)/xlator/* /output/lib/x86_64-linux-gnu/glusterfs/$(GLUSTERFS_VERSION)/xlator/ && \
	        cp -r /opt/oneclient/lib/x86_64-linux-gnu/glusterfs/$(GLUSTERFS_VERSION)/rpc-transport/* /output/lib/x86_64-linux-gnu/glusterfs/$(GLUSTERFS_VERSION)/rpc-transport/'
	# Collect all dynamic libraries GlusterFS dependencies
	docker run -v $(CURDIR)/$(ONECLIENT_FPMPACKAGE_TMP)/root:/output \
		-v $(CURDIR)/cpld.sh:/bin/cpld.sh --entrypoint /bin/sh \
		-t docker.onedata.org/oneclient-base:$(ONECLIENT_BASE_IMAGE) \
		-c "find /opt/oneclient/lib/x86_64-linux-gnu/glusterfs/$(GLUSTERFS_VERSION)/ -name '*so*' -exec /bin/cpld.sh '{}' /output/lib \;"
	# Change the ld loader and rpath in Oneclient binary
	docker run -v $(CURDIR)/$(ONECLIENT_FPMPACKAGE_TMP)/root:/output \
		-t $(PATCHELF_DOCKER_IMAGE) \
		--set-interpreter /opt/oneclient/lib/ld-linux-x86-64.so.2 \
		--set-rpath /opt/oneclient/lib --force-rpath \
		/output/bin/oneclient
	docker run -v $(CURDIR)/$(ONECLIENT_FPMPACKAGE_TMP)/root:/output \
		-t $(PATCHELF_DOCKER_IMAGE) \
		--set-interpreter /opt/oneclient/lib/ld-linux-x86-64.so.2 \
		--set-rpath /opt/oneclient/lib --force-rpath \
		/output/bin/onebench
	# Create binary archive
	cd $(ONECLIENT_FPMPACKAGE_TMP)/root && \
		tar -cf oneclient-bin.tar * && \
		gzip -f oneclient-bin.tar && \
		mv oneclient-bin.tar.gz ../ && \
		cd ../..
	# Cleanup the temporary files
	docker run -v $(CURDIR)/$(ONECLIENT_FPMPACKAGE_TMP):/output \
		-v $(CURDIR)/cpld.sh:/bin/cpld.sh --entrypoint /bin/sh \
		-t docker.onedata.org/oneclient-base:$(ONECLIENT_BASE_IMAGE) \
		-c "rm -rf /output/root"

#
# Build Oneclient self-contained rpm
#
oneclient_rpm: $(ONECLIENT_FPMPACKAGE_TMP)/oneclient-bin.tar.gz
	# Build RPM package for the distribution specified using FPM
	cp pkg_config/fpm/oneclient_rpm.pre $(ONECLIENT_FPMPACKAGE_TMP)/
	cp pkg_config/fpm/oneclient_rpm.post $(ONECLIENT_FPMPACKAGE_TMP)/
	docker run -u=$$UID:$$GID -v /etc/passwd:/etc/passwd:ro -v /etc/group:/etc/group:ro \
		   -v $(CURDIR)/$(ONECLIENT_FPMPACKAGE_TMP):/data \
		   -t $(FPM_DOCKER_IMAGE) \
		   fpm -t rpm --rpm-dist $(RPM_DIST) -s tar \
		   --prefix=/opt/oneclient -n oneclient -v $(ONECLIENT_VERSION) \
		   --architecture=x86_64 \
		   --iteration $(PKG_BUILD) --license "Apache 2.0" \
		   --after-install=/data/oneclient_rpm.pre \
		   --after-remove=/data/oneclient_rpm.post \
		   --depends fuse --depends ca-certificates \
		   --maintainer "Onedata Package Maintainers <info@onedata.org>" \
		   --description "Self-contained Onedata Oneclient command-line client package" \
		   /data/oneclient-bin.tar.gz

#
# Build Oneclient self-contained deb
#
oneclient_deb: $(ONECLIENT_FPMPACKAGE_TMP)/oneclient-bin.tar.gz
	# Build DEB package for the distribution specified using FPM
	cp pkg_config/fpm/oneclient_deb.pre $(ONECLIENT_FPMPACKAGE_TMP)/
	cp pkg_config/fpm/oneclient_deb.post $(ONECLIENT_FPMPACKAGE_TMP)/
	docker run -u=$$UID:$$GID -v /etc/passwd:/etc/passwd:ro -v /etc/group:/etc/group:ro \
		   -v $(CURDIR)/$(ONECLIENT_FPMPACKAGE_TMP):/data \
		   -t $(FPM_DOCKER_IMAGE) fpm -t deb -s tar \
		   -p /data/oneclient_$(ONECLIENT_VERSION)-$(PKG_BUILD)_amd64.deb \
		   --architecture=amd64 \
		   --prefix=/opt/oneclient -n oneclient -v $(ONECLIENT_VERSION) \
		   --iteration $(PKG_BUILD) --license "Apache 2.0" \
		   --after-install=/data/oneclient_deb.pre \
		   --after-remove=/data/oneclient_deb.post \
		   --depends fuse --depends ca-certificates \
		   --maintainer "Onedata Package Maintainers <info@onedata.org>" \
		   --description "Self-contained Onedata Oneclient command-line client package" \
		   /data/oneclient-bin.tar.gz

.PHONY: docker-base
docker-base:
	./docker_build.py --repository $(DOCKER_REG_NAME) --user $(DOCKER_REG_USER) \
                          --password $(DOCKER_REG_PASSWORD) --build-arg RELEASE=$(DOCKER_RELEASE) \
                          --build-arg VERSION=$(PKG_VERSION) --build-arg ONECLIENT_PACKAGE=oneclient-base \
                          --name oneclient-base --publish --remove docker

.PHONY: docker
docker:
	./docker_build.py --repository $(DOCKER_REG_NAME) --user $(DOCKER_REG_USER) \
                          --password $(DOCKER_REG_PASSWORD) --build-arg RELEASE=$(DOCKER_RELEASE) \
                          --build-arg VERSION=$(PKG_VERSION) --build-arg ONECLIENT_PACKAGE=oneclient \
                          --name oneclient --publish --remove docker

.PHONY: clean
clean:
	rm -rf debug release relwithdebinfo doc package package_fpm

.PHONY: clang-tidy
clang-tidy:
	cmake --build debug --target clang-tidy

.PHONY: clang-format
clang-format:
	docker run --rm -v $(CURDIR):/root/sources onedata/clang-format-check:1.1
