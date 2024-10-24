# distro for package building (oneof: xenial, centos-7-x86_64)
RELEASE               ?= 2102
DISTRIBUTION          ?= none

PKG_REVISION    ?= $(shell git describe --tags --always  --abbrev=7)
PKG_VERSION     ?= $(shell git describe --tags --always  --abbrev=7 | tr - .)
PKG_COMMIT      ?= $(shell git rev-parse HEAD)
HELPERS_COMMIT  ?= $(shell git -C helpers rev-parse HEAD)
PKG_BUILD       ?= 1
PKG_ID           = oneclient-base-$(PKG_VERSION)

FSONEDATAFS_VERSION ?= $(PKG_VERSION)

# Build with Ceph storge helper by default
WITH_CEPH         ?= ON
# Build with Swift storage helper by default
WITH_SWIFT        ?= ON
# Build with S3 storage helper by default
WITH_S3           ?= ON
# Build with GlusterFS storage helper by default
WITH_GLUSTERFS    ?= ON
# Build with WebDAV storage helper by default
WITH_WEBDAV       ?= ON
# Build with XRootD storage helper by default
WITH_XROOTD       ?= ON
# Build with NFS storage helper by default
WITH_NFS          ?= ON
# Build with onedatafs Python library
WITH_ONEDATAFS    ?= ON
# Build with ones3
WITH_ONES3        ?= ON
# Build with code coverage
WITH_COVERAGE     ?= ON
# Set Fuse version (2 or 3)
WITH_FUSE_VERSION ?= 3

# Oneclient FPM packaging variables
PATCHELF_DOCKER_IMAGE   ?= docker.onedata.org/patchelf:0.9
FPM_DOCKER_IMAGE        ?= docker.onedata.org/fpm:1.9.3
GLUSTERFS_VERSION       ?= 7.2
ONECLIENT_FPMPACKAGE_TMP := package_fpm

# Detect compilation on CentOS using Software Collections environment
ifeq ($(shell awk -F= '/^ID=/{print $$2}' /etc/os-release), "centos")
		OPENSSL_ROOT_DIR ?= /opt/onedata/onedata2102/root/usr
		TBB_INSTALL_DIR ?= /opt/onedata/onedata2102/root/usr
endif

.PHONY: all
all: debug test

.PRECIOUS: %/CMakeCache.txt
%/CMakeCache.txt: **/CMakeLists.txt test/integration/* test/integration/**/* \
                  helpers/test/integration/* helpers/test/integration/**
	mkdir -p $*
	cd $* && cmake -GNinja -DCMAKE_BUILD_TYPE=$* \
	                       -DFETCH_FOLLY=${FETCH_FOLLY} \
	                       -DGIT_VERSION=${PKG_REVISION} \
	                       -DGIT_COMMIT=${PKG_COMMIT} \
	                       -DGIT_HELPERS_COMMIT=${HELPERS_COMMIT} \
	                       -DCODE_COVERAGE=${WITH_COVERAGE} \
	                       -DWITH_CEPH=${WITH_CEPH} \
	                       -DWITH_SWIFT=${WITH_SWIFT} \
	                       -DWITH_S3=${WITH_S3} \
	                       -DWITH_GLUSTERFS=${WITH_GLUSTERFS} \
	                       -DWITH_WEBDAV=${WITH_WEBDAV} \
	                       -DWITH_XROOTD=${WITH_XROOTD} \
	                       -DWITH_NFS=${WITH_NFS} \
	                       -DWITH_ONEDATAFS=${WITH_ONEDATAFS} \
	                       -DWITH_ONES3=${WITH_ONES3} \
	                       -DWITH_FUSE_VERSION=${WITH_FUSE_VERSION} \
	                       -DCMAKE_INSTALL_PREFIX=${PWD}/debug/PREFIX \
	                       -DTBB_INSTALL_DIR=${TBB_INSTALL_DIR} \
	                       -DOPENSSL_ROOT_DIR=${OPENSSL_ROOT_DIR} ..
	touch $@

%/build-native:
	mkdir -p _build
	cd _build && cmake -DCMAKE_BUILD_TYPE=$* \
			-DFETCH_FOLLY=${FETCH_FOLLY} \
			-DGIT_VERSION=${PKG_VERSION} \
			-DCODE_COVERAGE=OFF \
			-DWITH_CEPH=${WITH_CEPH} \
			-DWITH_SWIFT=${WITH_SWIFT} \
			-DWITH_S3=${WITH_S3} \
			-DWITH_GLUSTERFS=${WITH_GLUSTERFS} \
			-DWITH_WEBDAV=${WITH_WEBDAV} \
			-DWITH_XROOTD=${WITH_XROOTD} \
			-DWITH_NFS=${WITH_NFS} \
			-DWITH_ONECLIENT=ON \
			-DWITH_ONEBENCH=ON \
			-DWITH_ONES3=${WITH_ONES3} \
			-DWITH_ONEDATAFS=OFF \
			-DWITH_FUSE_VERSION=${WITH_FUSE_VERSION} \
			-DFOLLY_SHARED=ON \
			-DWITH_LIBDL=ON \
			-DWITH_LIBRT=ON \
			-DWITH_TESTS=OFF \
			-DBoost_NO_BOOST_CMAKE=ON \
			-DCMAKE_INSTALL_PREFIX=${PREFIX} \
			.. && \
	make -j8 oneclient VERBOSE=1 && \
	make -j8 onebench VERBOSE=1

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

%/ones3: %/CMakeCache.txt phony
	cmake --build $* --target ones3

%/onebench: %/CMakeCache.txt phony
	cmake --build $* --target onebench

%/onedatafs-py2: %/CMakeCache.txt phony
	cmake --build $* --target onedatafs.py2

%/onedatafs-py3: %/CMakeCache.txt phony
	cmake --build $* --target onedatafs.py3

.PHONY: deb-info
deb-info: relwithdebinfo/oneclient

.PHONY: release
release: release/oneclient release/ones3 release/onebench release/onedatafs-py3

.PHONY: debug
debug: debug/oneclient debug/ones3 debug/onebench debug/onedatafs-py3

.PHONY: test
test: debug
ifdef ONEDATA_GIT_URL
	git config --global url."${ONEDATA_GIT_URL}".insteadOf "ssh://git@git.onedata.org:7999/vfs"
endif
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

#
# Coverage tests are collected from 2 types of tests:
# - Unit tests
# - Integration tests
#
# Unit test coverage results are managed by CMake and are collected
# in ./coverage/cunit/ subdirectory and should be generated after
# running cunit target.
#
# Integration tests coverage results have to be collected after
# each integration test (e.g. fslogic_test) and are stored in
# ./coverage/integration/TEST_NAME.
# A combined report containing the sum of coverage from individual
# integration tests can be generated using coverage_integration target
# and the result will be stored in ./coverage/integration/combined
#
.PHONY: coverage
coverage_cunit coverage:
	lcov --quiet --base-directory `pwd`/debug --directory `pwd`/debug --capture --output-file `pwd`/oneclient.info
	lcov --quiet --base-directory `pwd`/debug --remove `pwd`/oneclient.info 'test/*' '/usr/*' 'asio/*' '**/messages/*' \
	                                   'relwithdebinfo/*' 'debug/*' 'release/*' \
	                                   '**/helpers/*' 'deps/*' \
	     --output-file `pwd`/oneclient_cunit.info.cleaned
	find `pwd`/debug/ -name "*.gcda" -type f -delete
	genhtml -o `pwd`/coverage/cunit `pwd`/oneclient_cunit.info.cleaned
	@echo "Coverage written to `pwd`/coverage/cunit/index.html"

.PHONY: coverage/%
coverage/%:
	lcov --quiet --base-directory `pwd`/debug --directory `pwd`/debug --capture --output-file `pwd`/oneclient.info
	lcov --quiet --base-directory `pwd`/debug --remove `pwd`/oneclient.info 'test/*' '/usr/*' 'asio/*' '**/messages/*' \
	                                   'relwithdebinfo/*' 'debug/*' 'release/*' \
	                                   '**/helpers/*' 'deps/*' \
	     --output-file `pwd`/oneclient_integration_$*.info.cleaned
	find `pwd`/debug/ -name "*.gcda" -type f -delete
	genhtml -o `pwd`/coverage/integration/$* `pwd`/oneclient_integration_$*.info.cleaned
	@echo "Coverage written to `pwd`/coverage/integration/$*/index.html"

.PHONY: coverage_integration
coverage_integration:
	lcov --quiet -a `pwd`/oneclient_integration_events_test.info.cleaned \
		         -a `pwd`/oneclient_integration_fslogic_test.info.cleaned \
		         -o `pwd`/oneclient_integration_combined.info
	genhtml -o `pwd`/coverage/integration/combined `pwd`/oneclient_integration_combined.info
	@echo "Coverage written to `pwd`/coverage/integration/combined/index.html"

.PHONY: check_distribution
check_distribution:
ifeq ($(DISTRIBUTION), none)
	@echo "Please provide package distribution. Oneof: 'trusty', 'xenial', 'bionic', 'centos-7-x86_64'"
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
	$(eval bamboos_sha=$(shell git submodule status --recursive | grep "helpers/bamboos" | awk '{print $$1;}'))
	cd helpers/bamboos; git archive --format=tar --prefix=$(PKG_ID)/helpers/bamboos/ $(bamboos_sha) | (cd ../../package && tar -xf -); cd ../..
	find package/$(PKG_ID) -depth -name ".git" -exec rm -rf {} \;
	echo "set(GIT_VERSION ${PKG_REVISION})" > package/$(PKG_ID)/version.txt
	tar -C package -czf package/$(PKG_ID).tar.gz $(PKG_ID)

.PHONY: conda/oneclient
conda/oneclient: SHELL:=/bin/bash
conda/oneclient: package/$(PKG_ID).tar.gz
	cp /tmp/.condarc $$HOME/.condarc
	cat $$HOME/.condarc
	mkdir -p package/conda
	mkdir -p package/conda-bld
	cp -R conda/oneclient package/conda/
	sed -i "s|<<PKG_VERSION>>|$(PKG_VERSION)|g" package/conda/oneclient/meta.yaml
	sed -i "s|<<PKG_SOURCE>>|../../$(PKG_ID).tar.gz|g" package/conda/oneclient/meta.yaml
	source /opt/conda/bin/activate base && \
		PKG_VERSION=$(PKG_VERSION) CONDA_BLD_PATH=$$PWD/package/conda-bld \
		conda build --user onedata-devel --token "${CONDA_TOKEN}" \
		${CONDA_BUILD_OPTIONS} package/conda/oneclient
	echo "onedata-devel/oneclient/$(PKG_VERSION)/download/linux-64/oneclient-$(PKG_VERSION)-py39.tar.bz2" >> conda-pkg-list.txt

.PHONY: conda/oneclient_centos6
conda/oneclient_centos6: SHELL:=/bin/bash
conda/oneclient_centos6: package/$(PKG_ID).tar.gz
	cp /tmp/.condarc-forge $$HOME/.condarc
	cat $$HOME/.condarc
	mkdir -p package/conda
	mkdir -p package/conda-bld
	cp -R conda/oneclient package/conda/
	sed -i "s|<<PKG_VERSION>>|$(PKG_VERSION)|g" package/conda/oneclient/meta.yaml
	sed -i "s|<<PKG_SOURCE>>|../../$(PKG_ID).tar.gz|g" package/conda/oneclient/meta.yaml
	sed -i 's|conda-forge::libfuse|onedata-centos6::libfuse =2.8.3|g' package/conda/oneclient/meta.yaml
	sed -i 's|protobuf.*$$|protobuf =3.8.0|g' package/conda/oneclient/meta.yaml
	sed -i 's|onedata::|onedata-centos6::|g' package/conda/oneclient/meta.yaml
	sed -i 's|DWITH_FUSE_VERSION=3|DWITH_FUSE_VERSION=2|g' package/conda/oneclient/build.sh
	sed -i '/run:/ { :l; n; s/^.*libfuse.*$$//; tx; bl; :x; N; s/\n//; bl }' package/conda/oneclient/meta.yaml
	source /opt/conda/bin/activate base && \
		PKG_VERSION=$(PKG_VERSION) CONDA_BLD_PATH=$$PWD/package/conda-bld \
		conda build --user onedata-centos6-devel --token "${CONDA_TOKEN}" \
		${CONDA_BUILD_OPTIONS} package/conda/oneclient
	echo "onedata-centos6-devel/oneclient/$(PKG_VERSION)/download/linux-64/oneclient-$(PKG_VERSION)-py39.tar.bz2" >> conda-pkg-list.txt

.PHONY: conda/onedatafs
conda/onedatafs: SHELL:=/bin/bash
conda/onedatafs: package/$(PKG_ID).tar.gz
	cp /tmp/.condarc $$HOME/.condarc
	cat $$HOME/.condarc
	mkdir -p package/conda
	mkdir -p package/conda-bld
	cp -R conda/onedatafs package/conda/
	sed -i "s|<<PKG_VERSION>>|$(PKG_VERSION)|g" package/conda/onedatafs/meta.yaml
	sed -i "s|<<PKG_SOURCE>>|../../$(PKG_ID).tar.gz|g" package/conda/onedatafs/meta.yaml
	source /opt/conda/bin/activate base && \
		PKG_VERSION=$(PKG_VERSION) CONDA_BLD_PATH=$$PWD/package/conda-bld \
		conda build --user onedata-devel --token "${CONDA_TOKEN}" \
		${CONDA_BUILD_OPTIONS} package/conda/onedatafs
	echo "onedata-devel/onedatafs/$(PKG_VERSION)/download/linux-64/onedatafs-$(PKG_VERSION)-py39.tar.bz2" >> conda-pkg-list.txt

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

	cd package/$(PKG_ID) && sudo sg sbuild -c "sbuild -sd $(DISTRIBUTION) -j$$(nproc)"
	mv package/*$(PKG_VERSION).orig.tar.gz package/packages/
	mv package/*$(PKG_VERSION)-$(PKG_BUILD)*.deb package/packages/
	mv package/*$(PKG_VERSION)-$(PKG_BUILD)*.dsc package/packages/
	mv package/*$(PKG_VERSION)-$(PKG_BUILD)*_amd64.changes package/packages/
	-mv package/*$(PKG_VERSION)-$(PKG_BUILD)*.debian.tar.xz package/packages/ || true

.PHONY: rpm
rpm: check_distribution package/$(PKG_ID).tar.gz
	rm -rf package/packages && mkdir -p package/packages
	mv -f package/$(PKG_ID).tar.gz package/$(PKG_ID).orig.tar.gz

	cp pkg_config/oneclient.spec package/oneclient.spec
	patch -d package/ -p1 -i $(PKG_ID)/pkg_config/$(DISTRIBUTION).patch
	sed -i "s/{{version}}/$(PKG_VERSION)/g" package/oneclient.spec
	sed -i "s/{{build}}/$(PKG_BUILD)/g" package/oneclient.spec
	mock --root $(DISTRIBUTION) --enable-network --buildsrpm --spec package/oneclient.spec --resultdir=package/packages \
		--sources package/$(PKG_ID).orig.tar.gz
	mock --root $(DISTRIBUTION) --enable-network --resultdir=package/packages --rebuild package/packages/onedata$(RELEASE)-$(PKG_ID)*.src.rpm

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
	        cp -r /usr/lib/x86_64-linux-gnu/libXrd* /output/lib/ && \
	        cp -r /usr/lib/x86_64-linux-gnu/glusterfs/$(GLUSTERFS_VERSION)/xlator/* /output/lib/x86_64-linux-gnu/glusterfs/$(GLUSTERFS_VERSION)/xlator/ && \
	        cp -r /usr/lib/x86_64-linux-gnu/glusterfs/$(GLUSTERFS_VERSION)/rpc-transport/* /output/lib/x86_64-linux-gnu/glusterfs/$(GLUSTERFS_VERSION)/rpc-transport/'
	# Collect all dynamic libraries GlusterFS dependencies
	docker run -v $(CURDIR)/$(ONECLIENT_FPMPACKAGE_TMP)/root:/output \
		-v $(CURDIR)/cpld.sh:/bin/cpld.sh --entrypoint /bin/sh \
		-t docker.onedata.org/oneclient-base:$(ONECLIENT_BASE_IMAGE) \
		-c "find /usr/lib/x86_64-linux-gnu/glusterfs/$(GLUSTERFS_VERSION)/ -name '*so*' -exec /bin/cpld.sh '{}' /output/lib \;"
	# Change the ld loader and rpath in Oneclient binaries and dependencies
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
	docker run -v $(CURDIR)/$(ONECLIENT_FPMPACKAGE_TMP)/root:/output \
		--entrypoint /bin/sh -t $(PATCHELF_DOCKER_IMAGE) -c \
		"find /output/lib -name '*so*' -type f ! -path '*ld-[0-9]*.[0-9]*.so' ! -path '*ld-linux-x86-64.so.2' -exec patchelf --set-rpath /opt/oneclient/lib --force-rpath {} \;"
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
# Build OneS3 self-contained tarball
#
ones3_tar $(ONECLIENT_FPMPACKAGE_TMP)/ones3-bin.tar.gz:
	# Create directory structure
	mkdir -p $(ONECLIENT_FPMPACKAGE_TMP)/root/bin
	mkdir -p $(ONECLIENT_FPMPACKAGE_TMP)/root/etc
	mkdir -p $(ONECLIENT_FPMPACKAGE_TMP)/root/etc/init.d
	mkdir -p $(ONECLIENT_FPMPACKAGE_TMP)/root/lib
	mkdir -p $(ONECLIENT_FPMPACKAGE_TMP)/root/lib/x86_64-linux-gnu/glusterfs/$(GLUSTERFS_VERSION)/xlator
	mkdir -p $(ONECLIENT_FPMPACKAGE_TMP)/root/lib/x86_64-linux-gnu/glusterfs/$(GLUSTERFS_VERSION)/rpc-transport

	# Collect all necessary Oneclient files in one folder
	docker run -v $(CURDIR)/$(ONECLIENT_FPMPACKAGE_TMP)/root:/output \
		-v $(CURDIR)/cpld.sh:/bin/cpld.sh --entrypoint /bin/cpld.sh \
		-t docker.onedata.org/oneclient-base:$(ONECLIENT_BASE_IMAGE) \
		/usr/bin/ones3 /output/lib
	docker run -v $(CURDIR)/$(ONECLIENT_FPMPACKAGE_TMP)/root:/output \
		--entrypoint /bin/bash \
		-t docker.onedata.org/oneclient-base:$(ONECLIENT_BASE_IMAGE) \
		-c 'cp /usr/bin/ones3 /output/bin && \
		    cp /etc/ones3.conf /output/etc && \
		    cp /etc/init.d/ones3 /output/etc/init.d && \
		    cp -r /lib/x86_64-linux-gnu/* /output/lib/ && \
	        cp -r /usr/lib/x86_64-linux-gnu/libXrd* /output/lib/ && \
	        cp -r /usr/lib/x86_64-linux-gnu/glusterfs/$(GLUSTERFS_VERSION)/xlator/* /output/lib/x86_64-linux-gnu/glusterfs/$(GLUSTERFS_VERSION)/xlator/ && \
	        cp -r /usr/lib/x86_64-linux-gnu/glusterfs/$(GLUSTERFS_VERSION)/rpc-transport/* /output/lib/x86_64-linux-gnu/glusterfs/$(GLUSTERFS_VERSION)/rpc-transport/'
	# Collect all dynamic libraries GlusterFS dependencies
	docker run -v $(CURDIR)/$(ONECLIENT_FPMPACKAGE_TMP)/root:/output \
		-v $(CURDIR)/cpld.sh:/bin/cpld.sh --entrypoint /bin/sh \
		-t docker.onedata.org/oneclient-base:$(ONECLIENT_BASE_IMAGE) \
		-c "find /usr/lib/x86_64-linux-gnu/glusterfs/$(GLUSTERFS_VERSION)/ -name '*so*' -exec /bin/cpld.sh '{}' /output/lib \;"
	# Change the ld loader and rpath in Oneclient binaries and dependencies
	docker run -v $(CURDIR)/$(ONECLIENT_FPMPACKAGE_TMP)/root:/output \
		-t $(PATCHELF_DOCKER_IMAGE) \
		--set-interpreter /opt/ones3/lib/ld-linux-x86-64.so.2 \
		--set-rpath /opt/ones3/lib --force-rpath \
		/output/bin/ones3
	docker run -v $(CURDIR)/$(ONECLIENT_FPMPACKAGE_TMP)/root:/output \
		--entrypoint /bin/sh -t $(PATCHELF_DOCKER_IMAGE) -c \
		"find /output/lib -name '*so*' -type f ! -path '*ld-[0-9]*.[0-9]*.so' ! -path '*ld-linux-x86-64.so.2' -exec patchelf --set-rpath /opt/ones3/lib --force-rpath {} \;"
	# Create binary archive
	cd $(ONECLIENT_FPMPACKAGE_TMP)/root && \
		tar -cf ones3-bin.tar * && \
		gzip -f ones3-bin.tar && \
		mv ones3-bin.tar.gz ../ && \
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
		   --depends fuse3 --depends ca-certificates \
		   --maintainer "Onedata Package Maintainers <info@onedata.org>" \
		   --description "Self-contained Onedata Oneclient command-line client package" \
		   /data/oneclient-bin.tar.gz

#
# Build Oneclient self-contained deb
#
oneclient_deb: $(ONECLIENT_FPMPACKAGE_TMP)/oneclient-bin.tar.gz
	# Build DEB package for the distribution specified using FPM
	@echo "========== oneclient_deb =========="
	cp pkg_config/fpm/oneclient_deb.pre $(ONECLIENT_FPMPACKAGE_TMP)/
	cp pkg_config/fpm/oneclient_deb.post $(ONECLIENT_FPMPACKAGE_TMP)/
	docker run -u=$$UID:$$GID -v /etc/passwd:/etc/passwd:ro -v /etc/group:/etc/group:ro \
		   -v $(CURDIR)/$(ONECLIENT_FPMPACKAGE_TMP):/data \
		   -t $(FPM_DOCKER_IMAGE) fpm -t deb -s tar \
		   -p /data/oneclient_$(ONECLIENT_VERSION)-$(PKG_BUILD)~$(DISTRIBUTION)_amd64.deb \
		   --architecture=amd64 \
		   --prefix=/opt/oneclient -n oneclient \
		   -v $(ONECLIENT_VERSION)-$(PKG_BUILD)~$(DISTRIBUTION) --license "Apache 2.0" \
		   --after-install=/data/oneclient_deb.pre \
		   --after-remove=/data/oneclient_deb.post \
		   --depends fuse3 --depends ca-certificates \
		   --maintainer "Onedata Package Maintainers <info@onedata.org>" \
		   --description "Self-contained Onedata Oneclient command-line client package" \
		   /data/oneclient-bin.tar.gz

#
# Build OneS3 self-contained rpm
#
ones3_rpm: $(ONECLIENT_FPMPACKAGE_TMP)/ones3-bin.tar.gz
	# Build RPM package for the distribution specified using FPM
	cp pkg_config/fpm/ones3_rpm.pre $(ONECLIENT_FPMPACKAGE_TMP)/
	cp pkg_config/fpm/ones3_rpm.post $(ONECLIENT_FPMPACKAGE_TMP)/
	docker run -u=$$UID:$$GID -v /etc/passwd:/etc/passwd:ro -v /etc/group:/etc/group:ro \
		   -v $(CURDIR)/$(ONECLIENT_FPMPACKAGE_TMP):/data \
		   -t $(FPM_DOCKER_IMAGE) \
		   fpm -t rpm --rpm-dist $(RPM_DIST) -s tar \
		   --prefix=/opt/ones3 -n ones3 -v $(ONECLIENT_VERSION) \
		   --architecture=x86_64 \
		   --iteration $(PKG_BUILD) --license "Apache 2.0" \
		   --after-install=/data/ones3_rpm.pre \
		   --after-remove=/data/ones3_rpm.post \
		   --depends ca-certificates \
		   --maintainer "Onedata Package Maintainers <info@onedata.org>" \
		   --description "Self-contained Onedata OneS3 service" \
		   /data/ones3-bin.tar.gz

#
# Build OneS3 self-contained deb
#
ones3_deb: $(ONECLIENT_FPMPACKAGE_TMP)/ones3-bin.tar.gz
	# Build DEB package for the distribution specified using FPM
	@echo "========== ones3_deb =========="
	cp pkg_config/fpm/ones3_deb.pre $(ONECLIENT_FPMPACKAGE_TMP)/
	cp pkg_config/fpm/ones3_deb.post $(ONECLIENT_FPMPACKAGE_TMP)/
	docker run -u=$$UID:$$GID -v /etc/passwd:/etc/passwd:ro -v /etc/group:/etc/group:ro \
		   -v $(CURDIR)/$(ONECLIENT_FPMPACKAGE_TMP):/data \
		   -t $(FPM_DOCKER_IMAGE) fpm -t deb -s tar \
		   -p /data/ones3_$(ONECLIENT_VERSION)-$(PKG_BUILD)~$(DISTRIBUTION)_amd64.deb \
		   --architecture=amd64 \
		   --prefix=/opt/ones3 -n ones3 \
		   -v $(ONECLIENT_VERSION)-$(PKG_BUILD)~$(DISTRIBUTION) --license "Apache 2.0" \
		   --after-install=/data/ones3_deb.pre \
		   --after-remove=/data/ones3_deb.post \
		   --depends ca-certificates \
		   --maintainer "Onedata Package Maintainers <info@onedata.org>" \
		   --description "Self-contained Onedata OneS3 service" \
		   /data/ones3-bin.tar.gz


.PHONY: clean
clean:
	rm -rf debug release relwithdebinfo doc package package_fpm _build

.PHONY: clang-tidy
clang-tidy:
	cmake --build debug --target clang-tidy

.PHONY: clang-format
clang-format:
	docker run --rm -v $(CURDIR):/root/sources onedata/clang-format-check:1.3
