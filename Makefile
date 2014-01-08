RELEASE_DIR = release
DEBUG_DIR = debug

CMAKE = $(shell which cmake || which cmake28)
CPACK = $(shell which cpack || which cpack28)


.PHONY: rpm build release debug clean all
all: rpm test

rpm: release
	@cd ${RELEASE_DIR} && ${CPACK} -C CPackConfig.cmake -G RPM
	@cd ${RELEASE_DIR} && ${CPACK} -C CPackConfig.cmake -G DEB

	@echo ""
	@echo ""
	@echo "Fallowing packages are now available in PROJECT_DIR directory: "
	@echo ""
	@for pkg in `ls ${RELEASE_DIR}/*.rpm 2>/dev/null`; do echo $$pkg; done
	@for pkg in `ls ${RELEASE_DIR}/*.deb 2>/dev/null`; do echo $$pkg; done 

## Obsolete target, use 'make release' instead
build: release 
	@echo "*****************************************************"
	@echo "'build' target is obsolete, use 'release' instead !"
	@echo "*****************************************************"
	@ln -sf ${RELEASE_DIR} build

release: 
	@mkdir -p ${RELEASE_DIR}
	@cd ${RELEASE_DIR} && ${CMAKE} -DCMAKE_BUILD_TYPE=release ..
	@(cd ${RELEASE_DIR} && make veilFuse -j`nproc`)

debug: 
	@mkdir -p ${DEBUG_DIR}
	@cd ${DEBUG_DIR} && ${CMAKE} -DCMAKE_BUILD_TYPE=debug ..
	@(cd ${DEBUG_DIR} && make veilFuse -j`nproc`)

test: release
	@cd ${RELEASE_DIR} && make test

cunit: release
	@cd ${RELEASE_DIR} && make cunit

integration_tests: debug
	@cd ${DEBUG_DIR} && make integration_tests

install: release
	@cd ${RELEASE_DIR} && make install

clean: 
	@rm -rf ${DEBUG_DIR} ${RELEASE_DIR} build
