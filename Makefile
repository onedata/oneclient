RELEASE_DIR = release
DEBUG_DIR = debug

CMAKE = $(shell which cmake || which cmake28)
CPACK = $(shell which cpack || which cpack28)

.PHONY: rpm build release debug docs clean all
all: rpm test

rpm: dynamic
	@cd ${RELEASE_DIR} && ${CPACK} -C CPackConfig.cmake -G RPM
	@cd ${RELEASE_DIR} && ${CPACK} -C CPackConfig.cmake -G DEB

	@echo
	@echo
	@echo "Following packages are now available in PROJECT_DIR directory: "
	@echo
	@for pkg in `ls ${RELEASE_DIR}/*.rpm 2>/dev/null`; do echo $$pkg; done
	@for pkg in `ls ${RELEASE_DIR}/*.deb 2>/dev/null`; do echo $$pkg; done

## Obsolete target, use 'make release' instead
build: release
	@echo "*****************************************************"
	@echo "'build' target is obsolete, use 'release' instead !"
	@echo "*****************************************************"
	@ln -sf ${RELEASE_DIR} build

deb-info:
	@mkdir -p ${RELEASE_DIR}
	-@find ${RELEASE_DIR} -name "helpers-update" -exec rm -rf {} \;
	@cd ${RELEASE_DIR} && ${CMAKE} -GNinja -DCMAKE_BUILD_TYPE=RelWithDebInfo ..
	@cd ${RELEASE_DIR} && ninja oneclient

dynamic: deb-info
	@cd ${RELEASE_DIR} && ninja oneclient_dynamic

release:
	@mkdir -p ${RELEASE_DIR}
	-@find ${RELEASE_DIR} -name "helpers-update" -exec rm -rf {} \;
	@cd ${RELEASE_DIR} && ${CMAKE} -GNinja -DCMAKE_BUILD_TYPE=Release ..
	@cd ${RELEASE_DIR} && ninja oneclient

debug:
	@mkdir -p ${DEBUG_DIR}
	-@find ${DEBUG_DIR} -name "helpers-update" -exec rm -rf {} \;
	@cd ${DEBUG_DIR} && ${CMAKE} -GNinja -DCMAKE_BUILD_TYPE=Debug ..
	@cd ${DEBUG_DIR} && ninja oneclient

test: deb-info
	@cd ${RELEASE_DIR} && ninja
	@cd ${RELEASE_DIR} && ninja test

cunit: deb-info
	@cd ${RELEASE_DIR} && ninja
	@cd ${RELEASE_DIR} && ninja cunit

integration_tests: debug
	@cd ${DEBUG_DIR} && ninja integration_tests

install: release
	@cd ${RELEASE_DIR} && ninja install

docs:
	@doxygen Doxyfile

clean:
	@rm -rf ${DEBUG_DIR} ${RELEASE_DIR} build doc
