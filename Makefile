BUILD_DIR = build

CMAKE = $(shell which cmake || which cmake28)
CPACK = $(shell which cpack || which cpack28)

all: rpm test

rpm: build
	@cd ${BUILD_DIR} && ${CPACK} -C CPackConfig.cmake -G RPM
	@cd ${BUILD_DIR} && ${CPACK} -C CPackConfig.cmake -G DEB

	@echo ""
	@echo ""
	@echo "Fallowing packages are now available in PROJECT_DIR directory: "
	@echo ""
	@for pkg in `ls ${BUILD_DIR}/*.rpm 2>/dev/null`; do echo $$pkg; done
	@for pkg in `ls ${BUILD_DIR}/*.deb 2>/dev/null`; do echo $$pkg; done 

build: configure
	@(cd ${BUILD_DIR} && make -j`nproc`)

configure:
	@mkdir -p ${BUILD_DIR}
	@cd ${BUILD_DIR} && ${CMAKE} ..

test: build
	@cd ${BUILD_DIR} && make test

cunit: build
	@cd ${BUILD_DIR} && make cunit

integration_tests: build
	@cd ${BUILD_DIR} && make integration_tests

install: build
	@cd ${BUILD_DIR} && make install

clean: 
	@rm -rf ${BUILD_DIR}