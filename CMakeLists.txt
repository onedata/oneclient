cmake_minimum_required (VERSION 2.8.0)
project (VeilClient)

include(ExternalProject)

# The version number.
set(VeilClient_VERSION_MAJOR 0)
set(VeilClient_VERSION_MINOR 1)
set(VeilClient_VERSION_PATCH 0)
set(VeilClient_VERSION $(VeilClient_VERSION_MAJOR).$(VeilClient_VERSION_MINOR).$(VeilClient_VERSION_PATCH))

# Install dirs
set(CONFIG_DIR "etc")
set(BIN_DIR "bin")
set(LIB_DIR "lib")

# CMake config
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR})
configure_file (
	"${PROJECT_SOURCE_DIR}/include/veilConfig.h.in"
	"${PROJECT_BINARY_DIR}/veilConfig.h"
)

# Deps prefix
set(GLOG_PREFIX "src/google-glog")
set(YAML_CPP_PREFIX "src/yaml-cpp")

# Include paths
include_directories(${PROJECT_SOURCE_DIR}/include)
include_directories(${PROJECT_SOURCE_DIR}/include/fuse)
include_directories(${PROJECT_BINARY_DIR})

# Deps
include_directories(${YAML_CPP_PREFIX}/include)
include_directories(${GLOG_PREFIX}/src)

# Setup fuse
message(STATUS "Checking for FUSE...")
find_package(FUSE REQUIRED)
include_directories(${FUSE_INCLUDE_DIRS})

# Setup ProtoBuf
message(STATUS "Checking for Protobuf...")
find_package(Protobuf REQUIRED)
include_directories(${PROTOBUF_INCLUDE_DIR})

# Setup pthreads
message(STATUS "Checking for pthreads...")
find_package(Threads REQUIRED)

# Setup git
message(STATUS "Checking for Git...")
find_package(Git REQUIRED)

# Setup OpenSSL
message(STATUS "Checking for OpenSSL...")
find_package(OpenSSL REQUIRED)
include_directories(${OPENSSL_INCLUDE_DIR})

# Setup Boost
set(Boost_USE_STATIC_LIBS   ON)
find_package(Boost COMPONENTS system thread filesystem REQUIRED)
include_directories(${Boost_INCLUDE_DIRS})

# Pull git submodules
message(STATUS "Pulling submodules using git...")
execute_process(COMMAND ${GIT_EXECUTABLE} submodule init
				WORKING_DIRECTORY ${PROJECT_SOURCE_DIR})
execute_process(COMMAND ${GIT_EXECUTABLE} submodule update
				WORKING_DIRECTORY ${PROJECT_SOURCE_DIR})
execute_process(COMMAND ${GIT_EXECUTABLE} submodule foreach git checkout 
				WORKING_DIRECTORY ${PROJECT_SOURCE_DIR})

# Compile .proto files
file(GLOB PROTO_FILES "${PROJECT_SOURCE_DIR}/veilprotocol/proto/*.proto")
PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS ${PROTO_FILES})
include_directories(${CMAKE_CURRENT_BINARY_DIR}) 

# Setup sources
file(GLOB SOURCES "src/fuse/[a-zA-Z]*.cc")
file(GLOB HELPER_SOURCES "src/helpers/[a-zA-Z]*.cc")

# Setup compile flags
set(CUSTOM_RPATH "${CMAKE_INSTALL_PREFIX}/${LIB_DIR}:${CMAKE_CURRENT_BINARY_DIR}:.:${LIB_DIR}:../${LIB_DIR}")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O2 ${FUSE_DEFINITIONS} -Wl,-rpath,${CUSTOM_RPATH}")
set(CMAKE_SKIP_RPATH TRUE) # Something is wrong with CMake RPATH config, we need to use old-fashion way

# Define targets
add_library(veilhelpers ${HELPER_SOURCES})
add_executable(veilFuse ${SOURCES} ${PROTO_SRCS})
add_dependencies(veilFuse yaml-cpp) 
set_target_properties(veilFuse PROPERTIES COMPILE_FLAGS "-Wall -g")


# glog
message(STATUS "Configuring google-glog...")
execute_process(COMMAND ./configure
				WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/${GLOG_PREFIX}
)
add_library(glog 
	${GLOG_PREFIX}/src/logging.cc
	${GLOG_PREFIX}/src/raw_logging.cc
	${GLOG_PREFIX}/src/vlog_is_on.cc
	${GLOG_PREFIX}/src/utilities.cc
	${GLOG_PREFIX}/src/demangle.cc
	${GLOG_PREFIX}/src/symbolize.cc
	${GLOG_PREFIX}/src/signalhandler.cc
)

# yaml-cpp
ExternalProject_Add(yaml-cpp
	PREFIX ${PROJECT_BINARY_DIR}
	SOURCE_DIR ${PROJECT_SOURCE_DIR}/${YAML_CPP_PREFIX}
	BINARY_DIR ${PROJECT_BINARY_DIR}/${YAML_CPP_PREFIX}
	INSTALL_COMMAND ""
)

target_link_libraries(veilFuse 
	${FUSE_LIBRARIES}
	${PROTOBUF_LIBRARY}
	${CMAKE_THREAD_LIBS_INIT}
	${OPENSSL_LIBRARIES}
	${Boost_LIBRARIES}
	${PROJECT_BINARY_DIR}/${YAML_CPP_PREFIX}/libyaml-cpp.a
	veilhelpers
	glog
)

# Install config
install(TARGETS veilFuse
		RUNTIME DESTINATION ${BIN_DIR}
		LIBRARY DESTINATION ${LIB_DIR}
)
install(DIRECTORY config/ DESTINATION ${CONFIG_DIR})

# build a CPack driven installer package
include(InstallRequiredSystemLibraries)
set(CPACK_SET_DESTDIR ON)
set(CPACK_RESOURCE_FILE_LICENSE  
     "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE.txt")
set(CPACK_PACKAGE_VERSION_MAJOR "${VeilClient_VERSION_MAJOR}")
set(CPACK_PACKAGE_VERSION_MINOR "${VeilClient_VERSION_MINOR}")
set(CPACK_PACKAGE_CONTACT "help@veilfs.com")

# DEB and RPM deps 
set(CPACK_DEBIAN_PACKAGE_DEPENDS "fuse, libfuse2, libprotobuf7, libssl1.0.0")
set(CPACK_RPM_PACKAGE_REQUIRES "fuse, fuse-libs, protobuf openssl")
set(CPACK_RPM_PACKAGE_AUTOREQPROV " no")

include(CPack)