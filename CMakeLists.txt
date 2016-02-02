cmake_minimum_required(VERSION 3.0.0)
project(oneclient C CXX)

# CMake config
set(CMAKE_FIND_FRAMEWORK LAST)
set(CMAKE_POSITION_INDEPENDENT_CODE True)
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake ${PROJECT_SOURCE_DIR}/helpers/cmake)

#================================================================================================
# Automatic git versioning: http://ipenguin.ws/2012/11/cmake-automatically-use-git-tags-as.html
# Make a version file containing the current version from git.
include(GetGitRevisionDescription)
git_describe(VERSION --tags)

#parse the version information into pieces.
string(REGEX REPLACE "^([0-9]+)\\..*" "\\1" VERSION_MAJOR "${VERSION}")
string(REGEX REPLACE "^[0-9]+\\.([0-9]+).*" "\\1" VERSION_MINOR "${VERSION}")
string(REGEX REPLACE "^[0-9]+\\.[0-9]+\\.([0-9]+).*" "\\1" VERSION_PATCH "${VERSION}")

set(oneclient_VERSION_MAJOR 0)
set(oneclient_VERSION_MINOR 0)
set(oneclient_VERSION_PATCH 0)

# set version (if no tag is specified, the version stays at 0.0.0)
if(NOT ${VERSION_MAJOR} MATCHES -128-NOTFOUND)
    set(oneclient_VERSION_MAJOR ${VERSION_MAJOR})
endif(NOT ${VERSION_MAJOR} MATCHES -128-NOTFOUND)

if(NOT ${VERSION_MINOR} MATCHES -128-NOTFOUND)
    set(oneclient_VERSION_MINOR ${VERSION_MINOR})
endif(NOT ${VERSION_MINOR} MATCHES -128-NOTFOUND)

if(NOT ${VERSION_PATCH} MATCHES -128-NOTFOUND)
    set(oneclient_VERSION_PATCH ${VERSION_PATCH})
endif(NOT ${VERSION_PATCH} MATCHES -128-NOTFOUND)

# The version number.
set(oneclient_VERSION $(oneclient_VERSION_MAJOR).$(oneclient_VERSION_MINOR).$(oneclient_VERSION_PATCH))
#================================================================================================

file(MAKE_DIRECTORY ${PROJECT_BINARY_DIR}/version)
configure_file(include/version.h.in ${PROJECT_BINARY_DIR}/version/version.h)

# Deps
add_subdirectory(deps/libmacaroons)
add_subdirectory(deps/libmacaroons-cpp)

# Setup fuse
message(STATUS "Checking for FUSE...")
find_package(FUSE REQUIRED)

# Setup pthreads
message(STATUS "Checking for thread support...")
find_package(Threads REQUIRED)

# Setup Intel TBB
message(STATUS "Checking for Intel TBB...")
find_package(TBB REQUIRED)

# Setup glog
message(STATUS "Checking for glog...")
find_package(Glog REQUIRED)

# Setup Boost
set(Boost_USE_MULTITHREADED      ON)
set(Boost_USE_STATIC_RUNTIME    OFF)
set(Boost_USE_STATIC_LIBS       OFF)
find_package(Boost COMPONENTS filesystem program_options system REQUIRED)

# Setup libsodium
find_package(Sodium)

# Setup Erlang-TLS
set(ERLANG_TLS_DIR ${CMAKE_CURRENT_SOURCE_DIR}/helpers/erlang-tls/c_src)

# Setup BoringSSL
add_subdirectory(${ERLANG_TLS_DIR}/deps/boringssl)
set(BORINGSSL_LIBRARIES ssl crypto)
set(BORINGSSL_INCLUDE_DIRS ${ERLANG_TLS_DIR}/deps/boringssl/include)

# Setup Asio
set(ASIO_INCLUDE_DIRS ${ERLANG_TLS_DIR}/deps/asio/asio/include)

# Utility libs
find_library(LTDL_LIBRARY NAMES ltdl)
find_library(ZLIB_LIBRARY NAMES z)
find_library(RT_LIBRARY rt)
find_library(DL_LIBRARY dl)

# Rados library
find_library(RADOS_LIBRARY rados)

# Amazon S3 library
find_library(S3_LIBRARY s3)

# gpertools lib
find_library(PROFILER_LIBRARY NAMES profiler)

# Set up sources
file(GLOB_RECURSE SOURCES src/*.cc include/*.h)
set(MAIN_SOURCE_FILE ${CMAKE_CURRENT_SOURCE_DIR}/src/main.cc)
list(REMOVE_ITEM SOURCES ${MAIN_SOURCE_FILE})

# Set up compile flags
set(PLATFORM_EXTRA_LIBS
    ${CMAKE_THREAD_LIBS_INIT}
    ${LTDL_LIBRARY}
    ${ZLIB_LIBRARY}
    ${DL_LIBRARY}
    ${RADOS_LIBRARY}
    ${S3_LIBRARY})

# Code coverage
option(CODE_COVERAGE "Enable code coverage (gcc only)." OFF)
if(CODE_COVERAGE)
  message("Code coverage enabled.")
  add_compile_options(--coverage)
  set(PLATFORM_EXTRA_LIBS ${PLATFORM_EXTRA_LIBS} gcov)
endif(CODE_COVERAGE)

if(APPLE)
    set(SECTION_FRAGMENTATION_FLAGS -Wno-deprecated-declarations)
    set(CUSTOM_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
else(NOT APPLE)
    set(PLATFORM_EXTRA_LIBS ${PLATFORM_EXTRA_LIBS} ${RT_LIBRARY})
    set(SECTION_FRAGMENTATION_FLAGS -fdata-sections -ffunction-sections)
    set(CUSTOM_RPATH "${CMAKE_INSTALL_PREFIX}/lib" "\$ORIGIN" "\$ORIGIN/../lib")
endif(APPLE)

# Define build settings
add_compile_options(${SECTION_FRAGMENTATION_FLAGS} -Werror -Wshadow -Wall)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14")
add_definitions(
    ${FUSE_DEFINITIONS}
    -DFUSE_USE_VERSION=29
    -DBOOST_BIND_NO_PLACEHOLDERS
    -DBOOST_FILESYSTEM_NO_DEPRECATED
    -DBOOST_ALL_DYN_LINK
    -DASIO_STANDALONE
    -DBUILD_PROXY_IO)

# Add helpers after setting compilation flags but before setting include
# directories; helpers specify their own includes, pulling from package-set
# variables when needed.
add_subdirectory(helpers/clproto)
add_subdirectory(${ERLANG_TLS_DIR}/src)
add_subdirectory(helpers/src)

include_directories(
    include
    src
    ${PROJECT_BINARY_DIR}/version
    ${HELPERS_INCLUDE_DIRS})

include_directories(SYSTEM
    ${HELPERS_SYSTEM_INCLUDE_DIRS}
    ${FUSE_INCLUDE_DIRS}
    ${FUSE_INCLUDE_DIRS}/..
    ${PROTOBUF_INCLUDE_DIR}
    ${BORINGSSL_INCLUDE_DIR}
    ${ASIO_INCLUDE_DIRS}
    ${TBB_INCLUDE_DIRS}
    ${Boost_INCLUDE_DIRS}
    ${GLOG_INCLUDE_DIRS}
    ${SODIUM_INCLUDE_DIRS}
    ${CMAKE_CURRENT_SOURCE_DIR}/deps/libmacaroons
    ${CMAKE_CURRENT_SOURCE_DIR}/deps/libmacaroons-cpp)

set(CLIENT_LIBRARIES
    libmacaroons-cpp
    ${PLATFORM_EXTRA_LIBS}
    ${GLOG_LIBRARIES}
    ${FUSE_LIBRARIES}
    ${Boost_LIBRARIES}
    ${SODIUM_LIBRARIES}
    ${PROTOBUF_LIBRARIES}
    ${BORINGSSL_LIBRARIES}
    ${HELPERS_LIBRARIES})

# Define targets
add_library(client OBJECT ${SOURCES} ${HEADERS})
add_dependencies(client helpers clproto)

set(CLIENT_SOURCES
    ${MAIN_SOURCE_FILE}
    $<TARGET_OBJECTS:client>
    ${PROJECT_SOURCES})

add_executable(oneclient ${CLIENT_SOURCES})
target_link_libraries(oneclient PRIVATE ${CLIENT_LIBRARIES})
set_target_properties(oneclient PROPERTIES
    BUILD_WITH_INSTALL_RPATH true
    INSTALL_RPATH_USE_LINK_PATH true
    INSTALL_RPATH "${CUSTOM_RPATH}")

if(NOT ${PROFILER_LIBRARY} MATCHES PROFILER_LIBRARY-NOTFOUND)
    target_link_libraries(oneclient PRIVATE ${PROFILER_LIBRARY})
endif(NOT ${PROFILER_LIBRARY} MATCHES PROFILER_LIBRARY-NOTFOUND)

if(NOT SYSCONF_INSTALL_DIR)
    set(SYSCONF_INSTALL_DIR etc CACHE PATH "")
endif()

# Install config
install(TARGETS oneclient DESTINATION bin)
install(DIRECTORY config/ DESTINATION ${SYSCONF_INSTALL_DIR})

enable_testing()
add_subdirectory(test)
