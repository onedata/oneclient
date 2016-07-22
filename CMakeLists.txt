cmake_minimum_required(VERSION 3.0.0)
project(oneclient C CXX)

option(CODE_COVERAGE "Enable code coverage (gcc only)." OFF)
option(STATIC_BOOST "Link Boost statically" Off)
option(STATIC_LIBSTDCPP "Link libstdc++ statically" Off)

# CMake config
set(CMAKE_SKIP_INSTALL_ALL_DEPENDENCY True)
set(CMAKE_FIND_FRAMEWORK LAST)
set(CMAKE_POSITION_INDEPENDENT_CODE True)
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake ${PROJECT_SOURCE_DIR}/helpers/cmake)

if(NOT SYSCONF_INSTALL_DIR)
    set(SYSCONF_INSTALL_DIR etc CACHE PATH "")
endif()

# Version
set(oneclient_VERSION_MAJOR 0)
set(oneclient_VERSION_MINOR 0)
set(oneclient_VERSION_PATCH 0)
set(oneclient_VERSION_TWEAK "")
set(oneclient_VERSION_FALLBACK "unknown version")

include(version.txt OPTIONAL)

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

# Setup CURL
message(STATUS "Checking for CURL...")
find_package(CURL REQUIRED)

# Setup Boost
message(STATUS "Checking for Boost.Filesystem, Boost.System and Boost.Program_options...")
set(Boost_USE_MULTITHREADED      ON)
set(Boost_USE_STATIC_RUNTIME     ${STATIC_LIBSTDCPP})
set(Boost_USE_STATIC_LIBS        ${STATIC_BOOST})
find_package(Boost COMPONENTS filesystem program_options system REQUIRED)

# Setup libsodium
find_package(Sodium)

# Setup etls
set(ETLS_DIR ${CMAKE_CURRENT_SOURCE_DIR}/helpers/etls/c_src)

# Setup BoringSSL
add_subdirectory(${ETLS_DIR}/deps/boringssl)
set(BORINGSSL_LIBRARIES ssl crypto)
set(BORINGSSL_INCLUDE_DIRS ${ETLS_DIR}/deps/boringssl/include)

# Setup Asio
set(ASIO_INCLUDE_DIRS ${ETLS_DIR}/deps/asio/asio/include)

# Utility libs
find_library(LTDL_LIBRARY NAMES ltdl)
find_library(ZLIB_LIBRARY NAMES z)
find_library(RT_LIBRARY rt)
find_library(DL_LIBRARY dl)

# Rados library
find_library(RADOS_LIBRARY rados)

# AWS SDK library
find_library(AWS_SKD_CORE_LIBRARY aws-cpp-sdk-core)
find_library(AWS_SKD_S3_LIBRARY aws-cpp-sdk-s3)
set(AWS_SDK_LIBRARIES
    ${AWS_SKD_S3_LIBRARY}
    ${AWS_SKD_CORE_LIBRARY}
    ${CURL_LIBRARIES})

# Swift SDK library
find_library(SWIFT_SDK_LIBRARY Swift)
find_library(POCO_UTIL PocoUtil)
find_library(POCO_NET PocoNet)
find_library(POCO_XML PocoXML)
find_library(POCO_FOUNDATION PocoFoundation)
set(SWIFT_SDK_LIBRARIES
    ${SWIFT_SDK_LIBRARY}
    ${POCO_UTIL}
    ${POCO_NET}
    ${POCO_XML}
    ${POCO_FOUNDATION})

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
    ${DL_LIBRARY})

if(STATIC_LIBSTDCPP)
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static-libstdc++")
endif(STATIC_LIBSTDCPP)

# Code coverage
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
add_compile_options(${SECTION_FRAGMENTATION_FLAGS} -Werror -Wshadow -Wall -Wno-unused-result)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14")
add_definitions(
    ${FUSE_DEFINITIONS}
    -DFUSE_USE_VERSION=29
    -DBOOST_BIND_NO_PLACEHOLDERS
    -DBOOST_FILESYSTEM_NO_DEPRECATED
    -DBOOST_ALL_DYN_LINK
    -DASIO_STANDALONE
    -DBUILD_PROXY_IO
    -DGLOG_STL_LOGGING_FOR_UNORDERED)

# Add helpers after setting compilation flags but before setting include
# directories; helpers specify their own includes, pulling from package-set
# variables when needed.
add_subdirectory(helpers/clproto)
add_subdirectory(${ETLS_DIR}/src)
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
    ${RADOS_LIBRARY}
    ${AWS_SDK_LIBRARIES}
    ${SWIFT_SDK_LIBRARIES}
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

# Install config
install(TARGETS oneclient DESTINATION bin)
install(DIRECTORY config/ DESTINATION ${SYSCONF_INSTALL_DIR})

enable_testing()
add_subdirectory(test)
