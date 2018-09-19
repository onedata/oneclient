cmake_minimum_required(VERSION 3.0.0)
project(oneclient C CXX)

option(CODE_COVERAGE "Enable code coverage (gcc only)." OFF)
option(STATIC_BOOST "Link Boost statically" Off)
option(STATIC_LIBSTDCPP "Link libstdc++ statically" Off)
option(WITH_CEPH "Include Ceph direct IO support" ON)
option(WITH_S3 "Include S3 direct IO support" ON)
option(WITH_SWIFT "Include Swift direct IO support" ON)
option(WITH_GLUSTERFS "Include GlusterFS direct IO support" ON)

# CMake config
set(CMAKE_SKIP_INSTALL_ALL_DEPENDENCY True)
set(CMAKE_FIND_FRAMEWORK LAST)
set(CMAKE_POSITION_INDEPENDENT_CODE True)
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake ${PROJECT_SOURCE_DIR}/helpers/cmake)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include(FindLibraryUtils)
include(GNUInstallDirs)

# Version
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

# Setup OpenSSL
message(STATUS "Checking for OpenSSL...")
find_package(OpenSSL REQUIRED)

# Setup NSS
message(STATUS "Checking for NSS...")
find_package(NSS REQUIRED)

# Setup Folly
message(STATUS "Checking for Folly...")
find_package(Folly REQUIRED)
find_package(Wangle REQUIRED)
find_library(LIBEVENT_LIBRARY event)
find_library(IBERTY_LIBRARY iberty)
find_library(DOUBLE_CONVERSION_LIBRARY double-conversion)
set(FOLLY_LIBRARIES
    ${FOLLY_LIBRARIES}
    ${WANGLE_LIBRARIES}
    ${LIBEVENT_LIBRARY}
    ${IBERTY_LIBRARY}
    ${DOUBLE_CONVERSION_LIBRARY})

# Setup Boost
message(STATUS "Checking for Boost components...")
set(Boost_USE_MULTITHREADED      ON)
set(Boost_USE_STATIC_RUNTIME     ${STATIC_LIBSTDCPP})
set(Boost_USE_STATIC_LIBS        ${STATIC_BOOST})
find_package(Boost COMPONENTS atomic chrono context date_time filesystem
                              iostreams log log_setup program_options regex
                              system thread REQUIRED)

# Setup libsodium
find_package(Sodium)

# Setup helpers
set(HELPERS_DIR ${CMAKE_CURRENT_SOURCE_DIR}/helpers)

# Setup Asio
set(ASIO_INCLUDE_DIRS ${HELPERS_DIR}/deps/asio/asio/include)

# Setup cppmetrics
set(CPPMETRICS_INCLUDE_DIRS ${HELPERS_DIR}/deps/cppmetrics)

# Utility libs
find_library(LTDL_LIBRARY NAMES ltdl)
find_library(ZLIB_LIBRARY NAMES z)
find_library(RT_LIBRARY rt)
find_library(DL_LIBRARY dl)

# Rados library
if(WITH_CEPH)
    find_library(RADOS_LIBRARY rados)
    find_library(RADOSSTRIPER_LIBRARY radosstriper)
endif(WITH_CEPH)

# AWS SDK library
if(WITH_S3)
    find_library(AWS_SKD_CORE_LIBRARY aws-cpp-sdk-core HINTS /usr/lib/x86_64-linux-gnu)
    find_library(AWS_SKD_S3_LIBRARY aws-cpp-sdk-s3 HINTS /usr/lib/x86_64-linux-gnu)
    set(AWS_SDK_LIBRARIES
        ${AWS_SKD_S3_LIBRARY}
        ${AWS_SKD_CORE_LIBRARY}
        ${CURL_LIBRARIES})
    add_definitions(-DWITH_S3=1)
    if(APPLE)
      add_definitions(-DS3_HAS_NO_V2_SUPPORT)
    endif(APPLE)
else(WITH_S3)
    add_definitions(-DWITH_S3=0)
endif(WITH_S3)

# Swift SDK library
if(WITH_SWIFT)
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
endif(WITH_SWIFT)

# GlusterFS SDK library
if(WITH_GLUSTERFS)
    include(FindPkgConfig)
    if (PKG_CONFIG_FOUND)
        set(ENV{PKG_CONFIG_PATH} "/opt/oneclient/lib/x86_64-linux-gnu/pkgconfig:$ENV{PKG_CONFIG_PATH}")
        pkg_check_modules(GLUSTERFSAPI glusterfs-api)
        if(NOT GLUSTERFSAPI_FOUND EQUAL 1)
            message(SEND_ERROR "pkg-config for glusterfs-api is missing.")
        else()
            add_definitions(-DWITH_GLUSTERFS=1)
            link_directories(${GLUSTERFSAPI_LIBDIR})
        endif()
    endif (PKG_CONFIG_FOUND)
else(WITH_GLUSTERFS)
    add_definitions(-DWITH_GLUSTERFS=0)
endif(WITH_GLUSTERFS)

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
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14 -fdiagnostics-color")
add_definitions(
    ${FUSE_DEFINITIONS}
    -DFUSE_USE_VERSION=30
    -DBOOST_BIND_NO_PLACEHOLDERS
    -DBOOST_FILESYSTEM_NO_DEPRECATED
    -DBOOST_ALL_DYN_LINK
    -DASIO_STANDALONE
    -DBUILD_PROXY_IO
    -DGLOG_STL_LOGGING_FOR_UNORDERED
    -DFIBER_STACK_SIZE=1048576)

if(WITH_CEPH)
    add_definitions(-DWITH_CEPH=1)
else(WITH_CEPH)
    add_definitions(-DWITH_CEPH=0)
endif(WITH_CEPH)

if(WITH_S3)
    add_definitions(-DWITH_S3=1)
    if(APPLE)
      add_definitions(-DS3_HAS_NO_V2_SUPPORT)
    endif(APPLE)
else(WITH_S3)
    add_definitions(-DWITH_S3=0)
endif(WITH_S3)

if(WITH_SWIFT)
    add_definitions(-DWITH_SWIFT=1)
else(WITH_SWIFT)
    add_definitions(-DWITH_SWIFT=0)
endif(WITH_SWIFT)

if(WITH_GLUSTERFS)
    add_definitions(-DWITH_GLUSTERFS=1)
else(WITH_GLUSTERFS)
    add_definitions(-DWITH_GLUSTERFS=0)
endif(WITH_GLUSTERFS)

#
# Select version of Folly, latest versions available on OSX have TimedMutex
# defined without a template
#
if(APPLE)
    add_definitions(-DFOLLY_TIMEDMUTEX_IS_TEMPLATE=0)
else(APPLE)
    add_definitions(-DFOLLY_TIMEDMUTEX_IS_TEMPLATE=1)
endif(APPLE)

# Add helpers after setting compilation flags but before setting include
# directories; helpers specify their own includes, pulling from package-set
# variables when needed.
add_subdirectory(helpers/clproto)
add_subdirectory(helpers/src)

include_directories(
    include
    src
    ${PROJECT_BINARY_DIR}/version
    ${HELPERS_INCLUDE_DIRS})

include_directories(SYSTEM
    ${HELPERS_SYSTEM_INCLUDE_DIRS}
    ${GLUSTERFSAPI_INCLUDEDIR}
    ${FUSE_INCLUDE_DIRS}
    ${FUSE_INCLUDE_DIRS}/..
    ${PROTOBUF_INCLUDE_DIR}
    ${ASIO_INCLUDE_DIRS}
    ${TBB_INCLUDE_DIRS}
    ${Boost_INCLUDE_DIRS}
    ${GLOG_INCLUDE_DIRS}
    ${SODIUM_INCLUDE_DIRS}
    ${FOLLY_INCLUDE_DIR}
    ${WANGLE_INCLUDE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/deps/libmacaroons
    ${CMAKE_CURRENT_SOURCE_DIR}/deps/libmacaroons-cpp
    ${OPENSSL_INCLUDE_DIR})

set(CLIENT_LIBRARIES
    libmacaroons-cpp
    ${PLATFORM_EXTRA_LIBS}
    ${GLOG_LIBRARIES}
    ${FUSE_LIBRARIES}
    ${Boost_LIBRARIES}
    ${SODIUM_LIBRARIES}
    ${PROTOBUF_LIBRARIES}
    ${NSS_LIBRARIES}
    ${FOLLY_LIBRARIES}
    ${HELPERS_LIBRARIES}
    ${OPENSSL_LIBRARIES})

if(WITH_CEPH)
    list(APPEND CLIENT_LIBRARIES ${RADOS_LIBRARY} ${RADOSSTRIPER_LIBRARY})
endif(WITH_CEPH)

if(WITH_S3)
    list(APPEND CLIENT_LIBRARIES ${AWS_SDK_LIBRARIES})
endif(WITH_S3)

if(WITH_SWIFT)
    list(APPEND CLIENT_LIBRARIES ${SWIFT_SDK_LIBRARIES})
endif(WITH_SWIFT)

if(WITH_GLUSTERFS)
    list(APPEND CLIENT_LIBRARIES ${GLUSTERFSAPI_LIBRARIES})
endif(WITH_GLUSTERFS)

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

# Install
install(TARGETS oneclient DESTINATION ${CMAKE_INSTALL_FULL_BINDIR})
install(DIRECTORY config/ DESTINATION ${CMAKE_INSTALL_FULL_SYSCONFDIR})
install(FILES man/oneclient.1 DESTINATION ${CMAKE_INSTALL_FULL_MANDIR}/man1)
install(FILES man/oneclient.conf.5 DESTINATION ${CMAKE_INSTALL_FULL_MANDIR}/man5)
install(FILES LICENSE.txt DESTINATION ${CMAKE_INSTALL_FULL_DOCDIR})
install(FILES README.md DESTINATION ${CMAKE_INSTALL_FULL_DOCDIR})
if(UNIX AND NOT APPLE)
    install(DIRECTORY autocomplete/linux/ DESTINATION ${CMAKE_INSTALL_FULL_LOCALSTATEDIR}/lib/oneclient)
endif(UNIX AND NOT APPLE)
if(APPLE)
    install(DIRECTORY autocomplete/osx/ DESTINATION ${CMAKE_INSTALL_FULL_LOCALSTATEDIR}/lib/oneclient)
endif(APPLE)

find_program(
    CLANG_TIDY
    NAMES "run-clang-tidy"
    DOC "Path to run-clang-tidy script")

if(CLANG_TIDY)
    include(ProcessorCount)
    ProcessorCount(CLANG_TIDY_PARALLEL_JOBS)
    message(STATUS
        "run-clang-tidy script found: ${CLANG_TIDY} - adding target clang-tidy")
    set(CLANG_TIDY_SOURCE_FILTER "src/*.cc")
    file(GLOB_RECURSE CLANG_TIDY_SOURCES
         "${CMAKE_CURRENT_SOURCE_DIR}/${CLANG_TIDY_SOURCE_FILTER}")
    add_custom_target(clang-tidy COMMAND ${CLANG_TIDY}
        -export-fixes clang-tidy-suggested-fixes.yaml
        -j ${CLANG_TIDY_PARALLEL_JOBS}
        -extra-arg=-DTBB_USE_GLIBCXX_VERSION=50400
        ${CLANG_TIDY_SOURCES})
else(CLANG_TIDY)
    message(STATUS "run-clang-tidy script not found - target clang-tidy not available")
endif(CLANG_TIDY)


add_subdirectory(bench)

enable_testing()
add_subdirectory(test)
