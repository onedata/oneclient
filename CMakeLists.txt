cmake_minimum_required(VERSION 3.7.0)
project(oneclient C CXX)

option(CODE_COVERAGE "Enable code coverage (gcc only)." OFF)
option(STATIC_BOOST "Link Boost statically" OFF)
option(STATIC_LIBSTDCPP "Link libstdc++ statically" OFF)
option(WITH_CEPH "Include Ceph direct IO support" ON)
option(WITH_S3 "Include S3 direct IO support" ON)
option(WITH_SWIFT "Include Swift direct IO support" ON)
option(WITH_GLUSTERFS "Include GlusterFS direct IO support" ON)
option(WITH_WEBDAV "Include WebDAV and HTTP direct IO support" ON)
option(WITH_XROOTD "Include XRootD direct IO support" ON)
option(WITH_NFS "Include NFS direct IO support" ON)
option(WITH_ONECLIENT "Include oneclient binary" ON)
option(WITH_ONEBENCH "Include onebench binary" ON)
option(WITH_ONEDATAFS "Build onedatafs library" ON)
option(WITH_PYTHON2 "Build onedatafs for Python2" OFF)
option(WITH_PYTHON3 "Build onedatafs for Python3" ON)
option(WITH_ONES3 "Build ones3 binary" ON)

option(WITH_LIBDL "Link with libdl" ON)
option(WITH_LIBRT "Link with librt" ON)
option(WITH_TESTS "Build and run tests" ON)
option(ONEDATAFS_PYTHON_VERSION "Build onedatafs for specific Python version" "")
option(GIT_VERSION "Version of the Oneclient" "1.0.0")
option(FETCH_FOLLY "Fetch folly, fizz, wangle and proxygen and build in-place" OFF)

string(TOLOWER ${CMAKE_BUILD_TYPE} CMAKE_BUILD_TYPE_LOWERCASE)
message(STATUS "Build type: ${CMAKE_BUILD_TYPE_LOWERCASE}")

# CMake config
set(CMAKE_SKIP_INSTALL_ALL_DEPENDENCY True)
set(CMAKE_FIND_FRAMEWORK LAST)
set(CMAKE_POSITION_INDEPENDENT_CODE True)
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake ${PROJECT_SOURCE_DIR}/helpers/cmake)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(WITH_FUSE_VERSION "3" CACHE STRING "Select Fuse version (2 or 3)")
set(CONDA_BUILD_PREFIX "" CACHE STRING "Conda build prefix")

# Maintain compatibility for ExternalProject_Add with older cmake versions
if(CMAKE_MAJOR_VERSION GREATER_EQUAL 3 AND CMAKE_MINOR_VERSION GREATER_EQUAL 19)
cmake_policy(SET CMP0114 OLD)
endif()

set(LIB_INSTALL_DIR lib CACHE INTERNAL "")

include(FindLibraryUtils)
include(GNUInstallDirs)

# Version
include(version.txt OPTIONAL)
file(MAKE_DIRECTORY ${PROJECT_BINARY_DIR}/version)
configure_file(include/version.h.in ${PROJECT_BINARY_DIR}/version/version.h)

# Deps
add_subdirectory(deps/libmacaroons)
add_subdirectory(deps/libmacaroons-cpp)

if(CMAKE_BUILD_TYPE_LOWERCASE MATCHES "debug")
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(DW REQUIRED IMPORTED_TARGET libdw)
    set(BACKWARD_CPP_LIBRARIES PkgConfig::DW)
    add_definitions(-DENABLE_BACKWARD_CPP)
    message(STATUS "Enabled backward-cpp with libraries: ${BACKWARD_CPP_LIBRARIES}")
endif()

# Setup fuse, on Linux default required version is >= 3
# On other platforms it is not yet supported
if(WITH_FUSE_VERSION MATCHES "2")
    message(STATUS "Checking for FUSE 2...")
    find_package(FUSE 2 REQUIRED)
    set(FUSE_USE_VERSION 30)
else()
    message(STATUS "Checking for FUSE 3...")
    SET(FUSE_FIND_VERSION 3)
    find_package(FUSE 3 REQUIRED)
    if(FUSE_VERSION_MINOR LESS 10)
        set(FUSE_USE_VERSION 31)
    else()
        set(FUSE_USE_VERSION 34)
    endif()
endif()
message(STATUS "Found Fuse version: ${FUSE_VERSION}")
message(STATUS "FUSE_USE_VERSION: ${FUSE_USE_VERSION}")

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
find_package(OpenSSL 1.1.0 REQUIRED)

# Setup NSS
message(STATUS "Checking for NSS...")
if(NOT (CONDA_BUILD_PREFIX STREQUAL ""))
    set(NSS_INCLUDE_DIRS "${CONDA_BUILD_PREFIX}/include/nss")
    set(NSS_LIBRARIES "${CONDA_BUILD_PREFIX}/lib/libssl3.so;${CONDA_BUILD_PREFIX}/lib/libsmime3.so;${CONDA_BUILD_PREFIX}/lib/libnss3.so;${CONDA_BUILD_PREFIX}/lib/libnssutil3.so;${CONDA_BUILD_PREFIX}/lib/libnspr4.so")
endif()
find_package(NSS REQUIRED)

# Setup POCO
find_library(POCO_UTIL PocoUtil)
find_library(POCO_NET PocoNet)
find_library(POCO_NET_SSL PocoNetSSL)
find_library(POCO_XML PocoXML)
find_library(POCO_JSON PocoJSON)
find_library(POCO_FOUNDATION PocoFoundation)
set(POCO_LIBRARIES
        ${POCO_UTIL}
        ${POCO_NET}
        ${POCO_NET_SSL}
        ${POCO_XML}
        ${POCO_JSON}
        ${POCO_FOUNDATION})

# Setup Folly
option(FOLLY_SHARED "Link folly shared library" ON)
message(STATUS "Checking for Folly...")
find_library(LIBEVENT_LIBRARY event)
find_library(IBERTY_LIBRARY iberty)
find_library(DOUBLE_CONVERSION_LIBRARY double-conversion)
if(NOT FETCH_FOLLY)
    find_package(Folly REQUIRED)
    find_package(Wangle REQUIRED)
    find_package(Fizz REQUIRED)
    find_library(PROXYGEN_LIBRARY proxygen REQUIRED)
    set(FOLLY_LIBRARIES
        ${FOLLY_LIBRARIES}
        ${WANGLE_LIBRARIES}
        ${LIBEVENT_LIBRARY}
        ${IBERTY_LIBRARY}
        ${FIZZ_LIBRARY}
        ${PROXYGEN_LIBRARY}
        ${DOUBLE_CONVERSION_LIBRARY})
    if(NOT FOLLY_SHARED)
        list(APPEND FOLLY_LIBRARIES unwind gflags)
    endif(NOT FOLLY_SHARED)
else(FETCH_FOLLY)
    include(ExternalProject)
    ExternalProject_Add(folly
        GIT_REPOSITORY    https://github.com/facebook/folly.git
        GIT_TAG           v2021.01.04.00
        INSTALL_DIR       ${CMAKE_BINARY_DIR}/contrib
        CMAKE_ARGS        "-DCMAKE_CXX_FLAGS='-D__STDC_FORMAT_MACROS=1 -fPIC';-DCMAKE_C_FLAGS='-Wl,-lrt';-DCMAKE_EXE_LINKER_FLAGS='-lrt';-DCMAKE_EXE_LINKER_FLAGS='-lrt';-DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/contrib"
    )
    ExternalProject_Add_StepTargets(folly install)
    ExternalProject_Add(fizz
        GIT_REPOSITORY    https://github.com/facebookincubator/fizz.git
        GIT_TAG           v2021.01.04.00
        SOURCE_SUBDIR     fizz
        DEPENDS           folly-install
        INSTALL_DIR       ${CMAKE_BINARY_DIR}/contrib
        CMAKE_ARGS        "-DCMAKE_CXX_FLAGS='-D__STDC_FORMAT_MACROS=1 -fPIC';-DBUILD_TESTS=OFF;-DCMAKE_PREFIX_PATH=${CMAKE_BINARY_DIR}/contrib;-DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/contrib"
    )
    ExternalProject_Add_StepTargets(fizz install)
    ExternalProject_Add(wangle
        GIT_REPOSITORY    https://github.com/facebook/wangle.git
        GIT_TAG           v2021.01.04.00
        SOURCE_SUBDIR     wangle
        DEPENDS           fizz-install
        INSTALL_DIR       ${CMAKE_BINARY_DIR}/contrib
        CMAKE_ARGS        "-DCMAKE_CXX_FLAGS='-D__STDC_FORMAT_MACROS=1 -fPIC';-DCMAKE_PREFIX_PATH=${CMAKE_BINARY_DIR}/contrib;-DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/contrib"
    )
    ExternalProject_Add_StepTargets(wangle install)
    ExternalProject_Add(proxygen
        GIT_REPOSITORY    https://github.com/facebook/proxygen.git
        GIT_TAG           v2021.01.04.00
        DEPENDS           wangle-install
        INSTALL_DIR       ${CMAKE_BINARY_DIR}/contrib
        CMAKE_ARGS        "-DCMAKE_CXX_FLAGS='-D__STDC_FORMAT_MACROS=1 -fPIC';-DCMAKE_PREFIX_PATH=${CMAKE_BINARY_DIR}/contrib;-DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/contrib"
    )
    ExternalProject_Add_StepTargets(proxygen install)
    set(FOLLY_INCLUDE_DIR ${CMAKE_BINARY_DIR}/contrib/include)
    set(FOLLY_LIBRARIES
        ${CMAKE_BINARY_DIR}/contrib/lib/libfolly.a
        ${CMAKE_BINARY_DIR}/contrib/lib/libfollybenchmark.a
        ${CMAKE_BINARY_DIR}/contrib/lib/libwangle.a
        ${LIBEVENT_LIBRARY}
        ${IBERTY_LIBRARY}
        ${CMAKE_BINARY_DIR}/contrib/lib/libfizz.a
        ${CMAKE_BINARY_DIR}/contrib/lib/libproxygen.a
        ${DOUBLE_CONVERSION_LIBRARY}
        gflags
        glog)
endif(NOT FETCH_FOLLY)

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

# Find which Python interpreters are available
if(WITH_PYTHON2)
  find_package(Python2)
endif(WITH_PYTHON2)

if(WITH_PYTHON3)
  find_package(Python3)
endif(WITH_PYTHON3)

# Setup helpers
set(HELPERS_DIR ${CMAKE_CURRENT_SOURCE_DIR}/helpers)

# Setup Asio
set(ASIO_INCLUDE_DIRS ${HELPERS_DIR}/deps/asio/asio/include)
# Setup cppmetrics
set(CPPMETRICS_INCLUDE_DIRS ${HELPERS_DIR}/deps/cppmetrics)
# Setup backward-cpp
set(BACKWARD_CPP_INCLUDE_DIRS ${HELPERS_DIR}/deps/backward-cpp)

# Setup spdlog and fmt
if(FOLLY_SHARED)
    find_library(FMT_LIBRARY NAMES libfmt.a fmt REQUIRED)
    find_library(SPDLOG_LIBRARY NAMES libspdlog.a spdlog REQUIRED)
    add_definitions(-DSPDLOG_COMPILED_LIB -DSPDLOG_FMT_EXTERNAL)
else(FOLLY_SHARED)
    find_library(FMT_LIBRARY NAMES fmt libfmt.a REQUIRED)
    find_library(SPDLOG_LIBRARY NAMES spdlog libspdlog.a REQUIRED)
endif(FOLLY_SHARED)

# Utility libs
find_library(LTDL_LIBRARY NAMES ltdl)
find_library(ZLIB_LIBRARY NAMES z)

if(WITH_LIBRT)
find_library(RT_LIBRARY rt)
endif(WITH_LIBRT)

if(WITH_LIBDL)
find_library(DL_LIBRARY dl)
endif(WITH_LIBDL)

# Rados library
if(WITH_CEPH)
    find_library(CEPHCOMMON_LIBRARY "ceph-common" NAMES "ceph-common" "libceph-common.so.2" "libceph-common.so.0" PATH_SUFFIXES "ceph")
    find_library(RADOS_LIBRARY rados)
    find_library(RADOSSTRIPER_LIBRARY radosstriper)
    get_filename_component(CEPHCOMMON_RPATH "${CEPHCOMMON_LIBRARY}" DIRECTORY)
endif(WITH_CEPH)

# AWS SDK library
if(WITH_S3)
    find_library(AWS_SDK_C_COMMON_LIBRARY aws-c-common HINTS /usr/lib/x86_64-linux-gnu)
    find_library(AWS_SDK_CHECKSUMS_LIBRARY aws-checksums HINTS /usr/lib/x86_64-linux-gnu)
    find_library(AWS_SDK_C_EVENT_STREAM_LIBRARY aws-c-event-stream HINTS /usr/lib/x86_64-linux-gnu)
    find_library(AWS_SDK_CORE_LIBRARY aws-cpp-sdk-core HINTS /usr/lib/x86_64-linux-gnu)
    find_library(AWS_SDK_S3_LIBRARY aws-cpp-sdk-s3 HINTS /usr/lib/x86_64-linux-gnu)
    set(AWS_SDK_LIBRARIES
        ${AWS_SDK_S3_LIBRARY}
        ${AWS_SDK_CORE_LIBRARY}
        ${AWS_SDK_C_COMMON_LIBRARY}
        ${AWS_SDK_CHECKSUMS_LIBRARY}
        ${AWS_SDK_C_EVENT_STREAM_LIBRARY}
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
        pkg_check_modules(GLUSTERFSAPI glusterfs-api>=6.0)
        if(NOT GLUSTERFSAPI_FOUND EQUAL 1)
            message(SEND_ERROR "pkg-config for glusterfs-api is missing.")
        else()
            add_definitions(-DWITH_GLUSTERFS=1)
            link_directories(${GLUSTERFSAPI_LIBDIR})
	    message(STATUS "GLUSTERFSAPI_LIBDIR=${GLUSTERFSAPI_LIBDIR}")
	    message(STATUS "GLUSTERFSAPI_LIBRARIES=${GLUSTERFSAPI_LIBRARIES}")
	    message(STATUS "GLUSTERFSAPI_LINK_LIBRARIES=${GLUSTERFSAPI_LINK_LIBRARIES}")
        endif()
    endif (PKG_CONFIG_FOUND)
else(WITH_GLUSTERFS)
    add_definitions(-DWITH_GLUSTERFS=0)
endif(WITH_GLUSTERFS)

# WebDAV libraries
if(WITH_WEBDAV)
    add_definitions(-DWITH_WEBDAV=1)
else(WITH_WEBDAV)
    add_definitions(-DWITH_WEBDAV=0)
endif(WITH_WEBDAV)

if(WITH_XROOTD)
    include(/usr/share/xrootd/cmake/XRootDConfig.cmake OPTIONAL)
    include(XRootDConfig OPTIONAL)
    find_package(XRootD REQUIRED)
    add_definitions(-DWITH_XROOTD=1)
else(WITH_XROOTD)
    add_definitions(-DWITH_XROOTD=0)
endif(WITH_XROOTD)

# NFS libraries
if(WITH_NFS)
    find_package(NFS REQUIRED)
    add_definitions(-DWITH_NFS=1)
else(WITH_NFS)
    add_definitions(-DWITH_NFS=0)
endif(WITH_NFS)


# gpertools lib
find_library(PROFILER_LIBRARY NAMES profiler)

# Set up sources
file(GLOB_RECURSE SOURCES src/*.cc include/*.h)
set(MAIN_SOURCE_FILE ${CMAKE_CURRENT_SOURCE_DIR}/src/main.cc)
list(REMOVE_ITEM SOURCES ${MAIN_SOURCE_FILE})
list(REMOVE_ITEM SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/src/onedatafs.cc)

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
add_compile_options(${SECTION_FRAGMENTATION_FLAGS} -Wall -Wno-unused-result)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14 -fdiagnostics-color")

add_definitions(
    ${FUSE_DEFINITIONS}
    -DFUSE_USE_VERSION=${FUSE_USE_VERSION}
    -DBOOST_FILESYSTEM_NO_DEPRECATED
    -DBOOST_ALL_DYN_LINK
    -DASIO_STANDALONE
    -DBUILD_PROXY_IO
    -DGLOG_STL_LOGGING_FOR_UNORDERED
    -DFIBER_STACK_SIZE=1048576)

if(WITH_CEPH)
    add_definitions(-DWITH_CEPH=1)
    list(APPEND CUSTOM_RPATH "${CEPHCOMMON_RPATH}")
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
    ${OPENSSL_INCLUDE_DIR}
    ${BACKWARD_CPP_INCLUDE_DIRS})

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
    ${OPENSSL_LIBRARIES}
    ${BACKWARD_CPP_LIBRARIES})

if(WITH_CEPH)
    list(APPEND CLIENT_LIBRARIES ${RADOSSTRIPER_LIBRARY} ${RADOS_LIBRARY} ${CEPHCOMMON_LIBRARY})
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

if(WITH_WEBDAV)
    list(APPEND CLIENT_LIBRARIES ${PROXYGEN_LIBRARY} ${WANGLE_LIBRARIES})
endif(WITH_WEBDAV)

if(WITH_XROOTD)
    list(APPEND CLIENT_LIBRARIES ${XROOTD_LIBRARIES})
endif(WITH_XROOTD)

if(WITH_NFS)
    list(APPEND CLIENT_LIBRARIES ${NFS_LIBRARIES})
endif(WITH_NFS)

list(APPEND CLIENT_LIBRARIES ${SPDLOG_LIBRARY} ${FMT_LIBRARY})

# Define targets
add_library(client OBJECT ${SOURCES} ${HEADERS})
if(FETCH_FOLLY)
add_dependencies(helpers proxygen-install)
endif(FETCH_FOLLY)
add_dependencies(client helpers clproto)

if(WITH_ONECLIENT)
set(CLIENT_SOURCES
    ${MAIN_SOURCE_FILE}
    $<TARGET_OBJECTS:client>
    ${PROJECT_SOURCES})
endif(WITH_ONECLIENT)

if(WITH_ONEBENCH)
    add_subdirectory(bench)
endif(WITH_ONEBENCH)

if(WITH_ONES3)
    add_subdirectory(s3)
endif(WITH_ONES3)

if(WITH_ONEDATAFS)
  set(ONEDATAFS_SOURCES
      ${CMAKE_CURRENT_SOURCE_DIR}/src/onedatafs.cc
      $<TARGET_OBJECTS:client>
      ${PROJECT_SOURCES})

  if(PYTHON2_FOUND)
    set(CMAKE_SHARED_LIBRARY_PREFIX "")
    add_library(onedatafs.py2 SHARED ${ONEDATAFS_SOURCES})
    target_compile_definitions(onedatafs.py2 PRIVATE ONEDATAFS)
    target_link_libraries(onedatafs.py2 PRIVATE ${CLIENT_LIBRARIES} ${LIBBOOST_PYTHON2})
    set_target_properties(onedatafs.py2 PROPERTIES OUTPUT_NAME "onedatafs_py2")
    set_target_properties(onedatafs.py2 PROPERTIES COMPILE_FLAGS ${PYTHON2_CFLAGS})
    set_target_properties(onedatafs.py2 PROPERTIES LINK_FLAGS ${PYTHON2_LDFLAGS})
    set_target_properties(onedatafs.py2 PROPERTIES VERSION ${GIT_VERSION}
      SOVERSION ${GIT_VERSION})
    set_target_properties(onedatafs.py2 PROPERTIES
      BUILD_WITH_INSTALL_RPATH true
      INSTALL_RPATH_USE_LINK_PATH true
      INSTALL_RPATH "${CUSTOM_RPATH}")
  endif(PYTHON2_FOUND)

  if(PYTHON3_FOUND)
    set(CMAKE_SHARED_LIBRARY_PREFIX "")
    add_library(onedatafs.py3 SHARED ${ONEDATAFS_SOURCES})
    target_compile_definitions(onedatafs.py3 PUBLIC ONEDATAFS)
    target_link_libraries(onedatafs.py3 PRIVATE ${CLIENT_LIBRARIES} ${LIBBOOST_PYTHON3})
    set_target_properties(onedatafs.py3 PROPERTIES OUTPUT_NAME "onedatafs_py3")
    set_target_properties(onedatafs.py3 PROPERTIES COMPILE_FLAGS ${PYTHON3_CFLAGS})
    set_target_properties(onedatafs.py3 PROPERTIES LINK_FLAGS ${PYTHON3_LDFLAGS})
    set_target_properties(onedatafs.py3 PROPERTIES VERSION ${GIT_VERSION}
      SOVERSION ${GIT_VERSION})
    set_target_properties(onedatafs.py3 PROPERTIES
      BUILD_WITH_INSTALL_RPATH true
      INSTALL_RPATH_USE_LINK_PATH true
      INSTALL_RPATH "${CUSTOM_RPATH}")
  endif(PYTHON3_FOUND)
endif(WITH_ONEDATAFS)

if(WITH_ONECLIENT)
add_executable(oneclient ${CLIENT_SOURCES})
target_link_libraries(oneclient PRIVATE ${CLIENT_LIBRARIES})
set_target_properties(oneclient PROPERTIES
    BUILD_WITH_INSTALL_RPATH true
    INSTALL_RPATH_USE_LINK_PATH true
    INSTALL_RPATH "${CUSTOM_RPATH}")

if(NOT ${PROFILER_LIBRARY} MATCHES PROFILER_LIBRARY-NOTFOUND)
    target_link_libraries(oneclient PRIVATE ${PROFILER_LIBRARY})
endif(NOT ${PROFILER_LIBRARY} MATCHES PROFILER_LIBRARY-NOTFOUND)
endif(WITH_ONECLIENT)

# Install
if(WITH_ONECLIENT)
install(TARGETS oneclient DESTINATION ${CMAKE_INSTALL_FULL_BINDIR})
install(FILES man/oneclient.1 DESTINATION ${CMAKE_INSTALL_FULL_MANDIR}/man1)
endif(WITH_ONECLIENT)
install(FILES man/oneclient.conf.5 DESTINATION ${CMAKE_INSTALL_FULL_MANDIR}/man5)
install(FILES LICENSE.txt DESTINATION ${CMAKE_INSTALL_FULL_DOCDIR})
install(FILES README.md DESTINATION ${CMAKE_INSTALL_FULL_DOCDIR})
install(DIRECTORY config/ DESTINATION ${CMAKE_INSTALL_FULL_SYSCONFDIR})
if(WITH_ONEDATAFS)
  if(PYTHON2_FOUND)
    if($ENV{CONDA_BUILD})
      set(PYTHON2_INSTALL_DIR ${CMAKE_INSTALL_LIBDIR}/python${PYTHON2_VERSION_STRING}/${PYTHON2_SITE_DIR})
    else()
      set(PYTHON2_INSTALL_DIR ${PYTHON2_EXEC_PREFIX}/${LIB_INSTALL_DIR}/python${PYTHON2_VERSION_STRING}/${PYTHON2_SITE_DIR})
    endif()
    install(TARGETS onedatafs.py2 DESTINATION ${PYTHON2_INSTALL_DIR}/onedatafs)
    install(FILES onedatafs/python/__init__.py DESTINATION ${PYTHON2_INSTALL_DIR}/onedatafs)
  endif(PYTHON2_FOUND)
  if(PYTHON3_FOUND)
    if($ENV{CONDA_BUILD})
      set(PYTHON3_INSTALL_DIR ${CMAKE_INSTALL_LIBDIR}/python${PYTHON3_VERSION_STRING}/${PYTHON3_SITE_DIR})
    else()
      set(PYTHON3_INSTALL_DIR ${PYTHON3_EXEC_PREFIX}/${LIB_INSTALL_DIR}/python${PYTHON3_VERSION_STRING}/${PYTHON3_SITE_DIR})
    endif()
    install(TARGETS onedatafs.py3 DESTINATION ${PYTHON3_INSTALL_DIR}/onedatafs)
    install(FILES onedatafs/python/__init__.py DESTINATION ${PYTHON3_INSTALL_DIR}/onedatafs)
  endif(PYTHON3_FOUND)
endif(WITH_ONEDATAFS)

if(WITH_ONES3)
    install(TARGETS ones3 DESTINATION ${CMAKE_INSTALL_FULL_BINDIR})
endif(WITH_ONES3)

if(WITH_ONECLIENT)
  if(UNIX AND NOT APPLE)
      install(DIRECTORY autocomplete/linux/ DESTINATION ${CMAKE_INSTALL_FULL_LOCALSTATEDIR}/lib/oneclient)
  endif(UNIX AND NOT APPLE)
  if(APPLE)
      install(DIRECTORY autocomplete/osx/ DESTINATION ${CMAKE_INSTALL_FULL_LOCALSTATEDIR}/lib/oneclient)
  endif(APPLE)
endif(WITH_ONECLIENT)

find_program(
    CLANG_TIDY
    NAMES "run-clang-tidy"
    DOC "Path to run-clang-tidy script")

if(CLANG_TIDY)
    include(ProcessorCount)
    ProcessorCount(CLANG_TIDY_PARALLEL_JOBS)
    message(STATUS
        "run-clang-tidy script found: ${CLANG_TIDY} - adding target clang-tidy")
    set(CLANG_TIDY_ONECLIENT_SOURCE_FILTER "src/*.cc")
    set(CLANG_TIDY_ONES3_SOURCE_FILTER "s3/*.cc")
    file(GLOB_RECURSE CLANG_TIDY_ONECLIENT_SOURCES
         "${CMAKE_CURRENT_SOURCE_DIR}/${CLANG_TIDY_ONECLIENT_SOURCE_FILTER}")
    file(GLOB_RECURSE CLANG_TIDY_ONES3_SOURCES
            "${CMAKE_CURRENT_SOURCE_DIR}/${CLANG_TIDY_ONES3_SOURCE_FILTER}")
    add_custom_target(clang-tidy COMMAND ${CLANG_TIDY}
        -export-fixes clang-tidy-suggested-fixes.yaml
        -j ${CLANG_TIDY_PARALLEL_JOBS}
        ${CLANG_TIDY_ONECLIENT_SOURCES} ${CLANG_TIDY_ONES3_SOURCES})
else(CLANG_TIDY)
    message(STATUS "run-clang-tidy script not found - target clang-tidy not available")
endif(CLANG_TIDY)

if(WITH_TESTS)
enable_testing()
add_subdirectory(test)
endif(WITH_TESTS)
