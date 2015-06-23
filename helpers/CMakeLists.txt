cmake_minimum_required(VERSION 3.0.0)
project(helpers C CXX)

# The version number.
set(helpers_VERSION_MAJOR 0)
set(helpers_VERSION_MINOR 2)
set(helpers_VERSION_PATCH 0)
set(helpers_VERSION $(helpers_VERSION_MAJOR).$(helpers_VERSION_MINOR).$(helpers_VERSION_PATCH))

# CMake config
set(CMAKE_FIND_FRAMEWORK LAST)
set(CMAKE_POSITION_INDEPENDENT_CODE True)
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)

# Setup OpenSSL
message(STATUS "Checking for OpenSSL...")
find_package(OpenSSL REQUIRED)

# Setup pthreads
message(STATUS "Checking for pthreads...")
find_package(Threads REQUIRED)

# Setup fuse
message(STATUS "Checking for FUSE...")
find_package(FUSE REQUIRED)

# Setup Intel TBB
message(STATUS "Checking for Intel TBB...")
find_package(TBB REQUIRED)

# Setup glog
message(STATUS "Checking for glog...")
find_package(Glog REQUIRED)

# Setup Boost
set(Boost_USE_STATIC_LIBS       OFF)
set(Boost_USE_MULTITHREADED      ON)
set(Boost_USE_STATIC_RUNTIME    OFF)
find_package(Boost COMPONENTS coroutine filesystem system thread REQUIRED)

# Utility libs
find_library(LTDL_LIBRARY ltdl)
find_library(ZLIB_LIBRARY z)
find_library(RT_LIBRARY rt)
find_library(DL_LIBRARY dl)

# Setup compile flags
set(PLATFORM_EXTRA_LIBS
    ${CMAKE_THREAD_LIBS_INIT}
    ${LTDL_LIBRARY}
    ${ZLIB_LIBRARY}
    ${DL_LIBRARY})

if(APPLE)
    set(SECTION_FRAGMENTATION_FLAGS -Wno-deprecated-declarations)
    set(CUSTOM_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
else(NOT APPLE)
    set(PLATFORM_EXTRA_LIBS ${PLATFORM_EXTRA_LIBS} ${RT_LIBRARY})
    set(SECTION_FRAGMENTATION_FLAGS -fdata-sections -ffunction-sections)
    set(CUSTOM_RPATH "${CMAKE_INSTALL_PREFIX}/lib" "\$ORIGIN")
endif(APPLE)

add_compile_options(${SECTION_FRAGMENTATION_FLAGS} -Wall -std=c++14)
add_definitions(
    ${FUSE_DEFINITIONS}
    -DFUSE_USE_VERSION=29
    -DBOOST_BIND_NO_PLACEHOLDERS
    -DBOOST_THREAD_VERSION=4
    -DBOOST_THREAD_PROVIDES_EXECUTORS
    -DBOOST_CHRONO_HEADER_ONLY)

include_directories(SYSTEM
    ${FUSE_INCLUDE_DIRS}
    ${FUSE_INCLUDE_DIRS}/..
    ${PROTOBUF_INCLUDE_DIR}
    ${OPENSSL_INCLUDE_DIR}
    ${TBB_INCLUDE_DIRS}
    ${Boost_INCLUDE_DIRS})

add_subdirectory(object)
include_directories(${HELPERS_INCLUDE_DIRS})
include_directories(SYSTEM ${HELPERS_SYSTEM_INCLUDE_DIRS})

file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/dummy.cc "")
set(HELPERS_SOURCES ${CMAKE_CURRENT_BINARY_DIR}/dummy.cc ${PROJECT_SOURCES})

# Define targets
add_library(helpersShared SHARED ${HELPERS_SOURCES})
target_link_libraries(helpersShared PUBLIC ${HELPERS_LIBRARIES})
set_target_properties(helpersShared PROPERTIES
    OUTPUT_NAME helpers
    BUILD_WITH_INSTALL_RPATH true
    INSTALL_RPATH_USE_LINK_PATH true
    INSTALL_RPATH "${CUSTOM_RPATH}")

install(TARGETS helpersShared LIBRARY DESTINATION lib)

enable_testing()
add_subdirectory(test)
