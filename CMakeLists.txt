cmake_minimum_required(VERSION 2.8.0)
project(oneclient C CXX)

# The version number.
set(oneclient_VERSION_MAJOR 0)
set(oneclient_VERSION_MINOR 4)
set(oneclient_VERSION_PATCH 1)
set(oneclient_VERSION $(oneclient_VERSION_MAJOR).$(oneclient_VERSION_MINOR).$(oneclient_VERSION_PATCH))

# CMake config
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR})
# set(CMAKE_FIND_LIBRARY_SUFFIXES .a) # Uncomment in order to force looking for static achieves instead .so

configure_file (
    include/version.h.in
    ${PROJECT_BINARY_DIR}/version.h
)

# Deps
add_subdirectory(deps/glog)
add_subdirectory(deps/gmock)
add_subdirectory(deps/json11)
add_subdirectory(deps/helpers)

link_directories(${PROJECT_BINARY_DIR})

# Include paths
include_directories(
    include
    ${PROJECT_BINARY_DIR}
    ${HELPERS_INCLUDE_DIRS})
include_directories(SYSTEM
    include/fuse
    ${GLOG_INCLUDE_DIR}
    ${JSON11_INCLUDE_DIR})

# Setup fuse
message(STATUS "Checking for FUSE...")
find_package(FUSE REQUIRED)
#find_library(FUSE_LIBRARY_CUSTOM NAMES libfuse.a fuse)
set(FUSE_LIBRARY_CUSTOM ${FUSE_LIBRARIES})
include_directories(SYSTEM ${FUSE_INCLUDE_DIRS}/.. ${FUSE_INCLUDE_DIRS})
message(STATUS "FUSE: ${FUSE_LIBRARY_CUSTOM}")

# Setup ProtoBuf
message(STATUS "Checking for Protobuf...")
find_package(Protobuf REQUIRED)
find_library(PROTOBUF_LIBRARY_CUSTOM NAMES libprotobuf.a protobuf)
include_directories(SYSTEM ${PROTOBUF_INCLUDE_DIR})

# Setup OpenSSL
message(STATUS "Checking for OpenSSL...")
find_package(OpenSSL REQUIRED)
find_library(SSL_LIBRARY_S NAMES libssl.a ssl)
find_library(CRYPTO_LIBRARY_S NAMES libcrypto.a crypto)
find_library(SSL_LIBRARY_D NAMES ssl)
find_library(CRYPTO_LIBRARY_D NAMES crypto)
find_library(KRB5_LIBRARY krb5)
if(${KRB5_LIBRARY} MATCHES KRB5_LIBRARY-NOTFOUND)
    set(OPENSSL_LIBRARIES_S ${SSL_LIBRARY_S} ${CRYPTO_LIBRARY_S})
    set(OPENSSL_LIBRARIES_D ${SSL_LIBRARY_D} ${CRYPTO_LIBRARY_D})
else()
    set(OPENSSL_LIBRARIES_S ${SSL_LIBRARY_S} ${CRYPTO_LIBRARY_S} ${KRB5_LIBRARY})
    set(OPENSSL_LIBRARIES_D ${SSL_LIBRARY_D} ${CRYPTO_LIBRARY_D} ${KRB5_LIBRARY})
endif(${KRB5_LIBRARY} MATCHES KRB5_LIBRARY-NOTFOUND)
include_directories(SYSTEM ${OPENSSL_INCLUDE_DIR})

# Setup pthreads
message(STATUS "Checking for pthreads...")
find_package(Threads REQUIRED)

# Setup git
message(STATUS "Checking for Git...")
find_package(Git REQUIRED)

# Setup Boost
set(Boost_USE_STATIC_LIBS        ON)
set(Boost_USE_MULTITHREADED      ON)
set(Boost_USE_STATIC_RUNTIME    OFF)
find_package(Boost COMPONENTS system thread filesystem random program_options REQUIRED)
link_directories(${Boost_LIBRARY_DIRS})
include_directories(SYSTEM ${Boost_INCLUDE_DIRS})

# Utility libs
find_library(LTDL_LIBRARY NAMES libltdl.a ltdl)
find_library(RT_LIBRARY rt)
find_library(ZLIB_LIBRARY NAMES libz.a z)
find_library(DL_LIBRARY dl)

# gpertools lib
find_library(PROFILER_LIBRARY NAMES libprofiler.a)

# Pull git submodules
message(STATUS "Pulling submodules using git...")
execute_process(COMMAND ${GIT_EXECUTABLE} submodule init
                WORKING_DIRECTORY ${PROJECT_SOURCE_DIR})
execute_process(COMMAND ${GIT_EXECUTABLE} submodule update
                WORKING_DIRECTORY ${PROJECT_SOURCE_DIR})
execute_process(COMMAND ${GIT_EXECUTABLE} submodule foreach git checkout
                WORKING_DIRECTORY ${PROJECT_SOURCE_DIR})

# Compile .proto files
file(GLOB PROTO_FILES clproto/proto/*.proto)
PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS ${PROTO_FILES})
include_directories(${CMAKE_CURRENT_BINARY_DIR})

# Setup sources
file(GLOB_RECURSE SOURCES RELATIVE ${CMAKE_SOURCE_DIR} src/*.cc include/*.h)
set(MAIN_SOURCE_FILE src/main.cc)
list(REMOVE_ITEM SOURCES ${MAIN_SOURCE_FILE})

# Setup compile flags
execute_process(COMMAND ${CMAKE_CXX_COMPILER} -print-file-name=libstdc++.a OUTPUT_VARIABLE LIBSTD_NAME)
string(REGEX REPLACE "(\r?\n)+$" "" LIBSTD_NAME "${LIBSTD_NAME}")
execute_process(COMMAND ln -sf "${LIBSTD_NAME}" WORKING_DIRECTORY ${PROJECT_BINARY_DIR})

set(CUSTOM_RPATH "${CMAKE_INSTALL_PREFIX}/lib:\$ORIGIN:\$ORIGIN/../lib:lib:../lib")
set(PLATFORM_EXTRA_LIBS ${CMAKE_THREAD_LIBS_INIT} ${LTDL_LIBRARY} ${ZLIB_LIBRARY} ${DL_LIBRARY})

if(APPLE)
    set(SECTION_FRAGMENTATION_FLAGS "-Wno-deprecated-declarations")
else()
    set(PLATFORM_EXTRA_LIBS ${PLATFORM_EXTRA_LIBS} ${RT_LIBRARY})
    set(SECTION_FRAGMENTATION_FLAGS "-fdata-sections -ffunction-sections")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,--gc-sections -Wl,-rpath,${CUSTOM_RPATH}")
endif(APPLE)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${SECTION_FRAGMENTATION_FLAGS} -Wall -std=c++11")
add_definitions(${FUSE_DEFINITIONS} -DFUSE_USE_VERSION=29 -D_WEBSOCKETPP_CPP11_STL_ -DBOOST_BIND_NO_PLACEHOLDERS)

set(CMAKE_SKIP_RPATH TRUE) # Something is wrong with CMake RPATH config, we need to use old-fashion way

# Define targets
add_library(clproto ${PROTO_SRCS})
add_library(clientObjects OBJECT ${SOURCES} ${HEADERS})
add_executable(oneclient ${MAIN_SOURCE_FILE} $<TARGET_OBJECTS:clientObjects>)
add_executable(oneclient_dynamic ${MAIN_SOURCE_FILE} $<TARGET_OBJECTS:clientObjects>)

set_target_properties(oneclient_dynamic PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY dynamic/
    OUTPUT_NAME oneclient)

add_custom_command(TARGET oneclient_dynamic POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E create_symlink $<TARGET_FILE:oneclient_dynamic> oneclient_dynamic)

add_dependencies(clientObjects glog clproto helpers)
set(COMMON_LIBRARIES
    ${FUSE_LIBRARY_CUSTOM}
    ${Boost_LIBRARIES}
    ${GLOG_LIBRARIES}
    ${PROTOBUF_LIBRARY_CUSTOM}
    ${HELPERS_LIBRARIES}
    clproto
    json11
    ${PLATFORM_EXTRA_LIBS}
)

target_link_libraries(oneclient
    ${COMMON_LIBRARIES}
    ${OPENSSL_LIBRARIES_S}
)

target_link_libraries(oneclient_dynamic
    ${COMMON_LIBRARIES}
    ${OPENSSL_LIBRARIES_D}
)

target_link_libraries(clproto
    ${PROTOBUF_LIBRARY_CUSTOM}
)

if(NOT ${PROFILER_LIBRARY} MATCHES PROFILER_LIBRARY-NOTFOUND)
    target_link_libraries(oneclient ${PROFILER_LIBRARY})
    target_link_libraries(oneclient_dynamic ${PROFILER_LIBRARY})
endif(NOT ${PROFILER_LIBRARY} MATCHES PROFILER_LIBRARY-NOTFOUND)

# Install config
install(TARGETS oneclient_dynamic DESTINATION bin)
install(DIRECTORY config/ DESTINATION etc)

# build a CPack driven installer package
include(InstallRequiredSystemLibraries)
set(CPACK_SET_DESTDIR ON) # Uncomment this line if you need a non-relocatable RPM package
set(CPACK_RESOURCE_FILE_LICENSE ${CMAKE_SOURCE_DIR}/LICENSE.txt)
set(CPACK_PACKAGE_VERSION_MAJOR ${oneclient_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${oneclient_VERSION_MINOR})
set(CPACK_PACKAGE_CONTACT support@onedata.org)
set(CPACK_PACKAGE_FILE_NAME ${CMAKE_PROJECT_NAME}-${CMAKE_SYSTEM_NAME})

# DEB and RPM deps
set(CPACK_DEBIAN_PACKAGE_DEPENDS "fuse, openssl")
set(CPACK_RPM_PACKAGE_REQUIRES "fuse, openssl")

# autogenerate dependency information
# set (CPACK_DEBIAN_PACKAGE_SHLIBDEPS ON)
set(CPACK_RPM_PACKAGE_AUTOREQPROV " no") ## Uncomment ot get rid of automatic deps generation
set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "amd64")
include(CPack)

###########################
##         TESTS         ##
###########################

enable_testing()
include_directories(test/include ${HELPERS_TEST_INCLUDE_DIR})
include_directories(SYSTEM ${GTEST_INCLUDE_DIR} ${GMOCK_INCLUDE_DIR})

file(GLOB_RECURSE TEST_HEADERS test/include/*.h)

set(TEST_UTILS_DIR test/erl_core)
add_library(testCommon OBJECT
    test/testRunner.cc
    test/testCommon.cc
    ${TEST_UTILS_DIR}/erlTestCore.cc
)
add_dependencies(testCommon gmock gtest)

set(TEST_COMMON_LIBRARIES
    ${Boost_LIBRARIES}
    ${GLOG_LIBRARIES}
    ${GTEST_LIBRARIES}
    ${GMOCK_LIBRARIES}
    ${FUSE_LIBRARY_CUSTOM}
    ${HELPERS_LIBRARIES}
    ${PROTOBUF_LIBRARY_CUSTOM}
    ${OPENSSL_LIBRARIES}
    ${PLATFORM_EXTRA_LIBS}
    json11
    clproto
)

# Unit tests

add_custom_target(cunit)
macro(run_test test_target)
  string(REGEX REPLACE "_test" "" test_name "${test_target}")
  add_custom_target(${test_target}_runtest
      COMMAND ${test_target} --gtest_output="xml:cunit_results/TEST-${test_name}.xml"
      DEPENDS ${test_target}
      WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")
  add_dependencies(cunit ${test_target}_runtest)
endmacro()

file(GLOB TEST_SOURCES test/unit_tests/*_test.cc)
foreach(TEST_SRC ${TEST_SOURCES})
    get_filename_component(TEST_NAME ${TEST_SRC} NAME_WE)
    add_executable(${TEST_NAME} ${TEST_SRC} ${TEST_HEADERS} $<TARGET_OBJECTS:clientObjects> $<TARGET_OBJECTS:testCommon>)
    add_dependencies(${TEST_NAME} gtest gmock oneclient helpers clproto)
    target_link_libraries(${TEST_NAME} ${TEST_COMMON_LIBRARIES})
    add_test(${TEST_NAME} ${TEST_NAME})
    run_test(${TEST_NAME})
endforeach()


# Integration tests

file(GLOB erlc_files test/*/*.erl)
add_custom_target(build_erl_core
    COMMAND erlc -I ${TEST_UTILS_DIR} ${erlc_files}
    WORKING_DIRECTORY "${CMAKE_BINARY_DIR}"
)

add_custom_target(integration_tests)
macro(run_test_i test_target)
  string(REGEX REPLACE "_test" "" test_name "${test_target}")
  string(REGEX REPLACE "_test_i" "_test" beam_name "${test_target}")
  add_custom_target(${test_target}_runtest_i
      COMMAND ${TEST_UTILS_DIR}/run_test.escript ${beam_name} --gtest_output="xml:integration_results/TEST-${test_name}.xml"
      DEPENDS ${test_target} build_erl_core
      WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")
  add_dependencies(integration_tests ${test_target}_runtest_i)
endmacro()

file(GLOB ${TEST_SOURCES} test/integration_tests/*_test.cc)
foreach(TEST_SRC ${TEST_SOURCES})
    get_filename_component(TEST_NAME ${TEST_SRC} NAME_WE)
    set(TEST_NAME "${TEST_NAME}_i")
    add_executable(${TEST_NAME} ${TEST_SRC} ${TEST_HEADERS} $<TARGET_OBJECTS:clientObjects> $<TARGET_OBJECTS:testCommon>)
    add_dependencies(${TEST_NAME} gtest gmock oneclient helpers clproto)
    target_link_libraries(${TEST_NAME} ${TEST_COMMON_LIBRARIES})
    run_test_i(${TEST_NAME})
endforeach()
