cmake_minimum_required (VERSION 2.8.0)
project (VeilClient C CXX)

include(ExternalProject)

# The version number.
set(VeilClient_VERSION_MAJOR 0)
set(VeilClient_VERSION_MINOR 4)
set(VeilClient_VERSION_PATCH 1)
set(VeilClient_VERSION $(VeilClient_VERSION_MAJOR).$(VeilClient_VERSION_MINOR).$(VeilClient_VERSION_PATCH))

# Install dirs
set(CONFIG_DIR "etc")
set(BIN_DIR "bin")
set(LIB_DIR "lib")

# CMake config
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR})
set(DEPS_DIR "deps")
# set(CMAKE_FIND_LIBRARY_SUFFIXES .a) # Uncomment in order to force looking for static achives instead .so

string( TOLOWER "${CMAKE_BUILD_TYPE}" CMAKE_BUILD_TYPE_L )
if(CMAKE_BUILD_TYPE_L MATCHES "debug") 
    set(VeilClient_DEBUG 1)
else() 
    set(VeilClient_DEBUG 0)
endif() 

configure_file (
    "${PROJECT_SOURCE_DIR}/include/veilConfig.h.in"
    "${PROJECT_BINARY_DIR}/veilConfig.h"
)

# Deps
add_subdirectory(${DEPS_DIR}/glog)
add_subdirectory(${DEPS_DIR}/yaml-cpp)
add_subdirectory(${DEPS_DIR}/gmock)
add_subdirectory(${DEPS_DIR}/veilhelpers)

add_subdirectory(src/globus-proxy-utils)

link_directories(${PROJECT_BINARY_DIR})


# Include paths
include_directories(${PROJECT_SOURCE_DIR}/include)
include_directories(${PROJECT_SOURCE_DIR}/include/fuse)
include_directories(${PROJECT_BINARY_DIR})
include_directories(${GLOG_INCLUDE_DIR})
include_directories(${YAML_INCLUDE_DIR})
include_directories(${VEILHELPERS_INCLUDE_DIR})
include_directories(${PROJECT_SOURCE_DIR}/src/globus-proxy-utils)

# Setup fuse
message(STATUS "Checking for FUSE...")
find_package(FUSE REQUIRED)
find_library(FUSE_LIBRARY_CUSTOM NAMES libfuse.a fuse)
include_directories(${FUSE_INCLUDE_DIRS})
message(STATUS "FUSE: ${FUSE_LIBRARY_CUSTOM}") 

# Setup ProtoBuf
message(STATUS "Checking for Protobuf...")
find_package(Protobuf REQUIRED)
find_library(PROTOBUF_LIBRARY_CUSTOM NAMES libprotobuf.a protobuf)
include_directories(${PROTOBUF_INCLUDE_DIR})


# Setup OpenSSL
message(STATUS "Checking for OpenSSL...")
find_package(OpenSSL REQUIRED)
find_library(SSL_LIBRARY NAMES libssl.a ssl)
find_library(CRYPTO_LIBRARY NAMES libcrypto.a crypto)
find_library(KRB5_LIBRARY krb5)
if(${KRB5_LIBRARY} MATCHES KRB5_LIBRARY-NOTFOUND)
    set(OPENSSL_LIBRARIES "${SSL_LIBRARY};${CRYPTO_LIBRARY}")
else(${KRB5_LIBRARY} MATCHES KRB5_LIBRARY-NOTFOUND)
    set(OPENSSL_LIBRARIES "${SSL_LIBRARY};${CRYPTO_LIBRARY};${KRB5_LIBRARY}")
endif(${KRB5_LIBRARY} MATCHES KRB5_LIBRARY-NOTFOUND)
include_directories(${OPENSSL_INCLUDE_DIR})


# Setup pthreads
message(STATUS "Checking for pthreads...")
find_package(Threads REQUIRED)


# Setup git
message(STATUS "Checking for Git...")
find_package(Git REQUIRED)


# Setup Globus Toolkit
message(STATUS "Checking for Globus Toolkit...")

set(GLOBUS_LIBS globus_common;globus_oldgaa;globus_openssl;globus_gsi_callback;globus_gsi_cert_utils;globus_gsi_sysconfig;globus_proxy_ssl;globus_gsi_proxy_core;globus_gsi_credential;globus_openssl_error)

foreach(LIB ${GLOBUS_LIBS})
    find_library(GLOBUS_${LIB}_LIBRARY NAMES lib${LIB}.a ${LIB})
    if(${GLOBUS_${LIB}_LIBRARY} MATCHES .*_LIBRARY-NOTFOUND)
        message( FATAL_ERROR "ERROR: ${LIB} NOT FOUND!")
    endif()
    set(GLOBUS_DEPS ${GLOBUS_DEPS};${GLOBUS_${LIB}_LIBRARY})
endforeach()


# Setup Boost
set(Boost_USE_STATIC_LIBS        ON)
set(Boost_USE_MULTITHREADED      ON)
set(Boost_USE_STATIC_RUNTIME    OFF)
find_package(Boost COMPONENTS system thread filesystem random REQUIRED)

set(BOOST_LIBS boost_system boost_thread boost_filesystem boost_random)

foreach(LIB ${BOOST_LIBS})
    find_library(BOOST_${LIB}_LIBRARY NAMES lib${LIB}-mt.a lib${LIB}.a ${LIB}-mt ${LIB})
    if(${BOOST_${LIB}_LIBRARY} MATCHES .*_LIBRARY-NOTFOUND)
        message( FATAL_ERROR "ERROR: ${LIB} NOT FOUND!")
    endif()
    set(BOOST_DEPS ${BOOST_DEPS};${BOOST_${LIB}_LIBRARY})
endforeach()

set(Boost_LIBRARIES ${BOOST_DEPS})

link_directories(${Boost_LIBRARY_DIRS})
include_directories(${Boost_INCLUDE_DIRS})


# Utility libs
find_library(LTDL_LIBRARY libltdl.a ltdl)
find_library(RT_LIBRARY rt)
find_library(ZLIB_LIBRARY libz.a z)
find_library(DL_LIBRARY dl)

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
file(GLOB SOURCES "src/[a-zA-Z]*.cc")
file(GLOB MAIN_SOURCE_FILE "src/veilFuse.cc")
string(REGEX REPLACE "${MAIN_SOURCE_FILE}" "" SOURCES "${SOURCES}")

# Setup compile flags
execute_process(COMMAND g++ -print-file-name=libstdc++.a OUTPUT_VARIABLE LIBSTD_NAME)
string(REGEX REPLACE "(\r?\n)+$" "" LIBSTD_NAME "${LIBSTD_NAME}")
execute_process(COMMAND ln -sf "${LIBSTD_NAME}" WORKING_DIRECTORY ${PROJECT_BINARY_DIR})

if(APPLE)
    set(PLATFORM_EXTRA_LIBS "${CMAKE_THREAD_LIBS_INIT};${LTDL_LIBRARY};${ZLIB_LIBRARY};${DL_LIBRARY}")
    set(SECTION_FRAGMENTATION_FLAGS "-Wno-deprecated-declarations")
else()
    set(PLATFORM_EXTRA_LIBS "${CMAKE_THREAD_LIBS_INIT};${LTDL_LIBRARY};${ZLIB_LIBRARY};${DL_LIBRARY};${RT_LIBRARY}")
    set(SECTION_FRAGMENTATION_FLAGS "-fdata-sections -ffunction-sections -Wl,--gc-sections -Wl,-rpath,${CUSTOM_RPATH}")
endif()

set(CUSTOM_RPATH "${CMAKE_INSTALL_PREFIX}/${LIB_DIR}:${CMAKE_CURRENT_BINARY_DIR}:.:${LIB_DIR}:../${LIB_DIR}")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${SECTION_FRAGMENTATION_FLAGS} ${FUSE_DEFINITIONS} -DFUSE_USE_VERSION=29")
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Wall -O0 -g")
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O2")
set(CMAKE_SKIP_RPATH TRUE) # Something is wrong with CMake RPATH config, we need to use old-fashion way

# Define targets

add_library(veilprotocol ${PROTO_SRCS})
add_library(veilclient ${SOURCES})
add_executable(veilFuse ${MAIN_SOURCE_FILE})

add_dependencies(veilFuse yaml-cpp glog veilclient) 
add_dependencies(veilclient yaml-cpp glog veilprotocol veilhelpers) 
set_target_properties(veilFuse PROPERTIES COMPILE_FLAGS "-g")

target_link_libraries(veilprotocol
    ${PROTOBUF_LIBRARY_CUSTOM}
)

target_link_libraries(veilclient
    ${FUSE_LIBRARY_CUSTOM}
    ${Boost_LIBRARIES}
    ${GLOG_LIBRARIES}
    ${YAML_LIBRARIES}
    ${PROTOBUF_LIBRARY_CUSTOM}
    ${VEILHELPERS_LIBRARIES}
    ${GLOBUS_DEPS}
    globus_proxy_utils
    veilprotocol
    ${PLATFORM_EXTRA_LIBS}
)
target_link_libraries(veilFuse
    ${GLOG_LIBRARIES}
    ${Boost_LIBRARIES}
    ${FUSE_LIBRARY_CUSTOM}
    ${VEILHELPERS_LIBRARIES}
    ${PROTOBUF_LIBRARY_CUSTOM}
    veilclient
    globus_proxy_utils
    ${GLOBUS_DEPS}
    ${OPENSSL_LIBRARIES}
    ${PLATFORM_EXTRA_LIBS}
)

# Install config
install(TARGETS veilFuse
        RUNTIME DESTINATION ${BIN_DIR}
        LIBRARY DESTINATION ${LIB_DIR}
)
install(DIRECTORY config/ DESTINATION ${CONFIG_DIR})

# build a CPack driven installer package
include(InstallRequiredSystemLibraries)
include(CPack)

# set(CPACK_SET_DESTDIR ON) # Uncomment this line if you need a non-relocatable RPM package
set(CPACK_RESOURCE_FILE_LICENSE  
     "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE.txt")
set(CPACK_PACKAGE_VERSION_MAJOR "${VeilClient_VERSION_MAJOR}")
set(CPACK_PACKAGE_VERSION_MINOR "${VeilClient_VERSION_MINOR}")
set(CPACK_PACKAGE_CONTACT "help@veilfs.com")
set(CPACK_DEBIAN_PACKAGE_MAINTAINER, "PLGrid")

set(CPACK_PACKAGE_FILE_NAME "${CMAKE_PROJECT_NAME}-${CMAKE_SYSTEM_NAME}")

# DEB and RPM deps 
set(CPACK_DEBIAN_PACKAGE_DEPENDS "fuse")
set(CPACK_RPM_PACKAGE_REQUIRES "fuse")

# autogenerate dependency information
set (CPACK_DEBIAN_PACKAGE_SHLIBDEPS ON)
# set(CPACK_RPM_PACKAGE_AUTOREQPROV " no") ## Uncomment ot get rid of automatic deps generation

###########################
##         TESTS         ##
###########################

enable_testing()
include_directories(${GTEST_INCLUDE_DIR})
include_directories(${GMOCK_INCLUDE_DIR})

set(TEST_SOURCES_DIR "${PROJECT_SOURCE_DIR}/test")
include_directories(${TEST_SOURCES_DIR}/include)
include_directories(${VEILHELPERS_TEST_INCLUDE_DIR})

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

file(GLOB files ${TEST_SOURCES_DIR}/unit_tests/*_test.cc)
foreach(file ${files})
    get_filename_component(TEST_NAME ${file} NAME_WE)
    add_executable(${TEST_NAME} ${file})
    add_dependencies(${TEST_NAME} gtest gmock veilFuse veilhelpers veilclient veilprotocol)
    target_link_libraries(${TEST_NAME}
        ${GTEST_LIBRARIES}
        ${GMOCK_LIBRARIES}
        ${FUSE_LIBRARY_CUSTOM}
        ${VEILHELPERS_LIBRARIES}
        ${PROTOBUF_LIBRARY_CUSTOM}
        veilclient
        globus_proxy_utils
        ${GLOBUS_DEPS}
        ${OPENSSL_LIBRARIES}
        ${PLATFORM_EXTRA_LIBS}
    )    
    add_test(${TEST_NAME} ${TEST_NAME})
    run_test(${TEST_NAME})
endforeach()


# Integration tests

set(TEST_UTILS_DIR ${TEST_SOURCES_DIR}/erl_core)
add_library(erlTestCore ${TEST_UTILS_DIR}/erlTestCore.cc)

file(GLOB erlc_files ${TEST_SOURCES_DIR}/*/*.erl)
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

file(GLOB files ${TEST_SOURCES_DIR}/integration_tests/*_test.cc)
foreach(file ${files})
    get_filename_component(TEST_NAME ${file} NAME_WE)
    set(TEST_NAME "${TEST_NAME}_i")
    add_executable(${TEST_NAME} ${file} ${TEST_UTILS_DIR}/erlTestCore.cc)
    add_dependencies(${TEST_NAME} gtest gmock veilFuse veilhelpers veilclient veilprotocol)
    target_link_libraries(${TEST_NAME}
        ${GTEST_LIBRARIES}
        ${GMOCK_LIBRARIES}
        ${FUSE_LIBRARY_CUSTOM}
        ${VEILHELPERS_LIBRARIES}
        ${PROTOBUF_LIBRARY_CUSTOM}
        veilclient
        globus_proxy_utils
        ${GLOBUS_DEPS}
        ${OPENSSL_LIBRARIES}
        erlTestCore
        ${PLATFORM_EXTRA_LIBS}
    )    
    run_test_i(${TEST_NAME})
endforeach()
