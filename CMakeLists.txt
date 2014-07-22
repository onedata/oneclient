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

configure_file (
    "${PROJECT_SOURCE_DIR}/include/veilConfig.h.in"
    "${PROJECT_BINARY_DIR}/veilConfig.h"
)

# Deps
add_subdirectory(${DEPS_DIR}/glog)
add_subdirectory(${DEPS_DIR}/gmock)
add_subdirectory(${DEPS_DIR}/veilhelpers)

link_directories(${PROJECT_BINARY_DIR})


# Include paths
include_directories(${PROJECT_SOURCE_DIR}/include)
include_directories(${PROJECT_SOURCE_DIR}/include/fuse)
include_directories(${PROJECT_BINARY_DIR})
include_directories(${GLOG_INCLUDE_DIR})
include_directories(${VEILHELPERS_INCLUDE_DIR})

# Setup fuse
message(STATUS "Checking for FUSE...")
find_package(FUSE REQUIRED)
#find_library(FUSE_LIBRARY_CUSTOM NAMES libfuse.a fuse)
set(FUSE_LIBRARY_CUSTOM ${FUSE_LIBRARIES})
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
find_library(SSL_LIBRARY_S NAMES libssl.a ssl)
find_library(CRYPTO_LIBRARY_S NAMES libcrypto.a crypto)
find_library(SSL_LIBRARY_D NAMES ssl)
find_library(CRYPTO_LIBRARY_D NAMES crypto)
find_library(KRB5_LIBRARY krb5)
if(${KRB5_LIBRARY} MATCHES KRB5_LIBRARY-NOTFOUND)
    set(OPENSSL_LIBRARIES_S "${SSL_LIBRARY_S};${CRYPTO_LIBRARY_S}")
    set(OPENSSL_LIBRARIES_D "${SSL_LIBRARY_D};${CRYPTO_LIBRARY_D}")
else(${KRB5_LIBRARY} MATCHES KRB5_LIBRARY-NOTFOUND)
    set(OPENSSL_LIBRARIES_S "${SSL_LIBRARY_S};${CRYPTO_LIBRARY_S};${KRB5_LIBRARY}")
    set(OPENSSL_LIBRARIES_D "${SSL_LIBRARY_D};${CRYPTO_LIBRARY_D};${KRB5_LIBRARY}")
endif(${KRB5_LIBRARY} MATCHES KRB5_LIBRARY-NOTFOUND)
include_directories(${OPENSSL_INCLUDE_DIR})


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

set(BOOST_LIBS boost_system boost_thread boost_filesystem boost_random boost_program_options)

link_directories(${Boost_LIBRARY_DIRS})
include_directories(${Boost_INCLUDE_DIRS})


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
file(GLOB PROTO_FILES "${PROJECT_SOURCE_DIR}/veilprotocol/proto/*.proto")
PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS ${PROTO_FILES})
include_directories(${CMAKE_CURRENT_BINARY_DIR})

# Setup sources
file(GLOB_RECURSE SOURCES src/*.cc)
file(GLOB_RECURSE HEADERS include/*.h)
set(MAIN_SOURCE_FILE src/veilFuse.cc)
string(REGEX REPLACE "${MAIN_SOURCE_FILE}" "" SOURCES "${SOURCES}")

# Setup compile flags
execute_process(COMMAND ${CMAKE_CXX_COMPILER} -print-file-name=libstdc++.a OUTPUT_VARIABLE LIBSTD_NAME)
string(REGEX REPLACE "(\r?\n)+$" "" LIBSTD_NAME "${LIBSTD_NAME}")
execute_process(COMMAND ln -sf "${LIBSTD_NAME}" WORKING_DIRECTORY ${PROJECT_BINARY_DIR})

set(CUSTOM_RPATH "${CMAKE_INSTALL_PREFIX}/${LIB_DIR}:${CMAKE_CURRENT_BINARY_DIR}:\$ORIGIN:\$ORIGIN/../${LIB_DIR}:${LIB_DIR}:../${LIB_DIR}")

if(APPLE)
    set(PLATFORM_EXTRA_LIBS "${CMAKE_THREAD_LIBS_INIT};${LTDL_LIBRARY};${ZLIB_LIBRARY};${DL_LIBRARY}")
    set(SECTION_FRAGMENTATION_FLAGS "-Wno-deprecated-declarations")
else()
    set(PLATFORM_EXTRA_LIBS "${CMAKE_THREAD_LIBS_INIT};${LTDL_LIBRARY};${ZLIB_LIBRARY};${DL_LIBRARY};${RT_LIBRARY}")
    set(SECTION_FRAGMENTATION_FLAGS "-fdata-sections -ffunction-sections")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,--gc-sections -Wl,-rpath,${CUSTOM_RPATH}")
endif()

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${SECTION_FRAGMENTATION_FLAGS} -Wall -std=c++11")
add_definitions(${FUSE_DEFINITIONS} -DFUSE_USE_VERSION=29 -D_WEBSOCKETPP_CPP11_STL_ -DBOOST_BIND_NO_PLACEHOLDERS)

set(CMAKE_SKIP_RPATH TRUE) # Something is wrong with CMake RPATH config, we need to use old-fashion way

# Define targets

add_library(veilprotocol ${PROTO_SRCS})
add_library(veilclient ${SOURCES} ${HEADERS})
add_executable(veilFuse ${MAIN_SOURCE_FILE})
add_executable(veilFuse_dynamic ${MAIN_SOURCE_FILE})

add_dependencies(veilFuse_dynamic glog veilclient)
add_dependencies(veilFuse glog veilclient)
add_dependencies(veilclient glog veilprotocol veilhelpers)

target_link_libraries(veilprotocol
    ${PROTOBUF_LIBRARY_CUSTOM}
)

target_link_libraries(veilclient
    ${FUSE_LIBRARY_CUSTOM}
    ${Boost_LIBRARIES}
    ${GLOG_LIBRARIES}
    ${PROTOBUF_LIBRARY_CUSTOM}
    ${VEILHELPERS_LIBRARIES}
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
    ${OPENSSL_LIBRARIES_S}
    ${PLATFORM_EXTRA_LIBS}
)

target_link_libraries(veilFuse_dynamic
    ${GLOG_LIBRARIES}
    ${Boost_LIBRARIES}
    ${FUSE_LIBRARY_CUSTOM}
    ${VEILHELPERS_LIBRARIES}
    ${PROTOBUF_LIBRARY_CUSTOM}
    veilclient
    ${OPENSSL_LIBRARIES_D}
    ${PLATFORM_EXTRA_LIBS}
)

if(NOT ${PROFILER_LIBRARY} MATCHES PROFILER_LIBRARY-NOTFOUND)
    target_link_libraries(veilFuse
        ${PROFILER_LIBRARY}
    )

    target_link_libraries(veilFuse_dynamic
        ${PROFILER_LIBRARY}
    )
endif(NOT ${PROFILER_LIBRARY} MATCHES PROFILER_LIBRARY-NOTFOUND)

# Install config
get_property(VEILFUSE_DYNAMIC_LOCATION TARGET veilFuse_dynamic PROPERTY LOCATION)
install(FILES ${VEILFUSE_DYNAMIC_LOCATION} 
        PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
        DESTINATION ${BIN_DIR}
        RENAME veilFuse
)
install(DIRECTORY config/ DESTINATION ${CONFIG_DIR})

# build a CPack driven installer package
include(InstallRequiredSystemLibraries)
set(CPACK_SET_DESTDIR ON) # Uncomment this line if you need a non-relocatable RPM package
set(CPACK_RESOURCE_FILE_LICENSE
     "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE.txt")
set(CPACK_PACKAGE_VERSION_MAJOR "${VeilClient_VERSION_MAJOR}")
set(CPACK_PACKAGE_VERSION_MINOR "${VeilClient_VERSION_MINOR}")
set(CPACK_PACKAGE_CONTACT "help@veilfs.com")

set(CPACK_PACKAGE_FILE_NAME "${CMAKE_PROJECT_NAME}-${CMAKE_SYSTEM_NAME}")

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
include_directories(${GTEST_INCLUDE_DIR})
include_directories(${GMOCK_INCLUDE_DIR})

set(TEST_SOURCES_DIR "${PROJECT_SOURCE_DIR}/test")
include_directories(${TEST_SOURCES_DIR}/include)
include_directories(${VEILHELPERS_TEST_INCLUDE_DIR})

set(TEST_UTILS_DIR ${TEST_SOURCES_DIR}/erl_core)
add_library(testCommon OBJECT
    ${TEST_SOURCES_DIR}/testRunner.cc
    ${TEST_SOURCES_DIR}/testCommon.cc
    ${TEST_UTILS_DIR}/erlTestCore.cc
)
add_dependencies(testCommon veilclient gmock gtest)

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
    add_executable(${TEST_NAME} ${file} $<TARGET_OBJECTS:testCommon>)
    add_dependencies(${TEST_NAME} gtest gmock veilFuse veilhelpers veilclient veilprotocol)
    target_link_libraries(${TEST_NAME}
        ${GTEST_LIBRARIES}
        ${GMOCK_LIBRARIES}
        ${FUSE_LIBRARY_CUSTOM}
        ${VEILHELPERS_LIBRARIES}
        ${PROTOBUF_LIBRARY_CUSTOM}
        veilclient
        ${OPENSSL_LIBRARIES}
        ${PLATFORM_EXTRA_LIBS}
    )
    add_test(${TEST_NAME} ${TEST_NAME})
    run_test(${TEST_NAME})
endforeach()


# Integration tests

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
    add_executable(${TEST_NAME} ${file} $<TARGET_OBJECTS:testCommon>)
    add_dependencies(${TEST_NAME} gtest gmock veilFuse veilhelpers veilclient veilprotocol)
    target_link_libraries(${TEST_NAME}
        ${GTEST_LIBRARIES}
        ${GMOCK_LIBRARIES}
        ${FUSE_LIBRARY_CUSTOM}
        ${VEILHELPERS_LIBRARIES}
        ${PROTOBUF_LIBRARY_CUSTOM}
        veilclient
        ${OPENSSL_LIBRARIES}
        ${PLATFORM_EXTRA_LIBS}
    )
    run_test_i(${TEST_NAME})
endforeach()
