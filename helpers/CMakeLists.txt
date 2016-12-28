cmake_minimum_required(VERSION 3.0.0)
project(helpers C CXX)

# CMake config
set(CMAKE_SKIP_INSTALL_ALL_DEPENDENCY True)
set(CMAKE_FIND_FRAMEWORK LAST)
set(CMAKE_POSITION_INDEPENDENT_CODE True)
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)

include(FindLibraryUtils)

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

# Setup CURL
message(STATUS "Checking for CURL...")
find_package(CURL REQUIRED)

# Setup NSS
message(STATUS "Checking for NSS...")
find_package(NSS REQUIRED)

# Setup Folly
message(STATUS "Checking for Folly...")
find_package(Folly REQUIRED)
find_library(LIBEVENT_LIBRARY event)
find_library(IBERTY_LIBRARY iberty)
find_library(DOUBLE_CONVERSION_LIBRARY double-conversion)
set(FOLLY_LIBRARIES
    ${FOLLY_LIBRARIES}
    ${LIBEVENT_LIBRARY}
    ${IBERTY_LIBRARY}
    ${DOUBLE_CONVERSION_LIBRARY})

# Setup Boost
message(STATUS "Checking for Boost components...")
set(Boost_USE_STATIC_LIBS       OFF)
set(Boost_USE_MULTITHREADED      ON)
set(Boost_USE_STATIC_RUNTIME    OFF)
find_package(Boost COMPONENTS atomic chrono context date_time filesystem
                              iostreams log log_setup program_options regex
                              system thread REQUIRED)

set(ETLS_DIR ${CMAKE_CURRENT_SOURCE_DIR}/etls/c_src)

# Setup BoringSSL
add_subdirectory(${ETLS_DIR}/deps/boringssl)
set(BORINGSSL_LIBRARIES ssl crypto)
set(BORINGSSL_INCLUDE_DIRS ${ETLS_DIR}/deps/boringssl/include)

# Setup Asio
set(ASIO_INCLUDE_DIRS ${ETLS_DIR}/deps/asio/asio/include)

# Utility libs
find_library(LTDL_LIBRARY ltdl)
find_library(ZLIB_LIBRARY z)
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
find_library(POCO_FOUNDATION PocoFoundation)
find_library(POCO_NET PocoNet)
find_library(POCO_XML PocoXML)
find_library(POCO_UTIL PocoUtil)
set(SWIFT_SDK_LIBRARIES
    ${SWIFT_SDK_LIBRARY}
    ${POCO_FOUNDATION}
    ${POCO_NET}
    ${POCO_XML}
    ${POCO_UTIL})

# Setup compile flags
set(PLATFORM_EXTRA_LIBS
    ${CMAKE_THREAD_LIBS_INIT}
    ${LTDL_LIBRARY}
    ${ZLIB_LIBRARY}
    ${DL_LIBRARY})

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
    set(CUSTOM_RPATH "${CMAKE_INSTALL_PREFIX}/lib" "\$ORIGIN")
endif(APPLE)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14")
add_compile_options(${SECTION_FRAGMENTATION_FLAGS} -Werror -Wshadow -Wall -Wpedantic)
add_definitions(
    ${FUSE_DEFINITIONS}
    -DFUSE_USE_VERSION=29
    -DBOOST_BIND_NO_PLACEHOLDERS
    -DBOOST_FILESYSTEM_NO_DEPRECATED
    -DBOOST_ALL_DYN_LINK
    -DASIO_STANDALONE
    -DGLOG_STL_LOGGING_FOR_UNORDERED)

    option(BUILD_PROXY_IO "Build Proxy IO helper." ON)
    if(BUILD_PROXY_IO)
        add_definitions("-DBUILD_PROXY_IO")
    endif(BUILD_PROXY_IO)

add_subdirectory(clproto)
add_subdirectory(${ETLS_DIR}/src)
add_subdirectory(src)

file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/dummy.cc "")
set(HELPERS_SOURCES ${CMAKE_CURRENT_BINARY_DIR}/dummy.cc ${PROJECT_SOURCES})

# Define targets
add_library(helpersStatic STATIC ${HELPERS_SOURCES})
target_include_directories(helpersStatic SYSTEM PUBLIC ${HELPERS_SYSTEM_INCLUDE_DIRS})
target_include_directories(helpersStatic PUBLIC ${HELPERS_INCLUDE_DIRS})
set_target_properties(helpersStatic PROPERTIES
    OUTPUT_NAME helpers)

add_library(helpersShared SHARED ${HELPERS_SOURCES})
target_link_libraries(helpersShared PUBLIC ${HELPERS_LIBRARIES})
target_include_directories(helpersShared SYSTEM PUBLIC ${HELPERS_SYSTEM_INCLUDE_DIRS})
target_include_directories(helpersShared PUBLIC ${HELPERS_INCLUDE_DIRS})
set_target_properties(helpersShared PROPERTIES
    OUTPUT_NAME helpers
    BUILD_WITH_INSTALL_RPATH true
    INSTALL_RPATH_USE_LINK_PATH true
    INSTALL_RPATH "${CUSTOM_RPATH}")

install(TARGETS helpersShared LIBRARY DESTINATION lib)
install(TARGETS helpersStatic ARCHIVE DESTINATION lib)
install(DIRECTORY include DESTINATION .)
install(DIRECTORY ${ASIO_INCLUDE_DIRS} DESTINATION .)


enable_testing()
add_subdirectory(test)
