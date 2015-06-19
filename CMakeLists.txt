cmake_minimum_required(VERSION 3.0.0)
project(oneclient C CXX)

# CMake config
set(CMAKE_POSITION_INDEPENDENT_CODE True)
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR} ${PROJECT_SOURCE_DIR}/helpers)

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
add_subdirectory(deps/json11)

# Setup fuse
message(STATUS "Checking for FUSE...")
find_package(FUSE REQUIRED)

# Setup OpenSSL
message(STATUS "Checking for OpenSSL...")
find_package(OpenSSL REQUIRED)

# Setup pthreads
message(STATUS "Checking for thread support...")
find_package(Threads REQUIRED)

# Setup Intel TBB
message(STATUS "Checking for Intel TBB...")
find_package(TBB REQUIRED)

# Setup Boost
set(Boost_USE_MULTITHREADED      ON)
set(Boost_USE_STATIC_RUNTIME    OFF)
set(Boost_USE_STATIC_LIBS       OFF)
find_package(Boost COMPONENTS coroutine filesystem program_options system thread REQUIRED)

# Utility libs
find_library(LTDL_LIBRARY NAMES ltdl)
find_library(ZLIB_LIBRARY NAMES z)
find_library(RT_LIBRARY rt)
find_library(DL_LIBRARY dl)

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

if(APPLE)
    set(SECTION_FRAGMENTATION_FLAGS -Wno-deprecated-declarations)
    set(CUSTOM_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
else(NOT APPLE)
    set(PLATFORM_EXTRA_LIBS ${PLATFORM_EXTRA_LIBS} ${RT_LIBRARY})
    set(SECTION_FRAGMENTATION_FLAGS -fdata-sections -ffunction-sections)
    set(CUSTOM_RPATH "${CMAKE_INSTALL_PREFIX}/lib" "\$ORIGIN" "\$ORIGIN/../lib")
endif(APPLE)

# Define build settings
add_compile_options(${SECTION_FRAGMENTATION_FLAGS} -Werror -Wall -std=c++14)
add_definitions(
    ${FUSE_DEFINITIONS}
    -DFUSE_USE_VERSION=29
    -DBOOST_BIND_NO_PLACEHOLDERS
    -DBOOST_THREAD_VERSION=4
    -DBOOST_THREAD_PROVIDES_EXECUTORS
    -DBOOST_CHRONO_HEADER_ONLY)

# Add helpers after setting compilation flags but before setting include
# directories; helpers specify their own includes, pulling from package-set
# variables when needed.
add_subdirectory(helpers/object)

include_directories(
    include
    ${PROJECT_BINARY_DIR}/version
    ${HELPERS_INCLUDE_DIRS})

include_directories(SYSTEM
    ${HELPERS_SYSTEM_INCLUDE_DIRS}
    ${FUSE_INCLUDE_DIRS}
    ${PROTOBUF_INCLUDE_DIR}
    ${OPENSSL_INCLUDE_DIR}
    ${TBB_INCLUDE_DIRS}
    ${Boost_INCLUDE_DIRS}
    ${GLOG_INCLUDE_DIR}
    ${WSPP_INCLUDE_DIR}
    ${JSON11_INCLUDE_DIR})

set(CLIENT_LIBRARIES
    json11
    ${PLATFORM_EXTRA_LIBS}
    ${GLOG_LIBRARIES}
    ${FUSE_LIBRARIES}
    ${Boost_LIBRARIES}
    ${PROTOBUF_LIBRARIES}
    ${OPENSSL_LIBRARIES}
    ${HELPERS_LIBRARIES})

# Define targets
add_library(client OBJECT ${SOURCES} ${HEADERS})
add_dependencies(client glog)

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
install(DIRECTORY config/ DESTINATION etc)

enable_testing()
add_subdirectory(test)
