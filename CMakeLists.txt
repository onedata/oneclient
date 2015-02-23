cmake_minimum_required(VERSION 3.0.0)
project(oneclient C CXX)

# CMake config
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR})
# set(CMAKE_FIND_LIBRARY_SUFFIXES .a) # Uncomment in order to force looking for static achieves instead .so

#================================================================================================
# Automatic git versioning: http://ipenguin.ws/2012/11/cmake-automatically-use-git-tags-as.html
# Make a version file containing the current version from git.
include(GetGitRevisionDescription)
git_describe(VERSION --tags)

#parse the version information into pieces.
string(REGEX REPLACE "^v([0-9]+)\\..*" "\\1" VERSION_MAJOR "${VERSION}")
string(REGEX REPLACE "^v[0-9]+\\.([0-9]+).*" "\\1" VERSION_MINOR "${VERSION}")
string(REGEX REPLACE "^v[0-9]+\\.[0-9]+\\.([0-9]+).*" "\\1" VERSION_PATCH "${VERSION}")

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
include_directories(SYSTEM ${FUSE_INCLUDE_DIRS})

# Setup ProtoBuf
message(STATUS "Checking for Protobuf...")
find_package(Protobuf REQUIRED)
include_directories(SYSTEM ${PROTOBUF_INCLUDE_DIR})

# Setup OpenSSL
message(STATUS "Checking for OpenSSL...")
find_package(OpenSSL REQUIRED)
include_directories(SYSTEM ${OPENSSL_INCLUDE_DIR})

# Setup pthreads
message(STATUS "Checking for pthreads...")
find_package(Threads REQUIRED)

# Setup git
message(STATUS "Checking for Git...")
find_package(Git REQUIRED)

# Setup Boost
set(Boost_USE_MULTITHREADED      ON)
set(Boost_USE_STATIC_RUNTIME    OFF)
set(Boost_USE_STATIC_LIBS       OFF)
find_package(Boost COMPONENTS system thread filesystem random program_options REQUIRED)
include_directories(SYSTEM ${Boost_INCLUDE_DIRS})

# Utility libs
find_library(LTDL_LIBRARY NAMES ltdl)
find_library(RT_LIBRARY rt)
find_library(ZLIB_LIBRARY NAMES z)
find_library(DL_LIBRARY dl)

# gpertools lib
find_library(PROFILER_LIBRARY NAMES profiler)

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
#set(MAIN_SOURCE_FILE src/main.cc)
#list(REMOVE_ITEM SOURCES ${MAIN_SOURCE_FILE})

# Setup compile flags
execute_process(COMMAND ${CMAKE_CXX_COMPILER} -print-file-name=libstdc++.a OUTPUT_VARIABLE LIBSTD_NAME)
string(STRIP ${LIBSTD_NAME} LIBSTD_NAME)
if(EXISTS ${LIBSTD_NAME})
    execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink ${LIBSTD_NAME} ${PROJECT_BINARY_DIR}/libstdc++.a)
endif(EXISTS ${LIBSTD_NAME})

set(PLATFORM_EXTRA_LIBS ${CMAKE_THREAD_LIBS_INIT} ${LTDL_LIBRARY} ${ZLIB_LIBRARY} ${DL_LIBRARY})

if(APPLE)
    set(SECTION_FRAGMENTATION_FLAGS -Wno-deprecated-declarations)
    set(CUSTOM_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
else(NOT APPLE)
    set(PLATFORM_EXTRA_LIBS ${PLATFORM_EXTRA_LIBS} ${RT_LIBRARY})
    set(SECTION_FRAGMENTATION_FLAGS -fdata-sections -ffunction-sections)
    set(CUSTOM_RPATH "${CMAKE_INSTALL_PREFIX}/lib" "\$ORIGIN" "\$ORIGIN/../lib")
endif(APPLE)

# Define targets
add_library(commonOptions INTERFACE)
target_compile_options(commonOptions INTERFACE -Wall -std=c++14)
target_compile_definitions(commonOptions INTERFACE
    ${FUSE_DEFINITIONS}
    -DFUSE_USE_VERSION=29
    -D_WEBSOCKETPP_CPP11_STL_)

add_library(clientLibs INTERFACE)
target_link_libraries(clientLibs INTERFACE
    commonOptions
    json11
    ${GLOG_LIBRARIES}
    ${HELPERS_LIBRARIES}
    ${FUSE_LIBRARIES}
    ${Boost_LIBRARIES}
    ${PROTOBUF_LIBRARIES}
    ${PLATFORM_EXTRA_LIBS}
    ${OPENSSL_LIBRARIES})

add_library(clientObjects OBJECT ${SOURCES} ${HEADERS} ${PROTO_SRCS})
add_dependencies(clientObjects glog helpers)
set_target_properties(clientObjects PROPERTIES POSITION_INDEPENDENT_CODE True)
target_compile_options(clientObjects PRIVATE
    $<TARGET_PROPERTY:commonOptions,INTERFACE_COMPILE_OPTIONS>)
target_compile_definitions(clientObjects PRIVATE
    $<TARGET_PROPERTY:commonOptions,INTERFACE_COMPILE_DEFINITIONS>
    -DBOOST_BIND_NO_PLACEHOLDERS)

add_executable(oneclient $<TARGET_OBJECTS:clientObjects>)
target_link_libraries(oneclient clientLibs)
set_target_properties(oneclient PROPERTIES
    BUILD_WITH_INSTALL_RPATH true
    INSTALL_RPATH_USE_LINK_PATH true
    INSTALL_RPATH "${CUSTOM_RPATH}")

if(NOT ${PROFILER_LIBRARY} MATCHES PROFILER_LIBRARY-NOTFOUND)
    target_link_libraries(oneclient ${PROFILER_LIBRARY})
endif(NOT ${PROFILER_LIBRARY} MATCHES PROFILER_LIBRARY-NOTFOUND)

# Install config
install(TARGETS oneclient DESTINATION bin)
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

enable_testing()
add_subdirectory(test)
