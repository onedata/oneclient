# Deps
add_subdirectory(../deps/gmock ${CMAKE_CURRENT_BINARY_DIR}/../deps/gmock)

# Add clproto target
add_subdirectory(../clproto ${CMAKE_CURRENT_BINARY_DIR}/../clproto)

# Set up helpers
set(HELPERS_SYSTEM_INCLUDE_DIRS
    ${FUSE_INCLUDE_DIRS}
    ${FUSE_INCLUDE_DIRS}/..
    ${PROTOBUF_INCLUDE_DIR}
    ${OPENSSL_INCLUDE_DIR}
    ${TBB_INCLUDE_DIRS}
    ${Boost_INCLUDE_DIRS}
    ${CLPROTO_SYSTEM_INCLUDE_DIRS}
    ${GLOG_INCLUDE_DIRS}
    ${CMAKE_CURRENT_SOURCE_DIR}/../deps/asio/asio/include)

file(GLOB_RECURSE HELPER_SOURCES ../src/*.cc ../src/*.h)
add_library(helpers OBJECT ${HELPER_SOURCES})
add_dependencies(helpers clproto)
target_include_directories(helpers PRIVATE ../src)
target_include_directories(helpers SYSTEM PRIVATE
    ${HELPERS_SYSTEM_INCLUDE_DIRS})

set(HELPERS_INCLUDE_DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/../src
    PARENT_SCOPE)

set(HELPERS_SYSTEM_INCLUDE_DIRS
    ${HELPERS_SYSTEM_INCLUDE_DIRS}
    PARENT_SCOPE)

set(HELPERS_LIBRARIES
    ${Boost_LIBRARIES}
    ${FUSE_LIBRARIES}
    ${GLOG_LIBRARIES}
    ${OPENSSL_LIBRARIES}
    ${PLATFORM_EXTRA_LIBS}
    ${TBB_LIBRARIES}
    ${CLPROTO_LIBRARIES}
    PARENT_SCOPE)

set(PROJECT_SOURCES
    $<TARGET_OBJECTS:helpers>
    ${PROJECT_SOURCES}
    PARENT_SCOPE)
