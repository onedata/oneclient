# Deps
add_subdirectory(../deps/gmock ${CMAKE_CURRENT_BINARY_DIR}/../deps/gmock)
add_subdirectory(../deps/glog ${CMAKE_CURRENT_BINARY_DIR}/../deps/glog)
add_subdirectory(../deps/websocketpp ${CMAKE_CURRENT_BINARY_DIR}/../deps/websocketpp)

# Add clproto target
add_subdirectory(../clproto ${CMAKE_CURRENT_BINARY_DIR}/../clproto)

# Set up helpers
file(GLOB_RECURSE HELPER_SOURCES ../src/*.cc ../include/*.h)
add_library(helpers OBJECT ${HELPER_SOURCES})
add_dependencies(helpers websocketpp glog)
target_include_directories(helpers PRIVATE ../include)
target_include_directories(helpers SYSTEM PRIVATE
    ${GLOG_INCLUDE_DIR}
    ${WSPP_INCLUDE_DIR}
    ${CLPROTO_SYSTEM_INCLUDE_DIRS})

set(HELPERS_INCLUDE_DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/../include/
    PARENT_SCOPE)

set(HELPERS_SYSTEM_INCLUDE_DIRS
    ${CLPROTO_SYSTEM_INCLUDE_DIRS}
    ${GLOG_INCLUDE_DIR}
    ${WSPP_INCLUDE_DIR}
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
