file(COPY ${PROJECT_SOURCE_DIR}/appmock DESTINATION ${PROJECT_BINARY_DIR})
file(COPY ${PROJECT_SOURCE_DIR}/bamboos DESTINATION ${PROJECT_BINARY_DIR})
file(COPY conftest.py test_common.py common_test_base.py key_value_test_base.py
          posix_test_base.py xattr_test_base.py
          DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

set(APPMOCK_BINARY ${PROJECT_BINARY_DIR}/appmock/rel/appmock/bin/appmock)
add_custom_command(OUTPUT ${APPMOCK_BINARY}
    COMMAND make
    WORKING_DIRECTORY ${PROJECT_BINARY_DIR}/appmock)
add_custom_target(appmock ALL DEPENDS ${APPMOCK_BINARY})

set(PROTO_DIR ${PROJECT_SOURCE_DIR}/clproto/proto)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/proto)
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/proto/__init__.py "")
file(GLOB PROTO_FILES ${PROTO_DIR}/*.proto)

if(APPLE AND Protobuf_PROTOC_EXECUTABLE)
  # On OSX cmake generates the protoc executable variable with small caps
  # name so we have to manually set the capitalized version
  set(PROTOBUF_PROTOC_EXECUTABLE ${Protobuf_PROTOC_EXECUTABLE})
endif(APPLE AND Protobuf_PROTOC_EXECUTABLE)

add_custom_target(pythonProto
    COMMAND ${PROTOBUF_PROTOC_EXECUTABLE} --python_out ${CMAKE_CURRENT_BINARY_DIR}/proto -I ${PROTO_DIR} ${PROTO_FILES}
    COMMENT "Running Python protocol buffer compiler for integration tests"
    VERBATIM)

if(APPLE)
    # On OSX we have to force Python 2 to run tests using default
    # boost-python library
    find_package(Boost COMPONENTS python REQUIRED)
    find_package(PythonLibs 2 REQUIRED)
    find_package(PythonInterp 2 REQUIRED)
else(APPLE)
    find_package(Boost COMPONENTS python REQUIRED)
    find_package(PythonLibs REQUIRED)
endif(APPLE)
include_directories(SYSTEM ${PYTHON_INCLUDE_DIRS} ${Boost_INCLUDE_DIRS})
remove_definitions(-DBOOST_BIND_NO_PLACEHOLDERS)

file(GLOB TEST_DIRS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *_test)
#
# Remove helper test when building without them
#
if(NOT WITH_CEPH)
    list(REMOVE_ITEM TEST_DIRS  ceph_helper_test)
endif(NOT WITH_CEPH)

if(NOT WITH_S3)
    list(REMOVE_ITEM TEST_DIRS  s3_helper_test)
endif(NOT WITH_S3)

if(NOT WITH_SWIFT)
    list(REMOVE_ITEM TEST_DIRS  swift_helper_test)
endif(NOT WITH_SWIFT)

if(NOT WITH_GLUSTERFS)
    list(REMOVE_ITEM TEST_DIRS  glusterfs_helper_test)
endif(NOT WITH_GLUSTERFS)

foreach(TEST_DIR ${TEST_DIRS})
    string(REGEX REPLACE "(.*)_test" "\\1" TEST_NAME ${TEST_DIR})
    file(COPY ${TEST_DIR} DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
    file(GLOB_RECURSE TEST_SRC ${TEST_DIR}/*.cc ${TEST_DIR}/*.h)

    # Add Posix types to GlusterFS test
    if("${TEST_DIR}" MATCHES ".*glusterfs.*")
        add_library(posix_test_types SHARED ${TEST_DIR}/../posixTestTypes.cc)
        set_target_properties(posix_test_types PROPERTIES
            PREFIX ""
            LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
            BUILD_WITH_INSTALL_RPATH true
            INSTALL_RPATH_USE_LINK_PATH true
            INSTALL_RPATH "\$ORIGIN/../../..")
        target_link_libraries(posix_test_types
            helpersShared
            ${Boost_LIBRARIES}
            ${PYTHON_LIBRARIES})
    endif()

    add_library(${TEST_NAME} SHARED ${TEST_SRC})

    set_target_properties(${TEST_NAME} PROPERTIES
        PREFIX ""
        LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${TEST_DIR}
        BUILD_WITH_INSTALL_RPATH true
        INSTALL_RPATH_USE_LINK_PATH true
        INSTALL_RPATH "\$ORIGIN/../../..")

    target_link_libraries(${TEST_NAME}
        helpersShared
        ${Boost_LIBRARIES}
        ${PYTHON_LIBRARIES})

    add_dependencies(${TEST_NAME} pythonProto)
endforeach()
