include_directories(SYSTEM
    ${HELPERS_DIR}/deps/gmock/include
    ${HELPERS_DIR}/deps/gtest/include)

file(COPY ${PROJECT_SOURCE_DIR}/helpers/appmock DESTINATION ${PROJECT_BINARY_DIR})
file(COPY ${PROJECT_SOURCE_DIR}/helpers/bamboos DESTINATION ${PROJECT_BINARY_DIR})
file(COPY
    ${PROJECT_SOURCE_DIR}/helpers/test/integration/test_common.py
    ${PROJECT_SOURCE_DIR}/helpers/test/integration/conftest.py
    DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

set(APPMOCK_BINARY ${PROJECT_BINARY_DIR}/appmock/rel/appmock/bin/appmock)
add_custom_command(OUTPUT ${APPMOCK_BINARY}
    COMMAND make
    WORKING_DIRECTORY ${PROJECT_BINARY_DIR}/appmock)
add_custom_target(appmock ALL DEPENDS ${APPMOCK_BINARY})

if(APPLE)
    # On OSX we have to force Python 2 to run tests using default
    # boost-python library
    find_package(Boost COMPONENTS python27-mt REQUIRED)
    find_package(PythonLibs 2 REQUIRED)
    find_package(PythonInterp 2 REQUIRED)
else(APPLE)
    find_package(Boost COMPONENTS python REQUIRED)
    find_package(PythonLibs 2 REQUIRED)
endif(APPLE)
include_directories(SYSTEM ${PYTHON_INCLUDE_DIRS} ${Boost_INCLUDE_DIRS})
remove_definitions(-DBOOST_BIND_NO_PLACEHOLDERS)

set(PROTO_DIR ${PROJECT_SOURCE_DIR}/helpers/clproto/proto)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/proto)
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/proto/__init__.py "")
file(GLOB PROTO_FILES ${PROTO_DIR}/*.proto)

if(Protobuf_PROTOC_EXECUTABLE)
  # On OSX cmake generates the protoc executable variable with small caps
  # name so we have to manually set the capitalized version
  set(PROTOBUF_PROTOC_EXECUTABLE ${Protobuf_PROTOC_EXECUTABLE})
endif(Protobuf_PROTOC_EXECUTABLE)

add_custom_target(pythonProto
    COMMAND ${PROTOBUF_PROTOC_EXECUTABLE} --python_out ${CMAKE_CURRENT_BINARY_DIR}/proto -I ${PROTO_DIR} ${PROTO_FILES}
    COMMENT "Running Python protocol buffer compiler for integration tests"
    VERBATIM)

file(GLOB TEST_DIRS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *_test)
foreach(TEST_DIR ${TEST_DIRS})
    string(REGEX REPLACE "(.*)_test" "\\1" TEST_NAME ${TEST_DIR})
    file(COPY ${TEST_DIR} DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
    if(NOT TEST_NAME STREQUAL "onedatafs")
        file(GLOB_RECURSE TEST_SRC ${TEST_DIR}/*.cc ${TEST_DIR}/*.h)

        add_library(${TEST_NAME} SHARED ${TEST_SRC})

        set_target_properties(${TEST_NAME} PROPERTIES
            PREFIX ""
            LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${TEST_DIR}
            BUILD_WITH_INSTALL_RPATH true
            INSTALL_RPATH_USE_LINK_PATH true
            INSTALL_RPATH "\$ORIGIN/../..")

        target_link_libraries(${TEST_NAME}
            clientShared
            gmock_main
            gtest
            ${Boost_LIBRARIES}
            ${PYTHON_LIBRARIES})

        add_dependencies(${TEST_NAME} pythonProto)
    endif()
endforeach()
