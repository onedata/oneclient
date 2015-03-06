file(COPY ${PROJECT_SOURCE_DIR}/appmock DESTINATION ${PROJECT_BINARY_DIR})
file(COPY ${PROJECT_SOURCE_DIR}/bamboos DESTINATION ${PROJECT_BINARY_DIR})
file(COPY test_common.py DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

set(APPMOCK_BINARY ${PROJECT_BINARY_DIR}/appmock/rel/appmock/bin/appmock)
add_custom_command(OUTPUT ${APPMOCK_BINARY}
    COMMAND make
    WORKING_DIRECTORY ${PROJECT_BINARY_DIR}/appmock)
add_custom_target(appmock ALL DEPENDS ${APPMOCK_BINARY})

find_package(Boost COMPONENTS python REQUIRED)
find_package(PythonLibs REQUIRED)
include_directories(SYSTEM ${PYTHON_INCLUDE_DIRS} ${Boost_INCLUDE_DIRS})
remove_definitions(-DBOOST_BIND_NO_PLACEHOLDERS)

file(GLOB TEST_DIRS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *_test)
foreach(TEST_DIR ${TEST_DIRS})
    string(REGEX REPLACE "(.*)_test" "\\1" TEST_NAME ${TEST_DIR})
    file(COPY ${TEST_DIR} DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
    file(GLOB_RECURSE TEST_SRC ${TEST_DIR}/*.cc ${TEST_DIR}/*.h)

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
endforeach()
