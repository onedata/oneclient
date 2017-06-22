add_subdirectory(
    ${CMAKE_SOURCE_DIR}/deps/gmock
    ${PROJECT_BINARY_DIR}/deps/gmock)

include_directories(SYSTEM
    ${CMAKE_SOURCE_DIR}/deps/gmock/include
    ${CMAKE_SOURCE_DIR}/deps/gtest/include)

include_directories(include)

file(COPY communication/etls/server.pem communication/etls/server.key
        DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

file(COPY communication/etls/server.pem communication/etls/server.key
        DESTINATION ${PROJECT_BINARY_DIR})

file(GLOB_RECURSE HELPER_TEST_HEADERS include/*.h)
add_library(testRunner OBJECT testRunner.cc)
add_dependencies(testRunner gmock gtest)

add_custom_target(cunit)
macro(run_test test_target)
  string(REGEX REPLACE "_test" "" test_name "${test_target}")
  add_custom_target(${test_target}_runtest
      COMMAND ${test_target} --gtest_output="xml:cunit_results/TEST-${test_name}.xml"
      DEPENDS ${test_target}
      WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")
  add_dependencies(cunit ${test_target}_runtest)
endmacro()

file(GLOB_RECURSE HELPER_TEST_SOURCES *_test.cc)
foreach(TEST_FILE ${HELPER_TEST_SOURCES})
    get_filename_component(TEST_NAME ${TEST_FILE} NAME_WE)
    add_executable(${TEST_NAME}
        ${TEST_FILE}
        ${HELPER_TEST_HEADERS}
        $<TARGET_OBJECTS:testRunner>)
    target_link_libraries(${TEST_NAME}
        helpersShared
        gmock_main
        gtest)
    add_test(${TEST_NAME} ${TEST_NAME})
    run_test(${TEST_NAME})
endforeach()
