add_subdirectory(
    ${HELPERS_DIR}/deps/gmock
    ${PROJECT_BINARY_DIR}/deps/gmock)

include_directories(SYSTEM
    ${HELPERS_DIR}/deps/gmock/include
    ${HELPERS_DIR}/deps/gtest/include)

include_directories(include ${PROJECT_SOURCE_DIR}/helpers/test/unit/include)

file(GLOB_RECURSE TEST_HEADERS include/*.h)
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

file(GLOB_RECURSE TEST_SOURCES *_test.cc)
foreach(TEST_SRC ${TEST_SOURCES})
    get_filename_component(TEST_NAME ${TEST_SRC} NAME_WE)
    add_executable(${TEST_NAME}
	${TEST_SRC}
	${TEST_HEADERS}
	$<TARGET_OBJECTS:testRunner>)
    target_link_libraries(${TEST_NAME} PRIVATE
	clientShared
	${BACKWARD_CPP_LIBRARIES}
	gmock_main
	gtest)
    add_test(${TEST_NAME} ${TEST_NAME})
    run_test(${TEST_NAME})
endforeach()
