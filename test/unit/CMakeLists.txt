include_directories(include ${HELPERS_TEST_INCLUDE_DIR})
include_directories(SYSTEM ${GTEST_INCLUDE_DIR} ${GMOCK_INCLUDE_DIR})

file(GLOB_RECURSE TEST_HEADERS include/*.h)
add_library(testCommon OBJECT testRunner.cc testCommon.cc)
add_dependencies(testCommon gmock gtest)

add_custom_target(cunit)
macro(run_test test_target)
  string(REGEX REPLACE "_test" "" test_name "${test_target}")
  add_custom_target(${test_target}_runtest
      COMMAND ${test_target} --gtest_output="xml:cunit_results/TEST-${test_name}.xml"
      DEPENDS ${test_target}
      WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")
  add_dependencies(cunit ${test_target}_runtest)
endmacro()

file(GLOB TEST_SOURCES *_test.cc)
foreach(TEST_SRC ${TEST_SOURCES})
    get_filename_component(TEST_NAME ${TEST_SRC} NAME_WE)
    add_executable(${TEST_NAME} ${TEST_SRC} ${TEST_HEADERS} $<TARGET_OBJECTS:clientObjects> $<TARGET_OBJECTS:testCommon>)
    add_dependencies(${TEST_NAME} gtest gmock oneclient helpers clproto)
    target_link_libraries(${TEST_NAME}
        ${Boost_LIBRARIES}
        ${GLOG_LIBRARIES}
        ${GTEST_LIBRARIES}
        ${GMOCK_LIBRARIES}
        ${FUSE_LIBRARY_CUSTOM}
        ${HELPERS_LIBRARIES}
        ${PROTOBUF_LIBRARY_CUSTOM}
        ${OPENSSL_LIBRARIES}
        ${PLATFORM_EXTRA_LIBS}
        json11
        clproto)
    add_test(${TEST_NAME} ${TEST_NAME})
    run_test(${TEST_NAME})
endforeach()
