# TODO: define target to run all benchmarks
add_custom_target(benchmark)

find_library(FOLLY_BENCHMARK_LIBRARIES follybenchmark)

set(CLIENT_BENCHMARK_LIBRARIES
    libmacaroons-cpp
    ${PLATFORM_EXTRA_LIBS}
    ${GLOG_LIBRARIES}
    ${FUSE_LIBRARIES}
    ${Boost_LIBRARIES}
    ${CLPROTO_LIBRARIES}
    ${TBB_LIBRARIES}
    ${SODIUM_LIBRARIES}
    ${PROTOBUF_LIBRARIES}
    ${NSS_LIBRARIES}
    ${OPENSSL_LIBRARIES})

add_library(clientBenchmarkShared SHARED ${CLIENT_SOURCES})

if(WITH_CEPH)
    list(APPEND CLIENT_BENCHMARK_LIBRARIES ${RADOS_LIBRARY} ${RADOSSTRIPER_LIBRARY})
endif(WITH_CEPH)

if(WITH_S3)
    list(APPEND CLIENT_BENCHMARK_LIBRARIES ${AWS_SDK_LIBRARIES})
endif(WITH_S3)

if(WITH_SWIFT)
    list(APPEND CLIENT_BENCHMARK_LIBRARIES ${SWIFT_SDK_LIBRARIES})
endif(WITH_SWIFT)

if(WITH_GLUSTERFS)
    list(APPEND CLIENT_BENCHMARK_LIBRARIES ${GLUSTERFSAPI_LIBRARIES})
endif(WITH_GLUSTERFS)

target_link_libraries(clientBenchmarkShared PUBLIC ${CLIENT_BENCHMARK_LIBRARIES})


file(GLOB_RECURSE TEST_SOURCES *_benchmark.cc)

foreach(TEST_SRC ${TEST_SOURCES})
    get_filename_component(TEST_NAME ${TEST_SRC} NAME_WE)
    add_executable(${TEST_NAME}
	    ${TEST_SRC}
	    ${TEST_HEADERS})
    target_link_libraries(${TEST_NAME} PRIVATE
	    clientBenchmarkShared
        ${FOLLY_BENCHMARK_LIBRARIES})
    add_custom_target(${TEST_NAME}_runbenchmark
        COMMAND ${TEST_NAME}
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
    add_dependencies(benchmark ${TEST_NAME}_runbenchmark)
endforeach()
