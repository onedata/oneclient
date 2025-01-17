add_library(clientShared SHARED ${CLIENT_SOURCES})
target_link_libraries(clientShared PUBLIC ${CLIENT_LIBRARIES})

set(BUILD_INTEGRATION_TESTS TRUE CACHE BOOL "Enable building integration tests")
if(BUILD_INTEGRATION_TESTS)
    add_subdirectory(integration)
    add_subdirectory(onenv_tests)
endif()

add_subdirectory(unit)
if(WITH_ONEBENCH)
    add_subdirectory(benchmark)
endif(WITH_ONEBENCH)