add_library(clientShared SHARED ${CLIENT_SOURCES})
target_link_libraries(clientShared PUBLIC ${CLIENT_LIBRARIES})

set(BUILD_INTEGRATION_TESTS TRUE CACHE BOOL "Enable building integration tests")
if(BUILD_INTEGRATION_TESTS)
    add_subdirectory(integration)
endif()

add_subdirectory(unit)

add_subdirectory(benchmark)
