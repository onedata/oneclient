set(BUILD_INTEGRATION_TESTS TRUE CACHE BOOL "Enable building integration tests")
if(BUILD_INTEGRATION_TESTS)
    add_subdirectory(integration)
endif()

add_subdirectory(unit)
