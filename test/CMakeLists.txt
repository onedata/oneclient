add_library(clientShared SHARED $<TARGET_OBJECTS:clientObjects>)
target_link_libraries(clientShared clientLibs)

set(BUILD_INTEGRATION_TESTS TRUE CACHE BOOL "Enable building integration tests")
if(BUILD_INTEGRATION_TESTS)
    add_subdirectory(integration)
endif()

add_subdirectory(unit)
