include(ExternalProject)

ExternalProject_Add(helpersBuild
    PREFIX ${CMAKE_CURRENT_BINARY_DIR}
    GIT_REPOSITORY ssh://git@git.plgrid.pl:7999/vfs/veilhelpers.git
    GIT_TAG d3ae67f
    CMAKE_ARGS -DCMAKE_INSTALL_PREFIX:PATH=${PROJECT_BINARY_DIR}
               -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
    UPDATE_COMMAND git pull || exit 0
)

# Export dep includes and lib paths
ExternalProject_Get_Property(helpersBuild binary_dir)
ExternalProject_Get_Property(helpersBuild source_dir)

set(HELPERS_INCLUDE_DIRS ${source_dir}/include ${binary_dir}/include PARENT_SCOPE)
set(HELPERS_TEST_INCLUDE_DIR ${source_dir}/test/include PARENT_SCOPE)
set(HELPERS_LIBRARIES ${binary_dir}/libhelpers.a)
set(HELPERS_LIBRARIES ${HELPERS_LIBRARIES} PARENT_SCOPE)

add_custom_command(OUTPUT ${HELPERS_LIBRARIES} DEPENDS helpersBuild)
add_custom_target(helpers DEPENDS ${HELPERS_LIBRARIES})
