include(ExternalProject)

ExternalProject_Add(gmockBuild
    PREFIX ${CMAKE_CURRENT_BINARY_DIR}
    SVN_REPOSITORY http://googlemock.googlecode.com/svn/trunk/
    SVN_REVISION -r 476
    UPDATE_COMMAND ""
    INSTALL_COMMAND ""
    CMAKE_ARGS -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
               -DCMAKE_CXX_FLAGS=-fPIC -std=c++11 -w
               --no-warn-unused-cli
)

# Export dep includes and lib paths
ExternalProject_Get_Property(gmockBuild binary_dir)
ExternalProject_Get_Property(gmockBuild source_dir)

set(GMOCK_INCLUDE_DIR ${source_dir}/include CACHE PATH "")
set(GMOCK_LIBRARIES ${binary_dir}/libgmock.a CACHE FILEPATH "")
set(GTEST_INCLUDE_DIR ${source_dir}/gtest/include CACHE PATH "")
set(GTEST_LIBRARIES ${binary_dir}/gtest/libgtest.a CACHE FILEPATH "")

add_custom_command(OUTPUT ${GMOCK_LIBRARIES} ${GTEST_LIBRARIES} DEPENDS gmockBuild)
add_custom_target(gmock DEPENDS ${GMOCK_LIBRARIES})
add_custom_target(gtest DEPENDS ${GTEST_LIBRARIES})
