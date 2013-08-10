project(dep-gtest)

include(ExternalProject)

ExternalProject_Add(gtest
    PREFIX "${CMAKE_CURRENT_BINARY_DIR}"
    SVN_REPOSITORY http://googletest.googlecode.com/svn/trunk/
    INSTALL_COMMAND ""
    UPDATE_COMMAND ""
)

# Export dep includes and lib paths
ExternalProject_Get_Property(gtest binary_dir)
ExternalProject_Get_Property(gtest source_dir)
set(GTEST_INCLUDE_DIR ${source_dir}/include PARENT_SCOPE)
set(GTEST_LIBRARIES ${binary_dir}/libgtest.a  ${binary_dir}/libgtest_main.a PARENT_SCOPE)