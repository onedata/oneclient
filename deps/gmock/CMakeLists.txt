project(dep-gmock)

include(ExternalProject)

ExternalProject_Add(gmock
    PREFIX "${CMAKE_CURRENT_BINARY_DIR}"
    SVN_REPOSITORY http://googlemock.googlecode.com/svn/trunk/
    INSTALL_COMMAND ""
    UPDATE_COMMAND ""
)

# Export dep includes and lib paths
ExternalProject_Get_Property(gmock binary_dir)
ExternalProject_Get_Property(gmock source_dir)
set(GMOCK_INCLUDE_DIR ${source_dir}/include PARENT_SCOPE)
set(GMOCK_LIBRARIES ${binary_dir}/libgmock.a  ${binary_dir}/libgmock_main.a PARENT_SCOPE)