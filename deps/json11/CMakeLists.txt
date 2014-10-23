include(ExternalProject)

ExternalProject_Add(json11Build
    GIT_REPOSITORY https://github.com/dropbox/json11.git
    UPDATE_COMMAND ""
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND ""
)

# Export dep includes and lib paths
ExternalProject_Get_Property(json11Build source_dir)

set(JSON11_INCLUDE_DIR ${source_dir} PARENT_SCOPE)
set(JSON11_SOURCE ${source_dir}/json11.cpp)
set(JSON11_SOURCE ${JSON11_SOURCE} PARENT_SCOPE)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -w")
add_custom_command(OUTPUT ${JSON11_SOURCE} DEPENDS json11Build)
add_library(json11 STATIC ${JSON11_SOURCE})
