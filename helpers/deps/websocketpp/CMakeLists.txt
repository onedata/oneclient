include(ExternalProject)

ExternalProject_Add(websocketpp
    GIT_REPOSITORY https://github.com/RoXeon/websocketpp
    UPDATE_COMMAND ""
    INSTALL_DIR ${PROJECT_BINARY_DIR}
    CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=<INSTALL_DIR>
               -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
               -DCMAKE_CXX_FLAGS=-fPIC -std=c++11 -w
)

ExternalProject_Get_Property(websocketpp install_dir)

# Export dep includes and lib paths
set(WSPP_INCLUDE_DIR ${install_dir}/include CACHE PATH "")
