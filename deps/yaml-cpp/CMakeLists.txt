project(dep-yaml-cpp)

include(ExternalProject)

find_library(YAML_FIND_LIBRARY libyaml-cpp.a)
if(${YAML_FIND_LIBRARY} MATCHES YAML_FIND_LIBRARY-NOTFOUND)

    ExternalProject_Add(yaml-cpp
        PREFIX "${CMAKE_CURRENT_BINARY_DIR}"
        GIT_REPOSITORY https://github.com/nebirhos/yaml-cpp.git
        INSTALL_COMMAND ""
        UPDATE_COMMAND ""
    )

    # Export dep includes and lib paths
    ExternalProject_Get_Property(yaml-cpp binary_dir)
    ExternalProject_Get_Property(yaml-cpp source_dir)
    set(YAML_INCLUDE_DIR ${source_dir}/include PARENT_SCOPE)
    set(YAML_LIBRARIES ${binary_dir}/libyaml-cpp.a PARENT_SCOPE)

else()

    ## Assume that yaml-cpp include path resides in default compiler's include dirs
    set(YAML_INCLUDE_DIR "" PARENT_SCOPE)
    set(YAML_LIBRARIES ${YAML_FIND_LIBRARY} PARENT_SCOPE)

endif()