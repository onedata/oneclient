project(dep-veilhelpers)

include(ExternalProject)

ExternalProject_Add(veilhelpers
    PREFIX "${CMAKE_CURRENT_BINARY_DIR}"
    GIT_REPOSITORY ssh://git@git.plgrid.pl:7999/vfs/veilhelpers.git
    INSTALL_COMMAND ""
)

# Export dep includes and lib paths
ExternalProject_Get_Property(veilhelpers binary_dir)
ExternalProject_Get_Property(veilhelpers source_dir)
set(VEILHELPERS_INCLUDE_DIR ${source_dir}/include PARENT_SCOPE)
set(VEILHELPERS_LIBRARIES ${binary_dir}/libveilhelpers.a PARENT_SCOPE)