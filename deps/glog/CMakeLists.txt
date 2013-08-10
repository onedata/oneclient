project(dep-glog)

include(ExternalProject)

set(GLOG_BUILD_COMMAND make -j8)
set(GLOG_CONFIGURE_COMMAND ./configure --prefix=<INSTALL_DIR>)
set(GLOG_BUILD_DIR "${CMAKE_CURRENT_BINARY_DIR}/src/glog")
if(EXISTS ${GLOG_BUILD_DIR}/.libs/libglog.a)
    set(GLOG_CONFIGURE_COMMAND "cd")    # nop
    set(GLOG_BUILD_COMMAND "cd")        # nop
endif()

ExternalProject_Add(glog
    PREFIX "${CMAKE_CURRENT_BINARY_DIR}"
    SOURCE_DIR ${GLOG_BUILD_DIR}
    SVN_REPOSITORY http://google-glog.googlecode.com/svn/trunk
    CONFIGURE_COMMAND ${GLOG_CONFIGURE_COMMAND}
    UPDATE_COMMAND ""
    BUILD_COMMAND ${GLOG_BUILD_COMMAND}
    BUILD_IN_SOURCE 1
    INSTALL_COMMAND ""
)

# Export dep includes and lib paths
ExternalProject_Get_Property(glog prefix)
ExternalProject_Get_Property(glog source_dir)
set(GLOG_INCLUDE_DIR ${source_dir}/src PARENT_SCOPE)
set(GLOG_LIBRARIES ${source_dir}/.libs/libglog.a PARENT_SCOPE)
