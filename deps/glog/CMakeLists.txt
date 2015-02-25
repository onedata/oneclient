include(ExternalProject)

find_library(GLOG_FIND_LIBRARY libglog.a)
if(${GLOG_FIND_LIBRARY} MATCHES GLOG_FIND_LIBRARY-NOTFOUND)

    set(GLOG_CXXFLAGS "-fPIC -w")
    if(CMAKE_BUILD_TYPE MATCHES Debug)
        set(GLOG_CXXFLAGS "${GLOG_CXXFLAGS} ${CMAKE_CXX_FLAGS_DEBUG}")
    elseif(CMAKE_BUILD_TYPE MATCHES Release)
        set(GLOG_CXXFLAGS "${GLOG_CXXFLAGS} ${CMAKE_CXX_FLAGS_RELEASE}")
    elseif(CMAKE_BUILD_TYPE MATCHES RelWithDebInfo)
        set(GLOG_CXXFLAGS "${GLOG_CXXFLAGS} ${CMAKE_CXX_FLAGS_RELWITHDEBINFO}")
    elseif(CMAKE_BUILD_TYPE MATCHES MinSizeRel)
        set(GLOG_CXXFLAGS "${GLOG_CXXFLAGS} ${CMAKE_CXX_FLAGS_MINSIZEREL}")
    endif()

    set(GLOG_BUILD_DIR "${CMAKE_CURRENT_BINARY_DIR}/src/glog")
    if(EXISTS ${GLOG_BUILD_DIR}/.libs/libglog.a)
        set(GLOG_CONFIGURE_COMMAND "cd")    # nop
        set(GLOG_BUILD_COMMAND "cd")        # nop
    endif()

    ExternalProject_Add(glogBuild
        PREFIX ${CMAKE_CURRENT_BINARY_DIR}
        SOURCE_DIR ${GLOG_BUILD_DIR}
        GIT_REPOSITORY https://github.com/RoXeon/google-glog.git
        CONFIGURE_COMMAND ./configure --prefix=<INSTALL_DIR> CXXFLAGS=${GLOG_CXXFLAGS} CC=${CMAKE_CXX_COMPILER} CFLAGS=${GLOG_CXXFLAGS}
        UPDATE_COMMAND ""
        BUILD_COMMAND ${MAKE}
        BUILD_IN_SOURCE 1
        INSTALL_COMMAND ""
    )

    # Export dep includes and lib paths
    ExternalProject_Get_Property(glogBuild prefix)
    ExternalProject_Get_Property(glogBuild source_dir)
    set(GLOG_INCLUDE_DIR ${source_dir}/src CACHE PATH "")
    set(GLOG_LIBRARIES ${source_dir}/.libs/libglog.a CACHE FILEPATH "")

    add_custom_command(OUTPUT ${GLOG_LIBRARIES} DEPENDS glogBuild)

else()

    get_filename_component(GLOG_TMP_PATH ${GLOG_FIND_LIBRARY} PATH)

    set(GLOG_INCLUDE_DIR "${GLOG_TMP_PATH}/../include" CACHE PATH "")
    set(GLOG_LIBRARIES ${GLOG_FIND_LIBRARY} CACHE FILEPATH "")

endif()

add_custom_target(glog DEPENDS ${GLOG_LIBRARIES})
