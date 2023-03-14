find_package(GFlags REQUIRED)

include(../cmake/CPM.cmake)

CPMAddPackage(
        NAME drogon
        VERSION 1.8.2
        GITHUB_REPOSITORY bkryza/drogon
        GIT_TAG 505b15c96367d0ffc7ef7b54345c1aeb96807cb9
        OPTIONS "BUILD_SHARED_LIBS OFF" "BUILD_EXAMPLES OFF" "BUILD_ORM OFF" "BUILD_CTL OFF" "BUILD_BROTLI OFF"
)

set(ONES3_LIBRARIES
        libmacaroons-cpp
        ${PLATFORM_EXTRA_LIBS}
        ${FUSE_LIBRARIES}
        ${Boost_LIBRARIES}
        ${SODIUM_LIBRARIES}
        ${PROTOBUF_LIBRARIES}
        ${POCO_LIBRARIES}
        ${NSS_LIBRARIES}
        ${FOLLY_LIBRARIES}
        ${HELPERS_LIBRARIES}
        ${OPENSSL_LIBRARIES}
        gflags
        glog
        unwind
        drogon
        )

if(WITH_CEPH)
    list(APPEND ONES3_LIBRARIES ${RADOSSTRIPER_LIBRARY} ${RADOS_LIBRARY} ${CEPHCOMMON_LIBRARY})
endif(WITH_CEPH)

if(WITH_S3)
    list(APPEND ONES3_LIBRARIES ${AWS_SDK_LIBRARIES})
endif(WITH_S3)

if(WITH_SWIFT)
    list(APPEND ONES3_LIBRARIES ${SWIFT_SDK_LIBRARIES})
endif(WITH_SWIFT)

if(WITH_GLUSTERFS)
    list(APPEND ONES3_LIBRARIES ${GLUSTERFSAPI_LIBRARIES})
endif(WITH_GLUSTERFS)

if(WITH_XROOTD)
    list(APPEND ONES3_LIBRARIES ${XROOTD_LIBRARIES})
endif(WITH_XROOTD)

if(WITH_NFS)
    list(APPEND ONES3_LIBRARIES ${NFS_LIBRARIES})
endif(WITH_NFS)

include_directories(SYSTEM
        ${CMAKE_CURRENT_SOURCE_DIR}/../src
        ${drogon_SOURCE_DIR}/lib/src)

add_executable(ones3
        ones3.cc
        s3Server.cc
        serialization.cc
        s3Logic.cc
        s3Server.cc
        s3Exception.cc
        restCommon.cc
        onepanelRestClient.cc
        onezoneRestClient.cc
        $<TARGET_OBJECTS:helpers>
        $<TARGET_OBJECTS:client>
        $<TARGET_OBJECTS:clproto>)

add_dependencies(ones3 helpers)

target_link_libraries(ones3 PRIVATE
        ${ONES3_LIBRARIES})

set_target_properties(ones3 PROPERTIES
        BUILD_WITH_INSTALL_RPATH true
        INSTALL_RPATH_USE_LINK_PATH true
        INSTALL_RPATH "${CUSTOM_RPATH}")

install(TARGETS ones3 DESTINATION ${CMAKE_INSTALL_FULL_BINDIR})
