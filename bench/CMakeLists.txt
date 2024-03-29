find_package(GFlags REQUIRED)

set(ONEBENCH_LIBRARIES
    libmacaroons-cpp
    ${PLATFORM_EXTRA_LIBS}
    ${FUSE_LIBRARIES}
    ${Boost_LIBRARIES}
    ${SODIUM_LIBRARIES}
    ${PROTOBUF_LIBRARIES}
    ${NSS_LIBRARIES}
    ${FOLLY_LIBRARIES}
    ${HELPERS_LIBRARIES}
    ${OPENSSL_LIBRARIES}
    gflags
    glog
    unwind
    )

if(WITH_CEPH)
    list(APPEND ONEBENCH_LIBRARIES ${RADOSSTRIPER_LIBRARY} ${RADOS_LIBRARY} ${CEPHCOMMON_LIBRARY})
endif(WITH_CEPH)

if(WITH_S3)
    list(APPEND ONEBENCH_LIBRARIES ${AWS_SDK_LIBRARIES})
endif(WITH_S3)

if(WITH_SWIFT)
    list(APPEND ONEBENCH_LIBRARIES ${SWIFT_SDK_LIBRARIES})
endif(WITH_SWIFT)

if(WITH_GLUSTERFS)
    list(APPEND ONEBENCH_LIBRARIES ${GLUSTERFSAPI_LIBRARIES})
endif(WITH_GLUSTERFS)

if(WITH_XROOTD)
    list(APPEND ONEBENCH_LIBRARIES ${XROOTD_LIBRARIES})
endif(WITH_XROOTD)

if(WITH_NFS)
    list(APPEND ONEBENCH_LIBRARIES ${NFS_LIBRARIES})
endif(WITH_NFS)

add_executable(onebench
    "onebench.cc"
    "testRunner.cc"
    "testRunnerConfig.cc"
    "testWorkerRndWr.cc"
    "testWorkerRndRd.cc"
    "testWorkerRndRdHTTP.cc"
    $<TARGET_OBJECTS:helpers>
    $<TARGET_OBJECTS:clproto>)

add_dependencies(onebench helpers)

target_link_libraries(onebench PRIVATE
    ${ONEBENCH_LIBRARIES})

set_target_properties(onebench PROPERTIES
    BUILD_WITH_INSTALL_RPATH true
    INSTALL_RPATH_USE_LINK_PATH true
    INSTALL_RPATH "${CUSTOM_RPATH}")

install(TARGETS onebench DESTINATION ${CMAKE_INSTALL_FULL_BINDIR})
