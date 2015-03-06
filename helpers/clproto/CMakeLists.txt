find_package(Protobuf REQUIRED)

file(GLOB PROTO_FILES proto/*.proto)

protobuf_generate_cpp(PROTO_SRCS PROTO_HDRS ${PROTO_FILES})

add_library(clproto OBJECT ${PROTO_SRCS} ${PROTO_HDRS})
target_compile_options(clproto PRIVATE -w)
target_include_directories(clproto SYSTEM PRIVATE ${PROTOBUF_INCLUDE_DIR})

set(CLPROTO_SYSTEM_INCLUDE_DIRS
    ${PROTOBUF_INCLUDE_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}
    PARENT_SCOPE)

set(CLPROTO_LIBRARIES
    ${PROTOBUF_LIBRARIES}
    PARENT_SCOPE)

set(PROJECT_SOURCES
    $<TARGET_OBJECTS:clproto>
    PARENT_SCOPE)
