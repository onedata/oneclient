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
    ${PROJECT_SOURCES}
    PARENT_SCOPE)

option(BUILD_NIF_LIBS "Build the nif library" OFF)
if(BUILD_NIF_LIBS)

add_library(messages.nif SHARED EXCLUDE_FROM_ALL build/messages.nif.cc $<TARGET_OBJECTS:clproto>)
target_link_libraries(messages.nif protobuf)
set_target_properties(messages.nif PROPERTIES PREFIX "" SUFFIX .so)

if(APPLE)
set_target_properties(messages.nif PROPERTIES
    LINK_FLAGS "-flat_namespace -undefined dynamic_lookup")
endif(APPLE)

endif(BUILD_NIF_LIBS)
