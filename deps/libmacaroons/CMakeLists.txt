add_library(libmacaroons INTERFACE)
target_sources(libmacaroons INTERFACE
    ${CMAKE_CURRENT_SOURCE_DIR}/base64.c
    ${CMAKE_CURRENT_SOURCE_DIR}/macaroons.c
    ${CMAKE_CURRENT_SOURCE_DIR}/packet.c
    ${CMAKE_CURRENT_SOURCE_DIR}/port.c)

target_include_directories(libmacaroons INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})
