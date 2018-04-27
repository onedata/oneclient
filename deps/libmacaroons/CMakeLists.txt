add_library(libmacaroons STATIC
    ${CMAKE_CURRENT_SOURCE_DIR}/base64.c
    ${CMAKE_CURRENT_SOURCE_DIR}/macaroons.c
    ${CMAKE_CURRENT_SOURCE_DIR}/packet.c
    ${CMAKE_CURRENT_SOURCE_DIR}/port.c)

find_package(Sodium)

target_include_directories(libmacaroons PUBLIC ${CMAKE_CURRENT_SOURCE_DIR} ${SODIUM_INCLUDE_DIRS})
