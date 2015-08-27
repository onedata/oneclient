add_library(libmacaroons-cpp INTERFACE)
target_link_libraries(libmacaroons-cpp INTERFACE libmacaroons)
target_include_directories(libmacaroons-cpp INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})
