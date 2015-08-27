add_library(json11 INTERFACE)
target_sources(json11 INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/json11.cpp)
target_include_directories(json11 INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})
