set(GSI_UTILS_SOURCES grid_proxy_init.c grid_proxy_info.c globus_stdio_ui.c)

# Theres no fmemopen function on Darwin platform, so compile custom one.
if(APPLE)
    add_library(globus_proxy_utils ${GSI_UTILS_SOURCES} fmemopen.c)
else()
    add_library(globus_proxy_utils ${GSI_UTILS_SOURCES})
endif()