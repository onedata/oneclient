# Finds python 2
# Exports the following variables:
# - PYTHON2_FOUND
# - PYTHON2_LDFLAGS
# - PYTHON2_CFLAGS
# - PYTHON2_VERSION_STRING

find_program(PYTHON2_CONFIG NAMES "python2-config")
if(PYTHON2_CONFIG)
    set(PYTHON2_FOUND ON)

    execute_process(COMMAND python2-config --ldflags OUTPUT_VARIABLE PYTHON2_LDFLAGS)
    string(STRIP "${PYTHON2_LDFLAGS}" PYTHON2_LDFLAGS)

    execute_process(COMMAND python2-config --includes OUTPUT_VARIABLE PYTHON2_CFLAGS)
    string(STRIP "${PYTHON2_CFLAGS}" PYTHON2_CFLAGS)

    execute_process(COMMAND python2-config --exec-prefix OUTPUT_VARIABLE PYTHON2_EXEC_PREFIX)
    string(STRIP "${PYTHON2_EXEC_PREFIX}" PYTHON2_EXEC_PREFIX)


	if($ENV{CONDA_BUILD})
        set(PYTHON2_VERSION "$ENV{CONDA_PY}")
        string(SUBSTRING "${PYTHON2_VERSION}" 0 1 PYTHON2_VERSION_STRING_MAJOR)
        string(SUBSTRING "${PYTHON2_VERSION}" 1 1 PYTHON2_VERSION_STRING_MINOR)
        set(PYTHON2_VERSION_STRING "${PYTHON2_VERSION_STRING_MAJOR}.${PYTHON2_VERSION_STRING_MINOR}")
    else()
        execute_process(COMMAND python --version ERROR_VARIABLE PYTHON2_VERSION)
        string(REGEX REPLACE "\n$" "" PYTHON2_VERSION "${PYTHON2_VERSION}")
        string(STRIP "${PYTHON2_VERSION}" PYTHON2_VERSION)
        string(REGEX MATCH "([0-9]+\\.[0-9]+)" PYTHON2_VERSION_STRING "${PYTHON2_VERSION}")
    endif()

    execute_process(COMMAND bash -c "cat /etc/os-release | grep ID_LIKE" OUTPUT_VARIABLE OS_ID_LIKE)
    if($ENV{CONDA_BUILD})
        set(PYTHON2_SITE_DIR "site-packages")
    elseif(OS_ID_LIKE MATCHES ".+debian.+")
        set(PYTHON2_SITE_DIR "dist-packages")
    else()
        set(PYTHON2_SITE_DIR "site-packages")
    endif()

    find_library(LIBBOOST_PYTHON2 NAMES boost_python-py27 boost_python)

    message(STATUS "Found Python2 version: ${PYTHON2_VERSION_STRING} (${PYTHON2_VERSION})")
    message(STATUS "Python2 ldflags: ${PYTHON2_LDFLAGS}")
    message(STATUS "Python2 cflags: ${PYTHON2_CFLAGS}")
    message(STATUS "Python2 libboost-python: ${LIBBOOST_PYTHON2}")
    message(STATUS "Python2 site directory: ${PYTHON2_SITE_DIR}")
else(PYTHON2_CONFIG)
    message("Couldn't find Python2")
endif(PYTHON2_CONFIG)
