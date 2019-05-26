# Finds python 3
# Exports the following variables:
# - PYTHON3_FOUND
# - PYTHON3_LDFLAGS
# - PYTHON3_CFLAGS
# - PYTHON3_VERSION_STRING

find_program(PYTHON3_CONFIG NAMES "python3-config")
if(PYTHON3_CONFIG)
    set(PYTHON3_FOUND ON)

    execute_process(COMMAND python3-config --ldflags OUTPUT_VARIABLE PYTHON3_LDFLAGS)
    string(STRIP "${PYTHON3_LDFLAGS}" PYTHON3_LDFLAGS)

    execute_process(COMMAND python3-config --includes OUTPUT_VARIABLE PYTHON3_CFLAGS)
    string(STRIP "${PYTHON3_CFLAGS}" PYTHON3_CFLAGS)

    execute_process(COMMAND python3-config --exec-prefix OUTPUT_VARIABLE PYTHON3_EXEC_PREFIX)
    string(STRIP "${PYTHON3_EXEC_PREFIX}" PYTHON3_EXEC_PREFIX)

    execute_process(COMMAND python3 --version OUTPUT_VARIABLE PYTHON3_VERSION)
    string(STRIP "${PYTHON3_VERSION}" PYTHON3_VERSION)
    string(REGEX MATCH "([0-9]+\\.[0-9]+)" PYTHON3_VERSION_STRING "${PYTHON3_VERSION}")

    find_library(LIBBOOST_PYTHON3 NAMES boost_python-py35 boost_python-py36 boost_python-py37 boost_python3 libboost_python3.so.1.58.0)

    execute_process(COMMAND bash -c "cat /etc/os-release | grep ID_LIKE" OUTPUT_VARIABLE OS_ID_LIKE)
    if(OS_ID_LIKE MATCHES ".+debian.+")
        set(PYTHON3_SITE_DIR "dist-packages")
    else()
        set(PYTHON3_SITE_DIR "site-packages")
    endif()

    message(STATUS "Found Python3 version: ${PYTHON3_VERSION_STRING} (${PYTHON3_VERSION})")
    message(STATUS "Python3 ldflags: ${PYTHON3_LDFLAGS}")
    message(STATUS "Python3 cflags: ${PYTHON3_CFLAGS}")
    message(STATUS "Python3 libboost-python: ${LIBBOOST_PYTHON3}")
    message(STATUS "Python3 site directory: ${PYTHON3_SITE_DIR}")
else(PYTHON3_CONFIG)
    set(_PYTHON3_VERSIONS 3.7 3.6 3.5)
    foreach(_PYTHON3_VERSION ${_PYTHON3_VERSIONS})
        execute_process(COMMAND pkg-config --cflags "python-${_PYTHON3_VERSION}" RESULT_VARIABLE RETCODE)
        if("${RETCODE}" STREQUAL "0")
            set(PYTHON3_FOUND ON)
            execute_process(COMMAND pkg-config --libs "python-${_PYTHON3_VERSION}" OUTPUT_VARIABLE PYTHON3_LDFLAGS)
            string(STRIP "${PYTHON3_LDFLAGS}" PYTHON3_LDFLAGS)

            execute_process(COMMAND pkg-config --cflags-only-I "python-${_PYTHON3_VERSION}" OUTPUT_VARIABLE PYTHON3_CFLAGS)
            string(STRIP "${PYTHON3_CFLAGS}" PYTHON3_CFLAGS)

            execute_process(COMMAND python3 --version OUTPUT_VARIABLE PYTHON3_VERSION)
            string(STRIP "${PYTHON3_VERSION}" PYTHON3_VERSION)
            string(REGEX MATCH "([0-9]+\\.[0-9]+)" PYTHON3_VERSION_STRING "${PYTHON3_VERSION}")

            message(STATUS "Found Python3 version: ${PYTHON3_VERSION_STRING} (${PYTHON3_VERSION})")
            message(STATUS "Python3 ldflags: ${PYTHON3_LDFLAGS}")
            message(STATUS "Python3 cflags: ${PYTHON3_CFLAGS}")
            break()
        endif()
    endforeach()
    message("Couldn't find Python3")
endif(PYTHON3_CONFIG)
