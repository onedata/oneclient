/**
 * @file logging.h
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include <boost/algorithm/string.hpp>
#include <boost/current_function.hpp>
#include <boost/range/adaptor/map.hpp>
#include <folly/Conv.h>
#include <glog/logging.h>

#include <bitset>
#include <limits>
#include <map>
#include <unordered_map>

#pragma once

// clang-format off
/**
 * This file provides a set of logging macros to be used in helpers and
 * oneclient components.
 *
 * The macros are wrappers over Google GLOG library logging facilities. The
 * following rules should be followed when adding logs in the code:
 *  - Runtime info, warning and error logs which will be included in release
 *    builds should be logged using LOG(INFO), LOG(WARNING) and LOG(ERROR)
 *    macros. They should be kept to minimum and provide information on
 *    errors and their possible mitigation.
 *  - Debug logs which will be included in debug builds only, should be logged
 *    using GLOG verbose logging facility, i.e. DVLOG(n) macros. Below are
 *    several convenience macros which should be used in the code instead of
 *    GLOG native macros:
 *     * LOG_DBG(n) - log with specific verbosity level
 *     * LOG_FCALL() - log function signature, should be used at the beginning
 *                     of functions and methods, which are relevant for the
 *                     program flow
 *     * LOG_FARG(arg) - appends to LOG_FCALL() name and value of a specific
 *                       function argument, example use:
 *                       LOG_FCALL() << LOG_FARG(arg1) << LOG_FARG(arg2)
 *     * LOG_FARGx(arg) - allows to easily log more complex values, such as
 *                        numbers in different numeric bases (LOG_FARGB,
 *                        LOG_FARGO, LOG_FARGH), vectors and lists (LOG_FARGV)
 *                        and maps (LOG_FARGM)
 *    Debug logs can be enabled by setting global GLOG variables:
 *     * FLAGS_v = n; - where n determines the verbosity level
 *     * FLAGS_vmodule = pattern; - where pattern specifies a filter to enable
 *                                 logs only for specific compilation units
 *                                 with specific verbosity levels
 */
// clang-format on

/**
 * Logs a value in binary format
 */
#define LOG_BIN(X)                                                             \
    std::bitset<std::numeric_limits<decltype(X)>::digits>(X) << "b"

/**
 * Logs a value in octal format
 */
#define LOG_OCT(X) "0" << std::oct << X << std::dec

/**
 * Logs a value in hexadecimal format
 */
#define LOG_HEX(X) "0x" << std::hex << X << std::dec

/**
 * Appends to stream a serialized vector of strings
 */
#define LOG_VEC(X) "[" << boost::algorithm::join(X, ",") << "]"

/**
 * Appends to stream a serialized map in the form:
 * {key1,key2,...,keyN} => {val1,val2,...,valN}
 */
#define LOG_MAP(X) one::logging::mapToString(X)

/**
 * Macro for verbose logging in debug mode
 */
#define LOG_DBG(X) DVLOG(X)

/**
 * Logs function call, should be added at the beginning of the function or
 * method body and log the values of main parameters.
 */
#define LOG_FCALL()                                                            \
    DVLOG(2) << "Called " << BOOST_CURRENT_FUNCTION << " with arguments: "

/**
 * Logs function return including optionally the return value.
 */
#define LOG_FRET()                                                             \
    DVLOG(2) << "Returning from " << BOOST_CURRENT_FUNCTION << " with value: "

/**
 * Logs function argument - must be used in 'stream' context and preceded by
 * LOG_FCALL() or DVLOG(n).
 */
#define LOG_FARG(ARG) " " #ARG "=" << ARG

/**
 * Log macros for different numeric bases.
 */
#define LOG_FARGB(ARG) " " #ARG "=" << LOG_BIN(ARG)
#define LOG_FARGO(ARG) " " #ARG "=" << LOG_OCT(ARG)
#define LOG_FARGH(ARG) " " #ARG "=" << LOG_HEX(ARG)

/**
 * Logs function argument which is a vector - must be used in 'stream' context
 * and preceded by LOG_FCALL() or DVLOG(n).
 */
#define LOG_FARGV(ARG) " " #ARG "=" << LOG_VEC(ARG)

/**
 * Logs function argument which is a map - must be used in 'stream' context
 * and preceded by LOG_FCALL() or DVLOG(n).
 */
#define LOG_FARGM(ARG) " " #ARG "=" << LOG_MAP(ARG)

namespace one {
namespace logging {

/**
 * Converts any map to a string for logging.
 */
template <typename TMap, typename TResult = std::string>
TResult mapToString(const TMap &map)
{
    TResult result = "{ ";
    for (const auto &kv : map) {
        result += folly::to<TResult>(kv.first) + " => " +
            folly::to<TResult>(kv.second) + ", ";
    }
    result = result.substr(0, result.size() - 2);
    result += " }";
    return result;
}
}
}
