/*
 * Copyright 2000-2014 NeuStar, Inc. All rights reserved.
 * NeuStar, the Neustar logo and related names and logos are registered
 * trademarks, service marks or tradenames of NeuStar, Inc. All other
 * product names, company names, marks, logos and symbols may be trademarks
 * of their respective owners.
 */

/*
 * utils.h
 *
 *  Created on: Jun 12, 2014
 *      Author: vpoliboy
 */

#ifndef UTILS_H_
#define UTILS_H_

#include "cppmetrics/core/types.h"
#include <chrono>
#include <iomanip>
#include <locale>
#include <sstream>

namespace cppmetrics {
namespace core {

// bug fix: the duration from epoch must be in UTC (not localtime)
inline auto get_duration_from_epoch()
{
    std::chrono::system_clock::time_point time_t_epoch(
        std::chrono::system_clock::from_time_t(0));
    std::chrono::system_clock::time_point now =
        std::chrono::system_clock::now();
    return (now - time_t_epoch);
}

inline auto get_millis_from_epoch()
{
    return std::chrono::duration_cast<std::chrono::milliseconds>(
        get_duration_from_epoch())
        .count();
}

inline auto get_seconds_from_epoch()
{
    return std::chrono::duration_cast<std::chrono::seconds>(
        get_duration_from_epoch())
        .count();
}

inline std::string utc_timestamp(const std::locale &current_locale)
{
    std::ostringstream ss;
    auto now_c =
        std::chrono::system_clock::to_time_t(std::chrono::system_clock::now());
    ss << std::put_time(std::localtime(&now_c), "%c");
    return ss.str();
}
}
}

#endif /* UTILS_H_ */
