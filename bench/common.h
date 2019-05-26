/**
 * @file common.h
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include <folly/FBString.h>

#include <chrono>
#include <random>

namespace one {
namespace bench {

constexpr auto ONEBENCH_FILE_PREFIX = "onebench-";
constexpr auto ONEBENCH_FILEID_LENGTH = 16;

inline folly::fbstring randStr(const int len)
{
    static const char characters[] =
        "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

    folly::fbstring result;
    result.reserve(len);

    for (int i = 0; i < len; ++i) {
        result.push_back(characters[std::rand() % (sizeof(characters) - 1)]);
    }

    result[len] = 0;

    return result;
}

using TestWorkerID = int;
using TestResultID = int;

using Clock = std::chrono::system_clock;
using TimePoint = std::chrono::time_point<Clock>;
}
};
