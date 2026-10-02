/**
 * @file s3util.cc
 * @author Bartek Kryza
 * @copyright (C) 2022-present Onedata.org
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "s3util.h"

#include <regex>

namespace one {
namespace s3 {
namespace util {

namespace {
const std::regex &getValidBucketNamePattern()
{
    static std::regex validBucketNamePattern("^[a-zA-Z0-9._-]+$");
    return validBucketNamePattern;
}
} // namespace

bool isBucketNameValid(const std::string &name)
{
    bool result{true};

    constexpr auto kMinBucketLength{3};
    constexpr auto kMaxBucketLength{255};

    if (name.size() < kMinBucketLength) {
        result = false;
    }
    else if (name.size() > kMaxBucketLength) {
        result = false;
    }
    else if (!std::regex_match(name, getValidBucketNamePattern())) {
        result = false;
    }

    return result;
}

} // namespace util
} // namespace s3
} // namespace one