/**
 * @file s3util.h
 * @author Bartek Kryza
 * @copyright (C) 2022-present Onedata.org
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include <string>

namespace one {
namespace s3 {
namespace util {

bool isBucketNameValid(const std::string &name);

} // namespace util
} // namespace s3
} // namespace one