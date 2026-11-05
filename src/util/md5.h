/**
 * @file md5.h
 * @author Bartek Kryza
 * @copyright (C) 2022 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include <string>
#include <vector>

namespace one {
namespace client {
namespace util {
namespace md5 {

std::string md5(const char *input, size_t inputSize);
std::string md5(const std::string &);

}
}
}
}
