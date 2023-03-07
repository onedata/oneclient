/**
 * @file mime.h
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
namespace mime {

const std::string &mimeFromPath(const std::string &path);

}
}
}
}