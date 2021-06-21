/**
 * @file cdmi.h
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include <string>
#include <vector>

namespace one {
namespace client {
namespace util {
namespace cdmi {

/**
 * Converts a string to base 16 (hexadecimal) string
 */
template <typename I> std::string n2hexstr(I w, size_t hex_len = sizeof(I) << 1)
{
    static const char *digits = "0123456789ABCDEF";
    std::string rc(hex_len, '0');
    for (size_t i = 0, j = (hex_len - 1) * 4; i < hex_len; ++i, j -= 4)
        rc[i] = digits[(w >> j) & 0x0f];
    return rc;
}

/**
 * Converts a base16 encoded number passed as string to an ASCII string
 */
std::string hexstr2str(const std::string &hexstr);

std::string objectIdToUUID(const std::string &objectId);

std::string uuidToObjectId(const std::string &input);
}
}
}
}
