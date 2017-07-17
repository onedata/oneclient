/**
 * @file base64.h
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include <string>

namespace one {
namespace client {
namespace util {
namespace base64 {

/**
 * Encodes any std::string compatible string to b64
 *
 * @param bindata Binary data
 * @param result Output B64 result, if return value is true.
 *
 * @return Encoding status
 */
template <typename T, typename V = T>
bool base64_encode(const T &bindata, V &retval)
{
    static const char b64_table[65] =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

    const std::size_t binlen = bindata.size();
    // Padd the result with '='
    retval.clear();
    retval.assign(V((((binlen + 2) / 3) * 4), '='));

    std::size_t outpos = 0;
    int bits_collected = 0;
    unsigned int accumulator = 0;

    for (auto i = bindata.cbegin(); i != bindata.cend(); ++i) {
        accumulator = (accumulator << 8) | (*i & 0xffu);
        bits_collected += 8;
        while (bits_collected >= 6) {
            bits_collected -= 6;
            retval[outpos++] =
                b64_table[(accumulator >> bits_collected) & 0x3fu];
        }
    }
    if (bits_collected > 0) {
        accumulator <<= 6 - bits_collected;
        retval[outpos++] = b64_table[accumulator & 0x3fu];
    }

    return true;
}

/**
 * Decodes any b64 encoded std::string compatible string to binary data
 *
 * @param result Output B64 result, if return value is true.
 * @param bindata Result binary data
 *
 * @return Decoding status
 */
template <typename T, typename V = T>
bool base64_decode(const T &b64data, V &retval)
{
    static const char reverse_table[128] = {64, 64, 64, 64, 64, 64, 64, 64, 64,
        64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
        64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 62, 64,
        64, 64, 63, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 64, 64, 64, 64, 64,
        64, 64, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
        18, 19, 20, 21, 22, 23, 24, 25, 64, 64, 64, 64, 64, 64, 26, 27, 28, 29,
        30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
        48, 49, 50, 51, 64, 64, 64, 64, 64};

    int bits_collected = 0;
    unsigned int accumulator = 0;

    retval.clear();

    for (auto i = b64data.cbegin(); i != b64data.cend(); ++i) {
        const int c = *i;
        if (std::isspace(c) || c == '=') {
            continue;
        }
        if ((c > 127) || (c < 0) || (reverse_table[c] > 63)) {
            // Illegal characters in B64
            return false;
        }
        accumulator = (accumulator << 6) | reverse_table[c];
        bits_collected += 6;
        if (bits_collected >= 8) {
            bits_collected -= 8;
            retval += (char)((accumulator >> bits_collected) & 0xffu);
        }
    }
    return true;
}

} // namespace base64
} // namespace util
} // namespace client
} // namespace one