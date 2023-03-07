/**
 * @file md5.cc
 * @author Bartek Kryza
 * @copyright (C) 2022 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include <openssl/md5.h>

#include <iomanip>
#include <sstream>
#include <string>

namespace one {
namespace client {
namespace util {
namespace md5 {

std::string md5(const char *input, size_t inputSize)
{
    unsigned char result[MD5_DIGEST_LENGTH];
    MD5((unsigned char *)input, inputSize, result);

    std::ostringstream sout;
    sout << std::hex << std::setfill('0');
    for (long long c : result) {
        sout << std::setw(2) << (long long)c;
    }
    return sout.str();
}

std::string md5(const std::string &input)
{
    return md5(input.data(), input.size());
}

}
}
}
}