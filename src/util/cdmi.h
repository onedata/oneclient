/**
 * @file cdmi.h
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "util/base64.h"

#include <boost/crc.hpp>

#include <iomanip>
#include <sstream>
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

std::string uuidToObjectId(const std::string &input)
{
    std::string decodedUuid;
    base64::base64_decode(input, decodedUuid);

    const uint32_t enterpriseNumber = 0;

    /*
     * Build an object ID given an enterprise number and data as a
     * binary. We ensure here that the Data is not more than 32 bytes. The
     * object ID is composed of a number of fields:
     *
     * +----------+------------+-----------+--------+-------+-----------+
     * |     0    | 1 | 2 | 3  |     4     |   5    | 6 | 7 | 8 |..| 39 |
     * +----------+------------+-----------+--------+-------+-----------+
     * | Reserved | Enterprise | Reserverd | Length |  CRC  | Opaque    |
     * | (zero)   | Number     | (zero)    |        |       | Data      |
     * +----------+------------+-----------+--------+-------+-----------+
     */
    std::vector<uint8_t> objectId;
    // Reserved
    objectId.push_back(0);

    // Enterprise number
    objectId.push_back((0x00FFFFFF & enterpriseNumber) >> 16);
    objectId.push_back((0x0000FFFF & enterpriseNumber) >> 8);
    objectId.push_back(0x0000FF & enterpriseNumber);

    // Reserved
    objectId.push_back(0);

    if (decodedUuid.size() > 255)
        throw std::runtime_error(
            "Input too long to encode as CDMI Object ID: " + decodedUuid);

    // Length
    objectId.push_back(decodedUuid.size());

    // CRC
    // At first set to 0x0000 - then replace with CRC of entire message
    objectId.push_back((uint8_t)(0));
    objectId.push_back((uint8_t)(0));

    // Opaque data
    for (char &c : decodedUuid)
        objectId.push_back((uint8_t)c);

    // The CDMI specification requires a CRC field as part of the object
    // ID. The CRC algorithim specified has the following parameters:
    // -- Name: "CRC-16"
    // -- Width: 16
    // -- Poly: 0x8005
    // -- Init: 0x0000
    // -- RefIn: True
    // -- RefOut: True
    // -- XorOut: 0x0000
    // -- Check: 0xBB3D
    boost::crc_optimal<16, 0x8005, 0, 0, true, true> crc16;
    crc16.process_bytes(objectId.data(), objectId.size());
    objectId[6] = (uint8_t)(crc16.checksum() >> 8);
    objectId[7] = (uint8_t)(crc16.checksum() & 0xFF);

    // Convert to Base 16
    std::stringstream ss;
    for (uint8_t &b : objectId)
        ss << n2hexstr(b);

    return ss.str();
}
}
}
}
}
