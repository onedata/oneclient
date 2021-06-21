/**
 * @file cdmi.cc
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "cdmi.h"

#include "logging.h"
#include "util/base64.h"

#include <boost/crc.hpp>

#include <iomanip>
#include <iostream>

namespace one {
namespace client {
namespace util {
namespace cdmi {

/**
 * Converts a base16 encoded number passed as string to an ASCII string
 */
std::string hexstr2str(const std::string &hexstr)
{
    auto len = hexstr.length();
    std::string out;
    out.reserve(len / 2);
    constexpr auto kHexBase = 16U;
    for (auto i = 0UL; i < len; i += 2) {
        std::string hexByte = hexstr.substr(i, 2);
        out.push_back(static_cast<char>(stoi(hexByte, nullptr, kHexBase)));
    }
    return out;
}

std::string objectIdToUUID(const std::string &objectId)
{
    auto objectIdBytes = hexstr2str(objectId);

    const auto kCDMIObjectIDDataOffset = 8U;

    std::string result;
    base64::base64_url_encode(
        objectIdBytes.substr(kCDMIObjectIDDataOffset), result);
    return result;
}

std::string uuidToObjectId(const std::string &input)
{
    std::string decodedUuid;
    base64::base64_decode(input, decodedUuid);

    const uint32_t enterpriseNumber = 0;
    const auto kCDMIObjectIDSize = 64;

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
    objectId.reserve(kCDMIObjectIDSize);

    // Reserved
    objectId.push_back(0);

    // Enterprise number
    objectId.push_back((0x00FFFFFF & enterpriseNumber) >> 16); // NOLINT
    objectId.push_back((0x0000FFFF & enterpriseNumber) >> 8); // NOLINT
    objectId.push_back(0x0000FF & enterpriseNumber); // NOLINT

    // Reserved
    objectId.push_back(0);

    if (decodedUuid.size() > 255) // NOLINT
        throw std::runtime_error(
            "Input too long to encode as CDMI Object ID: " + decodedUuid);

    // Length
    objectId.push_back(decodedUuid.size());

    // CRC
    // At first set to 0x0000 - then replace with CRC of entire message
    objectId.push_back(static_cast<uint8_t>(0));
    objectId.push_back(static_cast<uint8_t>(0));

    // Opaque data
    for (char &c : decodedUuid)
        objectId.push_back(static_cast<uint8_t>(c));

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
    const auto kCRCWidth = 16U;
    const auto kCRCPoly = 0x8005;
    boost::crc_optimal<kCRCWidth, kCRCPoly, 0, 0, true, true> crc16;
    crc16.process_bytes(objectId.data(), objectId.size());
    objectId[6] = static_cast<uint8_t>(crc16.checksum() >> 8); // NOLINT
    objectId[7] = static_cast<uint8_t>(crc16.checksum() & 0xFF); // NOLINT

    // Convert to Base 16
    std::stringstream ss;
    for (uint8_t &b : objectId)
        ss << n2hexstr(b);

    return ss.str();
}
} // namespace cdmi
} // namespace util
} // namespace client
} // namespace one
