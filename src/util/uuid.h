/**
 * @file uuid.h
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "base64.h"

#include <folly/FBString.h>
#include <folly/String.h>
#include <spdlog/fmt/fmt.h>

namespace one {
namespace client {
namespace util {
namespace uuid {

/**
 * Extracts space Id from Onedata UUID.
 * @param uuid Onedata uuid Based64 encoded Onedata uuid.
 */
folly::fbstring uuidToSpaceId(const folly::fbstring &uuid)
{
    folly::fbstring result;
    folly::fbstring decodedUuid;
    auto status = util::base64::base64_url_decode(uuid, decodedUuid);

    if (status) {
        std::vector<folly::StringPiece> v;
        folly::split("#", decodedUuid, v);

        if ((v[0] != "guid"))
            throw std::invalid_argument("Invalid Onedata uuid format.");

        auto spaceFragment = v[1];

        if (spaceFragment.removePrefix("space_")) {
            return spaceFragment.toString();
        }

        throw std::invalid_argument("Onedata uuid does not contain space id.");
    }
    else
        throw std::invalid_argument("Base64 decoding of Onedata uuid failed.");
}

/**
 * Generates space UUID from spaceId, which is in the format:
 *    guid#space_<spaceId>#<spaceId>
 * < and > are omitted
 *
 * @param spaceId ID of the space
 * @return Space UUID
 */
folly::fbstring spaceIdToSpaceUUID(const folly::fbstring &spaceId)
{
    folly::fbstring result;
    util::base64::base64_url_encode(
        fmt::format(
            "guid#space_{}#{}", spaceId.toStdString(), spaceId.toStdString()),
        result);
    return result;
}
}
}
}
}
