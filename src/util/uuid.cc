/**
 * @file uuid.cc
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "uuid.h"

#include "base64.h"

#include <spdlog/fmt/fmt.h>

namespace one {
namespace client {
namespace util {
namespace uuid {

folly::fbstring uuidToSpaceId(const folly::fbstring &uuid)
{
    folly::fbstring result;
    folly::fbstring decodedUuid;
    auto status = util::base64::base64_url_decode(uuid, decodedUuid);

    if (status) {
        std::vector<folly::StringPiece> v;
        folly::split("#", decodedUuid, v);

        if ((v.size() < 3) || (v[0] != "guid" && v[0] != "shareGuid"))
            throw std::invalid_argument("Invalid Onedata uuid format.");

        return v[2].toString();
    }

    throw std::invalid_argument("Base64 decoding of Onedata uuid failed.");
}

folly::fbstring uuidToTmpDirId(const folly::fbstring &uuid)
{
    const auto spaceId = uuidToSpaceId(uuid);
    return guidToUUID(spaceId, fmt::format("tmp_{}", spaceId.toStdString()));
}

folly::fbstring uuidToGuid(const folly::fbstring &uuid)
{
    folly::fbstring result;
    folly::fbstring decodedUuid;
    auto status = util::base64::base64_url_decode(uuid, decodedUuid);

    if (status) {
        std::vector<folly::StringPiece> v;
        folly::split("#", decodedUuid, v);

        if ((v.size() < 3) || (v[0] != "guid" && v[0] != "shareGuid"))
            throw std::invalid_argument("Invalid Onedata uuid format.");

        return v[1].toString();
    }

    throw std::invalid_argument("Base64 decoding of Onedata uuid failed.");
}

folly::fbstring spaceIdToSpaceUUID(const folly::fbstring &spaceId)
{
    constexpr auto kOnedataGUIDLength{100U};

    folly::fbstring result;
    result.reserve(kOnedataGUIDLength);

    util::base64::base64_url_encode(
        fmt::format(
            "guid#space_{}#{}", spaceId.toStdString(), spaceId.toStdString()),
        result);
    return result;
}

folly::fbstring guidToUUID(
    const folly::fbstring &spaceId, const folly::fbstring &guid)
{
    constexpr auto kOnedataGUIDLength{100U};

    folly::fbstring result;
    result.reserve(kOnedataGUIDLength);

    util::base64::base64_url_encode(
        fmt::format("guid#{}#{}", guid.toStdString(), spaceId.toStdString()),
        result);
    return result;
}
} // namespace uuid
} // namespace util
} // namespace client
} // namespace one
