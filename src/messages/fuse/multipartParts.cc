/**
 * @file multipartParts.cc
 * @author Bartek Kryza
 * @copyright (C) 2022 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "multipartParts.h"

#include "messages.pb.h"

#include <folly/Range.h>

#include <algorithm>
#include <sstream>
#include <system_error>

namespace one {
namespace messages {
namespace fuse {

MultipartParts::MultipartParts(
    std::unique_ptr<ProtocolServerMessage> serverMessage)
{
    if (!serverMessage->fuse_response().has_multipart_parts())
        throw std::system_error{std::make_error_code(std::errc::protocol_error),
            "multipart_parts field missing"};

    auto *msg =
        serverMessage->mutable_fuse_response()->mutable_multipart_parts();

    auto *parts = msg->mutable_parts();

    if (msg->has_is_last()) {
        m_isLast = msg->has_is_last();
    }

    for (const auto &partMsg :
        folly::range(parts->pointer_begin(), parts->pointer_end())) {
        m_parts.emplace_back(MultipartPart{*partMsg});
    }
}

/**
 * @return A list of directory's children, specified by their UUID and
 * filename.
 */
const folly::fbvector<MultipartPart> &MultipartParts::parts() const
{
    return m_parts;
}

const bool MultipartParts::isLast() const { return m_isLast; }

folly::Optional<size_t> MultipartParts::nextPartMarker() const
{
    return m_nextPartMarker;
}

std::string MultipartParts::toString() const
{
    std::stringstream stream;

    stream << "type: 'MultipartParts', isLast: '"
           << (m_isLast ? "true" : "false");

    if (m_nextPartMarker)
        stream << "', nextPartMarker: '" << m_nextPartMarker.value();

    stream << "', parts: [";

    for (const auto &part : m_parts)
        stream << part.toString() << " ";

    stream << "]";

    return stream.str();
}

} // namespace fuse
} // namespace messages
} // namespace one
