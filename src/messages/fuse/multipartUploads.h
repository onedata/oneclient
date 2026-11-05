/**
 * @file multipartUploads.h
 * @author Bartek Kryza
 * @copyright (C) 2022 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "fuseResponse.h"

#include "multipartUpload.h"

#include <folly/FBString.h>
#include <folly/FBVector.h>
#include <folly/Optional.h>
#include <folly/Range.h>

#include <memory>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The MultipartUploads class represents server-sent file children list.
 */
class MultipartUploads : public FuseResponse {
public:
    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing
     * @c MultipartUploads counterpart.
     */
    MultipartUploads(std::unique_ptr<ProtocolServerMessage> serverMessage)
    {
        if (!serverMessage->fuse_response().has_multipart_uploads())
            throw std::system_error{
                std::make_error_code(std::errc::protocol_error),
                "multipart_uploads field missing"};

        auto *msg =
            serverMessage->mutable_fuse_response()->mutable_multipart_uploads();

        auto *uploads = msg->mutable_uploads();

        if (msg->has_is_last()) {
            m_isLast = msg->has_is_last();
        }

        if (msg->has_next_page_token()) {
            m_nextPageToken = msg->next_page_token();
        }

        for (auto &uploadMsg :
            folly::range(uploads->pointer_begin(), uploads->pointer_end())) {

            m_uploads.emplace_back(
                MultipartUpload{uploadMsg->multipart_upload_id(),
                    uploadMsg->path(), uploadMsg->creation_time()});
        }
    }

    /**
     * @return A list current users ongoing uploads.
     */
    const folly::fbvector<MultipartUpload> &uploads() const
    {
        return m_uploads;
    }

    const bool isLast() const { return m_isLast; }

    folly::Optional<folly::fbstring> indexToken() const
    {
        return m_nextPageToken;
    }

    std::string toString() const override
    {
        std::stringstream stream;

        stream << "type: 'MultipartUploads', isLast: '"
               << (m_isLast ? "true" : "false");

        if (m_nextPageToken)
            stream << "', nextPageToken: '" << m_nextPageToken.value();

        stream << "', uploads: [";

        for (const auto &upload : m_uploads)
            stream << upload.toString() << " ";

        stream << "]";

        return stream.str();
    }

private:
    folly::fbvector<MultipartUpload> m_uploads;
    bool m_isLast{false};
    folly::Optional<folly::fbstring> m_nextPageToken;
};

} // namespace fuse
} // namespace messages
} // namespace one
