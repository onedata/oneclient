/**
 * @file listMultipartParts.h
 * @author Bartek Kryza
 * @copyright (C) 2022 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "multipartUploadRequest.h"

#include <folly/FBString.h>
#include <folly/Optional.h>

#include <sys/types.h>

#include <cstdint>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The ListMultipartParts class represents a FUSE request for file children.
 */
class ListMultipartParts : public MultipartUploadRequest {
public:
    /**
     */
    ListMultipartParts(const folly::fbstring &uploadId, const std::size_t limit,
        folly::Optional<size_t> partMarker)
        : m_uploadId{uploadId}
        , m_limit{limit}
        , m_partMarker{partMarker}
    {
    }

    std::string toString() const override
    {
        std::stringstream stream;

        stream << "type: 'ListMultipartParts', uploadId: '" << m_uploadId
               << "'";

        if (m_partMarker)
            stream << ", partMarker: '" << m_partMarker.value();

        stream << "', limit: '" << m_limit << "'";

        return stream.str();
    }

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override
    {
        auto msg = MultipartUploadRequest::serializeAndDestroy();
        auto *lmup = msg->mutable_fuse_request()
                         ->mutable_multipart_upload_request()
                         ->mutable_list_multipart_parts();

        lmup->set_multipart_upload_id(m_uploadId.toStdString());

        lmup->set_limit(m_limit);

        if (m_partMarker)
            lmup->set_part_marker(m_partMarker.value());

        return msg;
    }

    const folly::fbstring m_uploadId;
    const std::size_t m_limit{1000};
    folly::Optional<size_t> m_partMarker;
};

} // namespace fuse
} // namespace messages
} // namespace one