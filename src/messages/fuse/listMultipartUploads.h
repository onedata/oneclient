/**
 * @file listMultipartUploads.h
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
 * The ListMultipartUploads class represents a FUSE request for file children.
 */
class ListMultipartUploads : public MultipartUploadRequest {
public:
    /**
     */
    ListMultipartUploads(const folly::fbstring &spaceId,
        const std::size_t limit, folly::Optional<folly::fbstring> indexToken)
        : m_spaceId{spaceId}
        , m_limit{limit}
        , m_indexToken{indexToken}
    {
    }

    std::string toString() const override
    {
        std::stringstream stream;

        stream << "type: 'ListMultipartUploads', spaceId: '" << m_spaceId
               << "', limit: '" << m_limit << "'";

        if (m_indexToken)
            stream << ", indexToken: '" << m_indexToken.value();

        return stream.str();
    }

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override
    {
        auto msg = MultipartUploadRequest::serializeAndDestroy();
        auto *lmup = msg->mutable_fuse_request()
                         ->mutable_multipart_upload_request()
                         ->mutable_list_multipart_uploads();

        lmup->set_space_id(m_spaceId.toStdString());
        lmup->set_limit(m_limit);

        if (m_indexToken)
            lmup->set_index_token(m_indexToken.value().toStdString());

        return msg;
    }

    folly::fbstring m_spaceId;
    const std::size_t m_limit{1000};
    folly::Optional<folly::fbstring> m_indexToken;
};

} // namespace fuse
} // namespace messages
} // namespace one