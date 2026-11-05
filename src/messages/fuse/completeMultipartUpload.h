/**
 * @file completeMultipartUpload.h
 * @author Bartek Kryza
 * @copyright (C) 2022 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_COMPLETE_MULTIPART_UPLOAD_H
#define ONECLIENT_MESSAGES_FUSE_COMPLETE_MULTIPART_UPLOAD_H

#include "multipartUploadRequest.h"

#include <cstdint>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The CompleteMultipartUpload aborts a multipart upload for a specific file.
 */
class CompleteMultipartUpload : public MultipartUploadRequest {
public:
    /**
     * Constructor.
     */
    CompleteMultipartUpload(std::string id)
        : m_id{std::move(id)}
    {
    }

    std::string toString() const override
    {
        std::stringstream stream;
        stream << "type: 'CompleteMultipartUpload', id: " << m_id;
        return stream.str();
    }

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override
    {
        auto msg = MultipartUploadRequest::serializeAndDestroy();
        auto *df = msg->mutable_fuse_request()
                       ->mutable_multipart_upload_request()
                       ->mutable_complete_multipart_upload();

        df->set_multipart_upload_id(m_id);

        return msg;
    }

    std::string m_id;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_COMPLETE_MULTIPART_UPLOAD_H
