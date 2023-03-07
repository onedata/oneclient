/**
 * @file abortMultipartUpload.h
 * @author Bartek Kryza
 * @copyright (C) 2022 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_ABORT_MULTIPART_UPLOAD_H
#define ONECLIENT_MESSAGES_FUSE_ABORT_MULTIPART_UPLOAD_H

#include "multipartUploadRequest.h"

#include <cstdint>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The AbortMultipartUpload aborts a multipart upload for a specific file.
 */
class AbortMultipartUpload : public MultipartUploadRequest {
public:
    /**
     * Constructor.
     */
    AbortMultipartUpload(std::string id)
        : m_id{std::move(id)}
    {
    }

    std::string toString() const override
    {
        std::stringstream stream;
        stream << "type: 'AbortMultipartUpload', id: " << m_id;
        return stream.str();
    }

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override
    {
        auto msg = MultipartUploadRequest::serializeAndDestroy();
        auto *df = msg->mutable_fuse_request()
                       ->mutable_multipart_upload_request()
                       ->mutable_abort_multipart_upload();

        df->set_multipart_upload_id(m_id);

        return msg;
    }

    std::string m_id;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_ABORT_MULTIPART_UPLOAD_H
