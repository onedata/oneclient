/**
 * @file createMultipartUpload.h
 * @author Bartek Kryza
 * @copyright (C) 2022 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_CREATE_MULTIPART_UPLOAD_H
#define ONECLIENT_MESSAGES_FUSE_CREATE_MULTIPART_UPLOAD_H

#include "multipartUploadRequest.h"

#include <cstdint>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The CreateMultipartUpload initiates a multipart upload for a specific file.
 */
class CreateMultipartUpload : public MultipartUploadRequest {
public:
    /**
     * Constructor.
     */
    CreateMultipartUpload(std::string spaceId, std::string path)
        : m_spaceId{std::move(spaceId)}
        , m_path{std::move(path)}
    {
    }

    std::string toString() const override
    {
        std::stringstream stream;
        stream << "type: 'CreateMultipartUpload', spaceId: " << m_spaceId
               << ", path: " << m_path;
        return stream.str();
    }

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override
    {
        auto msg = MultipartUploadRequest::serializeAndDestroy();
        auto *df = msg->mutable_fuse_request()
                       ->mutable_multipart_upload_request()
                       ->mutable_create_multipart_upload();

        df->set_space_id(m_spaceId);
        df->set_path(m_path);

        return msg;
    }

    std::string m_spaceId;
    std::string m_path;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_CREATE_MULTIPART_UPLOAD_H
