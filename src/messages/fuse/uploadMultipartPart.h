/**
 * @file uploadMultipartPart.h
 * @author Bartek Kryza
 * @copyright (C) 2022 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_UPLOAD_MULTIPART_PART_H
#define ONECLIENT_MESSAGES_FUSE_UPLOAD_MULTIPART_PART_H

#include "multipartPart.h"
#include "multipartUploadRequest.h"

#include <cstdint>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The UploadMultipartPart initiates a multipart upload for a specific file.
 */
class UploadMultipartPart : public MultipartUploadRequest {
public:
    /**
     * Constructor.
     */
    UploadMultipartPart(std::string uploadId, std::string etag,
        uint64_t modified, size_t number, size_t size)
        : m_uploadId{std::move(uploadId)}
        , m_etag{std::move(etag)}
        , m_modified{modified}
        , m_number{number}
        , m_size{size}
    {
    }

    std::string toString() const override
    {
        std::stringstream stream;
        stream << "type: 'UploadMultipartPart', uploadId: " << m_uploadId
               << ", etag: " << m_etag << ", modified: " << m_modified
               << ", number: " << m_number << ", size: " << m_size;

        return stream.str();
    }

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override
    {
        auto msg = MultipartUploadRequest::serializeAndDestroy();
        auto *ump = msg->mutable_fuse_request()
                        ->mutable_multipart_upload_request()
                        ->mutable_upload_multipart_part();

        ump->set_multipart_upload_id(m_uploadId);

        ump->mutable_part()->set_etag(m_etag);
        ump->mutable_part()->set_last_modified(m_modified);
        ump->mutable_part()->set_number(m_number);
        ump->mutable_part()->set_size(m_size);

        return msg;
    }

    std::string m_uploadId;
    std::string m_etag;
    uint64_t m_modified;
    size_t m_number;
    size_t m_size;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_UPLOAD_MULTIPART_PART_H
