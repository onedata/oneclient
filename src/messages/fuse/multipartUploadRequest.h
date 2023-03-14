/**
 * @file multipartUploadRequest.h
 * @author Bartek Kryza
 * @copyright (C) 2022 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_MULTIPART_UPLOAD_REQUEST_H
#define ONECLIENT_MESSAGES_FUSE_MULTIPART_UPLOAD_REQUEST_H

#include "messages/clientMessage.h"

#include <boost/filesystem/path.hpp>
#include <folly/Optional.h>

#include <memory>
#include <sstream>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The @c MultipartUploadRequest class represents FUSE request targeting file.
 */
class MultipartUploadRequest : public ClientMessage {
public:
    /**
     * Constructor.
     */
    MultipartUploadRequest() = default;

    virtual ~MultipartUploadRequest() = default;

protected:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override
    {
        auto msg = std::make_unique<ProtocolClientMessage>();

        msg->mutable_fuse_request()->mutable_multipart_upload_request();

        return msg;
    }
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_MULTIPART_UPLOAD_REQUEST_H
