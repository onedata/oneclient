/**
 * @file multipartUpload.h
 * @author Bartek Kryza
 * @copyright (C) 2022 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "fuseResponse.h"

#include "messages.pb.h"
#include "spdlog/spdlog.h"

#include <sstream>
#include <system_error>

namespace one {
namespace messages {
namespace fuse {

/**
 * The @c MultipartUpload represents the response to a CreateMultipartUpload
 * request.
 */
class MultipartUpload : public FuseResponse {
public:
    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing
     * @c MultipartUpload response to a readlink request.
     */
    MultipartUpload(std::unique_ptr<ProtocolServerMessage> serverMessage)
        : FuseResponse{serverMessage}
    {
        if (!serverMessage->fuse_response().has_multipart_upload()) {
            throw std::system_error{
                std::make_error_code(std::errc::protocol_error),
                "multipart upload field missing"};
        }

        auto *multipartUpload =
            serverMessage->mutable_fuse_response()->mutable_multipart_upload();

        m_id = multipartUpload->multipart_upload_id();
    }

    MultipartUpload(
        const std::string &id, const std::string &path, const time_t timestamp)
        : m_id{id}
        , m_path{path}
        , m_timestamp{timestamp}
    {
    }

    /**
     * @return The id of the multipart upload.
     */
    const std::string &id() const { return m_id; }

    const std::string &path() const { return m_path; }

    time_t timestamp() const { return m_timestamp; }

    std::string toString() const override
    {
        return fmt::format(
            "type: 'MultipartUpload', multipart_upload_id: '{}'", m_id);
    }

private:
    std::string m_id;
    std::string m_path;
    time_t m_timestamp;
};

} // namespace fuse
} // namespace messages
} // namespace one
