/**
 * @file fileRequest.cc
 * @author Mateusz Paciorek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileRequest.h"

#include "messages.pb.h"

namespace one {
namespace messages {
namespace fuse {

FileRequest::FileRequest(
    std::string contextGuid, folly::Optional<bool> extendedDirectIO)
    : m_contextGuid{std::move(contextGuid)}
    , m_extendedDirectIO{std::move(extendedDirectIO)}
{
}

std::unique_ptr<ProtocolClientMessage> FileRequest::serializeAndDestroy()
{
    auto msg = std::make_unique<ProtocolClientMessage>();

    auto cg = msg->mutable_fuse_request()
                  ->mutable_file_request()
                  ->mutable_context_guid();
    cg->swap(m_contextGuid);

    if (m_extendedDirectIO) {
        msg->mutable_fuse_request()
            ->mutable_file_request()
            ->set_extended_direct_io(*m_extendedDirectIO);
    }

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
