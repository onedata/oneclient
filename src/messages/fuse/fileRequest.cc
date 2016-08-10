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

FileRequest::FileRequest(std::string contextGuid)
    : m_contextGuid{std::move(contextGuid)}
{
}

std::unique_ptr<ProtocolClientMessage> FileRequest::serializeAndDestroy()
{
    auto msg = std::make_unique<ProtocolClientMessage>();

    auto cg = msg->mutable_fuse_request()
                  ->mutable_file_request()
                  ->mutable_context_guid();
    cg->swap(m_contextGuid);

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
