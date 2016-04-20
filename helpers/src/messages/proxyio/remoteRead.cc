/**
 * @file remoteRead.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "remoteRead.h"

#include "messages.pb.h"

#include <glog/stl_logging.h>

#include <sstream>

namespace one {
namespace messages {
namespace proxyio {

RemoteRead::RemoteRead(std::unordered_map<std::string, std::string> parameters,
    std::string storageId, std::string fileId, const off_t offset,
    const std::size_t size)
    : ProxyIORequest{std::move(parameters), std::move(storageId),
          std::move(fileId)}
    , m_offset{offset}
    , m_size{size}
{
}

std::string RemoteRead::toString() const
{
    std::stringstream stream;
    stream << "type: 'RemoteRead', parameters: " << m_parameters
           << ", storageId: '" << m_storageId << "', fileId: '" << m_fileId
           << "', offset: " << m_offset << ", size: " << m_size;
    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> RemoteRead::serializeAndDestroy()
{
    auto clientMsg = ProxyIORequest::serializeAndDestroy();
    auto readMsg = clientMsg->mutable_proxyio_request()->mutable_remote_read();

    readMsg->set_offset(m_offset);
    readMsg->set_size(m_size);

    return clientMsg;
}

} // namespace proxyio
} // namespace messages
} // namespace one
