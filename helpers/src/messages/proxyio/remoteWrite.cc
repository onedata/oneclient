/**
 * @file remoteWrite.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "remoteWrite.h"

#include "messages.pb.h"

#include <glog/stl_logging.h>

#include <sstream>

namespace one {
namespace messages {
namespace proxyio {

RemoteWrite::RemoteWrite(
    std::unordered_map<std::string, std::string> parameters,
    std::string storageId, std::string fileId, const off_t offset,
    asio::const_buffer data)
    : ProxyIORequest{std::move(parameters), std::move(storageId),
          std::move(fileId)}
    , m_offset{offset}
    , m_data{data}
{
}

std::string RemoteWrite::toString() const
{
    std::stringstream stream;
    stream << "type: 'RemoteWrite', parameters: " << m_parameters
           << ", storageId: '" << m_storageId << "', fileId: '" << m_fileId
           << "', offset: " << m_offset
           << ", data size: " << asio::buffer_size(m_data);
    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> RemoteWrite::serializeAndDestroy()
{
    auto clientMsg = ProxyIORequest::serializeAndDestroy();
    auto writeMsg =
        clientMsg->mutable_proxyio_request()->mutable_remote_write();

    writeMsg->set_offset(m_offset);
    writeMsg->mutable_data()->assign(
        asio::buffer_cast<const char *>(m_data), asio::buffer_size(m_data));

    return clientMsg;
}

} // namespace proxyio
} // namespace messages
} // namespace one
