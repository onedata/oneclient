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
    std::string storageId, std::string fileId,
    std::vector<std::pair<off_t, std::string>> data)
    : ProxyIORequest{std::move(parameters), std::move(storageId),
          std::move(fileId)}
    , m_data{std::move(data)}
{
}

std::string RemoteWrite::toString() const
{
    std::stringstream stream;
    stream << "type: 'RemoteWrite', parameters: '" << m_parameters
           << "', storageId: '" << m_storageId << "', fileId: '" << m_fileId
           << "', byte sequences: " << m_data.size();
    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> RemoteWrite::serializeAndDestroy()
{
    auto clientMsg = ProxyIORequest::serializeAndDestroy();
    auto writeMsg =
        clientMsg->mutable_proxyio_request()->mutable_remote_write();

    for (auto &e : m_data) {
        auto byteSequence = writeMsg->add_byte_sequence();
        byteSequence->set_offset(e.first);
        byteSequence->mutable_data()->swap(e.second);
    }

    return clientMsg;
}

} // namespace proxyio
} // namespace messages
} // namespace one
