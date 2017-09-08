/**
 * @file remoteWriteResult.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "remoteWriteResult.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {
namespace proxyio {

RemoteWriteResult::RemoteWriteResult(
    std::unique_ptr<ProtocolServerMessage> serverMessage)
    : ProxyIOResponse{serverMessage}
{
    if (!serverMessage->proxyio_response().has_remote_write_result())
        throw std::system_error{std::make_error_code(std::errc::protocol_error),
            "remote_write_result field missing"};

    m_wrote = serverMessage->proxyio_response().remote_write_result().wrote();
}

std::string RemoteWriteResult::toString() const
{
    std::stringstream stream;
    stream << "type: 'RemoteWriteResult', wrote: " << m_wrote;
    return stream.str();
}

} // namespace proxyio
} // namespace messages
} // namespace one
