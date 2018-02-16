/**
 * @file remoteData.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "remoteData.h"

#include "logging.h"
#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {
namespace proxyio {

RemoteData::RemoteData(std::unique_ptr<ProtocolServerMessage> serverMessage)
    : ProxyIOResponse(serverMessage)
{
    if (!serverMessage->proxyio_response().has_remote_data()) {
        LOG(ERROR) << "Invalid ProxyIOResponse - missing remote_data field";
        throw std::system_error{std::make_error_code(std::errc::protocol_error),
            "remote_data field missing"};
    }

    m_data.swap(*serverMessage->mutable_proxyio_response()
                     ->mutable_remote_data()
                     ->mutable_data());
}

std::string RemoteData::toString() const
{
    std::stringstream stream;
    stream << "type: 'RemoteData', data size: " << m_data.size();
    return stream.str();
}

} // namespace proxyio
} // namespace messages
} // namespace one
