/**
 * @file protocolVersion.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "protocolVersion.h"

#include "messages.pb.h"

#include <sstream>

#ifdef major
#undef major
#endif
#ifdef minor
#undef minor
#endif

namespace one {
namespace messages {

ProtocolVersion::ProtocolVersion(
    std::unique_ptr<ProtocolServerMessage> serverMessage)
{
    auto &protocolVersionMsg = serverMessage->protocol_version();
    m_major = protocolVersionMsg.major();
    m_minor = protocolVersionMsg.minor();
}

uint32_t ProtocolVersion::majorVersion() const { return m_major; }

uint32_t ProtocolVersion::minorVersion() const { return m_minor; }

std::string ProtocolVersion::toString() const
{
    std::stringstream stream;
    stream << "type: 'ProtocolVersion', major: " << m_major
           << ", minor: " << m_minor;
    return stream.str();
}

} // namespace messages
} // namespace one
