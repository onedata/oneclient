/**
* @file protocolVersion.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef HELPERS_MESSAGES_PROTOCOL_VERSION_H
#define HELPERS_MESSAGES_PROTOCOL_VERSION_H

#include "messages/serverMessage.h"

#include <memory>

namespace one {
namespace messages {

/**
* The ProtocolVersion class represents a message that is sent by the server to
* inform the client about communication protocol version.
*/
class ProtocolVersion : public ServerMessage {
public:
    /**
    * Constructor.
    * @param serverMessage Protocol Buffers message representing @c
    * ProtocolVersion counterpart.
    */
    ProtocolVersion(std::unique_ptr<ProtocolServerMessage> serverMessage);

    /**
     * @return Communication protocol major version.
     */
    uint32_t major() const;

    /**
     * @return Communication protocol minor version.
     */
    uint32_t minor() const;

private:
    uint32_t m_major;
    uint32_t m_minor;
};

} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_PROTOCOL_VERSION_H
