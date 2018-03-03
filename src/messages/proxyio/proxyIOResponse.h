/**
 * @file proxyIOResponse.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_MESSAGES_PROXYIO_PROXYIO_RESPONSE_H
#define HELPERS_MESSAGES_PROXYIO_PROXYIO_RESPONSE_H

#include "messages/serverMessage.h"

#include <memory>

namespace one {
namespace messages {
namespace proxyio {

/**
 * The @c ProxyIOResponse class represents a response to a ProxyIO request.
 */
class ProxyIOResponse : public ServerMessage {
public:
    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing
     * @c ProxyIOResponse counterpart.
     * @note The constructor throws an applicable std::system_error exception if
     * received message's status is not OK.
     */
    ProxyIOResponse(
        const std::unique_ptr<ProtocolServerMessage> &serverMessage);

    virtual ~ProxyIOResponse() = default;
};

} // namespace proxyio
} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_PROXYIO_PROXYIO_RESPONSE_H
