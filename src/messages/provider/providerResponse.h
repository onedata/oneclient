/**
 * @file providerResponse.h
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "messages/serverMessage.h"

#include <memory>
#include <string>
#include <vector>

namespace one {
namespace messages {
namespace provider {

/**
 * The ProviderResponse class represents a response to a provider request
 * sent by the client.
 */
class ProviderResponse : public ServerMessage {
public:
    ProviderResponse() = default;

    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing
     * @c ProviderResponse counterpart.
     * @note The constructor throws an applicable std::system_error exception if
     * received message's status is not OK.
     */
    ProviderResponse(
        const std::unique_ptr<ProtocolServerMessage> &serverMessage);

    virtual std::string toString() const override;
};

} // namespace provider
} // namespace messages
} // namespace one
