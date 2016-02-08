/**
 * @file configuration.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_CONFIGURATION_H
#define ONECLIENT_MESSAGES_CONFIGURATION_H

#include "messages/serverMessage.h"
#include "events/subscriptionContainer.h"

#include <memory>
#include <string>

namespace one {
namespace messages {

/**
 * The Configuration class represents a message that is sent by the server to
 * inform the client about configuration.
 */
class Configuration : public ServerMessage {
public:
    /**
     * Constructor.
     */
    Configuration(){};

    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing @c
     * Configuration counterpart.
     */
    Configuration(std::unique_ptr<ProtocolServerMessage> serverMessage);

    /**
     * @return subscription container.
     */
    client::events::SubscriptionContainer subscriptionContainer();

    std::string toString() const override;

private:
    client::events::SubscriptionContainer m_subscriptionContainer;
};

} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_CONFIGURATION_H
