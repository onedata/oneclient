/**
 * @file configuration.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_CONFIGURATION_H
#define ONECLIENT_MESSAGES_CONFIGURATION_H

#include "events/subscriptionContainer.h"
#include "messages/serverMessage.h"

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

    /**
     * @return disabled spaces' container.
     */
    std::vector<std::string> disabledSpacesContainer();

    std::string toString() const override;

private:
    client::events::SubscriptionContainer m_subscriptionContainer;
    std::vector<std::string> m_disabledSpacesContainer;
};

} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_CONFIGURATION_H
