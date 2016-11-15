/**
 * @file configuration.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "configuration.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {

Configuration::Configuration(
    std::unique_ptr<ProtocolServerMessage> serverMessage)
{
    auto configurationMsg = serverMessage->mutable_configuration();

    m_rootUuid = configurationMsg->root_uuid();

    for (const auto &subscription : configurationMsg->subscriptions())
        m_subscriptionContainer.add(subscription);

    auto disabledSpaces = configurationMsg->mutable_disabled_spaces();
    std::transform(disabledSpaces->pointer_begin(),
        disabledSpaces->pointer_end(),
        std::back_inserter(m_disabledSpacesContainer),
        [](auto disabled_space) { return std::move(*disabled_space); });
}

client::events::SubscriptionContainer Configuration::subscriptionContainer()
{
    return m_subscriptionContainer;
}

std::vector<std::string> Configuration::disabledSpacesContainer()
{
    return m_disabledSpacesContainer;
}

std::string Configuration::toString() const
{
    std::stringstream stream;
    stream << "type: 'Configuration', root UUID: '" << m_rootUuid
           << "' subscriptions: " << m_subscriptionContainer.toString();
    return stream.str();
}

} // namespace messages
} // namespace one
