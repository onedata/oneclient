/**
 * @file configuration.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "configuration.h"

#include <sstream>

namespace one {
namespace messages {

Configuration::Configuration(
    std::unique_ptr<ProtocolServerMessage> serverMessage)
{
    auto configurationMsg = serverMessage->mutable_configuration();

    m_rootUuid = configurationMsg->root_uuid();

    auto subscriptions = configurationMsg->mutable_subscriptions();
    std::transform(subscriptions->pointer_begin(), subscriptions->pointer_end(),
        std::back_inserter(m_subscriptions),
        [](auto subscription) { return std::move(*subscription); });

    auto disabledSpaces = configurationMsg->mutable_disabled_spaces();
    std::transform(disabledSpaces->pointer_begin(),
        disabledSpaces->pointer_end(), std::back_inserter(m_disabledSpaces),
        [](auto disabled_space) { return std::move(*disabled_space); });
}

const folly::fbstring &Configuration::rootUuid() const { return m_rootUuid; }

const std::vector<clproto::Subscription> &Configuration::subscriptions() const
{
    return m_subscriptions;
}

const std::vector<std::string> &Configuration::disabledSpaces() const
{
    return m_disabledSpaces;
}

std::string Configuration::toString() const
{
    std::stringstream stream;
    stream << "type: 'Configuration', root UUID: '" << m_rootUuid << "'";
    return stream.str();
}

} // namespace messages
} // namespace one
