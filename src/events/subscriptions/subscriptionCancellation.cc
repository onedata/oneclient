/**
 * @file SubscriptionCancellation.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "subscriptionCancellation.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

SubscriptionCancellation::SubscriptionCancellation(
    const ProtocolMessage &message)
    : m_id{message.id()}
{
}

SubscriptionCancellation::SubscriptionCancellation(std::int64_t id_)
    : m_id{id_}
{
}

std::int64_t SubscriptionCancellation::id() const { return m_id; }

std::string SubscriptionCancellation::toString() const
{
    std::stringstream stream;
    stream << "type: 'SubscriptionCancellation', id: " << m_id;
    return stream.str();
}

std::unique_ptr<messages::ProtocolClientMessage>
SubscriptionCancellation::serialize() const
{
    auto clientMsg = std::make_unique<messages::ProtocolClientMessage>();
    auto subscriptionCancellationMsg =
        clientMsg->mutable_subscription_cancellation();

    subscriptionCancellationMsg->set_id(m_id);

    return clientMsg;
}

} // namespace events
} // namespace client
} // namespace one
