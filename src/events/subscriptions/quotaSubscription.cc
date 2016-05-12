/**
 * @file quotaSubscription.cc
 * @author Rafal Slota
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "quotaSubscription.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {


QuotaSubscription::QuotaSubscription() 
  : Subscription{1}
{
}

std::string QuotaSubscription::toString() const
{
    std::stringstream stream;
    stream << Subscription::toString("QuotaSubscription");
    return stream.str();
}

std::unique_ptr<messages::ProtocolClientMessage>
QuotaSubscription::serializeAndDestroy()
{
    auto clientMsg = std::make_unique<messages::ProtocolClientMessage>();
    auto subscriptionMsg = clientMsg->mutable_subscription();
    subscriptionMsg->mutable_quota_subscription();

    subscriptionMsg->set_id(m_id);

    return clientMsg;
}

} // namespace events
} // namespace client
} // namespace one
