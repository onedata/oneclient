/**
 * @file removeFileSubscription.cc
 * @author Michal Wrona
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "removeFileSubscription.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

RemoveFileSubscription::RemoveFileSubscription(
    std::string fileUuid_, std::size_t counterThreshold_)
    : Subscription{counterThreshold_}
    , m_fileUuid{std::move(fileUuid_)}
{
}

std::string RemoveFileSubscription::toString() const
{
    std::stringstream stream;
    stream << Subscription::toString("RemoveFileSubscription")
           << ", file UUID: '" << m_fileUuid;
    return stream.str();
}

std::unique_ptr<messages::ProtocolClientMessage>
RemoveFileSubscription::serializeAndDestroy()
{
    auto clientMsg = std::make_unique<messages::ProtocolClientMessage>();
    auto subscriptionMsg = clientMsg->mutable_subscription();
    auto removeFileSubscriptionMsg =
        subscriptionMsg->mutable_remove_file_subscription();

    subscriptionMsg->set_id(m_id);
    removeFileSubscriptionMsg->mutable_file_uuid()->swap(m_fileUuid);

    return clientMsg;
}

} // namespace events
} // namespace client
} // namespace one
