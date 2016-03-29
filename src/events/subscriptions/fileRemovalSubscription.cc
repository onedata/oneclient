/**
 * @file fileRemovalSubscription.cc
 * @author Michal Wrona
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileRemovalSubscription.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

FileRemovalSubscription::FileRemovalSubscription(
    std::string fileUuid_, std::size_t counterThreshold_)
    : Subscription{counterThreshold_}
    , m_fileUuid{std::move(fileUuid_)}
{
}

std::string FileRemovalSubscription::toString() const
{
    std::stringstream stream;
    stream << Subscription::toString("FileRemovalSubscription")
           << ", file UUID: '" << m_fileUuid;
    return stream.str();
}

std::unique_ptr<messages::ProtocolClientMessage>
FileRemovalSubscription::serializeAndDestroy()
{
    auto clientMsg = std::make_unique<messages::ProtocolClientMessage>();
    auto subscriptionMsg = clientMsg->mutable_subscription();
    auto fileRemovalSubscriptionMsg =
        subscriptionMsg->mutable_file_removal_subscription();

    subscriptionMsg->set_id(m_id);
    fileRemovalSubscriptionMsg->mutable_file_uuid()->swap(m_fileUuid);

    return clientMsg;
}

} // namespace events
} // namespace client
} // namespace one
