/**
 * @file fileRemovedSubscription.cc
 * @author Michal Wrona
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileRemovedSubscription.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

FileRemovedSubscription::FileRemovedSubscription(
    std::string fileUuid, std::size_t counterThreshold)
    : Subscription{counterThreshold}
    , m_fileUuid{std::move(fileUuid)}
{
}

std::string FileRemovedSubscription::toString() const
{
    std::stringstream stream;
    stream << Subscription::toString("FileRemovedSubscription")
           << ", file UUID: '" << m_fileUuid;
    return stream.str();
}

std::unique_ptr<messages::ProtocolClientMessage>
FileRemovedSubscription::serializeAndDestroy()
{
    auto clientMsg = std::make_unique<messages::ProtocolClientMessage>();
    auto subscriptionMsg = clientMsg->mutable_subscription();
    auto fileRemovedSubscriptionMsg =
        subscriptionMsg->mutable_file_removed_subscription();

    subscriptionMsg->set_id(m_id);
    fileRemovedSubscriptionMsg->mutable_file_uuid()->swap(m_fileUuid);

    return clientMsg;
}

} // namespace events
} // namespace client
} // namespace one
