/**
 * @file permissionChangedSubscription.cc
 * @author Tomasz Lichon
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "permissionChangedSubscription.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

PermissionChangedSubscription::PermissionChangedSubscription(std::string fileUuid_)
    : Subscription{}
    , m_fileUuid{std::move(fileUuid_)}
{
}

std::string PermissionChangedSubscription::toString() const
{
    std::stringstream stream;
    stream << Subscription::toString("PermissionChangedSubscription")
           << ", file UUID: '" << m_fileUuid;
    return stream.str();
}

std::unique_ptr<messages::ProtocolClientMessage>
PermissionChangedSubscription::serializeAndDestroy()
{
    auto clientMsg = std::make_unique<messages::ProtocolClientMessage>();
    auto subscriptionMsg = clientMsg->mutable_subscription();
    auto permissionChangedSubscriptionMsg =
        subscriptionMsg->mutable_permission_changed_subscription();

    subscriptionMsg->set_id(m_id);
    permissionChangedSubscriptionMsg->mutable_file_uuid()->swap(m_fileUuid);

    return clientMsg;
}

} // namespace events
} // namespace client
} // namespace one
