/**
 * @file fileRenamedSubscription.cc
 * @author Mateusz Paciorek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileRenamedSubscription.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

FileRenamedSubscription::FileRenamedSubscription(
    std::string fileUuid, std::size_t counterThreshold_)
    : Subscription{counterThreshold_}
    , m_fileUuid{std::move(fileUuid)}
{
}

std::string FileRenamedSubscription::toString() const
{
    std::stringstream stream;
    stream << Subscription::toString("FileRenamedSubscription")
           << ", file UUID: '" << m_fileUuid;
    return stream.str();
}

std::unique_ptr<messages::ProtocolClientMessage>
FileRenamedSubscription::serializeAndDestroy()
{
    auto clientMsg = std::make_unique<messages::ProtocolClientMessage>();
    auto subscriptionMsg = clientMsg->mutable_subscription();
    auto fileRenamedSubscriptionMsg =
        subscriptionMsg->mutable_file_renamed_subscription();

    subscriptionMsg->set_id(m_id);
    fileRenamedSubscriptionMsg->mutable_file_uuid()->swap(m_fileUuid);

    return clientMsg;
}

} // namespace events
} // namespace client
} // namespace one
