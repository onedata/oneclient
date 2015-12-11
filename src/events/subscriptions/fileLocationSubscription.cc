/**
 * @file fileLocationSubscription.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileLocationSubscription.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

FileLocationSubscription::FileLocationSubscription(std::string fileUuid_,
    boost::optional<std::size_t> counterThreshold_,
    boost::optional<std::chrono::milliseconds> timeThreshold_)
    : Subscription{std::move(counterThreshold_), std::move(timeThreshold_)}
    , m_fileUuid{std::move(fileUuid_)}
{
}

std::string FileLocationSubscription::toString() const
{
    std::stringstream stream;
    stream << Subscription::toString("FileLocationSubscription")
           << ", file UUID: '" << m_fileUuid;
    return stream.str();
}

std::unique_ptr<messages::ProtocolClientMessage>
FileLocationSubscription::serializeAndDestroy()
{
    auto clientMsg = std::make_unique<messages::ProtocolClientMessage>();
    auto subscriptionMsg = clientMsg->mutable_subscription();
    auto fileLocationSubscriptionMsg =
        subscriptionMsg->mutable_file_location_subscription();

    subscriptionMsg->set_id(m_id);
    fileLocationSubscriptionMsg->mutable_file_uuid()->swap(m_fileUuid);

    if (m_counterThreshold)
        fileLocationSubscriptionMsg->set_counter_threshold(
            m_counterThreshold.get());
    if (m_timeThreshold)
        fileLocationSubscriptionMsg->set_time_threshold(
            m_timeThreshold.get().count());

    return clientMsg;
}

} // namespace events
} // namespace client
} // namespace one
