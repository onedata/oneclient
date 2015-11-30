/**
 * @file fileAttrSubscription.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "events/subscriptions/fileAttrSubscription.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

FileAttrSubscription::FileAttrSubscription(std::string fileUuid_,
    boost::optional<std::size_t> counterThreshold_,
    boost::optional<std::chrono::milliseconds> timeThreshold_)
    : Subscription{std::move(counterThreshold_), std::move(timeThreshold_)}
    , m_fileUuid{std::move(fileUuid_)}
{
}

std::string FileAttrSubscription::toString() const
{
    std::stringstream stream;
    stream << Subscription::toString("FileAttrSubscription") << ", file UUID: '"
           << m_fileUuid;
    return stream.str();
}

std::unique_ptr<messages::ProtocolClientMessage>
FileAttrSubscription::serialize() const
{
    auto clientMsg = std::make_unique<messages::ProtocolClientMessage>();
    auto subscriptionMsg = clientMsg->mutable_subscription();
    auto fileAttrSubscriptionMsg =
        subscriptionMsg->mutable_file_attr_subscription();

    subscriptionMsg->set_id(m_id);
    fileAttrSubscriptionMsg->set_file_uuid(m_fileUuid);

    if (m_counterThreshold)
        fileAttrSubscriptionMsg->set_counter_threshold(
            m_counterThreshold.get());
    if (m_timeThreshold)
        fileAttrSubscriptionMsg->set_time_threshold(
            m_timeThreshold.get().count());

    return clientMsg;
}

} // namespace events
} // namespace client
} // namespace one
