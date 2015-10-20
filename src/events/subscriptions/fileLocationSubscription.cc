/**
 * @file fileLocationSubscription.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "events/subscriptions/fileLocationSubscription.h"

#include "messages.pb.h"

#include <boost/optional/optional_io.hpp>

#include <sstream>

namespace one {
namespace client {
namespace events {

FileLocationSubscription::FileLocationSubscription(std::string fileUuid_,
    boost::optional<std::size_t> counterThreshold_,
    boost::optional<std::chrono::milliseconds> timeThreshold_)
    : m_fileUuid{std::move(fileUuid_)}
    , m_counterThreshold{std::move(counterThreshold_)}
    , m_timeThreshold{std::move(timeThreshold_)}

{
}

const std::int64_t FileLocationSubscription::id() const { return m_id; }

void FileLocationSubscription::id(std::int64_t id_) { m_id = id_; }

const std::string FileLocationSubscription::fileUuid() const
{
    return m_fileUuid;
}

const boost::optional<std::size_t> &
FileLocationSubscription::counterThreshold() const
{
    return m_counterThreshold;
}

const boost::optional<std::chrono::milliseconds> &
FileLocationSubscription::timeThreshold() const
{
    return m_timeThreshold;
}

bool FileLocationSubscription::empty() const
{
    return !(m_counterThreshold || m_timeThreshold);
}

std::string FileLocationSubscription::toString() const
{
    std::stringstream stream;

    stream << "type: 'FileLocationSubscription', file UUID: '" << m_fileUuid
           << "', counter threshold: " << m_counterThreshold
           << ", time threshold: ";
    if (m_timeThreshold)
        stream << m_timeThreshold.get().count();
    else
        stream << "--";

    return stream.str();
}

std::unique_ptr<messages::ProtocolClientMessage>
FileLocationSubscription::serialize() const
{
    auto clientMsg = std::make_unique<messages::ProtocolClientMessage>();
    auto subscriptionMsg = clientMsg->mutable_subscription();
    auto fileLocationSubscriptionMsg =
        subscriptionMsg->mutable_file_location_subscription();

    subscriptionMsg->set_id(m_id);
    fileLocationSubscriptionMsg->set_file_uuid(m_fileUuid);

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
